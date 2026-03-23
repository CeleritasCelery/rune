//! Heap dump module for rune.
//! After a successful bootstrap in which elisp code is converted to bytecode,
//! the VM heap memory can be serialized to a rune.pdmp file, and
//! subsequent runs can avoid bootstrapping and just load rune.pdmp instead.
//!
//! The idea is based on emacs' portable dumper (pdump) which replaced the older unexec.
//! However, this pdmp format is a somewhat human-readable text format and makes no attempt to
//! have any compatibility with unexec or pdump.
//! https://lwn.net/Articles/707619/
//! https://github.com/emacs-mirror/emacs/blob/master/src/pdumper.h
use crate::core::{
    env::{Env, INTERNED_SYMBOLS},
    gc::{Context, Rt},
    object::{NIL, Object, ObjectType},
};
use std::collections::HashMap;
use std::fmt::{self, Display, Write};
use std::path::Path;

/// Unique id for each object pointer, used to handle cycles and sharing.
type ObjId = u32;

/// A serialized representation of a single GC object, decoupled from live
/// pointers. Children are referenced by ObjId rather than raw addresses.
enum DumpedObject {
    Nil,
    Int(i64),
    Float(f64),
    String(std::string::String),
    ByteString(Vec<u8>),
    Symbol {
        name: std::string::String,
        interned: bool,
    },
    Cons {
        car: ObjId,
        cdr: ObjId,
    },
    Vec(Vec<ObjId>),
    ByteFn {
        args: u64,
        depth: usize,
        codes: Vec<u8>,
        consts: Vec<ObjId>,
    },
    /// SubrFn contains a Rust fn pointer which can't be serialized.
    /// We store only the name during serialization; the loader will
    /// recover the function pointer by name lookup.
    Subr(std::string::String),
    Record(Vec<ObjId>),
    HashTable(Vec<(ObjId, ObjId)>),
    Buffer,
    CharTable,
    BigInt(std::string::String),
    ChannelSender,
    ChannelReceiver,
}

/// An entry in the symbol table section: name -> (symbol object id, optional function object id)
struct DumpedSymbol {
    name: std::string::String,
    sym_id: ObjId,
    func_id: Option<ObjId>,
}

/// An entry in the env section: a variable binding from symbol to value.
struct DumpedBinding {
    sym_id: ObjId,
    val_id: ObjId,
}

struct DumpState {
    /// Maps live pointer address -> assigned object id (handles cycles + dedup)
    seen: HashMap<usize, ObjId>,
    objects: Vec<DumpedObject>,
    symbols: Vec<DumpedSymbol>,
    env: Vec<DumpedBinding>,
}

impl Display for DumpedObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Int(x) => write!(f, "int {x}"),
            Self::Float(x) => write!(f, "float {x}"),
            Self::String(s) => write!(f, "string \"{}\"", escape_str(s)),
            Self::ByteString(bytes) => {
                write!(f, "bytestring #")?;
                for b in bytes {
                    write!(f, "{b:02x}")?;
                }
                Ok(())
            }
            Self::Symbol { name, interned } => {
                write!(f, "symbol \"{}\" interned={interned}", escape_str(name))
            }
            Self::Cons { car, cdr } => write!(f, "cons car=@{car} cdr=@{cdr}"),
            Self::Vec(elems) => {
                write!(f, "vec [")?;
                fmt_id_list(f, elems)?;
                write!(f, "]")
            }
            Self::ByteFn { args, depth, codes, consts } => {
                write!(f, "bytefn args={args} depth={depth} codes=#")?;
                for b in codes {
                    write!(f, "{b:02x}")?;
                }
                write!(f, " consts=[")?;
                fmt_id_list(f, consts)?;
                write!(f, "]")
            }
            Self::Subr(name) => write!(f, "subr \"{}\"", escape_str(name)),
            Self::Record(elems) => {
                write!(f, "record [")?;
                fmt_id_list(f, elems)?;
                write!(f, "]")
            }
            Self::HashTable(entries) => {
                write!(f, "hashtable [")?;
                for (i, (k, v)) in entries.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{k}:{v}")?;
                }
                write!(f, "]")
            }
            Self::Buffer => write!(f, "buffer <opaque>"),
            Self::CharTable => write!(f, "chartable <opaque>"),
            Self::BigInt(s) => write!(f, "bigint {s}"),
            Self::ChannelSender => write!(f, "channel-sender <opaque>"),
            Self::ChannelReceiver => write!(f, "channel-receiver <opaque>"),
        }
    }
}

fn fmt_id_list(f: &mut fmt::Formatter<'_>, ids: &[ObjId]) -> fmt::Result {
    for (i, id) in ids.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write!(f, "@{id}")?;
    }
    Ok(())
}

impl DumpState {
    fn new() -> Self {
        Self { seen: HashMap::new(), objects: Vec::new(), symbols: Vec::new(), env: Vec::new() }
    }

    /// Serialize an object and return its id. If already visited (cycle or
    /// shared reference), returns the cached id immediately.
    fn dump_object(&mut self, obj: Object) -> ObjId {
        let addr = obj.into_raw().addr();

        // nil is special: always id 0
        if obj.ptr_eq(NIL) {
            if !self.seen.contains_key(&addr) {
                let id = self.alloc_id(DumpedObject::Nil);
                self.seen.insert(addr, id);
            }
            return self.seen[&addr];
        }

        // If already visited -> return cached id (handles cycles + sharing)
        if let Some(&id) = self.seen.get(&addr) {
            return id;
        }

        // Pre-insert before recursing into children to break cycles.
        // If a child points back to this object, dump_object will find
        // it in `seen` and return the id without infinite recursion.
        let id = self.alloc_id(DumpedObject::Nil);
        self.seen.insert(addr, id);

        let dumped = match obj.untag() {
            ObjectType::Int(x) => DumpedObject::Int(x),
            ObjectType::Float(x) => DumpedObject::Float(**x),
            ObjectType::String(s) => DumpedObject::String(s.inner().to_owned()),
            ObjectType::ByteString(s) => DumpedObject::ByteString(s.to_vec()),
            ObjectType::Symbol(sym) => {
                DumpedObject::Symbol { name: sym.name().to_owned(), interned: sym.interned() }
            }
            ObjectType::Cons(cons) => {
                let car = self.dump_object(cons.car());
                let cdr = self.dump_object(cons.cdr());
                DumpedObject::Cons { car, cdr }
            }
            ObjectType::Vec(vec) => {
                let elems = vec.iter().map(|cell| self.dump_object(cell.get())).collect();
                DumpedObject::Vec(elems)
            }
            ObjectType::ByteFn(bf) => {
                // Bytecode is trivially serializable; the constants
                // vector contains arbitrary Objects so we recurse into each.
                let consts = bf.consts().iter().map(|c| self.dump_object(*c)).collect();
                DumpedObject::ByteFn {
                    args: bf.args.into_arg_spec(),
                    depth: bf.depth,
                    codes: bf.codes().to_vec(),
                    consts,
                }
            }
            ObjectType::SubrFn(subr) => DumpedObject::Subr(subr.name.to_owned()),
            ObjectType::HashTable(ht) => {
                let entries: Vec<(ObjId, ObjId)> = (0..ht.len())
                    .filter_map(|i| ht.get_index(i))
                    .map(|(k, v)| (self.dump_object(k), self.dump_object(v)))
                    .collect();
                DumpedObject::HashTable(entries)
            }
            ObjectType::Record(rec) => {
                let elems = rec.iter().map(|cell| self.dump_object(cell.get())).collect();
                DumpedObject::Record(elems)
            }
            ObjectType::Buffer(_) => DumpedObject::Buffer,
            ObjectType::CharTable(_) => DumpedObject::CharTable,
            ObjectType::BigInt(n) => DumpedObject::BigInt(n.to_string()),
            ObjectType::ChannelSender(_) => DumpedObject::ChannelSender,
            ObjectType::ChannelReceiver(_) => DumpedObject::ChannelReceiver,
        };

        // Overwrite the Nil placeholder with the real object
        self.objects[id as usize] = dumped;
        id
    }

    fn alloc_id(&mut self, placeholder: DumpedObject) -> ObjId {
        let id = self.objects.len() as ObjId;
        self.objects.push(placeholder);
        id
    }

    /// Walk the global symbol table and serialize each symbol + its function cell.
    fn dump_symbols(&mut self, cx: &Context) {
        let map = INTERNED_SYMBOLS.lock().unwrap();
        for (name, sym) in map.iter() {
            let sym_obj: Object = sym.into();
            let sym_id = self.dump_object(sym_obj);
            let func_id = if sym.has_func() {
                let func = sym.func(cx).unwrap();
                Some(self.dump_object(func.as_obj()))
            } else {
                None
            };
            self.symbols.push(DumpedSymbol { name: name.to_owned(), sym_id, func_id });
        }
    }

    /// Serialize the Env variable bindings (symbol -> value mappings).
    fn dump_env(&mut self, env: &Rt<Env>, _cx: &Context) {
        for (sym_slot, val_slot) in env.vars.iter() {
            // Slot<Symbol> derefs to Symbol, Slot<Object> derefs to Object.
            // Double-deref: &Slot<T> -> &T -> T (Copy).
            let sym_obj: Object = (**sym_slot).into();
            let val: Object = **val_slot;
            let sym_id = self.dump_object(sym_obj);
            let val_id = self.dump_object(val);
            self.env.push(DumpedBinding { sym_id, val_id });
        }
    }

    fn into_output(self) -> std::string::String {
        let mut out = std::string::String::new();
        writeln!(out, ".HEADER").unwrap();
        writeln!(out, "  version 1").unwrap();
        writeln!(out, "  objects {}", self.objects.len()).unwrap();
        writeln!(out).unwrap();

        writeln!(out, ".OBJECTS").unwrap();
        for (id, obj) in self.objects.iter().enumerate() {
            writeln!(out, "  @{id} {obj}").unwrap();
        }
        writeln!(out).unwrap();

        writeln!(out, ".SYMBOLS").unwrap();
        for sym in &self.symbols {
            let escaped = escape_str(&sym.name);
            match sym.func_id {
                Some(fid) => writeln!(out, "  \"{escaped}\" -> @{} func=@{fid}", sym.sym_id),
                None => writeln!(out, "  \"{escaped}\" -> @{}", sym.sym_id),
            }
            .unwrap();
        }
        writeln!(out).unwrap();

        writeln!(out, ".ENV").unwrap();
        for b in &self.env {
            writeln!(out, "  @{} = @{}", b.sym_id, b.val_id).unwrap();
        }
        out
    }
}

fn escape_str(s: &str) -> std::string::String {
    let mut out = std::string::String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c => out.push(c),
        }
    }
    out
}

pub(crate) fn dump_to_file(path: &Path, env: &Rt<Env>, cx: &Context) -> Result<(), std::io::Error> {
    let mut state = DumpState::new();
    state.dump_symbols(cx);
    state.dump_env(env, cx);
    let output = state.into_output();
    std::fs::write(path, output)
}
