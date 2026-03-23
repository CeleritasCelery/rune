///! Heap dump module for rune.
///! After a successful bootstrap in which elisp code is converted to bytecode,
///! the VM heap memory can be serialized to a rune.pdmp file, and
///! subsequent runs can avoid bootstrapping and just load rune.pdmp instead.
///!
///! The idea is based on emacs' portable dumper (pdump) which replaced the older unexec.
///! However, this pdmp format is a somewhat human-readable text format and makes no attempt to
///! have any compatibility with unexec or pdump.
///! https://lwn.net/Articles/707619/
///! https://github.com/emacs-mirror/emacs/blob/master/src/pdumper.h

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
    Cons { car: ObjId, cdr: ObjId },
    Unimplemented,
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
            Self::Cons { car, cdr } => write!(f, "cons car=@{car} cdr=@{cdr}"),
            Self::Unimplemented  => write!(f, "unimplemented"),
        }
    }
}

impl DumpState {
    fn new() -> Self {
        Self {
            seen: HashMap::new(),
            objects: Vec::new(),
            symbols: Vec::new(),
            env: Vec::new(),
        }
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
            ObjectType::Cons(cons) => {
                let car = self.dump_object(cons.car());
                let cdr = self.dump_object(cons.cdr());
                DumpedObject::Cons { car, cdr }
            }
            _ => {
                DumpedObject::Nil
            }
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
            self.symbols.push(DumpedSymbol {
                name: name.to_owned(),
                sym_id,
                func_id,
            });
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

pub(crate) fn dump_to_file(
    path: &Path,
    env: &Rt<Env>,
    cx: &Context,
) -> Result<(), std::io::Error> {
    let mut state = DumpState::new();
    state.dump_symbols(cx);
    state.dump_env(env, cx);
    let output = state.into_output();
    std::fs::write(path, output)
}
