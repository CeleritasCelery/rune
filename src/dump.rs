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
use std::fmt::Write;
use std::path::Path;

/// Unique id for each object pointer, used to handle cycles and sharing.
type ObjId = u32;

struct DumpState {
    /// Maps live pointer address -> assigned object id (handles cycles + dedup)
    seen: HashMap<usize, ObjId>,
    objects: Vec<String>,
    symbols: Vec<String>,
    env: Vec<String>,
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

        // nil is special — always id 0
        if obj.ptr_eq(NIL) {
            if !self.seen.contains_key(&addr) {
                let id = self.alloc_id("".to_string());
                self.seen.insert(addr, id);
            }
            return self.seen[&addr];
        }

        // Already visited — return cached id (handles cycles + sharing)
        if let Some(&id) = self.seen.get(&addr) {
            return id;
        }

        // Pre-insert before recursing into children to break cycles.
        // If a child points back to this object, dump_object will find
        // it in `seen` and return the id without infinite recursion.
        let id = self.alloc_id("".to_string());
        self.seen.insert(addr, id);

        let dumped = match obj.untag() {
            ObjectType::Int(x) => format!("  int {x}"),
            ObjectType::Cons(cons) => {
                // Recurse into car/cdr — cycle-safe because we pre-inserted `id`
                let car = self.dump_object(cons.car());
                let cdr = self.dump_object(cons.cdr());
                format!("  cons @car={car} @cdr={cdr}")
            }
            _ => {
                format!("  unimplemented")
            }
        };

        // Overwrite the Nil placeholder with the real object
        self.objects[id as usize] = dumped;
        id
    }

    fn alloc_id(&mut self, placeholder: String) -> ObjId {
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
            if let Some(func_id) = func_id {
                self.symbols.push(format!("  \"{}\" -> @{} @{}", name.to_owned(), sym_id, func_id));
            } else {
                self.symbols.push(format!("  \"{}\" -> @{}", name.to_owned(), sym_id));
            };
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
            self.env.push(format!("  @{sym_id} = @{val_id}"));
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
            writeln!(out, "  {sym}").unwrap();
        }
        writeln!(out).unwrap();

        writeln!(out, ".ENV").unwrap();
        for b in &self.env {
            writeln!(out, "  {b}").unwrap();
        }
        out
    }
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
