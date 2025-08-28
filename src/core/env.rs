use super::gc::{Context, ObjectMap, Rto, Slot};
use super::object::{LispBuffer, Object, OpenBuffer, Symbol, WithLifetime};
use anyhow::{Result, anyhow};
use rune_macros::Trace;
use std::cell::OnceCell;

mod stack;
mod symbol_map;
pub(crate) use stack::*;
pub(crate) use symbol_map::*;

type PropertyMap<'a> = ObjectMap<Slot<Symbol<'a>>, Vec<(Slot<Symbol<'a>>, Slot<Object<'a>>)>>;
#[derive(Debug, Default, Trace)]
pub(crate) struct Env<'a> {
    pub(crate) vars: ObjectMap<Slot<Symbol<'a>>, Slot<Object<'a>>>,
    pub(crate) props: PropertyMap<'a>,
    pub(crate) catch_stack: Vec<Slot<Object<'a>>>,
    exception: (Slot<Object<'a>>, Slot<Object<'a>>),
    #[no_trace]
    exception_id: u32,
    binding_stack: Vec<(Slot<Symbol<'a>>, Option<Slot<Object<'a>>>)>,
    pub(crate) match_data: Slot<Object<'a>>,
    #[no_trace]
    pub(crate) current_buffer: CurrentBuffer<'a>,
    #[no_trace]
    pub(crate) selected_frame: Option<Slot<Object<'a>>>,
    pub(crate) stack: LispStack<'a>,
}

#[derive(Debug)]
pub(crate) struct CurrentBuffer<'a> {
    buffer: OnceCell<OpenBuffer<'a>>,
    pub(crate) buf_ref: &'a LispBuffer,
}

impl Default for CurrentBuffer<'_> {
    fn default() -> Self {
        let name = crate::buffer::generate_new_buffer_name("*scratch*", None);
        let buffer = {
            // // need to drop global to avoid deadlocks
            let global = INTERNED_SYMBOLS.lock().unwrap();
            unsafe { global.create_buffer(&name).with_lifetime() }
        };
        crate::buffer::BUFFERS.lock().unwrap().insert(name, buffer);
        Self { buffer: Default::default(), buf_ref: buffer }
    }
}

impl<'a> CurrentBuffer<'a> {
    fn lock(&self) -> OpenBuffer<'a> {
        unsafe { self.buf_ref.lock().unwrap().with_lifetime() }
    }

    pub(crate) fn get(&self) -> &OpenBuffer<'a> {
        self.buffer.get_or_init(|| self.lock())
    }

    pub(crate) fn get_mut(&mut self) -> &mut OpenBuffer<'a> {
        // there is no get_or_init_mut
        if self.buffer.get().is_none() {
            let locked = self.lock();
            let _ = self.buffer.set(locked);
        }
        self.buffer.get_mut().unwrap()
    }

    pub(crate) fn set(&mut self, buffer: &LispBuffer) {
        let buffer = unsafe { buffer.with_lifetime() };
        self.buf_ref = buffer;
        self.release();
    }

    pub(crate) fn release(&mut self) {
        self.buffer.take();
    }
}

impl PartialEq<LispBuffer> for CurrentBuffer<'_> {
    fn eq(&self, other: &LispBuffer) -> bool {
        self.buf_ref == other
    }
}

// RootedEnv created by #[derive(Trace)]
impl<'a> RootedEnv<'a> {
    pub(crate) fn set_var(&mut self, sym: Symbol, value: Object) -> Result<()> {
        if sym.is_const() {
            Err(anyhow!("Attempt to set a constant symbol: {sym}"))
        } else {
            self.vars.insert(sym, value);
            Ok(())
        }
    }

    pub(crate) fn set_prop(&mut self, symbol: Symbol, propname: Symbol, value: Object) {
        match self.props.get_mut(symbol) {
            Some(plist) => match plist.iter_mut().find(|x| x.0 == propname) {
                Some(x) => x.1.set(value),
                None => plist.push((propname, value)),
            },
            None => {
                self.props.insert(symbol, vec![(propname, value)]);
            }
        }
    }

    pub(crate) fn set_exception(&mut self, tag: Object, data: Object) -> u32 {
        self.exception.0.set(tag);
        self.exception.1.set(data);
        self.exception_id += 1;
        self.exception_id
    }

    pub(crate) fn get_exception(&self, id: u32) -> Option<(&Rto<Object<'a>>, &Rto<Object<'a>>)> {
        (id == self.exception_id).then_some((&self.exception.0, &self.exception.1))
    }

    pub(crate) fn varbind(&mut self, var: Symbol, value: Object, cx: &Context) {
        let prev_value = self.vars.get(var).map(|x| x.bind(cx));
        self.binding_stack.push((var, prev_value));
        self.vars.insert(var, value);
    }

    pub(crate) fn unbind(&mut self, count: u16, cx: &Context) {
        for _ in 0..count {
            match self.binding_stack.bind_mut(cx).pop() {
                Some((sym, val)) => match val {
                    Some(val) => self.vars.insert(*sym, *val),
                    None => self.vars.remove(*sym),
                },
                None => panic!("Binding stack was empty"),
            }
        }
    }

    pub(crate) fn defvar(&mut self, var: Symbol, value: Object) -> Result<()> {
        // TOOD: Handle `eval-sexp` on defvar, which should always update the
        // value
        if self.vars.get(var).is_none() {
            self.set_var(var, value)?;
            var.make_special();
        }

        // If this variable was unbound previously in the binding stack,
        // we will bind it to the new value
        for binding in &mut *self.binding_stack {
            if binding.0 == var && binding.1.is_none() {
                binding.1.set(Some(value));
            }
        }
        Ok(())
    }

    pub(crate) fn set_buffer(&mut self, buffer: &LispBuffer) {
        if buffer == self.current_buffer.buf_ref {
            return;
        }
        self.current_buffer.set(buffer);
    }

    pub(crate) fn with_buffer<T>(
        &self,
        buffer: &LispBuffer,
        mut func: impl FnMut(&OpenBuffer) -> T,
    ) -> Result<T> {
        if self.current_buffer == *buffer {
            Ok(func(self.current_buffer.get()))
        } else {
            let buffer = buffer.lock()?;
            Ok(func(&buffer))
        }
    }

    pub(crate) fn with_buffer_mut<T>(
        &mut self,
        buffer: &LispBuffer,
        mut func: impl FnMut(&mut OpenBuffer) -> T,
    ) -> Result<T> {
        if self.current_buffer == *buffer {
            Ok(func(self.current_buffer.get_mut()))
        } else {
            let mut buffer = buffer.lock()?;
            Ok(func(&mut buffer))
        }
    }
}
