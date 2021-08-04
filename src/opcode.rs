use std::{
    convert::{TryFrom, TryInto},
    fmt,
};

#[allow(dead_code)]
#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub(crate) enum OpCode {
    StackRef0 = 0,
    StackRef1,
    StackRef2,
    StackRef3,
    StackRef4,
    StackRef5,
    StackRefN,
    StackRefN2,
    StackSet0,
    StackSet1,
    StackSet2,
    StackSet3,
    StackSet4,
    StackSet5,
    StackSetN,
    StackSetN2,
    VarRef0,
    VarRef1,
    VarRef2,
    VarRef3,
    VarRef4,
    VarRef5,
    VarRefN,
    VarRefN2,
    VarSet0,
    VarSet1,
    VarSet2,
    VarSet3,
    VarSet4,
    VarSet5,
    VarSetN,
    VarSetN2,
    Constant0,
    Constant1,
    Constant2,
    Constant3,
    Constant4,
    Constant5,
    ConstantN,
    ConstantN2,
    Call0,
    Call1,
    Call2,
    Call3,
    Call4,
    Call5,
    CallN,
    CallN2,
    Discard,
    DiscardN,
    DiscardNKeepTOS,
    Duplicate,
    Jump,
    JumpNil,
    JumpNotNil,
    JumpNilElsePop,
    JumpNotNilElsePop,
    Ret,
    Unknown,
}

#[derive(PartialEq, Clone)]
pub(crate) struct CodeVec(Vec<u8>);

impl Default for CodeVec {
    fn default() -> Self {
        CodeVec(Vec::new())
    }
}

impl From<Vec<u8>> for CodeVec {
    fn from(vec: Vec<u8>) -> Self {
        CodeVec(vec)
    }
}

impl std::ops::Deref for CodeVec {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for CodeVec {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl fmt::Debug for CodeVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        #[allow(clippy::enum_glob_use)]
        use OpCode::*;
        let mut display: Vec<String> = vec![];
        let mut iter = self.0.iter();
        while let Some(i) = iter.next() {
            let op = (*i).try_into().unwrap_or(Unknown);
            display.push(format!("{:?}", op));
            match op {
                StackRefN | ConstantN | CallN | VarRefN | VarSetN | DiscardN | DiscardNKeepTOS => {
                    display.push(format!("{:?}", iter.next().unwrap()));
                }
                StackRefN2 | ConstantN2 | CallN2 | JumpNil | JumpNotNil | Jump | JumpNilElsePop
                | JumpNotNilElsePop | VarRefN2 | VarSetN2 => {
                    display.push(format!("{:?}", iter.next().unwrap()));
                    display.push(format!("{:?}", iter.next().unwrap()));
                }
                _ => {}
            }
        }
        write!(f, "{:?}", display)
    }
}

impl From<OpCode> for u8 {
    fn from(x: OpCode) -> u8 {
        x as u8
    }
}

impl TryFrom<u8> for OpCode {
    type Error = anyhow::Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value <= OpCode::Unknown as u8 {
            Ok(unsafe { std::mem::transmute::<u8, OpCode>(value) })
        } else {
            Err(anyhow::anyhow!("Invalid opcode value: {}", value))
        }
    }
}
