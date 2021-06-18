use std::fmt;

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum OpCode {
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
    End,
    Unknown,
}

#[derive(PartialEq, Clone)]
pub struct CodeVec(Vec<u8>);

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
        use OpCode::*;
        let mut display: Vec<String> = vec![];
        let mut iter = self.0.iter();
        while let Some(i) = iter.next() {
            let op = unsafe { OpCode::from_unchecked(*i) };
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
