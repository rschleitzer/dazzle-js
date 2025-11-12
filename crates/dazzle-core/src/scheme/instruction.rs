//! Bytecode instructions for the Scheme VM
//!
//! This module implements OpenJade's instruction-based execution model.
//! Instead of interpreting the AST directly (tree-walking), we compile
//! expressions to bytecode instructions once and execute them with a
//! simple stack-based VM.
//!
//! ## OpenJade Correspondence
//!
//! | Dazzle              | OpenJade                    |
//! |---------------------|-----------------------------|
//! | `Instruction` enum  | `Insn` class hierarchy      |
//! | `compile()`         | `Expression::compile()`     |
//! | `VM::run()`         | `VM::eval()`                |
//! | instruction index   | `InsnPtr` (cached pointer)  |
//!
//! ## Key Optimization
//!
//! OpenJade caches compiled instructions in Identifier::insn_:
//! ```cpp
//! class Identifier {
//!     Owner<Expression> def_;   // Parsed AST
//!     InsnPtr insn_;            // Compiled instructions (cached!)
//! };
//! ```
//!
//! We cache instruction start index in lambdas and construction rules.

use crate::scheme::arena::ValueId;

/// Bytecode instruction
///
/// Each instruction is a simple operation that manipulates the value stack.
/// Instructions are executed sequentially in a tight loop (no recursion).
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Push a constant value onto the stack
    Constant { value_id: ValueId },

    /// Look up a variable in the environment and push it
    Variable { depth: usize, offset: usize },

    /// Look up a global variable by name (for dynamic lookups)
    GlobalVariable { name: String },

    /// Pop n values from stack, pop a procedure, apply it, push result
    Apply { n_args: usize },

    /// Pop a value, if false jump to else_ip, otherwise continue
    Test { else_ip: usize },

    /// Jump unconditionally
    Jump { target_ip: usize },

    /// Pop n values, create a closure with them as free variables
    MakeClosure {
        params: Vec<String>,
        required_count: usize,
        body_ip: usize,
        n_free: usize,
    },

    /// Return the top of stack
    Return,

    /// Create a cons cell from top two stack values (car, cdr)
    Cons,

    /// Get car of top stack value
    Car,

    /// Get cdr of top stack value
    Cdr,

    /// Test if top stack value is null
    IsNull,

    /// Add two numbers
    Add,

    /// Subtract two numbers
    Subtract,

    /// Multiply two numbers
    Multiply,

    /// Divide two numbers
    Divide,

    /// Compare two values for equality
    Equal,

    /// Numeric less-than
    NumLt,

    /// Numeric greater-than
    NumGt,

    /// Create a list from top n stack values
    MakeList { n: usize },

    /// Pop top of stack (discard result)
    Pop,

    /// Duplicate top of stack
    Dup,

    /// Set lexical variable (pop value, set variable, push value back)
    SetVariable { depth: usize, offset: usize },

    /// Set global variable (pop value, set global, push value back)
    SetGlobalVariable { name: String },

    /// Define global variable (pop value, define global, push unspecified)
    DefineGlobal { name: String },
}

/// Compiled bytecode program
///
/// Just a sequence of instructions. The Arena is managed separately
/// and shared between compiler, VM, and program.
pub struct Program {
    /// Sequential instruction stream
    pub instructions: Vec<Instruction>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            instructions: Vec::new(),
        }
    }

    /// Emit an instruction and return its index
    pub fn emit(&mut self, insn: Instruction) -> usize {
        let ip = self.instructions.len();
        self.instructions.push(insn);
        ip
    }

    /// Patch a jump instruction at the given index
    pub fn patch_jump(&mut self, jump_ip: usize, target_ip: usize) {
        match &mut self.instructions[jump_ip] {
            Instruction::Jump { target_ip: ref mut t } => *t = target_ip,
            Instruction::Test { else_ip: ref mut e } => *e = target_ip,
            _ => panic!("Expected jump instruction at {}", jump_ip),
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scheme::arena::NIL_ID;

    #[test]
    fn test_emit_instruction() {
        let mut program = Program::new();
        let ip = program.emit(Instruction::Constant {
            value_id: NIL_ID,
        });
        assert_eq!(ip, 0);
        assert_eq!(program.instructions.len(), 1);
    }

    #[test]
    fn test_patch_jump() {
        let mut program = Program::new();
        let jump_ip = program.emit(Instruction::Jump { target_ip: 0 });
        let target_ip = program.emit(Instruction::Return);
        program.patch_jump(jump_ip, target_ip);

        match program.instructions[jump_ip] {
            Instruction::Jump { target_ip: t } => assert_eq!(t, target_ip),
            _ => panic!("Expected jump"),
        }
    }
}
