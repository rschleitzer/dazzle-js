//! Arena-based value storage with separate mark bitmap
//!
//! Simplified design:
//! - ValueData has no GC-specific fields
//! - Separate Vec<bool> for GC marks
//! - Works uniformly for all value types

use std::rc::Rc;
use crate::scheme::parser::Position;
use crate::scheme::value::{Unit, SourceInfo};
use crate::grove;

/// Value identifier - lightweight copyable index
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ValueId(pub u32);

/// Special constants
pub const NIL_ID: ValueId = ValueId(0);
pub const TRUE_ID: ValueId = ValueId(1);
pub const FALSE_ID: ValueId = ValueId(2);
pub const UNSPECIFIED_ID: ValueId = ValueId(3);
pub const ERROR_ID: ValueId = ValueId(4);
pub const FIRST_USER_ID: u32 = 5;

/// Value data (no GC fields)
pub enum ValueData {
    Nil,
    Bool(bool),
    Integer(i64),
    Real(f64),
    Quantity { magnitude: f64, unit: Unit },
    Char(char),
    String(String),
    Symbol(Rc<str>),
    Keyword(Rc<str>),
    Pair {
        car: ValueId,
        cdr: ValueId,
        pos: Option<Position>,
    },
    Vector(Vec<ValueId>),
    Procedure(ProcedureData),
    Node(Rc<Box<dyn grove::Node>>),
    NodeList(Rc<Box<dyn grove::NodeList>>),
    Sosofo,
    Unspecified,
    Error,
}

/// Procedure data
pub enum ProcedureData {
    Primitive {
        name: &'static str,
        func: fn(&Arena, &[ValueId]) -> Result<ValueId, String>,
    },
    Lambda {
        params: Vec<String>,
        required_count: usize,
        optional_defaults: Vec<ValueId>,
        body: ValueId,
        env: ValueId,
        source: Option<SourceInfo>,
        name: Option<String>,
    },
}

/// Arena with separate mark bitmap
pub struct Arena {
    values: Vec<ValueData>,
    marks: Vec<bool>,  // Separate mark bitmap
    free_list: Vec<u32>,
    next_id: u32,
    // Grove runtime state
    pub current_node: Option<Rc<Box<dyn grove::Node>>>,
    pub grove: Option<Rc<dyn grove::Grove>>,
}

impl Arena {
    pub fn new() -> Self {
        let mut arena = Arena {
            values: Vec::with_capacity(1024),
            marks: Vec::with_capacity(1024),
            free_list: Vec::new(),
            next_id: FIRST_USER_ID,
            current_node: None,
            grove: None,
        };

        // Pre-allocate constants
        arena.values.push(ValueData::Nil);
        arena.marks.push(true);  // Constants always marked
        arena.values.push(ValueData::Bool(true));
        arena.marks.push(true);
        arena.values.push(ValueData::Bool(false));
        arena.marks.push(true);
        arena.values.push(ValueData::Unspecified);
        arena.marks.push(true);
        arena.values.push(ValueData::Error);
        arena.marks.push(true);

        arena
    }

    pub fn alloc(&mut self, data: ValueData) -> ValueId {
        let id = if let Some(id) = self.free_list.pop() {
            self.values[id as usize] = data;
            self.marks[id as usize] = false;
            id
        } else {
            let id = self.next_id;
            self.next_id += 1;
            self.values.push(data);
            self.marks.push(false);
            id
        };
        ValueId(id)
    }

    #[inline]
    pub fn get(&self, id: ValueId) -> &ValueData {
        &self.values[id.0 as usize]
    }

    #[inline]
    pub fn get_mut(&mut self, id: ValueId) -> &mut ValueData {
        &mut self.values[id.0 as usize]
    }

    #[inline]
    pub fn cons(&mut self, car: ValueId, cdr: ValueId) -> ValueId {
        self.alloc(ValueData::Pair { car, cdr, pos: None })
    }

    #[inline]
    pub fn cons_with_pos(&mut self, car: ValueId, cdr: ValueId, pos: Position) -> ValueId {
        self.alloc(ValueData::Pair { car, cdr, pos: Some(pos) })
    }

    #[inline]
    pub fn int(&mut self, n: i64) -> ValueId {
        self.alloc(ValueData::Integer(n))
    }

    #[inline]
    pub fn real(&mut self, f: f64) -> ValueId {
        self.alloc(ValueData::Real(f))
    }

    #[inline]
    pub fn string(&mut self, s: String) -> ValueId {
        self.alloc(ValueData::String(s))
    }

    #[inline]
    pub fn symbol(&mut self, s: Rc<str>) -> ValueId {
        self.alloc(ValueData::Symbol(s))
    }

    #[inline]
    pub fn char(&mut self, c: char) -> ValueId {
        self.alloc(ValueData::Char(c))
    }

    #[inline]
    pub fn keyword(&mut self, k: Rc<str>) -> ValueId {
        self.alloc(ValueData::Keyword(k))
    }

    #[inline]
    pub fn vector(&mut self, elements: Vec<ValueId>) -> ValueId {
        self.alloc(ValueData::Vector(elements))
    }

    /// Mark phase - mark a value and recursively mark its children
    fn mark(&mut self, id: ValueId) {
        let idx = id.0 as usize;

        if self.marks[idx] {
            return; // Already marked
        }

        self.marks[idx] = true;

        // Get child IDs to mark
        let child_ids = match &self.values[idx] {
            ValueData::Pair { car, cdr, .. } => vec![*car, *cdr],
            ValueData::Vector(elements) => elements.clone(),
            ValueData::Procedure(ProcedureData::Lambda {
                optional_defaults,
                body,
                env,
                ..
            }) => {
                let mut ids = optional_defaults.clone();
                ids.push(*body);
                ids.push(*env);
                ids
            }
            _ => vec![],
        };

        // Recursively mark children
        for child_id in child_ids {
            self.mark(child_id);
        }
    }

    /// Sweep phase - collect unmarked values
    fn sweep(&mut self) {
        for i in FIRST_USER_ID as usize..self.values.len() {
            if !self.marks[i] {
                // Unmarked - free it
                self.values[i] = ValueData::Nil;
                self.free_list.push(i as u32);
            } else {
                // Marked - unmark for next GC
                self.marks[i] = false;
            }
        }
    }

    /// Garbage collect
    pub fn gc(&mut self, roots: &[ValueId]) {
        // Mark phase
        for root in roots {
            self.mark(*root);
        }

        // Sweep phase
        self.sweep();
    }

    pub fn len(&self) -> usize {
        self.values.len() - self.free_list.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == FIRST_USER_ID as usize
    }
}

impl Default for Arena {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arena_constants() {
        let arena = Arena::new();
        assert!(matches!(arena.get(NIL_ID), ValueData::Nil));
        assert!(matches!(arena.get(TRUE_ID), ValueData::Bool(true)));
        assert!(matches!(arena.get(FALSE_ID), ValueData::Bool(false)));
    }

    #[test]
    fn test_cons() {
        let mut arena = Arena::new();
        let one = arena.int(1);
        let two = arena.int(2);
        let pair = arena.cons(one, two);

        match arena.get(pair) {
            ValueData::Pair { car, cdr, .. } => {
                assert_eq!(*car, one);
                assert_eq!(*cdr, two);
            }
            _ => panic!("Expected pair"),
        }
    }

    #[test]
    fn test_gc_keeps_reachable() {
        let mut arena = Arena::new();

        let one = arena.int(1);
        let two = arena.int(2);
        let pair = arena.cons(one, two);

        arena.gc(&[pair]);

        // Verify values still accessible
        assert!(matches!(arena.get(pair), ValueData::Pair { .. }));
        assert!(matches!(arena.get(one), ValueData::Integer(1)));
        assert!(matches!(arena.get(two), ValueData::Integer(2)));
    }

    #[test]
    fn test_gc_collects_unreachable() {
        let mut arena = Arena::new();

        let one = arena.int(1);
        let _two = arena.int(2);  // Not referenced
        let three = arena.int(3);
        let pair = arena.cons(one, three);

        let before = arena.len();
        arena.gc(&[pair]);
        let after = arena.len();

        // Should have collected 'two'
        assert_eq!(before - after, 1);
    }
}
