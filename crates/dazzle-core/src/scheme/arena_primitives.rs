//! Arena-based primitive implementations (Phase 1 prototype)
//!
//! This module contains arena-based implementations of the hottest primitives
//! to benchmark the performance gain from eliminating Gc<T> overhead.
//!
//! Hot primitives (from DocBook profiling):
//! - equal?: 4,496 calls
//! - car: 4,221 calls
//! - null?: 3,968 calls
//! - cdr: 3,505 calls
//! - cons: ~3,000 calls

use crate::scheme::arena::{Arena, ValueId, ValueData, NIL_ID};

pub type ArenaResult = Result<ValueId, String>;

/// (car pair) → value
///
/// Arena version: Zero overhead - just copy u32
#[inline]
pub fn arena_car(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("car requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Pair { car, .. } => Ok(*car),
        _ => Err(format!("car: not a pair")),
    }
}

/// (cdr pair) → value
///
/// Arena version: Zero overhead - just copy u32
#[inline]
pub fn arena_cdr(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("cdr requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Pair { cdr, .. } => Ok(*cdr),
        _ => Err(format!("cdr: not a pair")),
    }
}

/// (cons obj1 obj2) → pair
///
/// Arena version: Direct arena allocation
#[inline]
pub fn arena_cons(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("cons requires exactly 2 arguments".to_string());
    }

    Ok(arena.cons(args[0], args[1]))
}

/// (null? obj) → boolean
///
/// Arena version: Simple equality check
#[inline]
pub fn arena_null(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("null? requires exactly 1 argument".to_string());
    }

    Ok(if args[0] == NIL_ID {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (equal? obj1 obj2) → boolean
///
/// Arena version: Recursive structural equality
pub fn arena_equal(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("equal? requires exactly 2 arguments".to_string());
    }

    Ok(if arena_equal_impl(arena, args[0], args[1]) {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// Implementation of structural equality
fn arena_equal_impl(arena: &Arena, a: ValueId, b: ValueId) -> bool {
    // Fast path: same ID
    if a == b {
        return true;
    }

    match (arena.get(a), arena.get(b)) {
        // Structural equality for pairs
        (ValueData::Pair { car: car1, cdr: cdr1, .. },
         ValueData::Pair { car: car2, cdr: cdr2, .. }) => {
            arena_equal_impl(arena, *car1, *car2) && arena_equal_impl(arena, *cdr1, *cdr2)
        }

        // Structural equality for vectors
        (ValueData::Vector(v1), ValueData::Vector(v2)) => {
            v1.len() == v2.len() &&
            v1.iter().zip(v2.iter()).all(|(a, b)| arena_equal_impl(arena, *a, *b))
        }

        // Value equality for primitives
        (ValueData::Integer(n1), ValueData::Integer(n2)) => n1 == n2,
        (ValueData::Real(f1), ValueData::Real(f2)) => f1 == f2,
        (ValueData::Bool(b1), ValueData::Bool(b2)) => b1 == b2,
        (ValueData::Char(c1), ValueData::Char(c2)) => c1 == c2,
        (ValueData::String(s1), ValueData::String(s2)) => s1 == s2,
        (ValueData::Symbol(s1), ValueData::Symbol(s2)) => s1 == s2,

        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arena_car() {
        let mut arena = Arena::new();
        let one = arena.int(1);
        let two = arena.int(2);
        let pair = arena.cons(one, two);

        let result = arena_car(&arena, &[pair]).unwrap();
        assert_eq!(result, one);
    }

    #[test]
    fn test_arena_cdr() {
        let mut arena = Arena::new();
        let one = arena.int(1);
        let two = arena.int(2);
        let pair = arena.cons(one, two);

        let result = arena_cdr(&arena, &[pair]).unwrap();
        assert_eq!(result, two);
    }

    #[test]
    fn test_arena_cons() {
        let mut arena = Arena::new();
        let one = arena.int(1);
        let two = arena.int(2);

        let pair = arena_cons(&mut arena, &[one, two]).unwrap();

        match arena.get(pair) {
            ValueData::Pair { car, cdr, .. } => {
                assert_eq!(*car, one);
                assert_eq!(*cdr, two);
            }
            _ => panic!("Expected pair"),
        }
    }

    #[test]
    fn test_arena_null() {
        let mut arena = Arena::new();

        assert_eq!(
            arena_null(&arena, &[NIL_ID]).unwrap(),
            crate::scheme::arena::TRUE_ID
        );

        let one = arena.int(1);
        assert_eq!(
            arena_null(&arena, &[one]).unwrap(),
            crate::scheme::arena::FALSE_ID
        );
    }

    #[test]
    fn test_arena_equal() {
        let mut arena = Arena::new();

        // Equal integers
        let one1 = arena.int(1);
        let one2 = arena.int(1);
        assert_eq!(
            arena_equal(&arena, &[one1, one2]).unwrap(),
            crate::scheme::arena::TRUE_ID
        );

        // Unequal integers
        let two = arena.int(2);
        assert_eq!(
            arena_equal(&arena, &[one1, two]).unwrap(),
            crate::scheme::arena::FALSE_ID
        );

        // Equal lists
        let list1 = arena.cons(one1, NIL_ID);
        let list2 = arena.cons(one2, NIL_ID);
        assert_eq!(
            arena_equal(&arena, &[list1, list2]).unwrap(),
            crate::scheme::arena::TRUE_ID
        );
    }
}
