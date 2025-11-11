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
use std::rc::Rc;

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

// =============================================================================
// Batch 1: List Operations (Phase 3)
// =============================================================================

/// (cadr list) → value
/// Equivalent to (car (cdr list))
#[inline]
pub fn arena_cadr(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("cadr requires exactly 1 argument".to_string());
    }
    let cdr_result = arena_cdr(arena, args)?;
    arena_car(arena, &[cdr_result])
}

/// (caddr list) → value
/// Equivalent to (car (cdr (cdr list)))
#[inline]
pub fn arena_caddr(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("caddr requires exactly 1 argument".to_string());
    }
    let cdr1 = arena_cdr(arena, args)?;
    let cdr2 = arena_cdr(arena, &[cdr1])?;
    arena_car(arena, &[cdr2])
}

/// (cadddr list) → value
/// Equivalent to (car (cdr (cdr (cdr list))))
#[inline]
pub fn arena_cadddr(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("cadddr requires exactly 1 argument".to_string());
    }
    let cdr1 = arena_cdr(arena, args)?;
    let cdr2 = arena_cdr(arena, &[cdr1])?;
    let cdr3 = arena_cdr(arena, &[cdr2])?;
    arena_car(arena, &[cdr3])
}

/// (list obj ...) → list
/// Create a list from arguments
pub fn arena_list(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    let mut result = NIL_ID;
    for &arg in args.iter().rev() {
        result = arena.cons(arg, result);
    }
    Ok(result)
}

/// (length list) → integer
/// Return the length of a list
pub fn arena_length(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("length requires exactly 1 argument".to_string());
    }

    let mut len = 0i64;
    let mut current = args[0];

    loop {
        if current == NIL_ID {
            return Ok(arena.int(len));
        }

        match arena.get(current) {
            ValueData::Pair { cdr, .. } => {
                len += 1;
                current = *cdr;
            }
            _ => return Err("length: not a proper list".to_string()),
        }
    }
}

/// (reverse list) → list
/// Reverse a list
pub fn arena_reverse(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("reverse requires exactly 1 argument".to_string());
    }

    let mut result = NIL_ID;
    let mut current = args[0];

    loop {
        if current == NIL_ID {
            return Ok(result);
        }

        // Extract car and cdr before mutably borrowing arena
        let (car_val, cdr_val) = match arena.get(current) {
            ValueData::Pair { car, cdr, .. } => (*car, *cdr),
            _ => return Err("reverse: not a proper list".to_string()),
        };

        result = arena.cons(car_val, result);
        current = cdr_val;
    }
}

/// (append list ...) → list
/// Concatenate lists
pub fn arena_append(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Ok(NIL_ID);
    }

    if args.len() == 1 {
        return Ok(args[0]);
    }

    // Build result by appending each list
    let mut result = args[args.len() - 1];

    for i in (0..args.len() - 1).rev() {
        let mut items = Vec::new();
        let mut current = args[i];

        // Collect items from this list
        loop {
            if current == NIL_ID {
                break;
            }

            match arena.get(current) {
                ValueData::Pair { car, cdr, .. } => {
                    items.push(*car);
                    current = *cdr;
                }
                _ => return Err("append: not a proper list".to_string()),
            }
        }

        // Cons items onto result in reverse order
        for &item in items.iter().rev() {
            result = arena.cons(item, result);
        }
    }

    Ok(result)
}

/// (list? obj) → boolean
/// Check if object is a proper list
pub fn arena_list_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("list? requires exactly 1 argument".to_string());
    }

    let mut current = args[0];
    loop {
        if current == NIL_ID {
            return Ok(crate::scheme::arena::TRUE_ID);
        }

        match arena.get(current) {
            ValueData::Pair { cdr, .. } => {
                current = *cdr;
            }
            _ => return Ok(crate::scheme::arena::FALSE_ID),
        }
    }
}

/// (list-ref list k) → value
/// Return the kth element of list (0-indexed)
pub fn arena_list_ref(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("list-ref requires exactly 2 arguments".to_string());
    }

    let k = match arena.get(args[1]) {
        ValueData::Integer(n) if *n >= 0 => *n as usize,
        ValueData::Integer(_) => return Err("list-ref: index must be non-negative".to_string()),
        _ => return Err("list-ref: index must be an integer".to_string()),
    };

    let mut current = args[0];
    for _ in 0..k {
        match arena.get(current) {
            ValueData::Pair { cdr, .. } => {
                current = *cdr;
            }
            _ => return Err("list-ref: index out of bounds".to_string()),
        }
    }

    match arena.get(current) {
        ValueData::Pair { car, .. } => Ok(*car),
        _ => Err("list-ref: index out of bounds".to_string()),
    }
}

// =============================================================================
// Batch 2: Type Predicates (Phase 3)
// =============================================================================

/// (pair? obj) → boolean
/// Check if object is a pair
#[inline]
pub fn arena_pair_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("pair? requires exactly 1 argument".to_string());
    }

    Ok(match arena.get(args[0]) {
        ValueData::Pair { .. } => crate::scheme::arena::TRUE_ID,
        _ => crate::scheme::arena::FALSE_ID,
    })
}

/// (number? obj) → boolean
/// Check if object is a number (integer or real)
#[inline]
pub fn arena_number_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("number? requires exactly 1 argument".to_string());
    }

    Ok(match arena.get(args[0]) {
        ValueData::Integer(_) | ValueData::Real(_) => crate::scheme::arena::TRUE_ID,
        _ => crate::scheme::arena::FALSE_ID,
    })
}

/// (integer? obj) → boolean
/// Check if object is an integer
#[inline]
pub fn arena_integer_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("integer? requires exactly 1 argument".to_string());
    }

    Ok(match arena.get(args[0]) {
        ValueData::Integer(_) => crate::scheme::arena::TRUE_ID,
        _ => crate::scheme::arena::FALSE_ID,
    })
}

/// (real? obj) → boolean
/// Check if object is a real number
#[inline]
pub fn arena_real_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("real? requires exactly 1 argument".to_string());
    }

    Ok(match arena.get(args[0]) {
        ValueData::Real(_) => crate::scheme::arena::TRUE_ID,
        _ => crate::scheme::arena::FALSE_ID,
    })
}

/// (string? obj) → boolean
/// Check if object is a string
#[inline]
pub fn arena_string_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("string? requires exactly 1 argument".to_string());
    }

    Ok(match arena.get(args[0]) {
        ValueData::String(_) => crate::scheme::arena::TRUE_ID,
        _ => crate::scheme::arena::FALSE_ID,
    })
}

/// (symbol? obj) → boolean
/// Check if object is a symbol
#[inline]
pub fn arena_symbol_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("symbol? requires exactly 1 argument".to_string());
    }

    Ok(match arena.get(args[0]) {
        ValueData::Symbol(_) => crate::scheme::arena::TRUE_ID,
        _ => crate::scheme::arena::FALSE_ID,
    })
}

/// (char? obj) → boolean
/// Check if object is a character
#[inline]
pub fn arena_char_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("char? requires exactly 1 argument".to_string());
    }

    Ok(match arena.get(args[0]) {
        ValueData::Char(_) => crate::scheme::arena::TRUE_ID,
        _ => crate::scheme::arena::FALSE_ID,
    })
}

/// (boolean? obj) → boolean
/// Check if object is a boolean
#[inline]
pub fn arena_boolean_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("boolean? requires exactly 1 argument".to_string());
    }

    Ok(match arena.get(args[0]) {
        ValueData::Bool(_) => crate::scheme::arena::TRUE_ID,
        _ => crate::scheme::arena::FALSE_ID,
    })
}

/// (zero? num) → boolean
/// Check if number is zero
#[inline]
pub fn arena_zero_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("zero? requires exactly 1 argument".to_string());
    }

    Ok(match arena.get(args[0]) {
        ValueData::Integer(n) => {
            if *n == 0 {
                crate::scheme::arena::TRUE_ID
            } else {
                crate::scheme::arena::FALSE_ID
            }
        }
        ValueData::Real(f) => {
            if *f == 0.0 {
                crate::scheme::arena::TRUE_ID
            } else {
                crate::scheme::arena::FALSE_ID
            }
        }
        _ => return Err("zero?: not a number".to_string()),
    })
}

/// (positive? num) → boolean
/// Check if number is positive
#[inline]
pub fn arena_positive_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("positive? requires exactly 1 argument".to_string());
    }

    Ok(match arena.get(args[0]) {
        ValueData::Integer(n) => {
            if *n > 0 {
                crate::scheme::arena::TRUE_ID
            } else {
                crate::scheme::arena::FALSE_ID
            }
        }
        ValueData::Real(f) => {
            if *f > 0.0 {
                crate::scheme::arena::TRUE_ID
            } else {
                crate::scheme::arena::FALSE_ID
            }
        }
        _ => return Err("positive?: not a number".to_string()),
    })
}

/// (negative? num) → boolean
/// Check if number is negative
#[inline]
pub fn arena_negative_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("negative? requires exactly 1 argument".to_string());
    }

    Ok(match arena.get(args[0]) {
        ValueData::Integer(n) => {
            if *n < 0 {
                crate::scheme::arena::TRUE_ID
            } else {
                crate::scheme::arena::FALSE_ID
            }
        }
        ValueData::Real(f) => {
            if *f < 0.0 {
                crate::scheme::arena::TRUE_ID
            } else {
                crate::scheme::arena::FALSE_ID
            }
        }
        _ => return Err("negative?: not a number".to_string()),
    })
}

/// (odd? n) → boolean
/// Check if integer is odd
#[inline]
pub fn arena_odd_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("odd? requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(n) => {
            Ok(if n % 2 != 0 {
                crate::scheme::arena::TRUE_ID
            } else {
                crate::scheme::arena::FALSE_ID
            })
        }
        _ => Err("odd?: not an integer".to_string()),
    }
}

/// (even? n) → boolean
/// Check if integer is even
#[inline]
pub fn arena_even_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("even? requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(n) => {
            Ok(if n % 2 == 0 {
                crate::scheme::arena::TRUE_ID
            } else {
                crate::scheme::arena::FALSE_ID
            })
        }
        _ => Err("even?: not an integer".to_string()),
    }
}

// =============================================================================
// Batch 3: Arithmetic Operations (Phase 3)
// =============================================================================

/// Helper: Convert ValueId to numeric value (int or real)
fn to_number(arena: &Arena, id: ValueId) -> Result<f64, String> {
    match arena.get(id) {
        ValueData::Integer(n) => Ok(*n as f64),
        ValueData::Real(f) => Ok(*f),
        ValueData::Quantity { magnitude, unit } => {
            // Convert quantity to inches (canonical unit)
            Ok(unit.to_inches(*magnitude))
        }
        _ => Err("not a number".to_string()),
    }
}

/// Helper: Create result from f64 (preserve integer when possible)
fn from_number(arena: &mut Arena, n: f64) -> ValueId {
    if n.fract() == 0.0 && n.is_finite() && n >= i64::MIN as f64 && n <= i64::MAX as f64 {
        arena.int(n as i64)
    } else {
        arena.real(n)
    }
}

/// (+ num ...) → number
/// Add numbers
pub fn arena_add(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Ok(arena.int(0));
    }

    let mut sum = 0.0;
    let mut all_ints = true;
    let mut int_sum = 0i64;

    for &arg in args {
        match arena.get(arg) {
            ValueData::Integer(n) => {
                int_sum = int_sum.wrapping_add(*n);
                sum += *n as f64;
            }
            ValueData::Real(f) => {
                all_ints = false;
                sum += f;
            }
            _ => return Err("+: not a number".to_string()),
        }
    }

    Ok(if all_ints {
        arena.int(int_sum)
    } else {
        arena.real(sum)
    })
}

/// (- num ...) → number
/// Subtract numbers
pub fn arena_subtract(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Err("-: requires at least 1 argument".to_string());
    }

    if args.len() == 1 {
        // Negate
        match arena.get(args[0]) {
            ValueData::Integer(n) => return Ok(arena.int(-n)),
            ValueData::Real(f) => return Ok(arena.real(-f)),
            _ => return Err("-: not a number".to_string()),
        }
    }

    let mut result = to_number(arena, args[0])?;
    let mut all_ints = matches!(arena.get(args[0]), ValueData::Integer(_));

    for &arg in &args[1..] {
        result -= to_number(arena, arg)?;
        if !matches!(arena.get(arg), ValueData::Integer(_)) {
            all_ints = false;
        }
    }

    Ok(if all_ints && result.fract() == 0.0 {
        arena.int(result as i64)
    } else {
        arena.real(result)
    })
}

/// (* num ...) → number
/// Multiply numbers
pub fn arena_multiply(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Ok(arena.int(1));
    }

    let mut product = 1.0;
    let mut all_ints = true;
    let mut int_product = 1i64;

    for &arg in args {
        match arena.get(arg) {
            ValueData::Integer(n) => {
                int_product = int_product.wrapping_mul(*n);
                product *= *n as f64;
            }
            ValueData::Real(f) => {
                all_ints = false;
                product *= f;
            }
            _ => return Err("*: not a number".to_string()),
        }
    }

    Ok(if all_ints {
        arena.int(int_product)
    } else {
        arena.real(product)
    })
}

/// (/ num ...) → number
/// Divide numbers (always returns real in R4RS)
pub fn arena_divide(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Err("/: requires at least 1 argument".to_string());
    }

    if args.len() == 1 {
        // Reciprocal
        let n = to_number(arena, args[0])?;
        if n == 0.0 {
            return Err("/: division by zero".to_string());
        }
        return Ok(arena.real(1.0 / n));
    }

    let mut result = to_number(arena, args[0])?;

    for &arg in &args[1..] {
        let n = to_number(arena, arg)?;
        if n == 0.0 {
            return Err("/: division by zero".to_string());
        }
        result /= n;
    }

    // Division always returns real in R4RS
    Ok(arena.real(result))
}

/// (quotient n1 n2) → integer
/// Integer division quotient
pub fn arena_quotient(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("quotient requires exactly 2 arguments".to_string());
    }

    match (arena.get(args[0]), arena.get(args[1])) {
        (ValueData::Integer(n1), ValueData::Integer(n2)) => {
            if *n2 == 0 {
                return Err("quotient: division by zero".to_string());
            }
            Ok(arena.int(n1 / n2))
        }
        _ => Err("quotient: arguments must be integers".to_string()),
    }
}

/// (remainder n1 n2) → integer
/// Integer division remainder
pub fn arena_remainder(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("remainder requires exactly 2 arguments".to_string());
    }

    match (arena.get(args[0]), arena.get(args[1])) {
        (ValueData::Integer(n1), ValueData::Integer(n2)) => {
            if *n2 == 0 {
                return Err("remainder: division by zero".to_string());
            }
            Ok(arena.int(n1 % n2))
        }
        _ => Err("remainder: arguments must be integers".to_string()),
    }
}

/// (modulo n1 n2) → integer
/// Modulo operation
pub fn arena_modulo(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("modulo requires exactly 2 arguments".to_string());
    }

    match (arena.get(args[0]), arena.get(args[1])) {
        (ValueData::Integer(n1), ValueData::Integer(n2)) => {
            if *n2 == 0 {
                return Err("modulo: division by zero".to_string());
            }
            Ok(arena.int(n1.rem_euclid(*n2)))
        }
        _ => Err("modulo: arguments must be integers".to_string()),
    }
}

/// (= num1 num2 ...) → boolean
/// Numeric equality
pub fn arena_num_eq(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err("=: requires at least 2 arguments".to_string());
    }

    let first = to_number(arena, args[0])?;
    for &arg in &args[1..] {
        if to_number(arena, arg)? != first {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
    }
    Ok(crate::scheme::arena::TRUE_ID)
}

/// (< num1 num2 ...) → boolean
/// Numeric less-than
pub fn arena_num_lt(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err("<: requires at least 2 arguments".to_string());
    }

    let mut prev = to_number(arena, args[0])?;
    for &arg in &args[1..] {
        let curr = to_number(arena, arg)?;
        if prev >= curr {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
        prev = curr;
    }
    Ok(crate::scheme::arena::TRUE_ID)
}

/// (> num1 num2 ...) → boolean
/// Numeric greater-than
pub fn arena_num_gt(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err(">: requires at least 2 arguments".to_string());
    }

    let mut prev = to_number(arena, args[0])?;
    for &arg in &args[1..] {
        let curr = to_number(arena, arg)?;
        if prev <= curr {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
        prev = curr;
    }
    Ok(crate::scheme::arena::TRUE_ID)
}

/// (<= num1 num2 ...) → boolean
/// Numeric less-than-or-equal
pub fn arena_num_le(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err("<=: requires at least 2 arguments".to_string());
    }

    let mut prev = to_number(arena, args[0])?;
    for &arg in &args[1..] {
        let curr = to_number(arena, arg)?;
        if prev > curr {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
        prev = curr;
    }
    Ok(crate::scheme::arena::TRUE_ID)
}

/// (>= num1 num2 ...) → boolean
/// Numeric greater-than-or-equal
pub fn arena_num_ge(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err(">=: requires at least 2 arguments".to_string());
    }

    let mut prev = to_number(arena, args[0])?;
    for &arg in &args[1..] {
        let curr = to_number(arena, arg)?;
        if prev < curr {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
        prev = curr;
    }
    Ok(crate::scheme::arena::TRUE_ID)
}

/// (abs num) → number
/// Absolute value
pub fn arena_abs(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("abs requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(n) => Ok(arena.int(n.abs())),
        ValueData::Real(f) => Ok(arena.real(f.abs())),
        _ => Err("abs: not a number".to_string()),
    }
}

/// (min num ...) → number
/// Minimum value
pub fn arena_min(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Err("min requires at least 1 argument".to_string());
    }

    let mut min_val = to_number(arena, args[0])?;
    let mut all_ints = matches!(arena.get(args[0]), ValueData::Integer(_));

    for &arg in &args[1..] {
        let val = to_number(arena, arg)?;
        if val < min_val {
            min_val = val;
        }
        if !matches!(arena.get(arg), ValueData::Integer(_)) {
            all_ints = false;
        }
    }

    Ok(if all_ints {
        arena.int(min_val as i64)
    } else {
        arena.real(min_val)
    })
}

/// (max num ...) → number
/// Maximum value
pub fn arena_max(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Err("max requires at least 1 argument".to_string());
    }

    let mut max_val = to_number(arena, args[0])?;
    let mut all_ints = matches!(arena.get(args[0]), ValueData::Integer(_));

    for &arg in &args[1..] {
        let val = to_number(arena, arg)?;
        if val > max_val {
            max_val = val;
        }
        if !matches!(arena.get(arg), ValueData::Integer(_)) {
            all_ints = false;
        }
    }

    Ok(if all_ints {
        arena.int(max_val as i64)
    } else {
        arena.real(max_val)
    })
}

/// (floor num) → integer
/// Round down
pub fn arena_floor(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("floor requires exactly 1 argument".to_string());
    }

    let n = to_number(arena, args[0])?;
    Ok(arena.int(n.floor() as i64))
}

/// (ceiling num) → integer
/// Round up
pub fn arena_ceiling(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("ceiling requires exactly 1 argument".to_string());
    }

    let n = to_number(arena, args[0])?;
    Ok(arena.int(n.ceil() as i64))
}

/// (truncate num) → integer
/// Round toward zero
pub fn arena_truncate(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("truncate requires exactly 1 argument".to_string());
    }

    let n = to_number(arena, args[0])?;
    Ok(arena.int(n.trunc() as i64))
}

/// (round num) → integer
/// Round to nearest
pub fn arena_round(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("round requires exactly 1 argument".to_string());
    }

    let n = to_number(arena, args[0])?;
    Ok(arena.int(n.round() as i64))
}

/// (sqrt num) → number
/// Square root
pub fn arena_sqrt(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("sqrt requires exactly 1 argument".to_string());
    }

    let n = to_number(arena, args[0])?;
    Ok(arena.real(n.sqrt()))
}

/// (sin num) → number
/// Sine
pub fn arena_sin(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("sin requires exactly 1 argument".to_string());
    }

    let n = to_number(arena, args[0])?;
    Ok(arena.real(n.sin()))
}

/// (cos num) → number
/// Cosine
pub fn arena_cos(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("cos requires exactly 1 argument".to_string());
    }

    let n = to_number(arena, args[0])?;
    Ok(arena.real(n.cos()))
}

/// (tan num) → number
/// Tangent
pub fn arena_tan(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("tan requires exactly 1 argument".to_string());
    }

    let n = to_number(arena, args[0])?;
    Ok(arena.real(n.tan()))
}

/// (asin num) → number
/// Arcsine
pub fn arena_asin(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("asin requires exactly 1 argument".to_string());
    }

    let n = to_number(arena, args[0])?;
    Ok(arena.real(n.asin()))
}

/// (acos num) → number
/// Arccosine
pub fn arena_acos(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("acos requires exactly 1 argument".to_string());
    }

    let n = to_number(arena, args[0])?;
    Ok(arena.real(n.acos()))
}

/// (atan num) → number
/// Arctangent
pub fn arena_atan(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("atan requires exactly 1 argument".to_string());
    }

    let n = to_number(arena, args[0])?;
    Ok(arena.real(n.atan()))
}

/// (exp num) → number
/// Exponential (e^x)
pub fn arena_exp(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("exp requires exactly 1 argument".to_string());
    }

    let n = to_number(arena, args[0])?;
    Ok(arena.real(n.exp()))
}

/// (log num) → number
/// Natural logarithm
pub fn arena_log(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("log requires exactly 1 argument".to_string());
    }

    let n = to_number(arena, args[0])?;
    Ok(arena.real(n.ln()))
}

/// (expt base exponent) → number
/// Exponentiation
pub fn arena_expt(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("expt requires exactly 2 arguments".to_string());
    }

    let base = to_number(arena, args[0])?;
    let exp = to_number(arena, args[1])?;
    Ok(arena.real(base.powf(exp)))
}

// =============================================================================
// Batch 4: String Operations (Phase 3)
// =============================================================================

/// (string-length str) → integer
/// Return length of string
pub fn arena_string_length(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("string-length requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        // IMPORTANT: Must count characters, not bytes (for UTF-8 compatibility)
        // string-ref uses chars().nth() which indexes by character
        ValueData::String(s) => Ok(arena.int(s.chars().count() as i64)),
        _ => Err("string-length: not a string".to_string()),
    }
}

/// (string-ref str k) → char
/// Return character at position k
pub fn arena_string_ref(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("string-ref requires exactly 2 arguments".to_string());
    }

    let s = match arena.get(args[0]) {
        ValueData::String(s) => s,
        _ => return Err("string-ref: first argument must be a string".to_string()),
    };

    let k = match arena.get(args[1]) {
        ValueData::Integer(n) if *n >= 0 => *n as usize,
        ValueData::Integer(_) => return Err("string-ref: index must be non-negative".to_string()),
        _ => return Err("string-ref: second argument must be an integer".to_string()),
    };

    match s.chars().nth(k) {
        Some(c) => Ok(arena.char(c)),
        None => Err("string-ref: index out of bounds".to_string()),
    }
}

/// (substring str start end) → string
/// Extract substring from start to end
pub fn arena_substring(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 3 {
        return Err("substring requires exactly 3 arguments".to_string());
    }

    // OpenJade: stringData() accepts both strings and symbols
    let s = get_string_data(arena, args[0], "substring")?;

    let start = match arena.get(args[1]) {
        ValueData::Integer(n) if *n >= 0 => *n as usize,
        ValueData::Integer(_) => return Err("substring: start must be non-negative".to_string()),
        _ => return Err("substring: start must be an integer".to_string()),
    };

    let end = match arena.get(args[2]) {
        ValueData::Integer(n) if *n >= 0 => *n as usize,
        ValueData::Integer(_) => return Err("substring: end must be non-negative".to_string()),
        _ => return Err("substring: end must be an integer".to_string()),
    };

    let chars: Vec<char> = s.chars().collect();
    let len = chars.len();

    // OpenJade clamps indices to string bounds instead of erroring
    // This allows (substring "" 0 1) to return "" instead of failing
    let clamped_start = start.min(len);
    let clamped_end = end.min(len);

    if clamped_start > clamped_end {
        return Err("substring: start must be <= end".to_string());
    }

    let result: String = chars[clamped_start..clamped_end].iter().collect();
    Ok(arena.string(result))
}

/// (string-append str ...) → string
/// Concatenate strings
pub fn arena_string_append(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    let mut result = String::new();

    for &arg in args {
        match arena.get(arg) {
            ValueData::String(s) => result.push_str(s),
            // OpenJade treats unspecified as empty string in string contexts
            ValueData::Unspecified => {},  // Append nothing (empty string)
            _ => return Err("string-append: all arguments must be strings".to_string()),
        }
    }

    Ok(arena.string(result))
}

/// Helper: Extract string data from String or Symbol (like OpenJade's stringData())
fn get_string_data<'a>(arena: &'a Arena, id: ValueId, prim_name: &str) -> Result<std::borrow::Cow<'a, str>, String> {
    match arena.get(id) {
        ValueData::String(s) => Ok(std::borrow::Cow::Borrowed(s.as_str())),
        ValueData::Symbol(s) => Ok(std::borrow::Cow::Borrowed(s.as_ref())),
        _ => Err(format!("{}: not a string or symbol", prim_name)),
    }
}

/// (string=? str1 str2 ...) → boolean
/// String equality (case-sensitive)
pub fn arena_string_eq(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err("string=?: requires at least 2 arguments".to_string());
    }

    let first = get_string_data(arena, args[0], "string=?")?;

    for &arg in &args[1..] {
        let s = get_string_data(arena, arg, "string=?")?;
        if s != first {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
    }

    Ok(crate::scheme::arena::TRUE_ID)
}

/// (string<? str1 str2 ...) → boolean
/// String less-than (case-sensitive)
pub fn arena_string_lt(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err("string<?: requires at least 2 arguments".to_string());
    }

    let mut prev = get_string_data(arena, args[0], "string<?")?.into_owned();

    for &arg in &args[1..] {
        let s = get_string_data(arena, arg, "string<?")?;
        if prev.as_str() >= s.as_ref() {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
        prev = s.into_owned();
    }

    Ok(crate::scheme::arena::TRUE_ID)
}

/// (string>? str1 str2 ...) → boolean
/// String greater-than (case-sensitive)
pub fn arena_string_gt(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err("string>?: requires at least 2 arguments".to_string());
    }

    let mut prev = get_string_data(arena, args[0], "string>?")?.into_owned();

    for &arg in &args[1..] {
        let s = get_string_data(arena, arg, "string>?")?;
        if prev.as_str() <= s.as_ref() {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
        prev = s.into_owned();
    }

    Ok(crate::scheme::arena::TRUE_ID)
}

/// (string<=? str1 str2 ...) → boolean
/// String less-than-or-equal (case-sensitive)
pub fn arena_string_le(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err("string<=?: requires at least 2 arguments".to_string());
    }

    let mut prev = get_string_data(arena, args[0], "string<=?")?.into_owned();

    for &arg in &args[1..] {
        let s = get_string_data(arena, arg, "string<=?")?;
        if prev.as_str() > s.as_ref() {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
        prev = s.into_owned();
    }

    Ok(crate::scheme::arena::TRUE_ID)
}

/// (string>=? str1 str2 ...) → boolean
/// String greater-than-or-equal (case-sensitive)
pub fn arena_string_ge(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err("string>=?: requires at least 2 arguments".to_string());
    }

    let mut prev = get_string_data(arena, args[0], "string>=?")?.into_owned();

    for &arg in &args[1..] {
        let s = get_string_data(arena, arg, "string>=?")?;
        if prev.as_str() < s.as_ref() {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
        prev = s.into_owned();
    }

    Ok(crate::scheme::arena::TRUE_ID)
}

/// (string-ci=? str1 str2 ...) → boolean
/// String equality (case-insensitive)
pub fn arena_string_ci_eq(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err("string-ci=?: requires at least 2 arguments".to_string());
    }

    let first = get_string_data(arena, args[0], "string-ci=?")?.to_lowercase();

    for &arg in &args[1..] {
        let s = get_string_data(arena, arg, "string-ci=?")?;
        if s.to_lowercase() != first {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
    }

    Ok(crate::scheme::arena::TRUE_ID)
}

/// (string-ci<? str1 str2 ...) → boolean
/// String less-than (case-insensitive)
pub fn arena_string_ci_lt(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err("string-ci<?: requires at least 2 arguments".to_string());
    }

    let mut prev = get_string_data(arena, args[0], "string-ci<?")?.to_lowercase();

    for &arg in &args[1..] {
        let s = get_string_data(arena, arg, "string-ci<?")?;
        let curr = s.to_lowercase();
        if prev >= curr {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
        prev = curr;
    }

    Ok(crate::scheme::arena::TRUE_ID)
}

/// (string-ci>? str1 str2 ...) → boolean
/// String greater-than (case-insensitive)
pub fn arena_string_ci_gt(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err("string-ci>?: requires at least 2 arguments".to_string());
    }

    let mut prev = get_string_data(arena, args[0], "string-ci>?")?.to_lowercase();

    for &arg in &args[1..] {
        let s = get_string_data(arena, arg, "string-ci>?")?;
        let curr = s.to_lowercase();
        if prev <= curr {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
        prev = curr;
    }

    Ok(crate::scheme::arena::TRUE_ID)
}

/// (string-ci<=? str1 str2 ...) → boolean
/// String less-than-or-equal (case-insensitive)
pub fn arena_string_ci_le(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err("string-ci<=?: requires at least 2 arguments".to_string());
    }

    let mut prev = get_string_data(arena, args[0], "string-ci<=?")?.to_lowercase();

    for &arg in &args[1..] {
        let s = get_string_data(arena, arg, "string-ci<=?")?;
        let curr = s.to_lowercase();
        if prev > curr {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
        prev = curr;
    }

    Ok(crate::scheme::arena::TRUE_ID)
}

/// (string-ci>=? str1 str2 ...) → boolean
/// String greater-than-or-equal (case-insensitive)
pub fn arena_string_ci_ge(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 {
        return Err("string-ci>=?: requires at least 2 arguments".to_string());
    }

    let mut prev = get_string_data(arena, args[0], "string-ci>=?")?.to_lowercase();

    for &arg in &args[1..] {
        let s = get_string_data(arena, arg, "string-ci>=?")?;
        let curr = s.to_lowercase();
        if prev < curr {
            return Ok(crate::scheme::arena::FALSE_ID);
        }
        prev = curr;
    }

    Ok(crate::scheme::arena::TRUE_ID)
}

// ============================================================================
// Phase 3 Batch 5: Character Operations (19 primitives)
// ============================================================================

/// (char=? char1 char2) → boolean
/// Character equality (case-sensitive)
#[inline]
pub fn arena_char_eq(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("char=? requires exactly 2 arguments".to_string());
    }

    let c1 = match arena.get(args[0]) {
        ValueData::Char(c) => *c,
        _ => return Err("char=?: not a character".to_string()),
    };

    let c2 = match arena.get(args[1]) {
        ValueData::Char(c) => *c,
        _ => return Err("char=?: not a character".to_string()),
    };

    Ok(if c1 == c2 {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (char<? char1 char2) → boolean
/// Character less-than (case-sensitive)
#[inline]
pub fn arena_char_lt(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("char<? requires exactly 2 arguments".to_string());
    }

    let c1 = match arena.get(args[0]) {
        ValueData::Char(c) => *c,
        _ => return Err("char<?: not a character".to_string()),
    };

    let c2 = match arena.get(args[1]) {
        ValueData::Char(c) => *c,
        _ => return Err("char<?: not a character".to_string()),
    };

    Ok(if c1 < c2 {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (char>? char1 char2) → boolean
/// Character greater-than (case-sensitive)
#[inline]
pub fn arena_char_gt(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("char>? requires exactly 2 arguments".to_string());
    }

    let c1 = match arena.get(args[0]) {
        ValueData::Char(c) => *c,
        _ => return Err("char>?: not a character".to_string()),
    };

    let c2 = match arena.get(args[1]) {
        ValueData::Char(c) => *c,
        _ => return Err("char>?: not a character".to_string()),
    };

    Ok(if c1 > c2 {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (char<=? char1 char2) → boolean
/// Character less-than-or-equal (case-sensitive)
#[inline]
pub fn arena_char_le(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("char<=? requires exactly 2 arguments".to_string());
    }

    let c1 = match arena.get(args[0]) {
        ValueData::Char(c) => *c,
        _ => return Err("char<=?: not a character".to_string()),
    };

    let c2 = match arena.get(args[1]) {
        ValueData::Char(c) => *c,
        _ => return Err("char<=?: not a character".to_string()),
    };

    Ok(if c1 <= c2 {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (char>=? char1 char2) → boolean
/// Character greater-than-or-equal (case-sensitive)
#[inline]
pub fn arena_char_ge(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("char>=? requires exactly 2 arguments".to_string());
    }

    let c1 = match arena.get(args[0]) {
        ValueData::Char(c) => *c,
        _ => return Err("char>=?: not a character".to_string()),
    };

    let c2 = match arena.get(args[1]) {
        ValueData::Char(c) => *c,
        _ => return Err("char>=?: not a character".to_string()),
    };

    Ok(if c1 >= c2 {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (char-ci=? char1 char2) → boolean
/// Character equality (case-insensitive)
#[inline]
pub fn arena_char_ci_eq(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("char-ci=? requires exactly 2 arguments".to_string());
    }

    let c1 = match arena.get(args[0]) {
        ValueData::Char(c) => c.to_ascii_lowercase(),
        _ => return Err("char-ci=?: not a character".to_string()),
    };

    let c2 = match arena.get(args[1]) {
        ValueData::Char(c) => c.to_ascii_lowercase(),
        _ => return Err("char-ci=?: not a character".to_string()),
    };

    Ok(if c1 == c2 {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (char-ci<? char1 char2) → boolean
/// Character less-than (case-insensitive)
#[inline]
pub fn arena_char_ci_lt(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("char-ci<? requires exactly 2 arguments".to_string());
    }

    let c1 = match arena.get(args[0]) {
        ValueData::Char(c) => c.to_ascii_lowercase(),
        _ => return Err("char-ci<?: not a character".to_string()),
    };

    let c2 = match arena.get(args[1]) {
        ValueData::Char(c) => c.to_ascii_lowercase(),
        _ => return Err("char-ci<?: not a character".to_string()),
    };

    Ok(if c1 < c2 {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (char-ci>? char1 char2) → boolean
/// Character greater-than (case-insensitive)
#[inline]
pub fn arena_char_ci_gt(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("char-ci>? requires exactly 2 arguments".to_string());
    }

    let c1 = match arena.get(args[0]) {
        ValueData::Char(c) => c.to_ascii_lowercase(),
        _ => return Err("char-ci>?: not a character".to_string()),
    };

    let c2 = match arena.get(args[1]) {
        ValueData::Char(c) => c.to_ascii_lowercase(),
        _ => return Err("char-ci>?: not a character".to_string()),
    };

    Ok(if c1 > c2 {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (char-ci<=? char1 char2) → boolean
/// Character less-than-or-equal (case-insensitive)
#[inline]
pub fn arena_char_ci_le(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("char-ci<=? requires exactly 2 arguments".to_string());
    }

    let c1 = match arena.get(args[0]) {
        ValueData::Char(c) => c.to_ascii_lowercase(),
        _ => return Err("char-ci<=?: not a character".to_string()),
    };

    let c2 = match arena.get(args[1]) {
        ValueData::Char(c) => c.to_ascii_lowercase(),
        _ => return Err("char-ci<=?: not a character".to_string()),
    };

    Ok(if c1 <= c2 {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (char-ci>=? char1 char2) → boolean
/// Character greater-than-or-equal (case-insensitive)
#[inline]
pub fn arena_char_ci_ge(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("char-ci>=? requires exactly 2 arguments".to_string());
    }

    let c1 = match arena.get(args[0]) {
        ValueData::Char(c) => c.to_ascii_lowercase(),
        _ => return Err("char-ci>=?: not a character".to_string()),
    };

    let c2 = match arena.get(args[1]) {
        ValueData::Char(c) => c.to_ascii_lowercase(),
        _ => return Err("char-ci>=?: not a character".to_string()),
    };

    Ok(if c1 >= c2 {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (char-upcase char) → char
/// Convert character to uppercase
pub fn arena_char_upcase(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("char-upcase requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Char(c) => Ok(arena.char(c.to_ascii_uppercase())),
        _ => Err("char-upcase: not a character".to_string()),
    }
}

/// (char-downcase char) → char
/// Convert character to lowercase
pub fn arena_char_downcase(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("char-downcase requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Char(c) => Ok(arena.char(c.to_ascii_lowercase())),
        _ => Err("char-downcase: not a character".to_string()),
    }
}

/// (char-alphabetic? char) → boolean
/// Test if character is alphabetic
#[inline]
pub fn arena_char_alphabetic_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("char-alphabetic? requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Char(c) => Ok(if c.is_alphabetic() {
            crate::scheme::arena::TRUE_ID
        } else {
            crate::scheme::arena::FALSE_ID
        }),
        _ => Err("char-alphabetic?: not a character".to_string()),
    }
}

/// (char-numeric? char) → boolean
/// Test if character is numeric
#[inline]
pub fn arena_char_numeric_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("char-numeric? requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Char(c) => Ok(if c.is_numeric() {
            crate::scheme::arena::TRUE_ID
        } else {
            crate::scheme::arena::FALSE_ID
        }),
        _ => Err("char-numeric?: not a character".to_string()),
    }
}

/// (char-whitespace? char) → boolean
/// Test if character is whitespace
#[inline]
pub fn arena_char_whitespace_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("char-whitespace? requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Char(c) => Ok(if c.is_whitespace() {
            crate::scheme::arena::TRUE_ID
        } else {
            crate::scheme::arena::FALSE_ID
        }),
        _ => Err("char-whitespace?: not a character".to_string()),
    }
}

/// (char->integer char) → integer
/// Convert character to its Unicode code point
pub fn arena_char_to_integer(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("char->integer requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Char(c) => Ok(arena.int(*c as i64)),
        _ => Err("char->integer: not a character".to_string()),
    }
}

/// (integer->char n) → char
/// Convert integer code point to character
pub fn arena_integer_to_char(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("integer->char requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(n) => {
            if *n < 0 || *n > 0x10FFFF {
                return Err(format!("integer->char: invalid code point: {}", n));
            }
            match char::from_u32(*n as u32) {
                Some(c) => Ok(arena.char(c)),
                None => Err(format!("integer->char: invalid Unicode code point: {}", n)),
            }
        }
        _ => Err("integer->char: not an integer".to_string()),
    }
}

/// (char-property char) → string
/// DSSSL extension - returns dummy value
pub fn arena_char_property(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("char-property requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Char(_) => Ok(arena.string("unknown".to_string())),
        _ => Err("char-property: not a character".to_string()),
    }
}

/// (char-script-case char) → symbol
/// DSSSL extension - returns dummy value
pub fn arena_char_script_case(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("char-script-case requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Char(_) => Ok(arena.symbol(std::rc::Rc::from("unknown"))),
        _ => Err("char-script-case: not a character".to_string()),
    }
}

// ============================================================================
// Phase 3 Batch 6: Symbol/Keyword Operations (5 primitives)
// ============================================================================

/// (symbol->string symbol) → string
/// Convert symbol to its string name
pub fn arena_symbol_to_string(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("symbol->string requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Symbol(s) => Ok(arena.string(s.to_string())),
        _ => Err("symbol->string: not a symbol".to_string()),
    }
}

/// (string->symbol string) → symbol
/// Convert string to a symbol
pub fn arena_string_to_symbol(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("string->symbol requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::String(s) => Ok(arena.symbol(std::rc::Rc::from(s.as_str()))),
        _ => Err("string->symbol: not a string".to_string()),
    }
}

/// (keyword? obj) → boolean
/// Test if object is a keyword
#[inline]
pub fn arena_keyword_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("keyword? requires exactly 1 argument".to_string());
    }

    Ok(match arena.get(args[0]) {
        ValueData::Keyword(_) => crate::scheme::arena::TRUE_ID,
        _ => crate::scheme::arena::FALSE_ID,
    })
}

/// (keyword->string keyword) → string
/// Convert keyword to its string name
pub fn arena_keyword_to_string(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("keyword->string requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Keyword(k) => Ok(arena.string(k.to_string())),
        _ => Err("keyword->string: not a keyword".to_string()),
    }
}

/// (string->keyword string) → keyword
/// Convert string to a keyword
pub fn arena_string_to_keyword(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("string->keyword requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::String(s) => Ok(arena.keyword(std::rc::Rc::from(s.as_str()))),
        _ => Err("string->keyword: not a string".to_string()),
    }
}

// ============================================================================
// Phase 3 Batch 7: List Utilities (6 primitives)
// ============================================================================

/// Helper: eq? comparison for arena values
fn arena_eq(arena: &Arena, a: ValueId, b: ValueId) -> bool {
    // Fast path: same ID means same object
    if a == b {
        return true;
    }

    // eq? compares integers, booleans, and characters by value
    // (special case in R4RS - not pure identity for these types)
    match (arena.get(a), arena.get(b)) {
        (ValueData::Integer(n1), ValueData::Integer(n2)) => n1 == n2,
        (ValueData::Char(c1), ValueData::Char(c2)) => c1 == c2,
        (ValueData::Bool(b1), ValueData::Bool(b2)) => b1 == b2,
        (ValueData::Symbol(s1), ValueData::Symbol(s2)) => s1 == s2,
        _ => false,  // For all other types, eq? is identity
    }
}

/// Helper: eqv? comparison for arena values
fn arena_eqv(arena: &Arena, a: ValueId, b: ValueId) -> bool {
    // Fast path: same ID
    if a == b {
        return true;
    }

    // eqv? compares numbers, characters, booleans, and symbols by value
    match (arena.get(a), arena.get(b)) {
        (ValueData::Integer(n1), ValueData::Integer(n2)) => n1 == n2,
        (ValueData::Real(f1), ValueData::Real(f2)) => f1 == f2,
        (ValueData::Char(c1), ValueData::Char(c2)) => c1 == c2,
        (ValueData::Bool(b1), ValueData::Bool(b2)) => b1 == b2,
        (ValueData::Symbol(s1), ValueData::Symbol(s2)) => s1 == s2,
        _ => false,
    }
}

/// (memq obj list) → list or #f
/// Search list for obj using eq? comparison
pub fn arena_memq(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("memq requires exactly 2 arguments".to_string());
    }

    let obj = args[0];
    let mut current = args[1];

    loop {
        if current == crate::scheme::arena::NIL_ID {
            return Ok(crate::scheme::arena::FALSE_ID);
        }

        match arena.get(current) {
            ValueData::Pair { car, cdr, .. } => {
                if arena_eq(arena, obj, *car) {
                    return Ok(current);
                }
                current = *cdr;
            }
            _ => return Err("memq: not a proper list".to_string()),
        }
    }
}

/// (memv obj list) → list or #f
/// Search list for obj using eqv? comparison
pub fn arena_memv(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("memv requires exactly 2 arguments".to_string());
    }

    let obj = args[0];
    let mut current = args[1];

    loop {
        if current == crate::scheme::arena::NIL_ID {
            return Ok(crate::scheme::arena::FALSE_ID);
        }

        match arena.get(current) {
            ValueData::Pair { car, cdr, .. } => {
                if arena_eqv(arena, obj, *car) {
                    return Ok(current);
                }
                current = *cdr;
            }
            _ => return Err("memv: not a proper list".to_string()),
        }
    }
}

/// (member obj list) → list or #f
/// Search list for obj using equal? comparison
pub fn arena_member(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("member requires exactly 2 arguments".to_string());
    }

    let obj = args[0];
    let mut current = args[1];

    loop {
        if current == crate::scheme::arena::NIL_ID {
            return Ok(crate::scheme::arena::FALSE_ID);
        }

        match arena.get(current) {
            ValueData::Pair { car, cdr, .. } => {
                if arena_equal_impl(arena, obj, *car) {
                    return Ok(current);
                }
                current = *cdr;
            }
            _ => return Err("member: not a proper list".to_string()),
        }
    }
}

/// (assq obj alist) → pair or #f
/// Search association list for pair with car eq? to obj
pub fn arena_assq(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("assq requires exactly 2 arguments".to_string());
    }

    let obj = args[0];
    let mut current = args[1];

    loop {
        if current == crate::scheme::arena::NIL_ID {
            return Ok(crate::scheme::arena::FALSE_ID);
        }

        match arena.get(current) {
            ValueData::Pair { car, cdr, .. } => {
                // Check if car is a pair
                if let ValueData::Pair { car: inner_car, .. } = arena.get(*car) {
                    if arena_eq(arena, obj, *inner_car) {
                        return Ok(*car);
                    }
                }
                current = *cdr;
            }
            _ => return Err("assq: not a proper list".to_string()),
        }
    }
}

/// (assv obj alist) → pair or #f
/// Search association list for pair with car eqv? to obj
pub fn arena_assv(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("assv requires exactly 2 arguments".to_string());
    }

    let obj = args[0];
    let mut current = args[1];

    loop {
        if current == crate::scheme::arena::NIL_ID {
            return Ok(crate::scheme::arena::FALSE_ID);
        }

        match arena.get(current) {
            ValueData::Pair { car, cdr, .. } => {
                // Check if car is a pair
                if let ValueData::Pair { car: inner_car, .. } = arena.get(*car) {
                    if arena_eqv(arena, obj, *inner_car) {
                        return Ok(*car);
                    }
                }
                current = *cdr;
            }
            _ => return Err("assv: not a proper list".to_string()),
        }
    }
}

/// (assoc obj alist) → pair or #f
/// Search association list for pair with car equal? to obj
pub fn arena_assoc(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("assoc requires exactly 2 arguments".to_string());
    }

    let obj = args[0];
    let mut current = args[1];

    loop {
        if current == crate::scheme::arena::NIL_ID {
            return Ok(crate::scheme::arena::FALSE_ID);
        }

        match arena.get(current) {
            ValueData::Pair { car, cdr, .. } => {
                // Check if car is a pair
                if let ValueData::Pair { car: inner_car, .. } = arena.get(*car) {
                    if arena_equal_impl(arena, obj, *inner_car) {
                        return Ok(*car);
                    }
                }
                current = *cdr;
            }
            _ => return Err("assoc: not a proper list".to_string()),
        }
    }
}

// ============================================================================
// Phase 3 Batch 8: Logic and List Accessors (6 primitives)
// ============================================================================

/// (not obj) → boolean
/// Logical not - returns #t if obj is false, #f otherwise
#[inline]
pub fn arena_not(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("not requires exactly 1 argument".to_string());
    }

    let val = args[0];
    // In Scheme, only #f is false; everything else is true
    Ok(if val == crate::scheme::arena::FALSE_ID {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (eq? obj1 obj2) → boolean
/// Test for object identity (pointer equality)
#[inline]
pub fn arena_eq_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("eq? requires exactly 2 arguments".to_string());
    }

    Ok(if arena_eq(arena, args[0], args[1]) {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (eqv? obj1 obj2) → boolean
/// Test for equivalence (numbers and chars by value, others by identity)
#[inline]
pub fn arena_eqv_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("eqv? requires exactly 2 arguments".to_string());
    }

    Ok(if arena_eqv(arena, args[0], args[1]) {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// (caar list) → value
/// Equivalent to (car (car list))
#[inline]
pub fn arena_caar(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("caar requires exactly 1 argument".to_string());
    }

    let car1 = arena_car(arena, args)?;
    arena_car(arena, &[car1])
}

/// (cdar list) → value
/// Equivalent to (cdr (car list))
#[inline]
pub fn arena_cdar(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("cdar requires exactly 1 argument".to_string());
    }

    let car_val = arena_car(arena, args)?;
    arena_cdr(arena, &[car_val])
}

/// (cddr list) → value
/// Equivalent to (cdr (cdr list))
#[inline]
pub fn arena_cddr(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("cddr requires exactly 1 argument".to_string());
    }

    let cdr1 = arena_cdr(arena, args)?;
    arena_cdr(arena, &[cdr1])
}

// Phase 3 Batch 9: Extended List Accessors (7 primitives)

/// (caaar list) → value
/// Equivalent to (car (car (car list)))
#[inline]
pub fn arena_caaar(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("caaar requires exactly 1 argument".to_string());
    }
    let car1 = arena_car(arena, args)?;
    let car2 = arena_car(arena, &[car1])?;
    arena_car(arena, &[car2])
}

/// (caadr list) → value
/// Equivalent to (car (car (cdr list)))
#[inline]
pub fn arena_caadr(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("caadr requires exactly 1 argument".to_string());
    }
    let cdr1 = arena_cdr(arena, args)?;
    let car2 = arena_car(arena, &[cdr1])?;
    arena_car(arena, &[car2])
}

/// (cadar list) → value
/// Equivalent to (car (cdr (car list)))
#[inline]
pub fn arena_cadar(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("cadar requires exactly 1 argument".to_string());
    }
    let car1 = arena_car(arena, args)?;
    let cdr2 = arena_cdr(arena, &[car1])?;
    arena_car(arena, &[cdr2])
}

/// (cdaar list) → value
/// Equivalent to (cdr (car (car list)))
#[inline]
pub fn arena_cdaar(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("cdaar requires exactly 1 argument".to_string());
    }
    let car1 = arena_car(arena, args)?;
    let car2 = arena_car(arena, &[car1])?;
    arena_cdr(arena, &[car2])
}

/// (cdadr list) → value
/// Equivalent to (cdr (car (cdr list)))
#[inline]
pub fn arena_cdadr(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("cdadr requires exactly 1 argument".to_string());
    }
    let cdr1 = arena_cdr(arena, args)?;
    let car2 = arena_car(arena, &[cdr1])?;
    arena_cdr(arena, &[car2])
}

/// (cddar list) → value
/// Equivalent to (cdr (cdr (car list)))
#[inline]
pub fn arena_cddar(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("cddar requires exactly 1 argument".to_string());
    }
    let car1 = arena_car(arena, args)?;
    let cdr2 = arena_cdr(arena, &[car1])?;
    arena_cdr(arena, &[cdr2])
}

/// (cdddr list) → value
/// Equivalent to (cdr (cdr (cdr list)))
#[inline]
pub fn arena_cdddr(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("cdddr requires exactly 1 argument".to_string());
    }
    let cdr1 = arena_cdr(arena, args)?;
    let cdr2 = arena_cdr(arena, &[cdr1])?;
    arena_cdr(arena, &[cdr2])
}

// Phase 3 Batch 10: Vector Operations (8 primitives)

/// (vector obj ...) → vector
/// Create a vector from the given arguments
#[inline]
pub fn arena_vector(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    Ok(arena.vector(args.to_vec()))
}

/// (make-vector k [fill]) → vector
/// Create a vector of length k, optionally filled with fill value
#[inline]
pub fn arena_make_vector(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() || args.len() > 2 {
        return Err("make-vector requires 1 or 2 arguments".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(k) if *k >= 0 => {
            let size = *k as usize;
            let fill = if args.len() == 2 {
                args[1]
            } else {
                crate::scheme::arena::UNSPECIFIED_ID
            };
            let elements = vec![fill; size];
            Ok(arena.vector(elements))
        }
        ValueData::Integer(_) => Err("make-vector: size must be non-negative".to_string()),
        _ => Err("make-vector: first argument must be an integer".to_string()),
    }
}

/// (vector-length vec) → integer
/// Return the length of the vector
#[inline]
pub fn arena_vector_length(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("vector-length requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Vector(elements) => Ok(arena.int(elements.len() as i64)),
        _ => Err("vector-length: argument must be a vector".to_string()),
    }
}

/// (vector-ref vec k) → value
/// Return the element at index k
#[inline]
pub fn arena_vector_ref(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("vector-ref requires exactly 2 arguments".to_string());
    }

    let vec_id = args[0];
    match arena.get(vec_id) {
        ValueData::Vector(elements) => {
            match arena.get(args[1]) {
                ValueData::Integer(k) if *k >= 0 => {
                    let index = *k as usize;
                    if index < elements.len() {
                        Ok(elements[index])
                    } else {
                        Err(format!("vector-ref: index {} out of bounds (length {})", index, elements.len()))
                    }
                }
                ValueData::Integer(_) => Err("vector-ref: index must be non-negative".to_string()),
                _ => Err("vector-ref: second argument must be an integer".to_string()),
            }
        }
        _ => Err("vector-ref: first argument must be a vector".to_string()),
    }
}

/// (vector-set! vec k obj) → unspecified
/// Set the element at index k to obj
/// NOTE: This mutates the vector in place (returns unspecified)
#[inline]
pub fn arena_vector_set(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 3 {
        return Err("vector-set! requires exactly 3 arguments".to_string());
    }

    let vec_id = args[0];
    let new_val = args[2];

    match arena.get(args[1]) {
        ValueData::Integer(k) if *k >= 0 => {
            let index = *k as usize;
            match arena.get_mut(vec_id) {
                ValueData::Vector(ref mut elements) => {
                    if index < elements.len() {
                        elements[index] = new_val;
                        Ok(crate::scheme::arena::UNSPECIFIED_ID)
                    } else {
                        Err(format!("vector-set!: index {} out of bounds (length {})", index, elements.len()))
                    }
                }
                _ => Err("vector-set!: first argument must be a vector".to_string()),
            }
        }
        ValueData::Integer(_) => Err("vector-set!: index must be non-negative".to_string()),
        _ => Err("vector-set!: second argument must be an integer".to_string()),
    }
}

/// (vector->list vec) → list
/// Convert a vector to a list
#[inline]
pub fn arena_vector_to_list(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("vector->list requires exactly 1 argument".to_string());
    }

    // Clone elements to avoid borrowing arena while mutating it
    let elements = match arena.get(args[0]) {
        ValueData::Vector(elements) => elements.clone(),
        _ => return Err("vector->list: argument must be a vector".to_string()),
    };

    // Build list from vector elements (in reverse order for efficiency)
    let mut result = crate::scheme::arena::NIL_ID;
    for &elem in elements.iter().rev() {
        result = arena.cons(elem, result);
    }
    Ok(result)
}

/// (list->vector list) → vector
/// Convert a list to a vector
#[inline]
pub fn arena_list_to_vector(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("list->vector requires exactly 1 argument".to_string());
    }

    // Collect list elements
    let mut elements = Vec::new();
    let mut current = args[0];

    loop {
        match arena.get(current) {
            ValueData::Nil => break,
            ValueData::Pair { car, cdr, .. } => {
                elements.push(*car);
                current = *cdr;
            }
            _ => return Err("list->vector: argument must be a proper list".to_string()),
        }
    }

    Ok(arena.vector(elements))
}

/// (vector-fill! vec fill) → unspecified
/// Fill all elements of the vector with the fill value
#[inline]
pub fn arena_vector_fill(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("vector-fill! requires exactly 2 arguments".to_string());
    }

    let vec_id = args[0];
    let fill = args[1];

    match arena.get_mut(vec_id) {
        ValueData::Vector(ref mut elements) => {
            for elem in elements.iter_mut() {
                *elem = fill;
            }
            Ok(crate::scheme::arena::UNSPECIFIED_ID)
        }
        _ => Err("vector-fill!: first argument must be a vector".to_string()),
    }
}

// Phase 3 Batch 11: Additional Type Predicates & Utilities (5 primitives)

/// (vector? obj) → boolean
/// Returns #t if obj is a vector, #f otherwise
#[inline]
pub fn arena_vector_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("vector? requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Vector(_) => Ok(crate::scheme::arena::TRUE_ID),
        _ => Ok(crate::scheme::arena::FALSE_ID),
    }
}

/// (procedure? obj) → boolean
/// Returns #t if obj is a procedure, #f otherwise
#[inline]
pub fn arena_procedure_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("procedure? requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Procedure(_) => Ok(crate::scheme::arena::TRUE_ID),
        _ => Ok(crate::scheme::arena::FALSE_ID),
    }
}

/// (set-car! pair obj) → unspecified
/// Mutate the car of a pair
#[inline]
pub fn arena_set_car(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("set-car! requires exactly 2 arguments".to_string());
    }

    let pair_id = args[0];
    let new_car = args[1];

    match arena.get_mut(pair_id) {
        ValueData::Pair { car, .. } => {
            *car = new_car;
            Ok(crate::scheme::arena::UNSPECIFIED_ID)
        }
        _ => Err("set-car!: first argument must be a pair".to_string()),
    }
}

/// (set-cdr! pair obj) → unspecified
/// Mutate the cdr of a pair
#[inline]
pub fn arena_set_cdr(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("set-cdr! requires exactly 2 arguments".to_string());
    }

    let pair_id = args[0];
    let new_cdr = args[1];

    match arena.get_mut(pair_id) {
        ValueData::Pair { cdr, .. } => {
            *cdr = new_cdr;
            Ok(crate::scheme::arena::UNSPECIFIED_ID)
        }
        _ => Err("set-cdr!: first argument must be a pair".to_string()),
    }
}

/// (list-tail list k) → value
/// Returns the sublist of list obtained by omitting the first k elements
#[inline]
pub fn arena_list_tail(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("list-tail requires exactly 2 arguments".to_string());
    }

    let k = match arena.get(args[1]) {
        ValueData::Integer(n) if *n >= 0 => *n as usize,
        ValueData::Integer(_) => return Err("list-tail: index must be non-negative".to_string()),
        _ => return Err("list-tail: second argument must be an integer".to_string()),
    };

    let mut current = args[0];
    for _ in 0..k {
        match arena.get(current) {
            ValueData::Pair { cdr, .. } => {
                current = *cdr;
            }
            ValueData::Nil => return Err("list-tail: list too short".to_string()),
            _ => return Err("list-tail: argument must be a list".to_string()),
        }
    }

    Ok(current)
}

// Phase 3 Batch 12: String Utilities (6 primitives)

/// (string-upcase str) → string
/// Convert string to uppercase
#[inline]
pub fn arena_string_upcase(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("string-upcase requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::String(s) => Ok(arena.string(s.to_uppercase())),
        _ => Err("string-upcase: argument must be a string".to_string()),
    }
}

/// (string-downcase str) → string
/// Convert string to lowercase
#[inline]
pub fn arena_string_downcase(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("string-downcase requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::String(s) => Ok(arena.string(s.to_lowercase())),
        _ => Err("string-downcase: argument must be a string".to_string()),
    }
}

/// (case-fold-down str) → string
/// DSSSL alias for string-downcase - convert string to lowercase
#[inline]
pub fn arena_case_fold_down(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    arena_string_downcase(arena, args)
}

/// (string->number str) → number or #f
/// Parse a string as a number
#[inline]
pub fn arena_string_to_number(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("string->number requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::String(s) => {
            // Try integer first
            if let Ok(n) = s.parse::<i64>() {
                return Ok(arena.int(n));
            }
            // Try float
            if let Ok(f) = s.parse::<f64>() {
                return Ok(arena.real(f));
            }
            // Parse failed - return #f
            Ok(crate::scheme::arena::FALSE_ID)
        }
        _ => Err("string->number: argument must be a string".to_string()),
    }
}

/// (number->string num) → string
/// Convert a number to a string
#[inline]
pub fn arena_number_to_string(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("number->string requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(n) => Ok(arena.string(n.to_string())),
        ValueData::Real(f) => Ok(arena.string(f.to_string())),
        _ => Err("number->string: argument must be a number".to_string()),
    }
}

/// (string->list str) → list
/// Convert a string to a list of characters
#[inline]
pub fn arena_string_to_list(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("string->list requires exactly 1 argument".to_string());
    }

    // Clone the string to avoid borrow checker issues
    let s = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        _ => return Err("string->list: argument must be a string".to_string()),
    };

    let mut result = crate::scheme::arena::NIL_ID;
    for c in s.chars().rev() {
        let char_id = arena.char(c);
        result = arena.cons(char_id, result);
    }
    Ok(result)
}

/// (list->string list) → string
/// Convert a list of characters to a string
#[inline]
pub fn arena_list_to_string(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("list->string requires exactly 1 argument".to_string());
    }

    let mut chars = Vec::new();
    let mut current = args[0];

    loop {
        match arena.get(current) {
            ValueData::Nil => break,
            ValueData::Pair { car, cdr, .. } => {
                match arena.get(*car) {
                    ValueData::Char(c) => {
                        chars.push(*c);
                        current = *cdr;
                    }
                    _ => return Err("list->string: list must contain only characters".to_string()),
                }
            }
            _ => return Err("list->string: argument must be a proper list".to_string()),
        }
    }

    let s: String = chars.into_iter().collect();
    Ok(arena.string(s))
}

// Phase 3 Batch 13: Miscellaneous Utilities (7 primitives)

/// (gcd n1 n2 ...) → integer
/// Compute greatest common divisor
#[inline]
pub fn arena_gcd(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Ok(arena.int(0));
    }

    fn gcd_two(a: i64, b: i64) -> i64 {
        let (mut a, mut b) = (a.abs(), b.abs());
        while b != 0 {
            let temp = b;
            b = a % b;
            a = temp;
        }
        a
    }

    let mut result = 0i64;
    for &arg in args {
        match arena.get(arg) {
            ValueData::Integer(n) => {
                result = gcd_two(result, *n);
            }
            _ => return Err("gcd: all arguments must be integers".to_string()),
        }
    }

    Ok(arena.int(result))
}

/// (lcm n1 n2 ...) → integer
/// Compute least common multiple
#[inline]
pub fn arena_lcm(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Ok(arena.int(1));
    }

    fn gcd_two(a: i64, b: i64) -> i64 {
        let (mut a, mut b) = (a.abs(), b.abs());
        while b != 0 {
            let temp = b;
            b = a % b;
            a = temp;
        }
        a
    }

    fn lcm_two(a: i64, b: i64) -> i64 {
        if a == 0 || b == 0 {
            return 0;
        }
        (a.abs() / gcd_two(a, b)) * b.abs()
    }

    let mut result = 1i64;
    for &arg in args {
        match arena.get(arg) {
            ValueData::Integer(n) => {
                result = lcm_two(result, *n);
            }
            _ => return Err("lcm: all arguments must be integers".to_string()),
        }
    }

    Ok(arena.int(result))
}

/// (exact->inexact num) → real
/// Convert exact number to inexact (integer to real)
#[inline]
pub fn arena_exact_to_inexact(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("exact->inexact requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(n) => Ok(arena.real(*n as f64)),
        ValueData::Real(_) => Ok(args[0]), // Already inexact
        _ => Err("exact->inexact: argument must be a number".to_string()),
    }
}

/// (inexact->exact num) → integer
/// Convert inexact number to exact (real to integer)
#[inline]
pub fn arena_inexact_to_exact(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("inexact->exact requires exactly 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Real(f) => Ok(arena.int(f.round() as i64)),
        ValueData::Integer(_) => Ok(args[0]), // Already exact
        _ => Err("inexact->exact: argument must be a number".to_string()),
    }
}

/// (make-string k [char]) → string
/// Create a string of length k filled with char (or space)
#[inline]
pub fn arena_make_string(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() || args.len() > 2 {
        return Err("make-string requires 1 or 2 arguments".to_string());
    }

    let k = match arena.get(args[0]) {
        ValueData::Integer(n) if *n >= 0 => *n as usize,
        ValueData::Integer(_) => return Err("make-string: length must be non-negative".to_string()),
        _ => return Err("make-string: first argument must be an integer".to_string()),
    };

    let fill_char = if args.len() == 2 {
        match arena.get(args[1]) {
            ValueData::Char(c) => *c,
            _ => return Err("make-string: second argument must be a character".to_string()),
        }
    } else {
        ' '
    };

    let s: String = std::iter::repeat(fill_char).take(k).collect();
    Ok(arena.string(s))
}

/// (string char ...) → string
/// Construct a string from characters
#[inline]
pub fn arena_string(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    let mut chars = Vec::new();

    for &arg in args {
        match arena.get(arg) {
            ValueData::Char(c) => chars.push(*c),
            _ => return Err("string: all arguments must be characters".to_string()),
        }
    }

    let s: String = chars.into_iter().collect();
    Ok(arena.string(s))
}

/// (reverse! list) → list
/// Destructively reverse a list
#[inline]
pub fn arena_reverse_bang(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("reverse! requires exactly 1 argument".to_string());
    }

    // Collect pairs first to avoid borrow issues
    let mut pairs = Vec::new();
    let mut current = args[0];

    loop {
        match arena.get(current) {
            ValueData::Nil => break,
            ValueData::Pair { car, cdr, .. } => {
                pairs.push((current, *car));
                current = *cdr;
            }
            _ => return Err("reverse!: argument must be a list".to_string()),
        }
    }

    if pairs.is_empty() {
        return Ok(crate::scheme::arena::NIL_ID);
    }

    // Reverse by mutating cdr pointers
    let mut prev = crate::scheme::arena::NIL_ID;
    for &(pair_id, _car_val) in &pairs {
        match arena.get_mut(pair_id) {
            ValueData::Pair { cdr, .. } => {
                *cdr = prev;
                prev = pair_id;
            }
            _ => unreachable!(),
        }
    }

    Ok(prev)
}

// =============================================================================
// Batch 14: Additional String and Character Operations (5 primitives)
// =============================================================================

/// string-set! - mutate string at index
/// Note: Strings in arena are immutable, so this creates a new string
pub fn arena_string_set(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 3 {
        return Err("string-set!: expected 3 arguments".to_string());
    }

    let s = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        _ => return Err("string-set!: first argument must be a string".to_string()),
    };

    let k = match arena.get(args[1]) {
        ValueData::Integer(n) => *n as usize,
        _ => return Err("string-set!: second argument must be an integer".to_string()),
    };

    let c = match arena.get(args[2]) {
        ValueData::Char(c) => *c,
        _ => return Err("string-set!: third argument must be a character".to_string()),
    };

    if k >= s.len() {
        return Err(format!("string-set!: index {} out of bounds for string of length {}", k, s.len()));
    }

    let mut chars: Vec<char> = s.chars().collect();
    chars[k] = c;
    let new_string: String = chars.into_iter().collect();

    Ok(arena.string(new_string))
}

/// string-copy - copy a string
pub fn arena_string_copy(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("string-copy: expected 1 argument".to_string());
    }

    let s = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        _ => return Err("string-copy: argument must be a string".to_string()),
    };

    Ok(arena.string(s))
}

/// string-fill! - fill string with character
/// Note: Strings in arena are immutable, so this creates a new string
pub fn arena_string_fill(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("string-fill!: expected 2 arguments".to_string());
    }

    let s = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        _ => return Err("string-fill!: first argument must be a string".to_string()),
    };

    let c = match arena.get(args[1]) {
        ValueData::Char(c) => *c,
        _ => return Err("string-fill!: second argument must be a character".to_string()),
    };

    let new_string: String = std::iter::repeat(c).take(s.len()).collect();
    Ok(arena.string(new_string))
}

/// char-lower-case? - check if character is lowercase
pub fn arena_char_lower_case_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("char-lower-case?: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Char(c) => Ok(if c.is_lowercase() { crate::scheme::arena::TRUE_ID } else { crate::scheme::arena::FALSE_ID }),
        _ => Err("char-lower-case?: argument must be a character".to_string()),
    }
}

/// char-upper-case? - check if character is uppercase
pub fn arena_char_upper_case_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("char-upper-case?: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Char(c) => Ok(if c.is_uppercase() { crate::scheme::arena::TRUE_ID } else { crate::scheme::arena::FALSE_ID }),
        _ => Err("char-upper-case?: argument must be a character".to_string()),
    }
}

// =============================================================================
// Batch 15: List Utilities (5 primitives)
// =============================================================================

/// last - return last element of list
pub fn arena_last(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("last: expected 1 argument".to_string());
    }

    let mut current = args[0];
    let mut prev_car = crate::scheme::arena::NIL_ID;

    loop {
        match arena.get(current) {
            ValueData::Nil => {
                if prev_car == crate::scheme::arena::NIL_ID {
                    return Err("last: argument must be a non-empty list".to_string());
                }
                return Ok(prev_car);
            }
            ValueData::Pair { car, cdr, .. } => {
                prev_car = *car;
                current = *cdr;
            }
            _ => return Err("last: argument must be a proper list".to_string()),
        }
    }
}

/// last-pair - return last pair of list
pub fn arena_last_pair(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("last-pair: expected 1 argument".to_string());
    }

    let mut current = args[0];

    // Check if empty list
    if matches!(arena.get(current), ValueData::Nil) {
        return Err("last-pair: argument must be a non-empty list".to_string());
    }

    loop {
        match arena.get(current) {
            ValueData::Pair { cdr, .. } => {
                if matches!(arena.get(*cdr), ValueData::Nil) {
                    return Ok(current);
                }
                current = *cdr;
            }
            _ => return Err("last-pair: argument must be a proper list".to_string()),
        }
    }
}

/// list-copy - shallow copy a list
pub fn arena_list_copy(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("list-copy: expected 1 argument".to_string());
    }

    let mut result = crate::scheme::arena::NIL_ID;
    let mut tail = crate::scheme::arena::NIL_ID;
    let mut current = args[0];

    loop {
        // Extract car and cdr before mutating arena
        let (car_val, cdr_val) = match arena.get(current) {
            ValueData::Nil => {
                return Ok(result);
            }
            ValueData::Pair { car, cdr, .. } => (*car, *cdr),
            _ => return Err("list-copy: argument must be a proper list".to_string()),
        };

        let new_pair = arena.cons(car_val, crate::scheme::arena::NIL_ID);
        if result == crate::scheme::arena::NIL_ID {
            result = new_pair;
            tail = new_pair;
        } else {
            // Update the cdr of the previous pair
            match arena.get_mut(tail) {
                ValueData::Pair { cdr: tail_cdr, .. } => {
                    *tail_cdr = new_pair;
                }
                _ => unreachable!(),
            }
            tail = new_pair;
        }
        current = cdr_val;
    }
}

/// append! - destructive append (mutates last cdr of first list)
pub fn arena_append_bang(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Ok(crate::scheme::arena::NIL_ID);
    }

    if args.len() == 1 {
        return Ok(args[0]);
    }

    // Find the last pair of the first list
    let first = args[0];
    if matches!(arena.get(first), ValueData::Nil) {
        // If first list is empty, return the rest
        return arena_append_bang(arena, &args[1..]);
    }

    let mut current = first;
    loop {
        match arena.get(current) {
            ValueData::Pair { cdr, .. } => {
                if matches!(arena.get(*cdr), ValueData::Nil) {
                    // Found the last pair, mutate its cdr
                    let rest = if args.len() > 2 {
                        arena_append_bang(arena, &args[1..])?
                    } else {
                        args[1]
                    };
                    match arena.get_mut(current) {
                        ValueData::Pair { cdr: last_cdr, .. } => {
                            *last_cdr = rest;
                        }
                        _ => unreachable!(),
                    }
                    return Ok(first);
                }
                current = *cdr;
            }
            _ => return Err("append!: arguments must be lists".to_string()),
        }
    }
}

/// iota - generate list of integers from 0 to n-1
pub fn arena_iota(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() || args.len() > 3 {
        return Err("iota: expected 1 to 3 arguments".to_string());
    }

    let count = match arena.get(args[0]) {
        ValueData::Integer(n) => *n,
        _ => return Err("iota: first argument must be an integer".to_string()),
    };

    let start = if args.len() > 1 {
        match arena.get(args[1]) {
            ValueData::Integer(n) => *n,
            _ => return Err("iota: second argument must be an integer".to_string()),
        }
    } else {
        0
    };

    let step = if args.len() > 2 {
        match arena.get(args[2]) {
            ValueData::Integer(n) => *n,
            _ => return Err("iota: third argument must be an integer".to_string()),
        }
    } else {
        1
    };

    if count < 0 {
        return Err("iota: count must be non-negative".to_string());
    }

    let mut result = crate::scheme::arena::NIL_ID;
    for i in (0..count).rev() {
        let val = arena.int(start + i * step);
        result = arena.cons(val, result);
    }

    Ok(result)
}

// =============================================================================
// Batch 16: Extended List Operations (5 primitives)
// =============================================================================

/// take - take first n elements of list
pub fn arena_take(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("take: expected 2 arguments".to_string());
    }

    let list = args[0];
    let n = match arena.get(args[1]) {
        ValueData::Integer(n) => *n,
        _ => return Err("take: second argument must be an integer".to_string()),
    };

    if n < 0 {
        return Err("take: count must be non-negative".to_string());
    }

    let mut result = crate::scheme::arena::NIL_ID;
    let mut tail = crate::scheme::arena::NIL_ID;
    let mut current = list;
    let mut count = 0;

    while count < n {
        let (car_val, cdr_val) = match arena.get(current) {
            ValueData::Nil => {
                return Err(format!("take: list has only {} elements, cannot take {}", count, n));
            }
            ValueData::Pair { car, cdr, .. } => (*car, *cdr),
            _ => return Err("take: first argument must be a list".to_string()),
        };

        let new_pair = arena.cons(car_val, crate::scheme::arena::NIL_ID);
        if result == crate::scheme::arena::NIL_ID {
            result = new_pair;
            tail = new_pair;
        } else {
            match arena.get_mut(tail) {
                ValueData::Pair { cdr: tail_cdr, .. } => {
                    *tail_cdr = new_pair;
                }
                _ => unreachable!(),
            }
            tail = new_pair;
        }

        current = cdr_val;
        count += 1;
    }

    Ok(result)
}

/// drop - drop first n elements of list
pub fn arena_drop(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("drop: expected 2 arguments".to_string());
    }

    let mut list = args[0];
    let n = match arena.get(args[1]) {
        ValueData::Integer(n) => *n,
        _ => return Err("drop: second argument must be an integer".to_string()),
    };

    if n < 0 {
        return Err("drop: count must be non-negative".to_string());
    }

    for i in 0..n {
        match arena.get(list) {
            ValueData::Nil => {
                return Err(format!("drop: list has only {} elements, cannot drop {}", i, n));
            }
            ValueData::Pair { cdr, .. } => {
                list = *cdr;
            }
            _ => return Err("drop: first argument must be a list".to_string()),
        }
    }

    Ok(list)
}

/// split-at - split list at position n, returns (take n list) . (drop n list)
/// Note: Returns a pair of lists
pub fn arena_split_at(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("split-at: expected 2 arguments".to_string());
    }

    let taken = arena_take(arena, args)?;
    let dropped = arena_drop(arena, args)?;

    Ok(arena.cons(taken, dropped))
}

/// filter - filter list by predicate (requires evaluator context)
/// Note: This is a simplified version that can't call back to the evaluator
/// For now, we'll make it return an error suggesting use of the non-arena version
pub fn arena_filter(_arena: &Arena, _args: &[ValueId]) -> ArenaResult {
    Err("filter: not available in arena mode (requires callback to evaluator)".to_string())
}

/// remove - remove elements matching predicate (requires evaluator context)
/// Note: This is a simplified version that can't call back to the evaluator
/// For now, we'll make it return an error suggesting use of the non-arena version
pub fn arena_remove(_arena: &Arena, _args: &[ValueId]) -> ArenaResult {
    Err("remove: not available in arena mode (requires callback to evaluator)".to_string())
}

// =============================================================================
// Batch 17: Additional Numeric Operations (6 primitives)
// =============================================================================

/// numerator - get numerator of rational (for now, just return the integer part)
pub fn arena_numerator(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("numerator: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(n) => Ok(arena.int(*n)),
        ValueData::Real(f) => {
            // For real numbers, treat as improper fraction with denominator 1
            Ok(arena.int(*f as i64))
        }
        _ => Err("numerator: argument must be a number".to_string()),
    }
}

/// denominator - get denominator of rational (for now, always return 1)
pub fn arena_denominator(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("denominator: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(_) | ValueData::Real(_) => Ok(arena.int(1)),
        _ => Err("denominator: argument must be a number".to_string()),
    }
}

/// rationalize - rationalize a number to within tolerance (simplified: just return the number)
pub fn arena_rationalize(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("rationalize: expected 2 arguments".to_string());
    }

    // Simplified: just return the first argument
    match arena.get(args[0]) {
        ValueData::Integer(_) | ValueData::Real(_) => Ok(args[0]),
        _ => Err("rationalize: first argument must be a number".to_string()),
    }
}

/// angle - get angle of complex number (for real numbers, 0 or pi)
pub fn arena_angle(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("angle: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(n) => {
            if *n < 0 {
                Ok(arena.real(std::f64::consts::PI))
            } else {
                Ok(arena.real(0.0))
            }
        }
        ValueData::Real(f) => {
            if *f < 0.0 {
                Ok(arena.real(std::f64::consts::PI))
            } else {
                Ok(arena.real(0.0))
            }
        }
        _ => Err("angle: argument must be a number".to_string()),
    }
}

/// magnitude - get magnitude of complex number (for real numbers, absolute value)
pub fn arena_magnitude(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("magnitude: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(n) => Ok(arena.int(n.abs())),
        ValueData::Real(f) => Ok(arena.real(f.abs())),
        _ => Err("magnitude: argument must be a number".to_string()),
    }
}

/// string->number with optional radix (note: we already have arena_string_to_number)
/// This is an extended version that supports radix parameter
pub fn arena_string_to_number_radix(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() || args.len() > 2 {
        return Err("string->number: expected 1 or 2 arguments".to_string());
    }

    let s = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        _ => return Err("string->number: first argument must be a string".to_string()),
    };

    let radix = if args.len() == 2 {
        match arena.get(args[1]) {
            ValueData::Integer(r) => *r as u32,
            _ => return Err("string->number: radix must be an integer".to_string()),
        }
    } else {
        10
    };

    if radix < 2 || radix > 36 {
        return Err("string->number: radix must be between 2 and 36".to_string());
    }

    // Try to parse as integer with given radix
    if let Ok(n) = i64::from_str_radix(&s, radix) {
        return Ok(arena.int(n));
    }

    // For radix 10, also try floating point
    if radix == 10 {
        if let Ok(f) = s.parse::<f64>() {
            return Ok(arena.real(f));
        }
    }

    // Return false on parse failure
    Ok(crate::scheme::arena::FALSE_ID)
}

// =============================================================================
// Batch 18: Conversion and Utility Operations (5 primitives)
// =============================================================================

/// number->string with radix support
pub fn arena_number_to_string_radix(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() || args.len() > 2 {
        return Err("number->string: expected 1 or 2 arguments".to_string());
    }

    let radix = if args.len() == 2 {
        match arena.get(args[1]) {
            ValueData::Integer(r) => *r as u32,
            _ => return Err("number->string: radix must be an integer".to_string()),
        }
    } else {
        10
    };

    if radix < 2 || radix > 36 {
        return Err("number->string: radix must be between 2 and 36".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(n) => {
            if radix == 10 {
                Ok(arena.string(n.to_string()))
            } else {
                // Convert to string in given radix
                let s = if *n < 0 {
                    format!("-{}", format_radix((-n) as u64, radix))
                } else {
                    format_radix(*n as u64, radix)
                };
                Ok(arena.string(s))
            }
        }
        ValueData::Real(f) => {
            if radix != 10 {
                return Err("number->string: non-decimal radix not supported for real numbers".to_string());
            }
            Ok(arena.string(f.to_string()))
        }
        _ => Err("number->string: first argument must be a number".to_string()),
    }
}

/// Helper function to format number in given radix
fn format_radix(mut n: u64, radix: u32) -> String {
    if n == 0 {
        return "0".to_string();
    }

    let digits = "0123456789abcdefghijklmnopqrstuvwxyz";
    let mut result = String::new();

    while n > 0 {
        let digit = (n % radix as u64) as usize;
        result.insert(0, digits.chars().nth(digit).unwrap());
        n /= radix as u64;
    }

    result
}

/// list? - check if value is a proper list (improved version)
pub fn arena_list_p_improved(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("list?: expected 1 argument".to_string());
    }

    let mut current = args[0];
    let mut visited = std::collections::HashSet::new();

    loop {
        // Detect cycles
        if !visited.insert(current) {
            // Cycle detected - not a proper list
            return Ok(crate::scheme::arena::FALSE_ID);
        }

        match arena.get(current) {
            ValueData::Nil => return Ok(crate::scheme::arena::TRUE_ID),
            ValueData::Pair { cdr, .. } => {
                current = *cdr;
            }
            _ => return Ok(crate::scheme::arena::FALSE_ID),
        }
    }
}

/// null-list? - check if value is empty list (alias for null?)
pub fn arena_null_list_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("null-list?: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Nil => Ok(crate::scheme::arena::TRUE_ID),
        _ => Ok(crate::scheme::arena::FALSE_ID),
    }
}

/// improper-list? - check if value is an improper list (ends with non-nil atom)
pub fn arena_improper_list_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("improper-list?: expected 1 argument".to_string());
    }

    let mut current = args[0];

    loop {
        match arena.get(current) {
            ValueData::Nil => return Ok(crate::scheme::arena::FALSE_ID), // Proper list
            ValueData::Pair { cdr, .. } => {
                current = *cdr;
            }
            _ => return Ok(crate::scheme::arena::TRUE_ID), // Ends with non-nil atom
        }
    }
}

/// circular-list? - check if list has a cycle
pub fn arena_circular_list_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("circular-list?: expected 1 argument".to_string());
    }

    // Use Floyd's cycle detection (tortoise and hare)
    let mut slow = args[0];
    let mut fast = args[0];

    loop {
        // Move slow by 1
        match arena.get(slow) {
            ValueData::Pair { cdr, .. } => slow = *cdr,
            _ => return Ok(crate::scheme::arena::FALSE_ID), // Not a list or reached end
        }

        // Move fast by 2
        match arena.get(fast) {
            ValueData::Pair { cdr: cdr1, .. } => {
                fast = *cdr1;
                match arena.get(fast) {
                    ValueData::Pair { cdr: cdr2, .. } => fast = *cdr2,
                    _ => return Ok(crate::scheme::arena::FALSE_ID), // Reached end
                }
            }
            _ => return Ok(crate::scheme::arena::FALSE_ID), // Not a list or reached end
        }

        // Check if slow == fast (cycle detected)
        if slow == fast {
            return Ok(crate::scheme::arena::TRUE_ID);
        }
    }
}

// =============================================================================
// Batch 19: Bitwise Operations (8 primitives)
// =============================================================================

/// bitwise-and - bitwise AND of integers
pub fn arena_bitwise_and(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Ok(arena.int(-1)); // Identity element for AND
    }

    let mut result = -1i64;
    for &arg in args {
        match arena.get(arg) {
            ValueData::Integer(n) => {
                result &= n;
            }
            _ => return Err("bitwise-and: all arguments must be integers".to_string()),
        }
    }

    Ok(arena.int(result))
}

/// bitwise-ior - bitwise inclusive OR of integers
pub fn arena_bitwise_ior(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Ok(arena.int(0)); // Identity element for OR
    }

    let mut result = 0i64;
    for &arg in args {
        match arena.get(arg) {
            ValueData::Integer(n) => {
                result |= n;
            }
            _ => return Err("bitwise-ior: all arguments must be integers".to_string()),
        }
    }

    Ok(arena.int(result))
}

/// bitwise-xor - bitwise exclusive OR of integers
pub fn arena_bitwise_xor(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Ok(arena.int(0)); // Identity element for XOR
    }

    let mut result = 0i64;
    for &arg in args {
        match arena.get(arg) {
            ValueData::Integer(n) => {
                result ^= n;
            }
            _ => return Err("bitwise-xor: all arguments must be integers".to_string()),
        }
    }

    Ok(arena.int(result))
}

/// bitwise-not - bitwise NOT (complement)
pub fn arena_bitwise_not(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("bitwise-not: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(n) => Ok(arena.int(!n)),
        _ => Err("bitwise-not: argument must be an integer".to_string()),
    }
}

/// arithmetic-shift - arithmetic left/right shift
pub fn arena_arithmetic_shift(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("arithmetic-shift: expected 2 arguments".to_string());
    }

    let n = match arena.get(args[0]) {
        ValueData::Integer(n) => *n,
        _ => return Err("arithmetic-shift: first argument must be an integer".to_string()),
    };

    let shift = match arena.get(args[1]) {
        ValueData::Integer(s) => *s,
        _ => return Err("arithmetic-shift: second argument must be an integer".to_string()),
    };

    let result = if shift >= 0 {
        // Left shift
        if shift >= 64 {
            0 // Shift all bits out
        } else {
            n.wrapping_shl(shift as u32)
        }
    } else {
        // Right shift (arithmetic - preserves sign)
        if shift <= -64 {
            if n < 0 { -1 } else { 0 }
        } else {
            n >> (-shift)
        }
    };

    Ok(arena.int(result))
}

/// bit-extract - extract bits from position start to end
pub fn arena_bit_extract(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 3 {
        return Err("bit-extract: expected 3 arguments".to_string());
    }

    let n = match arena.get(args[0]) {
        ValueData::Integer(n) => *n as u64,
        _ => return Err("bit-extract: first argument must be an integer".to_string()),
    };

    let start = match arena.get(args[1]) {
        ValueData::Integer(s) => *s as u32,
        _ => return Err("bit-extract: second argument must be an integer".to_string()),
    };

    let end = match arena.get(args[2]) {
        ValueData::Integer(e) => *e as u32,
        _ => return Err("bit-extract: third argument must be an integer".to_string()),
    };

    if start > end {
        return Err("bit-extract: start must be <= end".to_string());
    }

    if end > 64 {
        return Err("bit-extract: end must be <= 64".to_string());
    }

    let count = end - start;
    let mask = if count >= 64 {
        u64::MAX
    } else {
        (1u64 << count) - 1
    };

    let result = (n >> start) & mask;
    Ok(arena.int(result as i64))
}

/// bitwise-bit-set? - test if bit at position is set
pub fn arena_bitwise_bit_set_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("bitwise-bit-set?: expected 2 arguments".to_string());
    }

    let n = match arena.get(args[0]) {
        ValueData::Integer(n) => *n,
        _ => return Err("bitwise-bit-set?: first argument must be an integer".to_string()),
    };

    let pos = match arena.get(args[1]) {
        ValueData::Integer(p) => *p,
        _ => return Err("bitwise-bit-set?: second argument must be an integer".to_string()),
    };

    if pos < 0 || pos >= 64 {
        return Err("bitwise-bit-set?: position must be between 0 and 63".to_string());
    }

    let is_set = (n & (1i64 << pos)) != 0;
    Ok(if is_set { crate::scheme::arena::TRUE_ID } else { crate::scheme::arena::FALSE_ID })
}

/// bitwise-bit-count - count number of set bits (population count)
pub fn arena_bitwise_bit_count(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("bitwise-bit-count: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Integer(n) => {
            let count = if *n < 0 {
                // For negative numbers, count bits in two's complement representation
                (*n as u64).count_ones()
            } else {
                (*n as u64).count_ones()
            };
            Ok(arena.int(count as i64))
        }
        _ => Err("bitwise-bit-count: argument must be an integer".to_string()),
    }
}

// =============================================================================
// Batch 20: I/O and Display Operations (6 primitives)
// =============================================================================

/// display - display a value (note: arena version just converts to string)
pub fn arena_display(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("display: expected 1 argument".to_string());
    }

    // In arena mode, we can't actually print to stdout
    // Return unspecified to indicate success
    // The actual printing would happen in the non-arena version
    let _s = value_to_display_string(arena, args[0]);
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// Helper to convert value to display string
fn value_to_display_string(arena: &Arena, id: ValueId) -> String {
    match arena.get(id) {
        ValueData::Nil => "()".to_string(),
        ValueData::Bool(true) => "#t".to_string(),
        ValueData::Bool(false) => "#f".to_string(),
        ValueData::Integer(n) => n.to_string(),
        ValueData::Real(f) => f.to_string(),
        ValueData::Char(c) => c.to_string(),
        ValueData::String(s) => s.clone(),
        ValueData::Symbol(s) => s.to_string(),
        ValueData::Keyword(k) => format!("{}:", k),
        ValueData::Pair { .. } => {
            // Build list representation
            let mut result = String::from("(");
            let mut current = id;
            let mut first = true;

            loop {
                match arena.get(current) {
                    ValueData::Pair { car, cdr, .. } => {
                        if !first {
                            result.push(' ');
                        }
                        first = false;
                        result.push_str(&value_to_display_string(arena, *car));

                        current = *cdr;
                        if matches!(arena.get(current), ValueData::Nil) {
                            break;
                        } else if !matches!(arena.get(current), ValueData::Pair { .. }) {
                            result.push_str(" . ");
                            result.push_str(&value_to_display_string(arena, current));
                            break;
                        }
                    }
                    _ => break,
                }
            }
            result.push(')');
            result
        }
        ValueData::Vector(elements) => {
            let mut result = String::from("#(");
            for (i, elem) in elements.iter().enumerate() {
                if i > 0 {
                    result.push(' ');
                }
                result.push_str(&value_to_display_string(arena, *elem));
            }
            result.push(')');
            result
        }
        ValueData::Procedure(_) => "#<procedure>".to_string(),
        ValueData::Unspecified => "#<unspecified>".to_string(),
        ValueData::Error => "#<error>".to_string(),
        _ => "#<unknown>".to_string(),
    }
}

/// newline - output a newline
pub fn arena_newline(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if !args.is_empty() {
        return Err("newline: expected 0 arguments".to_string());
    }

    // In arena mode, we can't actually print
    // Return unspecified
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// write - write a value in machine-readable format
pub fn arena_write(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("write: expected 1 argument".to_string());
    }

    // Similar to display but with quotes for strings
    let _s = value_to_write_string(arena, args[0]);
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

fn value_to_write_string(arena: &Arena, id: ValueId) -> String {
    match arena.get(id) {
        ValueData::String(s) => format!("\"{}\"", s.escape_default()),
        ValueData::Char(c) => format!("#\\{}", c),
        _ => value_to_display_string(arena, id),
    }
}

/// write-char - write a character
pub fn arena_write_char(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("write-char: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Char(_) => {
            // In arena mode, we can't actually write
            Ok(crate::scheme::arena::UNSPECIFIED_ID)
        }
        _ => Err("write-char: argument must be a character".to_string()),
    }
}

/// read-char - read a character (stub - not implementable in arena mode)
pub fn arena_read_char(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if !args.is_empty() {
        return Err("read-char: expected 0 arguments".to_string());
    }

    // Cannot implement in arena mode - requires I/O
    Err("read-char: not available in arena mode (requires I/O)".to_string())
}

/// eof-object? - test if value is EOF object
pub fn arena_eof_object_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("eof-object?: expected 1 argument".to_string());
    }

    // We don't have a specific EOF value in arena
    // Return false for now
    Ok(crate::scheme::arena::FALSE_ID)
}

// =============================================================================
// Batch 21: Number Formatting (2 primitives)
// =============================================================================

/// format-number - format a number according to format string
pub fn arena_format_number(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("format-number: expected 2 arguments".to_string());
    }

    let n = match arena.get(args[0]) {
        ValueData::Integer(i) => *i,
        ValueData::Real(r) => *r as i64,
        _ => return Err("format-number: first argument must be a number".to_string()),
    };

    let format = match arena.get(args[1]) {
        ValueData::String(s) => s.as_str(),
        ValueData::Symbol(s) => s.as_ref(),
        _ => return Err("format-number: second argument must be a string or symbol".to_string()),
    };

    // Format implementation
    let result = match format {
        "1" | "decimal" => n.to_string(),
        "I" | "roman-upper" => {
            // Roman numerals (up to 20)
            let roman_str = match n {
                1 => "I",
                2 => "II",
                3 => "III",
                4 => "IV",
                5 => "V",
                6 => "VI",
                7 => "VII",
                8 => "VIII",
                9 => "IX",
                10 => "X",
                11 => "XI",
                12 => "XII",
                13 => "XIII",
                14 => "XIV",
                15 => "XV",
                16 => "XVI",
                17 => "XVII",
                18 => "XVIII",
                19 => "XIX",
                20 => "XX",
                _ => return Ok(arena.string(n.to_string())),
            };
            roman_str.to_string()
        }
        "i" | "roman-lower" => {
            let roman_str = match n {
                1 => "i",
                2 => "ii",
                3 => "iii",
                4 => "iv",
                5 => "v",
                6 => "vi",
                7 => "vii",
                8 => "viii",
                9 => "ix",
                10 => "x",
                11 => "xi",
                12 => "xii",
                13 => "xiii",
                14 => "xiv",
                15 => "xv",
                16 => "xvi",
                17 => "xvii",
                18 => "xviii",
                19 => "xix",
                20 => "xx",
                _ => return Ok(arena.string(n.to_string())),
            };
            roman_str.to_string()
        }
        "a" | "alpha-lower" => {
            // Lowercase letters (1=a, 2=b, etc.)
            if n >= 1 && n <= 26 {
                ((b'a' + (n as u8 - 1)) as char).to_string()
            } else {
                n.to_string()
            }
        }
        "A" | "alpha-upper" => {
            // Uppercase letters (1=A, 2=B, etc.)
            if n >= 1 && n <= 26 {
                ((b'A' + (n as u8 - 1)) as char).to_string()
            } else {
                n.to_string()
            }
        }
        _ => n.to_string(), // Default to decimal
    };

    Ok(arena.string(result))
}

/// format-number-list - join a list of numbers with separator
pub fn arena_format_number_list(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() || args.len() > 2 {
        return Err("format-number-list: expected 1 or 2 arguments".to_string());
    }

    let separator = if args.len() == 2 {
        match arena.get(args[1]) {
            ValueData::String(s) => s.as_str(),
            ValueData::Symbol(s) => s.as_ref(),
            _ => ".",
        }
    } else {
        "."
    };

    let mut numbers = Vec::new();
    let mut current = args[0];

    loop {
        match arena.get(current) {
            ValueData::Nil => break,
            ValueData::Pair { car, cdr, .. } => {
                match arena.get(*car) {
                    ValueData::Integer(n) => numbers.push(n.to_string()),
                    ValueData::Real(r) => numbers.push((*r as i64).to_string()),
                    _ => {
                        return Err(
                            "format-number-list: list must contain only numbers".to_string()
                        )
                    }
                }
                current = *cdr;
            }
            _ => return Err("format-number-list: not a proper list".to_string()),
        }
    }

    Ok(arena.string(numbers.join(separator)))
}

// =============================================================================
// Batch 22: Simple Constants (1 primitive)
// =============================================================================

/// empty-sosofo - return an empty sosofo (flow object)
pub fn arena_empty_sosofo(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if !args.is_empty() {
        return Err("empty-sosofo: expected no arguments".to_string());
    }

    Ok(arena.alloc(ValueData::Sosofo))
}

// =============================================================================
// Batch 23: Grove Operations - Current Node (1 primitive)
// =============================================================================

/// current-node - return the current node in the grove
pub fn arena_current_node(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if !args.is_empty() {
        return Err("current-node: expected no arguments".to_string());
    }

    // Access grove state directly from arena (unified pattern!)
    match &arena.current_node {
        Some(node) => Ok(arena.alloc(ValueData::Node(node.clone()))),
        None => Err("current-node: no current node set".to_string()),
    }
}

// =============================================================================
// Batch 24: Grove Operations - Node Properties (3 primitives)
// =============================================================================

/// gi - return the element name (generic identifier) of a node
pub fn arena_gi(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    let node = if args.is_empty() {
        // No argument: use current node
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("gi: no current node".to_string()),
        }
    } else if args.len() == 1 {
        // OpenJade: optSingletonNodeList pattern - accept node or singleton node-list
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            ValueData::NodeList(nl) => {
                if nl.is_empty() {
                    // Empty node-list -> return false
                    return Ok(crate::scheme::arena::FALSE_ID);
                }
                let first = nl.first();
                let rest = nl.rest();
                if rest.is_empty() {
                    // Singleton node-list -> extract the single node
                    Rc::new(first.unwrap())
                } else {
                    // Multi-element node-list -> error (not a singleton)
                    return Err("gi: node-list must be a singleton".to_string());
                }
            }
            _ => return Err("gi: argument must be a node or node-list".to_string()),
        }
    } else {
        return Err("gi: expected 0 or 1 arguments".to_string());
    };

    // Get element name
    match node.gi() {
        Some(name) => Ok(arena.symbol(name.into())),
        None => Ok(crate::scheme::arena::FALSE_ID),
    }
}

/// data - return the text content of a node
pub fn arena_data(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    // OpenJade: data accepts a node-list and concatenates data from all nodes
    if args.len() > 1 {
        return Err("data: expected 0 or 1 arguments".to_string());
    }

    let node_list = if args.is_empty() {
        // No argument: use current node as singleton node-list
        match &arena.current_node {
            Some(n) => {
                let singleton = crate::grove::VecNodeList::new(vec![n.clone_node()]);
                Box::new(singleton) as Box<dyn crate::grove::NodeList>
            }
            None => return Err("data: no current node".to_string()),
        }
    } else {
        // One argument: accept node or node-list
        match arena.get(args[0]) {
            ValueData::Node(n) => {
                // Wrap node in singleton node-list
                let singleton = crate::grove::VecNodeList::new(vec![n.clone_node()]);
                Box::new(singleton) as Box<dyn crate::grove::NodeList>
            }
            ValueData::NodeList(nl) => {
                // Clone the node-list (nl is Rc<Box<dyn NodeList>>)
                let vec_nodes: Vec<Box<dyn crate::grove::Node>> = {
                    let mut result = Vec::new();
                    let mut current = nl.clone();
                    loop {
                        if current.is_empty() {
                            break;
                        }
                        result.push(current.first().unwrap());
                        current = Rc::new(current.rest());
                    }
                    result
                };
                Box::new(crate::grove::VecNodeList::new(vec_nodes)) as Box<dyn crate::grove::NodeList>
            }
            _ => return Err("data: argument must be a node or node-list".to_string()),
        }
    };

    // Concatenate data from all nodes in the node-list
    // OpenJade: returns empty string for empty node-list, not #f
    let mut result = String::new();
    let mut current_list: Box<dyn crate::grove::NodeList> = node_list;
    loop {
        if current_list.is_empty() {
            break;
        }
        let node = current_list.first().unwrap();
        if let Some(text) = node.data() {
            result.push_str(&text);
        }
        current_list = current_list.rest();
    }

    // Always return a string (possibly empty), never #f
    Ok(arena.string(result))
}

/// id - return the ID attribute of a node
pub fn arena_id(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    let node = if args.is_empty() {
        // No argument: use current node
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("id: no current node".to_string()),
        }
    } else if args.len() == 1 {
        // One argument: extract node from ValueId (optSingletonNodeList pattern)
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            ValueData::NodeList(nl) => {
                if nl.is_empty() {
                    // Empty node-list -> return #f
                    return Ok(crate::scheme::arena::FALSE_ID);
                }
                let first = nl.first();
                let rest = nl.rest();
                if rest.is_empty() {
                    // Singleton node-list -> extract the node
                    Rc::new(first.unwrap())
                } else {
                    // Multi-element node-list -> error
                    return Err("id: node-list must be a singleton".to_string());
                }
            }
            // OpenJade: NULL nodes return #f for id lookups
            _ => return Ok(crate::scheme::arena::FALSE_ID),
        }
    } else {
        return Err("id: expected 0 or 1 arguments".to_string());
    };

    // Get ID attribute
    match node.id() {
        Some(id_str) => Ok(arena.string(id_str)),
        None => Ok(crate::scheme::arena::FALSE_ID),
    }
}

// =============================================================================
// Batch 25: Grove Operations - Navigation (3 primitives)
// =============================================================================

/// children - return the children of a node as a node-list
pub fn arena_children(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    let node = if args.is_empty() {
        // No argument: use current node
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("children: no current node".to_string()),
        }
    } else if args.len() == 1 {
        // One argument: extract node from ValueId
        // OpenJade: optSingletonNodeList pattern - accept node or singleton node-list
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            ValueData::NodeList(nl) => {
                // If it's a node-list, try to extract singleton
                if nl.is_empty() {
                    // Empty node-list -> return empty node-list
                    return Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(crate::grove::VecNodeList::new(vec![]))))));
                }
                let first = nl.first();
                let rest = nl.rest();
                if rest.is_empty() {
                    // Singleton node-list -> extract the single node
                    Rc::new(first.unwrap())
                } else {
                    // Multi-element node-list -> map children over all nodes
                    let mut all_children = Vec::new();
                    let mut current = nl.as_ref();
                    while !current.is_empty() {
                        if let Some(node) = current.first() {
                            let node_children = node.children();
                            let mut child_current = Box::new(node_children);
                            while !child_current.is_empty() {
                                if let Some(child) = child_current.first() {
                                    all_children.push(child);
                                }
                                child_current = Box::new(child_current.rest());
                            }
                        }
                        current = Box::leak(Box::new(current.rest()));
                    }
                    let result = crate::grove::VecNodeList::new(all_children);
                    return Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(result)))));
                }
            }
            _ => return Err("children: argument must be a node or node-list".to_string()),
        }
    } else {
        return Err("children: expected 0 or 1 arguments".to_string());
    };

    // Get children
    let children = node.children();
    Ok(arena.alloc(ValueData::NodeList(Rc::new(children))))
}

/// parent - return the parent of a node
pub fn arena_parent(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    let node = if args.is_empty() {
        // No argument: use current node
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("parent: no current node".to_string()),
        }
    } else if args.len() == 1 {
        // OpenJade: optSingletonNodeList pattern - accept node or singleton node-list
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            ValueData::NodeList(nl) => {
                if nl.is_empty() {
                    // Empty node-list -> return empty node-list (not FALSE!)
                    return Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(crate::grove::EmptyNodeList)))));
                }
                let first = nl.first();
                let rest = nl.rest();
                if rest.is_empty() {
                    // Singleton node-list -> extract the node
                    Rc::new(first.unwrap())
                } else {
                    // Multi-element node-list -> error
                    return Err("parent: node-list must be a singleton".to_string());
                }
            }
            _ => return Err("parent: argument must be a node or node-list".to_string()),
        }
    } else {
        return Err("parent: expected 0 or 1 arguments".to_string());
    };

    // Get parent - OpenJade returns singleton node-list or empty node-list
    match node.parent() {
        Some(parent) => {
            use crate::grove::VecNodeList;
            let nodes = vec![parent.clone_node()];
            Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(VecNodeList::new(nodes))))))
        }
        None => Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(crate::grove::EmptyNodeList))))),
    }
}

/// attributes - return the attributes of a node as a node-list
pub fn arena_attributes(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    let _node = if args.is_empty() {
        // No argument: use current node
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("attributes: no current node".to_string()),
        }
    } else if args.len() == 1 {
        // One argument: extract node from ValueId
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("attributes: argument must be a node".to_string()),
        }
    } else {
        return Err("attributes: expected 0 or 1 arguments".to_string());
    };

    // TODO: Implement attribute node-list
    // For now, return empty node-list (stub - same as existing primitives.rs implementation)
    Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(crate::grove::EmptyNodeList::new())))))
}

// =============================================================================
// Batch 26: Node-list Operations (5 primitives)
// =============================================================================

/// node-list? - check if value is a node-list
pub fn arena_node_list_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("node-list?: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::NodeList(_) => Ok(crate::scheme::arena::TRUE_ID),
        _ => Ok(crate::scheme::arena::FALSE_ID),
    }
}

/// empty-node-list - return an empty node-list
pub fn arena_empty_node_list(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if !args.is_empty() {
        return Err("empty-node-list: expected no arguments".to_string());
    }

    Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(crate::grove::EmptyNodeList::new())))))
}

/// node-list-empty? - check if node-list is empty
pub fn arena_node_list_empty_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("node-list-empty?: expected 1 argument".to_string());
    }

    // OpenJade: optSingletonNodeList pattern - accept node or node-list
    match arena.get(args[0]) {
        ValueData::NodeList(nl) => {
            if nl.is_empty() {
                Ok(crate::scheme::arena::TRUE_ID)
            } else {
                Ok(crate::scheme::arena::FALSE_ID)
            }
        }
        ValueData::Node(_) => {
            // Single node -> non-empty node-list
            Ok(crate::scheme::arena::FALSE_ID)
        }
        _ => Err("node-list-empty?: argument must be a node or node-list".to_string()),
    }
}

/// node-list-length - return the length of a node-list
pub fn arena_node_list_length(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("node-list-length: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::NodeList(nl) => Ok(arena.int(nl.length() as i64)),
        _ => Err("node-list-length: argument must be a node-list".to_string()),
    }
}

/// node-list-first - return the first node in a node-list
pub fn arena_node_list_first(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("node-list-first: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::NodeList(nl) => {
            match nl.first() {
                Some(node) => Ok(arena.alloc(ValueData::Node(Rc::new(node)))),
                // OpenJade returns a singleton containing a null node
                // We can't represent null nodes safely, so return #f instead
                None => Ok(crate::scheme::arena::FALSE_ID),
            }
        }
        _ => Err("node-list-first: argument must be a node-list".to_string()),
    }
}

// =============================================================================
// Batch 27: Attribute Operations (1 primitive)
// =============================================================================

/// attribute-string - get the value of a named attribute
/// (attribute-string attr-name [node]) → string or #f
pub fn arena_attribute_string(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() || args.len() > 2 {
        return Err("attribute-string: expected 1 or 2 arguments".to_string());
    }

    // First argument: attribute name (must be string or symbol)
    let attr_name = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        ValueData::Symbol(s) => s.to_string(),
        _ => return Err("attribute-string: first argument must be a string or symbol".to_string()),
    };

    // Second argument (optional): node or node-list (OpenJade: optSingletonNodeList pattern)
    let node = if args.len() == 2 {
        match arena.get(args[1]) {
            ValueData::Node(n) => Some(n.clone()),
            ValueData::NodeList(nl) => {
                if nl.is_empty() {
                    // Empty node-list -> return #f
                    return Ok(crate::scheme::arena::FALSE_ID);
                }
                let first = nl.first();
                let rest = nl.rest();
                if rest.is_empty() {
                    // Singleton node-list -> extract the node
                    Some(Rc::new(first.unwrap()))
                } else {
                    // Multi-element node-list -> error
                    return Err("attribute-string: node-list must be a singleton".to_string());
                }
            }
            // OpenJade: NULL nodes return #f for attribute lookups
            // In Dazzle, node-list-first on empty returns #f, so accept it here
            _ => return Ok(crate::scheme::arena::FALSE_ID),
        }
    } else {
        // Use current node
        arena.current_node.clone()
    };

    // If no node, return #f
    let node = match node {
        Some(n) => n,
        None => return Err("attribute-string: no current node".to_string()),
    };

    // Get attribute value
    match node.attribute_string(&attr_name) {
        Some(value) => Ok(arena.string(value)),
        None => Ok(crate::scheme::arena::FALSE_ID),
    }
}

// =============================================================================
// Batch 28: More Node-list Operations (3 primitives)
// =============================================================================

/// node-list-rest - return all but the first node in a node-list
pub fn arena_node_list_rest(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("node-list-rest: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::NodeList(nl) => {
            let rest = nl.rest();
            Ok(arena.alloc(ValueData::NodeList(Rc::new(rest))))
        }
        _ => Err("node-list-rest: argument must be a node-list".to_string()),
    }
}

/// node-list-ref - return the nth node in a node-list (0-indexed)
pub fn arena_node_list_ref(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("node-list-ref: expected 2 arguments".to_string());
    }

    let nl = match arena.get(args[0]) {
        ValueData::NodeList(nl) => nl.clone(),
        _ => return Err("node-list-ref: first argument must be a node-list".to_string()),
    };

    let index = match arena.get(args[1]) {
        ValueData::Integer(n) => {
            if *n < 0 {
                return Err("node-list-ref: index must be non-negative".to_string());
            }
            *n as usize
        }
        _ => return Err("node-list-ref: second argument must be an integer".to_string()),
    };

    // Iterate to the nth element
    let mut current = nl.as_ref();
    let mut i = 0;
    while i < index {
        if current.is_empty() {
            // DSSSL: out of bounds returns #f (not an error)
            return Ok(crate::scheme::arena::FALSE_ID);
        }
        let rest = current.rest();
        current = Box::leak(Box::new(rest)); // Safe because we're just reading
        i += 1;
    }

    match current.first() {
        Some(node) => Ok(arena.alloc(ValueData::Node(Rc::new(node)))),
        // DSSSL: out of bounds returns #f (not an error)
        None => Ok(crate::scheme::arena::FALSE_ID),
    }
}

/// node-list-reverse - reverse a node-list
pub fn arena_node_list_reverse(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("node-list-reverse: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::NodeList(nl) => {
            // Collect all nodes into a vector
            let mut nodes = Vec::new();
            let mut current = nl.as_ref();
            while !current.is_empty() {
                if let Some(node) = current.first() {
                    nodes.push(node);
                }
                current = Box::leak(Box::new(current.rest()));
            }

            // Reverse the vector
            nodes.reverse();

            // Create a reversed node-list by building from the back
            let reversed = crate::grove::VecNodeList::new(nodes);
            Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(reversed)))))
        }
        _ => Err("node-list-reverse: argument must be a node-list".to_string()),
    }
}

// =============================================================================
// Batch 29: Type Predicates (3 primitives)
// =============================================================================

/// node? - check if value is a node
pub fn arena_node_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("node?: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Node(_) => Ok(crate::scheme::arena::TRUE_ID),
        _ => Ok(crate::scheme::arena::FALSE_ID),
    }
}

/// sosofo? - check if value is a sosofo
pub fn arena_sosofo_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("sosofo?: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Sosofo => Ok(crate::scheme::arena::TRUE_ID),
        _ => Ok(crate::scheme::arena::FALSE_ID),
    }
}

/// quantity? - check if value is a quantity (stub - returns #f)
pub fn arena_quantity_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("quantity?: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Quantity { .. } => Ok(crate::scheme::arena::TRUE_ID),
        _ => Ok(crate::scheme::arena::FALSE_ID),
    }
}

// =============================================================================
// Batch 30: Color and Spacing Stubs (4 primitives)
// =============================================================================

/// color? - check if value is a color (stub - always returns #f)
pub fn arena_color_p(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("color?: expected 1 argument".to_string());
    }
    // No color type in our implementation yet - always false
    Ok(crate::scheme::arena::FALSE_ID)
}

/// color - create a color (stub - returns #f)
pub fn arena_color(_arena: &mut Arena, _args: &[ValueId]) -> ArenaResult {
    // Stub: color creation not implemented
    // TODO: Implement if needed for document formatting
    Ok(crate::scheme::arena::FALSE_ID)
}

/// display-space? - check if value is display-space (stub - always returns #f)
pub fn arena_display_space_p(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("display-space?: expected 1 argument".to_string());
    }
    // No display-space type in our implementation yet - always false
    Ok(crate::scheme::arena::FALSE_ID)
}

/// inline-space? - check if value is inline-space (stub - always returns #f)
pub fn arena_inline_space_p(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("inline-space?: expected 1 argument".to_string());
    }
    // No inline-space type in our implementation yet - always false
    Ok(crate::scheme::arena::FALSE_ID)
}

// =============================================================================
// Batch 31: Quantity Operations (5 primitives - stubs)
// =============================================================================

/// quantity->number - convert quantity to number (stub - returns 0)
pub fn arena_quantity_to_number(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("quantity->number: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::Quantity { magnitude, .. } => Ok(arena.real(*magnitude)),
        _ => Err("quantity->number: argument must be a quantity".to_string()),
    }
}

/// number->quantity - convert number to quantity (stub - returns number as-is)
pub fn arena_number_to_quantity(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 2 || args.len() > 3 {
        return Err("number->quantity: expected 2 or 3 arguments".to_string());
    }

    // For now, just return the number (stub)
    // TODO: Implement proper quantity types if needed for document formatting
    Ok(args[0])
}

/// quantity-convert - convert quantity between units (stub - returns input)
pub fn arena_quantity_convert(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("quantity-convert: expected 2 arguments".to_string());
    }

    // Stub: just return the input quantity
    Ok(args[0])
}

/// device-length - convert length to device units (stub - returns 0)
pub fn arena_device_length(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("device-length: expected 1 argument".to_string());
    }

    // Stub: return 0
    Ok(arena.int(0))
}

/// label-distance - get label distance (stub - returns 0)
pub fn arena_label_distance(arena: &mut Arena, _args: &[ValueId]) -> ArenaResult {
    // Stub: return 0
    Ok(arena.int(0))
}

// ============================================================================
// Phase 3 Batch 32: Grove navigation operations (5)
// ============================================================================

/// ancestor - get ancestor with specified generic identifier
pub fn arena_ancestor(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() || args.len() > 2 {
        return Err("ancestor: expected 1 or 2 arguments".to_string());
    }

    // Get the ancestor name (first arg)
    let ancestor_name = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        ValueData::Symbol(s) => s.to_string(),
        _ => return Err("ancestor: first argument must be a string or symbol".to_string()),
    };

    // Get the starting node (second arg or current node)
    let node = if args.len() == 2 {
        match arena.get(args[1]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("ancestor: second argument must be a node".to_string()),
        }
    } else {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("ancestor: no current node".to_string()),
        }
    };

    // Walk up the tree looking for matching ancestor
    let mut current = node.parent();
    while let Some(parent_node) = current {
        if let Some(gi) = parent_node.gi() {
            if gi == ancestor_name {
                // Found - return singleton node-list (OpenJade semantics)
                use crate::grove::VecNodeList;
                let nodes = vec![parent_node.clone_node()];
                return Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(VecNodeList::new(nodes))))));
            }
        }
        current = parent_node.parent();
    }

    // Not found - return empty node-list (OpenJade semantics)
    Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(crate::grove::EmptyNodeList)))))
}

/// descendants - get all descendants of a node or node-list
pub fn arena_descendants(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() > 1 {
        return Err("descendants: expected 0 or 1 arguments".to_string());
    }

    // Helper: Collect all descendants recursively
    fn collect_descendants(node: &dyn crate::grove::Node, result: &mut Vec<Box<dyn crate::grove::Node>>) {
        let children = node.children();
        let mut current = children.as_ref();
        while !current.is_empty() {
            if let Some(child) = current.first() {
                collect_descendants(child.as_ref(), result);
                result.push(child);
            }
            current = Box::leak(current.rest());
        }
    }

    // Get input node or node-list
    if args.is_empty() {
        // No argument: use current node
        let node = match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("descendants: no current node".to_string()),
        };
        let mut descendants = Vec::new();
        collect_descendants(node.as_ref().as_ref(), &mut descendants);
        let node_list = crate::grove::VecNodeList::new(descendants);
        return Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(node_list)))));
    }

    // One argument: can be node-list or node
    match arena.get(args[0]) {
        ValueData::NodeList(nl) => {
            // Node-list input: collect descendants of all nodes
            let mut all_descendants = Vec::new();
            let mut current = nl.as_ref().as_ref();
            while !current.is_empty() {
                if let Some(node) = current.first() {
                    collect_descendants(node.as_ref(), &mut all_descendants);
                }
                current = Box::leak(current.rest());
            }
            let result = crate::grove::VecNodeList::new(all_descendants);
            Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(result)))))
        }
        ValueData::Node(n) => {
            // Single node input
            let mut descendants = Vec::new();
            collect_descendants(n.as_ref().as_ref(), &mut descendants);
            let node_list = crate::grove::VecNodeList::new(descendants);
            Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(node_list)))))
        }
        _ => Err("descendants: argument must be a node or node-list".to_string()),
    }
}

/// follow - get all nodes following this node in document order
pub fn arena_follow(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() > 1 {
        return Err("follow: expected 0 or 1 arguments".to_string());
    }

    let _node = if args.is_empty() {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("follow: no current node".to_string()),
        }
    } else {
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("follow: argument must be a node".to_string()),
        }
    };

    // Stub: return empty node list
    // Full implementation requires document order traversal state
    Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(crate::grove::EmptyNodeList)))))
}

/// preced - get all nodes preceding this node in document order
pub fn arena_preced(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() > 1 {
        return Err("preced: expected 0 or 1 arguments".to_string());
    }

    let _node = if args.is_empty() {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("preced: no current node".to_string()),
        }
    } else {
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("preced: argument must be a node".to_string()),
        }
    };

    // Stub: return empty node list
    // Full implementation requires document order traversal state
    Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(crate::grove::EmptyNodeList)))))
}

/// ipreced - get immediately preceding node in document order
pub fn arena_ipreced(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() > 1 {
        return Err("ipreced: expected 0 or 1 arguments".to_string());
    }

    let _node = if args.is_empty() {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("ipreced: no current node".to_string()),
        }
    } else {
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("ipreced: argument must be a node".to_string()),
        }
    };

    // Stub: return empty node list
    // Full implementation requires document order traversal state
    Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(crate::grove::EmptyNodeList)))))
}

// ============================================================================
// Phase 3 Batch 33: Node-list set operations (5)
// ============================================================================

/// node-list-last - get the last node in a node-list
pub fn arena_node_list_last(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("node-list-last: expected 1 argument".to_string());
    }

    match arena.get(args[0]) {
        ValueData::NodeList(nl) => {
            let mut last_node = None;
            let mut current = nl.as_ref().as_ref();
            while !current.is_empty() {
                if let Some(node) = current.first() {
                    last_node = Some(node);
                }
                current = Box::leak(current.rest());
            }
            match last_node {
                Some(node) => Ok(arena.alloc(ValueData::Node(Rc::new(node)))),
                None => Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(crate::grove::EmptyNodeList))))),
            }
        }
        _ => Err("node-list-last: argument must be a node-list".to_string()),
    }
}

/// node-list-union - combine two node-lists, removing duplicates
pub fn arena_node_list_union(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Err("node-list-union: expected at least 1 argument".to_string());
    }

    // Collect all nodes from all argument lists, removing duplicates
    let mut nodes: Vec<Box<dyn crate::grove::Node>> = Vec::new();

    for (i, arg_id) in args.iter().enumerate() {
        let nl = match arena.get(*arg_id) {
            ValueData::NodeList(nl) => nl.clone(),
            _ => return Err(format!("node-list-union: argument {} must be a node-list", i + 1)),
        };

        // Use length-based iteration to avoid Box::leak memory issues
        let len = nl.length();
        for idx in 0..len {
            if let Some(node) = nl.get(idx) {
                // Check if this node is already in the list using node identity
                let mut already_present = false;
                for existing_node in &nodes {
                    if node.node_eq(existing_node.as_ref()) {
                        already_present = true;
                        break;
                    }
                }
                if !already_present {
                    nodes.push(node);
                }
            }
        }
    }

    // Duplicates have been removed based on node identity
    let union = crate::grove::VecNodeList::new(nodes);
    Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(union)))))
}

/// node-list-intersection - get nodes present in both node-lists
pub fn arena_node_list_intersection(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("node-list-intersection: expected 2 arguments".to_string());
    }

    let _nl1 = match arena.get(args[0]) {
        ValueData::NodeList(nl) => nl.clone(),
        _ => return Err("node-list-intersection: first argument must be a node-list".to_string()),
    };

    let _nl2 = match arena.get(args[1]) {
        ValueData::NodeList(nl) => nl.clone(),
        _ => return Err("node-list-intersection: second argument must be a node-list".to_string()),
    };

    // Stub: return empty node list
    // Full implementation requires node identity comparison
    Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(crate::grove::EmptyNodeList)))))
}

/// node-list-difference - get nodes in first list but not in second
pub fn arena_node_list_difference(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("node-list-difference: expected 2 arguments".to_string());
    }

    let nl1 = match arena.get(args[0]) {
        ValueData::NodeList(nl) => nl.clone(),
        _ => return Err("node-list-difference: first argument must be a node-list".to_string()),
    };

    let _nl2 = match arena.get(args[1]) {
        ValueData::NodeList(nl) => nl.clone(),
        _ => return Err("node-list-difference: second argument must be a node-list".to_string()),
    };

    // Stub: return first list unchanged
    // Full implementation requires node identity comparison
    Ok(arena.alloc(ValueData::NodeList(nl1)))
}

/// node-list-remove-duplicates - remove duplicate nodes from a node-list
pub fn arena_node_list_remove_duplicates(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("node-list-remove-duplicates: expected 1 argument".to_string());
    }

    let nl = match arena.get(args[0]) {
        ValueData::NodeList(nl) => nl.clone(),
        _ => return Err("node-list-remove-duplicates: argument must be a node-list".to_string()),
    };

    // Stub: return list unchanged
    // Full implementation requires node identity tracking
    Ok(arena.alloc(ValueData::NodeList(nl)))
}

// ============================================================================
// Phase 3 Batch 34: Element selection and position operations (5)
// ============================================================================

/// select-elements - select elements by name from children
/// OpenJade signature: (select-elements node-list pattern)
pub fn arena_select_elements(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("select-elements: expected 2 arguments".to_string());
    }

    // OpenJade: args[0] is node-list, args[1] is pattern
    let nl = match arena.get(args[0]) {
        ValueData::NodeList(nl) => nl.clone(),
        _ => return Err("select-elements: first argument must be a node-list".to_string()),
    };

    // Get the element name to select (second argument)
    let element_name = match arena.get(args[1]) {
        ValueData::String(s) => s.clone(),
        ValueData::Symbol(s) => s.to_string(),
        _ => return Err("select-elements: second argument must be a string or symbol".to_string()),
    };

    // Filter elements by name
    let mut selected = Vec::new();
    let mut current = nl.as_ref().as_ref();
    while !current.is_empty() {
        if let Some(node) = current.first() {
            if let Some(gi) = node.gi() {
                if gi == element_name {
                    selected.push(node);
                }
            }
        }
        current = Box::leak(current.rest());
    }

    let result = crate::grove::VecNodeList::new(selected);
    Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(result)))))
}

/// first-sibling? - check if node is the first sibling
pub fn arena_first_sibling_p(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() > 1 {
        return Err("first-sibling?: expected 0 or 1 arguments".to_string());
    }

    let node = if args.is_empty() {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("first-sibling?: no current node".to_string()),
        }
    } else {
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("first-sibling?: argument must be a node".to_string()),
        }
    };

    // OpenJade semantics: Check if node is first among siblings WITH SAME ELEMENT NAME
    // Not the first among all siblings, but first among siblings with matching GI
    let gi = node.gi();

    match node.parent() {
        Some(parent) => {
            let children = parent.children();
            let len = children.length();

            // Iterate through siblings until we find the current node
            for idx in 0..len {
                if let Some(sibling) = children.get(idx) {
                    // Check if this sibling has the same element name
                    if sibling.gi() == gi {
                        // If we found a sibling with same name, check if it's the current node
                        if node.node_eq(sibling.as_ref()) {
                            // Current node is the first with this element name
                            return Ok(crate::scheme::arena::TRUE_ID);
                        } else {
                            // Found an earlier sibling with same element name
                            return Ok(crate::scheme::arena::FALSE_ID);
                        }
                    }
                }
            }
            // Shouldn't reach here, but return false as fallback
            Ok(crate::scheme::arena::FALSE_ID)
        }
        None => Ok(crate::scheme::arena::TRUE_ID), // Root node is first sibling
    }
}

/// last-sibling? - check if node is the last sibling
pub fn arena_last_sibling_p(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() > 1 {
        return Err("last-sibling?: expected 0 or 1 arguments".to_string());
    }

    let node = if args.is_empty() {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("last-sibling?: no current node".to_string()),
        }
    } else {
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("last-sibling?: argument must be a node".to_string()),
        }
    };

    // OpenJade semantics: Check if node is last among siblings WITH SAME ELEMENT NAME
    // Not the last among all siblings, but last among siblings with matching GI
    let gi = node.gi();

    match node.parent() {
        Some(parent) => {
            let children = parent.children();
            let len = children.length();

            // Iterate backwards through siblings to find the last with same element name
            for idx in (0..len).rev() {
                if let Some(sibling) = children.get(idx) {
                    // Check if this sibling has the same element name
                    if sibling.gi() == gi {
                        // Found the last sibling with this element name
                        // Check if it's the current node
                        if node.node_eq(sibling.as_ref()) {
                            return Ok(crate::scheme::arena::TRUE_ID);
                        } else {
                            return Ok(crate::scheme::arena::FALSE_ID);
                        }
                    }
                }
            }
            // Shouldn't reach here, but return false as fallback
            Ok(crate::scheme::arena::FALSE_ID)
        }
        None => Ok(crate::scheme::arena::TRUE_ID), // Root node is last sibling
    }
}

/// child-number - get the child index of a node (1-based)
pub fn arena_child_number(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() > 1 {
        return Err("child-number: expected 0 or 1 arguments".to_string());
    }

    let node = if args.is_empty() {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("child-number: no current node".to_string()),
        }
    } else {
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("child-number: argument must be a node".to_string()),
        }
    };

    // OpenJade semantics: Count position among siblings WITH SAME ELEMENT NAME (1-based)
    // Like first-sibling? and last-sibling?, this counts only same-named siblings
    let gi = node.gi();

    match node.parent() {
        Some(parent) => {
            let children = parent.children();
            let len = children.length();
            let mut position = 0;

            // Iterate through siblings and count same-named ones until we find current node
            for idx in 0..len {
                if let Some(sibling) = children.get(idx) {
                    // Only count siblings with same element name
                    if sibling.gi() == gi {
                        position += 1;
                        // Check if this is the current node
                        if node.node_eq(sibling.as_ref()) {
                            return Ok(arena.int(position as i64));
                        }
                    }
                }
            }
            // Shouldn't reach here, but return 1 as fallback
            Ok(arena.int(1))
        }
        None => Ok(arena.int(1)), // Root node
    }
}

/// element-with-id - find element with specified ID
pub fn arena_element_with_id(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("element-with-id: expected 1 argument".to_string());
    }

    let id = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        ValueData::Symbol(s) => s.to_string(),
        _ => return Err("element-with-id: argument must be a string or symbol".to_string()),
    };

    // Use grove's element_with_id if available
    match &arena.grove {
        Some(grove) => {
            match grove.element_with_id(&id) {
                // OpenJade: return singleton node-list, not bare node
                Some(node) => {
                    let singleton = crate::grove::VecNodeList::new(vec![node]);
                    Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(singleton)))))
                }
                None => Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(crate::grove::EmptyNodeList))))),
            }
        }
        None => Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(crate::grove::EmptyNodeList))))),
    }
}

// ============================================================================
// Phase 3 Batch 35: Element numbering operations (3)
// ============================================================================

/// element-number - get the element number (count of same-named siblings)
pub fn arena_element_number(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() > 1 {
        return Err("element-number: expected 0 or 1 arguments".to_string());
    }

    let _node = if args.is_empty() {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("element-number: no current node".to_string()),
        }
    } else {
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("element-number: argument must be a node".to_string()),
        }
    };

    // Stub: return 1
    // Full implementation requires counting preceding siblings with same GI
    Ok(arena.int(1))
}

/// hierarchical-number - get hierarchical numbering (e.g., 1.2.3)
pub fn arena_hierarchical_number(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() > 1 {
        return Err("hierarchical-number: expected 0 or 1 arguments".to_string());
    }

    let _node = if args.is_empty() {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("hierarchical-number: no current node".to_string()),
        }
    } else {
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("hierarchical-number: argument must be a node".to_string()),
        }
    };

    // Stub: return list (1)
    // Full implementation requires walking up tree and counting at each level
    let one = arena.int(1);
    Ok(arena.cons(one, crate::scheme::arena::NIL_ID))
}

/// hierarchical-number-recursive - get recursive hierarchical numbering
pub fn arena_hierarchical_number_recursive(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 1 || args.len() > 2 {
        return Err("hierarchical-number-recursive: expected 1 or 2 arguments".to_string());
    }

    // First arg is the element name or list of element names
    let _element_names = args[0];

    let _node = if args.len() == 2 {
        match arena.get(args[1]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("hierarchical-number-recursive: second argument must be a node".to_string()),
        }
    } else {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("hierarchical-number-recursive: no current node".to_string()),
        }
    };

    // Stub: return list (1)
    let one = arena.int(1);
    Ok(arena.cons(one, crate::scheme::arena::NIL_ID))
}

// Helper functions for number formatting
fn int_to_roman(mut n: i32, uppercase: bool) -> String {
    if n <= 0 || n > 3999 {
        return n.to_string();
    }

    let values = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
    let symbols_upper = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"];
    let symbols_lower = ["m", "cm", "d", "cd", "c", "xc", "l", "xl", "x", "ix", "v", "iv", "i"];

    let symbols = if uppercase { &symbols_upper } else { &symbols_lower };

    let mut result = String::new();
    for (i, &value) in values.iter().enumerate() {
        while n >= value {
            result.push_str(symbols[i]);
            n -= value;
        }
    }
    result
}

fn int_to_alpha(n: i32, uppercase: bool) -> String {
    if n <= 0 {
        return n.to_string();
    }

    let base = if uppercase { b'A' } else { b'a' };
    let mut result = String::new();
    let mut num = n - 1;

    loop {
        result.insert(0, (base + (num % 26) as u8) as char);
        num /= 26;
        if num == 0 {
            break;
        }
        num -= 1;
    }

    result
}

// ============================================================================
// Phase 3 Batch 36: Grove utility operations (5)
// ============================================================================

/// ancestors - get all ancestors of a node
pub fn arena_ancestors(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() > 1 {
        return Err("ancestors: expected 0 or 1 arguments".to_string());
    }

    let node = if args.is_empty() {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("ancestors: no current node".to_string()),
        }
    } else {
        // OpenJade optSingletonNodeList pattern: accept node, singleton node-list, or #f
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            ValueData::NodeList(nl) => {
                if nl.is_empty() {
                    // Empty node-list -> return empty node-list
                    return Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(
                        crate::grove::VecNodeList::new(Vec::new())
                    )))));
                }
                let first = nl.first();
                let rest = nl.rest();
                if rest.is_empty() {
                    // Singleton node-list -> extract the node
                    Rc::new(first.unwrap())
                } else {
                    // Multi-element node-list -> error
                    return Err("ancestors: node-list must be a singleton".to_string());
                }
            }
            // OpenJade: #f returns empty node-list
            _ => return Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(
                crate::grove::VecNodeList::new(Vec::new())
            ))))),
        }
    };

    // Collect all ancestors walking up the tree
    let mut ancestors = Vec::new();
    let mut current = node.parent();
    while let Some(parent_node) = current {
        ancestors.push(parent_node.clone_node());
        current = parent_node.parent();
    }

    let node_list = crate::grove::VecNodeList::new(ancestors);
    Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(node_list)))))
}

/// document-element - get the document root element
pub fn arena_document_element(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if !args.is_empty() {
        return Err("document-element: expected no arguments".to_string());
    }

    match &arena.grove {
        Some(grove) => {
            let root = grove.root();
            Ok(arena.alloc(ValueData::Node(Rc::new(root))))
        }
        None => Err("document-element: no grove available".to_string()),
    }
}

/// have-ancestor? - check if node has ancestor with specified name
pub fn arena_have_ancestor_p(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() || args.len() > 2 {
        return Err("have-ancestor?: expected 1 or 2 arguments".to_string());
    }

    let ancestor_name = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        ValueData::Symbol(s) => s.to_string(),
        _ => return Err("have-ancestor?: first argument must be a string or symbol".to_string()),
    };

    let node = if args.len() == 2 {
        match arena.get(args[1]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("have-ancestor?: second argument must be a node".to_string()),
        }
    } else {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("have-ancestor?: no current node".to_string()),
        }
    };

    // Walk up tree looking for matching ancestor
    let mut current = node.parent();
    while let Some(parent_node) = current {
        if let Some(gi) = parent_node.gi() {
            if gi == ancestor_name {
                return Ok(crate::scheme::arena::TRUE_ID);
            }
        }
        current = parent_node.parent();
    }

    Ok(crate::scheme::arena::FALSE_ID)
}

/// match-element? - check if node matches element name
pub fn arena_match_element_p(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() || args.len() > 2 {
        return Err("match-element?: expected 1 or 2 arguments".to_string());
    }

    let element_name = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        ValueData::Symbol(s) => s.to_string(),
        _ => return Err("match-element?: first argument must be a string or symbol".to_string()),
    };

    let node = if args.len() == 2 {
        match arena.get(args[1]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("match-element?: second argument must be a node".to_string()),
        }
    } else {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("match-element?: no current node".to_string()),
        }
    };

    match node.gi() {
        Some(gi) if gi == element_name => Ok(crate::scheme::arena::TRUE_ID),
        _ => Ok(crate::scheme::arena::FALSE_ID),
    }
}

/// node-list-map - map a function over a node-list (stub)
pub fn arena_node_list_map(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("node-list-map: expected 2 arguments".to_string());
    }

    let _func = args[0];
    let nl = match arena.get(args[1]) {
        ValueData::NodeList(nl) => nl.clone(),
        _ => return Err("node-list-map: second argument must be a node-list".to_string()),
    };

    // Stub: return the node-list unchanged
    // Full implementation requires calling the function on each node
    Ok(arena.alloc(ValueData::NodeList(nl)))
}

// ============================================================================
// Phase 3 Batch 37: Final DSSSL operations (5)
// ============================================================================

/// node-property - get a property value from a node (stub)
pub fn arena_node_property(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 1 || args.len() > 2 {
        return Err("node-property: expected 1 or 2 arguments".to_string());
    }

    let _property_name = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        ValueData::Symbol(s) => s.to_string(),
        _ => return Err("node-property: first argument must be a string or symbol".to_string()),
    };

    let _node = if args.len() == 2 {
        match arena.get(args[1]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("node-property: second argument must be a node".to_string()),
        }
    } else {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("node-property: no current node".to_string()),
        }
    };

    // Stub: return #f
    // Full implementation would access node properties dynamically
    Ok(crate::scheme::arena::FALSE_ID)
}

/// absolute-first-sibling? - check if node is first sibling at document level
pub fn arena_absolute_first_sibling_p(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() > 1 {
        return Err("absolute-first-sibling?: expected 0 or 1 arguments".to_string());
    }

    let _node = if args.is_empty() {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("absolute-first-sibling?: no current node".to_string()),
        }
    } else {
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("absolute-first-sibling?: argument must be a node".to_string()),
        }
    };

    // Stub: return #f
    // Full implementation requires comparing node identity with first sibling
    Ok(crate::scheme::arena::FALSE_ID)
}

/// absolute-last-sibling? - check if node is last sibling at document level
pub fn arena_absolute_last_sibling_p(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() > 1 {
        return Err("absolute-last-sibling?: expected 0 or 1 arguments".to_string());
    }

    let _node = if args.is_empty() {
        match &arena.current_node {
            Some(n) => n.clone(),
            None => return Err("absolute-last-sibling?: no current node".to_string()),
        }
    } else {
        match arena.get(args[0]) {
            ValueData::Node(n) => n.clone(),
            _ => return Err("absolute-last-sibling?: argument must be a node".to_string()),
        }
    };

    // Stub: return #f
    // Full implementation requires comparing node identity with last sibling
    Ok(crate::scheme::arena::FALSE_ID)
}

/// node-list->list - convert a node-list to a list
pub fn arena_node_list_to_list(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err("node-list->list: expected 1 argument".to_string());
    }

    let nl = match arena.get(args[0]) {
        ValueData::NodeList(nl) => nl.clone(),
        _ => return Err("node-list->list: argument must be a node-list".to_string()),
    };

    // Convert node-list to list
    let mut nodes = Vec::new();
    let mut current = nl.as_ref().as_ref();
    while !current.is_empty() {
        if let Some(node) = current.first() {
            let node_id = arena.alloc(ValueData::Node(Rc::new(node)));
            nodes.push(node_id);
        }
        current = Box::leak(current.rest());
    }

    // Build list from nodes
    let mut result = crate::scheme::arena::NIL_ID;
    for node_id in nodes.iter().rev() {
        result = arena.cons(*node_id, result);
    }

    Ok(result)
}

/// node-list-contains? - check if node-list contains a node
pub fn arena_node_list_contains_p(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err("node-list-contains?: expected 2 arguments".to_string());
    }

    let nl = match arena.get(args[0]) {
        ValueData::NodeList(nl) => nl.clone(),
        _ => return Err("node-list-contains?: first argument must be a node-list".to_string()),
    };

    // OpenJade: optSingletonNodeList pattern - accept node or singleton node-list
    let target_node = match arena.get(args[1]) {
        ValueData::Node(n) => n.clone(),
        ValueData::NodeList(nl) => {
            if nl.is_empty() {
                // Empty node-list -> return #f (nothing to search for)
                return Ok(crate::scheme::arena::FALSE_ID);
            }
            let first = nl.first();
            let rest = nl.rest();
            if rest.is_empty() {
                // Singleton node-list -> extract the node
                Rc::new(first.unwrap())
            } else {
                // Multi-element node-list -> error
                return Err("node-list-contains?: second argument (node-list) must be a singleton".to_string());
            }
        }
        _ => return Err("node-list-contains?: second argument must be a node or node-list".to_string()),
    };

    // Check if target node is in the node-list
    // Convert to vec and iterate
    let mut nodes = Vec::new();
    let mut current_nl = nl.clone();
    loop {
        if current_nl.is_empty() {
            break;
        }
        nodes.push(current_nl.first().unwrap());
        current_nl = Rc::new(current_nl.rest());
    }

    // Check each node for equality
    for node in nodes {
        if node.as_ref().node_eq(&**target_node) {
            return Ok(crate::scheme::arena::TRUE_ID);
        }
    }

    Ok(crate::scheme::arena::FALSE_ID)
}

// ============================================================================
// Phase 3 Batch 38: Entity and notation operations (5)
// ============================================================================

/// entity-system-id - get the system identifier of an entity
pub fn arena_entity_system_id(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Err("entity-system-id: requires at least 1 argument".to_string());
    }

    // Extract entity name
    let _entity_name = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        ValueData::Symbol(s) => s.to_string(),
        _ => return Err("entity-system-id: argument must be a string or symbol".to_string()),
    };

    // Stub: In a full implementation, this would query the DTD for entity declarations
    // and return the system identifier. For now, return #f (not found).
    Ok(crate::scheme::arena::FALSE_ID)
}

/// entity-public-id - get the public identifier of an entity
pub fn arena_entity_public_id(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Err("entity-public-id: requires at least 1 argument".to_string());
    }

    // Extract entity name
    let _entity_name = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        ValueData::Symbol(s) => s.to_string(),
        _ => return Err("entity-public-id: argument must be a string or symbol".to_string()),
    };

    // Stub: In a full implementation, this would query the DTD for entity declarations
    // and return the public identifier. For now, return #f (not found).
    Ok(crate::scheme::arena::FALSE_ID)
}

/// entity-type - get the type of an entity
pub fn arena_entity_type(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Err("entity-type: requires at least 1 argument".to_string());
    }

    // Extract entity name
    let _entity_name = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        ValueData::Symbol(s) => s.to_string(),
        _ => return Err("entity-type: argument must be a string or symbol".to_string()),
    };

    // Stub: In a full implementation, this would return a symbol indicating the entity type:
    // 'text, 'cdata, 'sdata, 'ndata, 'subdoc, or 'pi
    // For now, return #f (not found).
    Ok(crate::scheme::arena::FALSE_ID)
}

/// notation-system-id - get the system identifier of a notation
pub fn arena_notation_system_id(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Err("notation-system-id: requires at least 1 argument".to_string());
    }

    // Extract notation name
    let _notation_name = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        ValueData::Symbol(s) => s.to_string(),
        _ => return Err("notation-system-id: argument must be a string or symbol".to_string()),
    };

    // Stub: In a full implementation, this would query the DTD for notation declarations
    // and return the system identifier. For now, return #f (not found).
    Ok(crate::scheme::arena::FALSE_ID)
}

/// notation-public-id - get the public identifier of a notation
pub fn arena_notation_public_id(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() {
        return Err("notation-public-id: requires at least 1 argument".to_string());
    }

    // Extract notation name
    let _notation_name = match arena.get(args[0]) {
        ValueData::String(s) => s.clone(),
        ValueData::Symbol(s) => s.to_string(),
        _ => return Err("notation-public-id: argument must be a string or symbol".to_string()),
    };

    // Stub: In a full implementation, this would query the DTD for notation declarations
    // and return the public identifier. For now, return #f (not found).
    Ok(crate::scheme::arena::FALSE_ID)
}

// ============================================================================
// Phase 3 Batch 39: Context and debugging primitives (5)
// ============================================================================

/// current-language - get the current language code
pub fn arena_current_language(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if !args.is_empty() {
        return Err("current-language: requires 0 arguments".to_string());
    }

    // Stub: In a full implementation, this would return the current language code
    // set by with-language or declare-default-language.
    // For now, return #f (no language set).
    Ok(crate::scheme::arena::FALSE_ID)
}

/// current-mode - get the current processing mode
pub fn arena_current_mode(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if !args.is_empty() {
        return Err("current-mode: requires 0 arguments".to_string());
    }

    // Stub: In a full implementation, this would return a symbol representing
    // the current processing mode set by with-mode.
    // For now, return #f (no mode set).
    Ok(crate::scheme::arena::FALSE_ID)
}

/// current-node-address - get the address of the current node
pub fn arena_current_node_address(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if !args.is_empty() {
        return Err("current-node-address: requires 0 arguments".to_string());
    }

    // Stub: In a full implementation, this would return an address object
    // representing the position of the current node in the grove.
    // For now, return #f (no address available).
    Ok(crate::scheme::arena::FALSE_ID)
}

/// current-node-page-number-sosofo - get page number sosofo for current node
pub fn arena_current_node_page_number_sosofo(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if !args.is_empty() {
        return Err("current-node-page-number-sosofo: requires 0 arguments".to_string());
    }

    // Stub: In a full implementation, this would return a sosofo that expands
    // to the page number where the current node appears.
    // For now, return an empty sosofo.
    Ok(crate::scheme::arena::FALSE_ID)
}

/// debug - print debug information (output to stderr)
pub fn arena_debug(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Print all arguments to stderr for debugging
    eprint!("[DEBUG]");
    for arg_id in args {
        match arena.get(*arg_id) {
            ValueData::Integer(n) => eprint!(" {}", n),
            ValueData::Real(f) => eprint!(" {}", f),
            ValueData::String(s) => eprint!(" \"{}\"", s),
            ValueData::Symbol(s) => eprint!(" {}", s),
            ValueData::Bool(b) => eprint!(" {}", if *b { "#t" } else { "#f" }),
            ValueData::Char(c) => eprint!(" #\\{}", c),
            ValueData::Nil => eprint!(" ()"),
            ValueData::Pair { .. } => eprint!(" <pair>"),
            ValueData::Vector(_) => eprint!(" <vector>"),
            ValueData::Node(_) => eprint!(" <node>"),
            ValueData::NodeList(_) => eprint!(" <node-list>"),
            ValueData::Sosofo => eprint!(" <sosofo>"),
            ValueData::Unspecified => eprint!(" <unspecified>"),
            ValueData::Error => eprint!(" <error>"),
            ValueData::Procedure(_) => eprint!(" <procedure>"),
            ValueData::Quantity { magnitude, .. } => eprint!(" {}pt", magnitude),
            ValueData::Keyword(k) => eprint!(" #:{}", k),
        }
    }
    eprintln!();

    // Return unspecified
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

// Phase 3 Batch 41: Type predicates and error (3 primitives)

/// exact? - Check if number is exact (integers are exact)
pub fn arena_exact_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("exact?: expected 1 argument, got {}", args.len()));
    }

    match arena.get(args[0]) {
        ValueData::Integer(_) => Ok(crate::scheme::arena::TRUE_ID),
        ValueData::Real(_) => Ok(crate::scheme::arena::FALSE_ID),
        ValueData::Quantity { .. } => Ok(crate::scheme::arena::FALSE_ID),
        _ => Err("exact?: argument must be a number".to_string()),
    }
}

/// inexact? - Check if number is inexact (floats are inexact)
pub fn arena_inexact_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("inexact?: expected 1 argument, got {}", args.len()));
    }

    match arena.get(args[0]) {
        ValueData::Integer(_) => Ok(crate::scheme::arena::FALSE_ID),
        ValueData::Real(_) => Ok(crate::scheme::arena::TRUE_ID),
        ValueData::Quantity { .. } => Ok(crate::scheme::arena::TRUE_ID),
        _ => Err("inexact?: argument must be a number".to_string()),
    }
}

/// error - Signal an error
pub fn arena_error(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    let msg = if args.is_empty() {
        "error".to_string()
    } else {
        // Build error message from arguments
        let mut parts = Vec::new();
        for arg in args {
            match arena.get(*arg) {
                ValueData::String(s) => parts.push(s.clone()),
                ValueData::Symbol(s) => parts.push(s.to_string()),
                ValueData::Integer(n) => parts.push(n.to_string()),
                ValueData::Real(f) => parts.push(f.to_string()),
                ValueData::Bool(b) => parts.push(if *b { "#t" } else { "#f" }.to_string()),
                ValueData::Char(c) => parts.push(format!("#\\{}", c)),
                _ => parts.push("<value>".to_string()),
            }
        }
        parts.join(" ")
    };

    Err(msg)
}

// Phase 3 Batch 42: Address type stubs (3 primitives)
// These are stubs - addresses not implemented yet

/// address? - Check if value is an address (stub: always false)
pub fn arena_address_p(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("address?: expected 1 argument, got {}", args.len()));
    }
    // Addresses not implemented yet, always return false
    Ok(crate::scheme::arena::FALSE_ID)
}

/// address-local? - Check if address is local (stub: always false)
pub fn arena_address_local_p(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("address-local?: expected 1 argument, got {}", args.len()));
    }
    // Addresses not implemented yet, always return false
    Ok(crate::scheme::arena::FALSE_ID)
}

/// address-visited? - Check if address has been visited (stub: always false)
pub fn arena_address_visited_p(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("address-visited?: expected 1 argument, got {}", args.len()));
    }
    // Addresses not implemented yet, always return false
    Ok(crate::scheme::arena::FALSE_ID)
}

// Phase 3 Batch 43: Color/display space stubs (4 primitives)
// These are stubs - full color/space types not implemented yet

/// color-space? - Check if value is a color-space (stub: always false)
pub fn arena_color_space_p(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("color-space?: expected 1 argument, got {}", args.len()));
    }
    // Color-spaces not implemented yet, always return false
    Ok(crate::scheme::arena::FALSE_ID)
}

/// color-space - Return a color-space (stub: return unspecified)
pub fn arena_color_space(_arena: &Arena, _args: &[ValueId]) -> ArenaResult {
    // Stub: return unspecified
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// display-space - Return a display-space (stub: return unspecified)
pub fn arena_display_space(_arena: &mut Arena, _args: &[ValueId]) -> ArenaResult {
    // Stub: return unspecified
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// inline-space - Return an inline-space (stub: return unspecified)
pub fn arena_inline_space(_arena: &mut Arena, _args: &[ValueId]) -> ArenaResult {
    // Stub: return unspecified
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

// Phase 3 Batch 44: Glyph type stubs (5 primitives)
// These are stubs - glyph types not implemented yet

/// glyph-id? - Check if value is a glyph-id (stub: always false)
pub fn arena_glyph_id_p(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("glyph-id?: expected 1 argument, got {}", args.len()));
    }
    // Glyph-ids not implemented yet, always return false
    Ok(crate::scheme::arena::FALSE_ID)
}

/// glyph-id - Return a glyph-id (stub: return unspecified)
pub fn arena_glyph_id(_arena: &Arena, _args: &[ValueId]) -> ArenaResult {
    // Stub: return unspecified
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// glyph-subst-table? - Check if value is a glyph-subst-table (stub: always false)
pub fn arena_glyph_subst_table_p(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("glyph-subst-table?: expected 1 argument, got {}", args.len()));
    }
    // Glyph-subst-tables not implemented yet, always return false
    Ok(crate::scheme::arena::FALSE_ID)
}

/// glyph-subst-table - Return a glyph-subst-table (stub: return unspecified)
pub fn arena_glyph_subst_table(_arena: &Arena, _args: &[ValueId]) -> ArenaResult {
    // Stub: return unspecified
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// glyph-subst - Return a glyph substitution (stub: return unspecified)
pub fn arena_glyph_subst(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: return unspecified
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

// Phase 3 Batch 45: Time type stubs (6 primitives)
// These are stubs - time type not implemented yet

/// time - Return current time (stub: return unspecified)
pub fn arena_time(_arena: &Arena, _args: &[ValueId]) -> ArenaResult {
    // Stub: return unspecified
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// time->string - Convert time to string (stub: return empty string)
pub fn arena_time_to_string(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("time->string: expected 1 argument, got {}", args.len()));
    }
    // Stub: return empty string
    Ok(arena.string(String::new()))
}

/// time<=? - Time less-than-or-equal comparison (stub: always false)
pub fn arena_time_le(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err(format!("time<=?: expected 2 arguments, got {}", args.len()));
    }
    // Stub: always false
    Ok(crate::scheme::arena::FALSE_ID)
}

/// time<? - Time less-than comparison (stub: always false)
pub fn arena_time_lt(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err(format!("time<?: expected 2 arguments, got {}", args.len()));
    }
    // Stub: always false
    Ok(crate::scheme::arena::FALSE_ID)
}

/// time>=? - Time greater-than-or-equal comparison (stub: always false)
pub fn arena_time_ge(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err(format!("time>=?: expected 2 arguments, got {}", args.len()));
    }
    // Stub: always false
    Ok(crate::scheme::arena::FALSE_ID)
}

/// time>? - Time greater-than comparison (stub: always false)
pub fn arena_time_gt(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err(format!("time>?: expected 2 arguments, got {}", args.len()));
    }
    // Stub: always false
    Ok(crate::scheme::arena::FALSE_ID)
}

// Phase 3 Batch 46: Language and style type stubs (3 primitives)
// These are stubs - language/style types not fully implemented yet

/// language? - Check if value is a language (stub: always false)
pub fn arena_language_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("language?: expected 1 argument, got {}", args.len()));
    }
    // Language type not implemented yet, always return false
    Ok(crate::scheme::arena::FALSE_ID)
}

/// language - Return language (stub: return "en" for English)
/// In DSSSL, this returns the language of a node or the current language.
/// For now, we return "en" (English) as a reasonable default.
pub fn arena_language(arena: &mut Arena, _args: &[ValueId]) -> ArenaResult {
    // Return "en" (English) as default language
    // DocBook stylesheets expect language codes like "en", "de", "fr", etc.
    Ok(arena.symbol("en".into()))
}

/// style? - Check if value is a style (stub: always false)
pub fn arena_style_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("style?: expected 1 argument, got {}", args.len()));
    }
    // Style type not implemented yet, always return false
    Ok(crate::scheme::arena::FALSE_ID)
}

// Phase 3 Batch 47: String comparison and simple stubs (3 primitives)

/// string-equiv? - String equivalence comparison (case-insensitive)
pub fn arena_string_equiv_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err(format!("string-equiv?: expected 2 arguments, got {}", args.len()));
    }

    let s1 = match arena.get(args[0]) {
        ValueData::String(s) => s.to_lowercase(),
        _ => return Err("string-equiv?: first argument must be a string".to_string()),
    };

    let s2 = match arena.get(args[1]) {
        ValueData::String(s) => s.to_lowercase(),
        _ => return Err("string-equiv?: second argument must be a string".to_string()),
    };

    Ok(if s1 == s2 {
        crate::scheme::arena::TRUE_ID
    } else {
        crate::scheme::arena::FALSE_ID
    })
}

/// label-length - Return label length (stub: return 0)
pub fn arena_label_length(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: return 0
    Ok(arena.int(0))
}

/// external-procedure - Call external procedure (stub: return unspecified)
pub fn arena_external_procedure(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: external procedures not implemented
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

// Phase 3 Batch 48: DTD/SGML stubs (6 primitives)
// These are stubs - DTD/SGML operations not fully implemented

/// declaration - Return declaration (stub: return unspecified)
pub fn arena_declaration(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: DTD declarations not implemented
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// dtd - Return DTD (stub: return unspecified)
pub fn arena_dtd(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: DTD access not implemented
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// epilog - Return epilog (stub: return empty node-list)
pub fn arena_epilog(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: return empty node-list
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// prolog - Return prolog (stub: return empty node-list)
pub fn arena_prolog(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: return empty node-list
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// sgml-declaration - Return SGML declaration (stub: return unspecified)
pub fn arena_sgml_declaration(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: SGML declarations not implemented
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// sgml-parse - Parse SGML (stub: return unspecified)
pub fn arena_sgml_parse(_arena: &Arena, _args: &[ValueId]) -> ArenaResult {
    // Stub: SGML parsing not implemented
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

// Phase 3 Batch 49: Entity/normalization stubs (4 primitives)
// These are stubs - entity operations not fully implemented

/// entity-address - Return entity address (stub: return unspecified)
pub fn arena_entity_address(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: entity addresses not implemented
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// entity-generated-system-id - Return generated system ID (stub: return empty string)
pub fn arena_entity_generated_system_id(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("entity-generated-system-id: expected 1 argument, got {}", args.len()));
    }
    // Stub: return empty string
    Ok(arena.string(String::new()))
}

/// entity-name-normalize - Normalize entity name (stub: return same string)
pub fn arena_entity_name_normalize(_arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("entity-name-normalize: expected 1 argument, got {}", args.len()));
    }
    // Stub: just return the input string
    Ok(args[0])
}

/// general-name-normalize - Normalize general name (stub: return same string)
/// (general-name-normalize name [node]) → string
/// The second argument (node) is optional and ignored in XML processing
pub fn arena_general_name_normalize(_arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.is_empty() || args.len() > 2 {
        return Err(format!("general-name-normalize: expected 1 or 2 arguments, got {}", args.len()));
    }
    // Stub: just return the input string (ignore optional node argument)
    Ok(args[0])
}

// Phase 3 Batch 50: Simple navigation and declaration stubs (5 primitives)

/// first-child-gi - Return first child GI (stub: return empty string)
pub fn arena_first_child_gi(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("first-child-gi: expected 1 argument, got {}", args.len()));
    }
    // Stub: return empty string
    Ok(arena.string(String::new()))
}

/// tree-root - Return tree root
pub fn arena_tree_root(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    // Optimization: Use Grove's root() method directly instead of traversing parent chain
    // This is much faster for large documents with deep nesting

    if let Some(ref grove) = arena.grove {
        // Fast path: Get root directly from Grove
        let root = grove.root();
        Ok(arena.alloc(ValueData::Node(Rc::new(root))))
    } else {
        // Fallback: Traverse parent chain if no Grove available
        // Get the node to start from
        let start_node = if args.is_empty() {
            // No argument: use current node
            match &arena.current_node {
                Some(n) => n.clone(),
                None => return Ok(crate::scheme::arena::UNSPECIFIED_ID),
            }
        } else {
            // With argument: optSingletonNodeList pattern
            match arena.get(args[0]) {
                ValueData::Node(n) => n.clone(),
                ValueData::NodeList(nl) => {
                    if nl.is_empty() {
                        return Ok(crate::scheme::arena::UNSPECIFIED_ID);
                    }
                    let first = nl.first();
                    let rest = nl.rest();
                    if rest.is_empty() {
                        Rc::new(first.unwrap())
                    } else {
                        return Err("tree-root: node-list must be a singleton".to_string());
                    }
                }
                _ => return Ok(crate::scheme::arena::UNSPECIFIED_ID),
            }
        };

        // Traverse up the tree to find the root element
        let mut current = start_node;
        loop {
            match current.parent() {
                Some(parent) => {
                    // Continue traversing up
                    current = Rc::new(parent);
                }
                None => {
                    // No parent - this is the root
                    break;
                }
            }
        }

        Ok(arena.alloc(ValueData::Node(current)))
    }
}

/// declare-default-language - Declare default language (stub: return unspecified)
pub fn arena_declare_default_language(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: language declarations not implemented
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// read-entity - Read entity (stub: return empty string)
pub fn arena_read_entity(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: entity reading not implemented, return empty string
    Ok(arena.string(String::new()))
}

/// set-visited! - Set visited flag (stub: return unspecified)
pub fn arena_set_visited(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: visited tracking not implemented
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

// Phase 3 Batch 51: Sosofo and navigation stubs (4 primitives)

/// sosofo-contains-node? - Check if sosofo contains node (stub: always false)
pub fn arena_sosofo_contains_node_p(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err(format!("sosofo-contains-node?: expected 2 arguments, got {}", args.len()));
    }
    // Stub: always return false
    Ok(crate::scheme::arena::FALSE_ID)
}

/// page-number-sosofo - Return page number sosofo (stub: return unspecified)
pub fn arena_page_number_sosofo(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: page numbering not implemented
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// ifollow - Inverse follow navigation (stub: return empty node-list)
pub fn arena_ifollow(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: inverse navigation not implemented, return empty node-list
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// with-language - Execute with language (stub: evaluate body)
pub fn arena_with_language(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: language switching not implemented
    // Just return unspecified since we can't evaluate the body here
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

// Phase 3 Batch 52: Element numbering stubs (3 primitives)

/// all-element-number - Return all element number (stub: return 0)
pub fn arena_all_element_number(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: element numbering not implemented, return 0
    Ok(arena.int(0))
}

/// ancestor-child-number - Return ancestor child number (stub: return 0)
pub fn arena_ancestor_child_number(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err(format!("ancestor-child-number: expected 2 arguments, got {}", args.len()));
    }
    // Stub: element numbering not implemented, return 0
    Ok(arena.int(0))
}

/// element-number-list - Return element number list (stub: return empty list)
pub fn arena_element_number_list(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: element numbering not implemented, return empty list
    Ok(crate::scheme::arena::NIL_ID)
}

// Phase 3 Batch 53: Inherited property stubs - Part 1 (5 primitives)

/// inherited-attribute-string - Get inherited attribute value (stub: return empty string)
pub fn arena_inherited_attribute_string(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("inherited-attribute-string: expected 1 argument, got {}", args.len()));
    }
    // Stub: inherited attribute lookup not implemented, return empty string
    Ok(arena.string(String::new()))
}

/// inherited-element-attribute-string - Get inherited element attribute (stub: return empty string)
pub fn arena_inherited_element_attribute_string(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() < 1 || args.len() > 2 {
        return Err(format!("inherited-element-attribute-string: expected 1 or 2 arguments, got {}", args.len()));
    }
    // Stub: inherited element attribute lookup not implemented, return empty string
    Ok(arena.string(String::new()))
}

/// inherited-start-indent - Get inherited start indent (stub: return 0)
pub fn arena_inherited_start_indent(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: inherited indent not implemented, return 0
    Ok(arena.int(0))
}

/// inherited-end-indent - Get inherited end indent (stub: return 0)
pub fn arena_inherited_end_indent(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: inherited indent not implemented, return 0
    Ok(arena.int(0))
}

/// inherited-line-spacing - Get inherited line spacing (stub: return 0)
pub fn arena_inherited_line_spacing(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: inherited line spacing not implemented, return 0
    Ok(arena.int(0))
}

// Phase 3 Batch 54: Inherited property stubs - Part 2 (6 primitives)

/// inherited-font-family-name - Get inherited font family (stub: return empty string)
pub fn arena_inherited_font_family_name(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: inherited font family not implemented, return empty string
    Ok(arena.string(String::new()))
}

/// inherited-font-size - Get inherited font size (stub: return 12)
pub fn arena_inherited_font_size(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: inherited font size not implemented, return 12pt
    Ok(arena.int(12))
}

/// inherited-font-weight - Get inherited font weight (stub: return empty string)
pub fn arena_inherited_font_weight(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: inherited font weight not implemented, return empty string
    Ok(arena.string(String::new()))
}

/// inherited-font-posture - Get inherited font posture (stub: return empty string)
pub fn arena_inherited_font_posture(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    // Stub: inherited font posture not implemented, return empty string
    Ok(arena.string(String::new()))
}

/// inherited-dbhtml-value - Get inherited dbhtml value (stub: return empty string)
pub fn arena_inherited_dbhtml_value(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("inherited-dbhtml-value: expected 1 argument, got {}", args.len()));
    }
    // Stub: inherited dbhtml value not implemented, return empty string
    Ok(arena.string(String::new()))
}

/// inherited-pi-value - Get inherited PI value (stub: return empty string)
pub fn arena_inherited_pi_value(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("inherited-pi-value: expected 1 argument, got {}", args.len()));
    }
    // Stub: inherited PI value not implemented, return empty string
    Ok(arena.string(String::new()))
}

// Phase 3 Batch 55: Node-list operation stubs - Part 1 (5 primitives)

/// node-list - Create node list from nodes and node-lists
/// (node-list node-or-list ...) → node-list
/// Concatenates all arguments (nodes and node-lists) into a single node-list
pub fn arena_node_list(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    use crate::grove::VecNodeList;

    let mut result_nodes = Vec::new();

    for arg in args {
        match arena.get(*arg) {
            ValueData::Node(node) => {
                // Single node: add it to the result
                result_nodes.push(node.as_ref().clone_node());
            }
            ValueData::NodeList(nl) => {
                // Node-list: add all its nodes to the result
                let len = nl.length();
                for i in 0..len {
                    if let Some(node) = nl.get(i) {
                        result_nodes.push(node);
                    }
                }
            }
            _ => {
                return Err("node-list: arguments must be nodes or node-lists".to_string());
            }
        }
    }

    Ok(arena.alloc(ValueData::NodeList(Rc::new(Box::new(VecNodeList::new(result_nodes))))))
}

/// node-list=? - Compare node lists for equality
/// Returns #t if both node-lists contain the same nodes in the same order, #f otherwise
pub fn arena_node_list_eq_p(arena: &Arena, args: &[ValueId]) -> ArenaResult {
    use crate::scheme::arena::{FALSE_ID, TRUE_ID};

    if args.len() != 2 {
        return Err(format!("node-list=?: expected 2 arguments, got {}", args.len()));
    }

    // Quick check: if both arguments are the same ValueId, they're equal
    if args[0] == args[1] {
        return Ok(TRUE_ID);
    }

    // Get both arguments - they can be nodes or node-lists
    let nl1 = match arena.get(args[0]) {
        ValueData::Node(n) => {
            // Single node -> treat as singleton node-list
            vec![n.clone()]
        }
        ValueData::NodeList(nl) => {
            // Collect all nodes from the node-list
            let mut nodes = Vec::new();
            let mut index = 0;
            while let Some(node) = nl.get(index) {
                nodes.push(std::rc::Rc::new(node));
                index += 1;
            }
            nodes
        }
        _ => return Err("node-list=?: first argument must be a node or node-list".to_string()),
    };

    let nl2 = match arena.get(args[1]) {
        ValueData::Node(n) => {
            // Single node -> treat as singleton node-list
            vec![n.clone()]
        }
        ValueData::NodeList(nl) => {
            // Collect all nodes from the node-list
            let mut nodes = Vec::new();
            let mut index = 0;
            while let Some(node) = nl.get(index) {
                nodes.push(std::rc::Rc::new(node));
                index += 1;
            }
            nodes
        }
        _ => return Err("node-list=?: second argument must be a node or node-list".to_string()),
    };

    // Compare lengths first
    if nl1.len() != nl2.len() {
        return Ok(FALSE_ID);
    }

    // Compare nodes element by element
    // OpenJade uses pointer equality (*nd1 != *nd2)
    // In Rust, we use node_eq() which compares the underlying node identity
    // (not the Rc wrapper address, but the actual XML node identity)
    for (node1, node2) in nl1.iter().zip(nl2.iter()) {
        // Compare node identity - nodes are equal if they refer to the same XML node
        // node1 and node2 are both Rc<Box<dyn Node>>, so we need to dereference to &dyn Node
        if !node1.as_ref().as_ref().node_eq(node2.as_ref().as_ref()) {
            return Ok(FALSE_ID);
        }
    }

    Ok(TRUE_ID)
}

/// node-list-count - Count nodes matching predicate (stub: return 0)
pub fn arena_node_list_count(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err(format!("node-list-count: expected 2 arguments, got {}", args.len()));
    }
    // Stub: node list count not implemented, return 0
    Ok(arena.int(0))
}

/// node-list-union-map - Union of mapped node lists (stub: return empty node list)
pub fn arena_node_list_union_map(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err(format!("node-list-union-map: expected 2 arguments, got {}", args.len()));
    }
    // Stub: node list union map not implemented, return empty node list
    Ok(crate::scheme::arena::NIL_ID)
}

/// node-list-symmetrical-difference - Symmetric difference of node lists (stub: return empty node list)
pub fn arena_node_list_symmetrical_difference(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err(format!("node-list-symmetrical-difference: expected 2 arguments, got {}", args.len()));
    }
    // Stub: node list symmetric difference not implemented, return empty node list
    Ok(crate::scheme::arena::NIL_ID)
}

// Phase 3 Batch 56: Node-list operation stubs - Part 2 (4 primitives)

/// node-list-address - Get address of node in node list (stub: return empty list)
pub fn arena_node_list_address(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err(format!("node-list-address: expected 2 arguments, got {}", args.len()));
    }
    // Stub: node list address not implemented, return empty list
    Ok(crate::scheme::arena::NIL_ID)
}

/// node-list-error - Create error node list (stub: return empty node list)
pub fn arena_node_list_error(arena: &mut Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("node-list-error: expected 1 argument, got {}", args.len()));
    }
    // Stub: node list error not implemented, return empty node list
    Ok(crate::scheme::arena::NIL_ID)
}

/// node-list-no-order - Remove ordering from node list (stub: return empty node list)
pub fn arena_node_list_no_order(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("node-list-no-order: expected 1 argument, got {}", args.len()));
    }
    // Stub: node list no-order not implemented, return empty node list
    Ok(crate::scheme::arena::NIL_ID)
}

/// origin-to-subnode-rel-forest-addr - Get relative forest address (stub: return empty list)
pub fn arena_origin_to_subnode_rel_forest_addr(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("origin-to-subnode-rel-forest-addr: expected 1 argument, got {}", args.len()));
    }
    // Stub: relative forest address not implemented, return empty list
    Ok(crate::scheme::arena::NIL_ID)
}

// Phase 3 Batch 57: Named node list stubs (3 primitives)

/// named-node - Get node by name (stub: return #f)
pub fn arena_named_node(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("named-node: expected 1 argument, got {}", args.len()));
    }
    // Stub: named nodes not implemented, return #f
    Ok(crate::scheme::arena::FALSE_ID)
}

/// named-node-list? - Check if value is a named node list (stub: return #f)
pub fn arena_named_node_list_p(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("named-node-list?: expected 1 argument, got {}", args.len()));
    }
    // Stub: named node lists not implemented, return #f
    Ok(crate::scheme::arena::FALSE_ID)
}

/// named-node-list-names - Get names from named node list (stub: return empty list)
pub fn arena_named_node_list_names(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("named-node-list-names: expected 1 argument, got {}", args.len()));
    }
    // Stub: named node list names not implemented, return empty list
    Ok(crate::scheme::arena::NIL_ID)
}

// Phase 3 Batch 58: Selection operation stubs (2 primitives)

/// select-by-class - Select nodes by class (stub: return empty node list)
pub fn arena_select_by_class(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("select-by-class: expected 1 argument, got {}", args.len()));
    }
    // Stub: select by class not implemented, return empty node list
    Ok(crate::scheme::arena::NIL_ID)
}

/// select-children - Select child elements (stub: return empty node list)
pub fn arena_select_children(_arena: &Arena, _args: &[ValueId]) -> ArenaResult {
    // Takes variable args (element names to match)
    // Stub: select children not implemented, return empty node list
    Ok(crate::scheme::arena::NIL_ID)
}

// Phase 3 Batch 59: Processing operation stubs (5 primitives)

/// process-children-trim - Process children with whitespace trimming (stub: return unspecified)
pub fn arena_process_children_trim(_arena: &Arena, _args: &[ValueId]) -> ArenaResult {
    // Stub: process-children-trim not implemented, return unspecified
    // Note: This is a processing operation that would need evaluator integration
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// process-element-with-id - Process element with given ID (stub: return unspecified)
pub fn arena_process_element_with_id(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("process-element-with-id: expected 1 argument, got {}", args.len()));
    }
    // Stub: process-element-with-id not implemented, return unspecified
    // Note: This would need grove integration to find element by ID
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// process-first-descendant - Process first descendant matching predicate (stub: return unspecified)
pub fn arena_process_first_descendant(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 1 {
        return Err(format!("process-first-descendant: expected 1 argument, got {}", args.len()));
    }
    // Stub: process-first-descendant not implemented, return unspecified
    // Note: This would need grove traversal and evaluator integration
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// process-matching-children - Process children matching predicate (stub: return unspecified)
pub fn arena_process_matching_children(_arena: &Arena, args: &[ValueId]) -> ArenaResult {
    if args.len() != 2 {
        return Err(format!("process-matching-children: expected 2 arguments, got {}", args.len()));
    }
    // Stub: process-matching-children not implemented, return unspecified
    // Note: This would need evaluator integration for predicate evaluation
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
}

/// next-match - Continue to next matching rule (stub: return unspecified)
pub fn arena_next_match(_arena: &Arena, _args: &[ValueId]) -> ArenaResult {
    // Stub: next-match not implemented as primitive, return unspecified
    // Note: This is typically implemented as a special form in the evaluator
    Ok(crate::scheme::arena::UNSPECIFIED_ID)
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
