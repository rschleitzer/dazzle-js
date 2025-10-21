//! Scheme primitive procedures
//!
//! Corresponds to OpenJade's `primitive.cxx` (~5,700 lines).
//!
//! ## Organization
//!
//! Primitives are organized by category:
//! - **List operations**: car, cdr, cons, list, append, length, etc.
//! - **Arithmetic**: +, -, *, /, <, >, =, etc.
//! - **String operations**: string-append, string-length, etc.
//! - **Type predicates**: pair?, list?, number?, string?, etc.
//! - **DSSSL-specific**: grove queries, processing functions
//!
//! ## OpenJade Correspondence
//!
//! | Category          | OpenJade Count | Dazzle Status |
//! |-------------------|----------------|---------------|
//! | R4RS standard     | ~90            | In progress   |
//! | DSSSL grove       | ~50            | TODO          |
//! | DSSSL processing  | ~20            | TODO          |
//! | DSSSL types       | ~30            | Stubs         |
//! | Extensions        | ~20            | TODO          |
//! | **Total**         | **~236**       |               |

use crate::scheme::value::Value;
use crate::grove::{EmptyNodeList, Node};

/// Result type for primitive procedures
pub type PrimitiveResult = Result<Value, String>;

// =============================================================================
// List Primitives (R4RS)
// =============================================================================

/// (car pair) → value
///
/// Returns the car (first element) of a pair.
///
/// **R4RS**: Required procedure
pub fn prim_car(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("car requires exactly 1 argument".to_string());
    }

    if let Value::Pair(ref p) = args[0] {
        let pair = p.borrow();
        Ok(pair.car.clone())
    } else {
        Err(format!("car: not a pair: {:?}", args[0]))
    }
}

/// (cdr pair) → value
///
/// Returns the cdr (rest) of a pair.
///
/// **R4RS**: Required procedure
pub fn prim_cdr(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("cdr requires exactly 1 argument".to_string());
    }

    if let Value::Pair(ref p) = args[0] {
        let pair = p.borrow();
        Ok(pair.cdr.clone())
    } else {
        Err(format!("cdr: not a pair: {:?}", args[0]))
    }
}

/// (cons obj1 obj2) → pair
///
/// Returns a newly allocated pair whose car is obj1 and cdr is obj2.
///
/// **R4RS**: Required procedure
pub fn prim_cons(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("cons requires exactly 2 arguments".to_string());
    }

    Ok(Value::cons(args[0].clone(), args[1].clone()))
}

/// (list obj ...) → list
///
/// Returns a newly allocated list of its arguments.
///
/// **R4RS**: Required procedure
pub fn prim_list(args: &[Value]) -> PrimitiveResult {
    let mut result = Value::Nil;
    for arg in args.iter().rev() {
        result = Value::cons(arg.clone(), result);
    }
    Ok(result)
}

/// (null? obj) → boolean
///
/// Returns #t if obj is the empty list, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_null_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("null? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(args[0].is_nil()))
}

/// (pair? obj) → boolean
///
/// Returns #t if obj is a pair, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_pair_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("pair? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(args[0].is_pair()))
}

/// (list? obj) → boolean
///
/// Returns #t if obj is a list (nil or pair), otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_list_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("list? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(args[0].is_list()))
}

/// (length list) → integer
///
/// Returns the length of list.
///
/// **R4RS**: Required procedure
pub fn prim_length(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("length requires exactly 1 argument".to_string());
    }

    let mut len = 0;
    let mut current = args[0].clone();

    loop {
        match current {
            Value::Nil => break,
            Value::Pair(ref p) => {
                len += 1;
                let pair = p.borrow();
                let cdr = pair.cdr.clone();
                drop(pair);
                current = cdr;
            }
            _ => return Err("length: not a proper list".to_string()),
        }
    }

    Ok(Value::integer(len))
}

/// (append list ...) → list
///
/// Returns a list consisting of the elements of the first list followed by
/// the elements of the other lists.
///
/// **R4RS**: Required procedure
pub fn prim_append(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() {
        return Ok(Value::Nil);
    }

    if args.len() == 1 {
        return Ok(args[0].clone());
    }

    // Convert all but last list to vectors
    let mut all_elements = Vec::new();

    for i in 0..args.len() - 1 {
        let mut current = args[i].clone();
        loop {
            match current {
                Value::Nil => break,
                Value::Pair(ref p) => {
                    let pair = p.borrow();
                    all_elements.push(pair.car.clone());
                    let cdr = pair.cdr.clone();
                    drop(pair);
                    current = cdr;
                }
                _ => return Err("append: not a proper list".to_string()),
            }
        }
    }

    // Build result list, ending with the last argument
    let mut result = args[args.len() - 1].clone();
    for elem in all_elements.iter().rev() {
        result = Value::cons(elem.clone(), result);
    }

    Ok(result)
}

/// (reverse list) → list
///
/// Returns a newly allocated list consisting of the elements of list in reverse order.
///
/// **R4RS**: Required procedure
pub fn prim_reverse(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("reverse requires exactly 1 argument".to_string());
    }

    let mut result = Value::Nil;
    let mut current = args[0].clone();

    loop {
        match current {
            Value::Nil => break,
            Value::Pair(ref p) => {
                let pair = p.borrow();
                result = Value::cons(pair.car.clone(), result);
                let cdr = pair.cdr.clone();
                drop(pair);
                current = cdr;
            }
            _ => return Err("reverse: not a proper list".to_string()),
        }
    }

    Ok(result)
}

/// (list-tail list k) → value
///
/// Returns the sublist of list obtained by omitting the first k elements.
///
/// **R4RS**: Required procedure
pub fn prim_list_tail(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("list-tail requires exactly 2 arguments".to_string());
    }

    let k = match args[1] {
        Value::Integer(n) if n >= 0 => n as usize,
        _ => return Err("list-tail: second argument must be a non-negative integer".to_string()),
    };

    let mut current = args[0].clone();
    for _ in 0..k {
        match current {
            Value::Pair(ref p) => {
                let pair = p.borrow();
                let cdr = pair.cdr.clone();
                drop(pair);
                current = cdr;
            }
            _ => return Err("list-tail: list too short".to_string()),
        }
    }

    Ok(current)
}

/// (list-ref list k) → value
///
/// Returns the kth element of list (zero-indexed).
///
/// **R4RS**: Required procedure
pub fn prim_list_ref(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("list-ref requires exactly 2 arguments".to_string());
    }

    let k = match args[1] {
        Value::Integer(n) if n >= 0 => n as usize,
        _ => return Err("list-ref: second argument must be a non-negative integer".to_string()),
    };

    let mut current = args[0].clone();
    for _ in 0..k {
        match current {
            Value::Pair(ref p) => {
                let pair = p.borrow();
                let cdr = pair.cdr.clone();
                drop(pair);
                current = cdr;
            }
            _ => return Err("list-ref: list too short".to_string()),
        }
    }

    match current {
        Value::Pair(ref p) => {
            let pair = p.borrow();
            Ok(pair.car.clone())
        }
        _ => Err("list-ref: index out of bounds".to_string()),
    }
}

// =============================================================================
// Number Primitives (R4RS)
// =============================================================================

/// (+ number ...) → number
///
/// Returns the sum of its arguments.
///
/// **R4RS**: Required procedure
pub fn prim_add(args: &[Value]) -> PrimitiveResult {
    let mut int_sum = 0i64;
    let mut real_sum = 0.0f64;
    let mut has_real = false;

    for arg in args {
        match arg {
            Value::Integer(n) => {
                int_sum += n;
                real_sum += *n as f64;
            }
            Value::Real(r) => {
                has_real = true;
                real_sum += r;
            }
            _ => return Err(format!("+: not a number: {:?}", arg)),
        }
    }

    if has_real {
        Ok(Value::real(real_sum))
    } else {
        Ok(Value::integer(int_sum))
    }
}

/// (- number number* ...) → number
///
/// With one argument, returns the negation. With multiple arguments, returns
/// the first argument minus the sum of the remaining arguments.
///
/// **R4RS**: Required procedure
pub fn prim_subtract(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() {
        return Err("-: requires at least 1 argument".to_string());
    }

    if args.len() == 1 {
        // Negation
        match args[0] {
            Value::Integer(n) => Ok(Value::integer(-n)),
            Value::Real(r) => Ok(Value::real(-r)),
            _ => Err(format!("-: not a number: {:?}", args[0])),
        }
    } else {
        // Subtraction
        let mut result_int: i64;
        let mut result_real: f64;
        let mut has_real = false;

        // First argument
        match args[0] {
            Value::Integer(n) => {
                result_int = n;
                result_real = n as f64;
            }
            Value::Real(r) => {
                has_real = true;
                result_int = 0; // dummy value
                result_real = r;
            }
            _ => return Err(format!("-: not a number: {:?}", args[0])),
        }

        // Subtract remaining arguments
        for arg in &args[1..] {
            match arg {
                Value::Integer(n) => {
                    result_int -= n;
                    result_real -= *n as f64;
                }
                Value::Real(r) => {
                    has_real = true;
                    result_real -= r;
                }
                _ => return Err(format!("-: not a number: {:?}", arg)),
            }
        }

        if has_real {
            Ok(Value::real(result_real))
        } else {
            Ok(Value::integer(result_int))
        }
    }
}

/// (* number ...) → number
///
/// Returns the product of its arguments.
///
/// **R4RS**: Required procedure
pub fn prim_multiply(args: &[Value]) -> PrimitiveResult {
    let mut int_product = 1i64;
    let mut real_product = 1.0f64;
    let mut has_real = false;

    for arg in args {
        match arg {
            Value::Integer(n) => {
                int_product *= n;
                real_product *= *n as f64;
            }
            Value::Real(r) => {
                has_real = true;
                real_product *= r;
            }
            _ => return Err(format!("*: not a number: {:?}", arg)),
        }
    }

    if has_real {
        Ok(Value::real(real_product))
    } else {
        Ok(Value::integer(int_product))
    }
}

/// (/ number number* ...) → number
///
/// With one argument, returns the reciprocal. With multiple arguments, returns
/// the first argument divided by the product of the remaining arguments.
///
/// **R4RS**: Required procedure
pub fn prim_divide(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() {
        return Err("/: requires at least 1 argument".to_string());
    }

    if args.len() == 1 {
        // Reciprocal
        match args[0] {
            Value::Integer(n) => {
                if n == 0 {
                    return Err("/: division by zero".to_string());
                }
                Ok(Value::real(1.0 / n as f64))
            }
            Value::Real(r) => {
                if r == 0.0 {
                    return Err("/: division by zero".to_string());
                }
                Ok(Value::real(1.0 / r))
            }
            _ => Err(format!("/: not a number: {:?}", args[0])),
        }
    } else {
        // Division - always returns real
        let mut result = match args[0] {
            Value::Integer(n) => n as f64,
            Value::Real(r) => r,
            _ => return Err(format!("/: not a number: {:?}", args[0])),
        };

        for arg in &args[1..] {
            match arg {
                Value::Integer(n) => {
                    if *n == 0 {
                        return Err("/: division by zero".to_string());
                    }
                    result /= *n as f64;
                }
                Value::Real(r) => {
                    if *r == 0.0 {
                        return Err("/: division by zero".to_string());
                    }
                    result /= r;
                }
                _ => return Err(format!("/: not a number: {:?}", arg)),
            }
        }

        Ok(Value::real(result))
    }
}

/// (quotient n1 n2) → integer
///
/// Returns the quotient of n1 and n2 (integer division).
///
/// **R4RS**: Required procedure
pub fn prim_quotient(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("quotient requires exactly 2 arguments".to_string());
    }

    let n1 = match args[0] {
        Value::Integer(n) => n,
        _ => return Err(format!("quotient: not an integer: {:?}", args[0])),
    };

    let n2 = match args[1] {
        Value::Integer(n) => n,
        _ => return Err(format!("quotient: not an integer: {:?}", args[1])),
    };

    if n2 == 0 {
        return Err("quotient: division by zero".to_string());
    }

    Ok(Value::integer(n1 / n2))
}

/// (remainder n1 n2) → integer
///
/// Returns the remainder of n1 divided by n2.
///
/// **R4RS**: Required procedure
pub fn prim_remainder(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("remainder requires exactly 2 arguments".to_string());
    }

    let n1 = match args[0] {
        Value::Integer(n) => n,
        _ => return Err(format!("remainder: not an integer: {:?}", args[0])),
    };

    let n2 = match args[1] {
        Value::Integer(n) => n,
        _ => return Err(format!("remainder: not an integer: {:?}", args[1])),
    };

    if n2 == 0 {
        return Err("remainder: division by zero".to_string());
    }

    Ok(Value::integer(n1 % n2))
}

/// (modulo n1 n2) → integer
///
/// Returns n1 modulo n2.
///
/// **R4RS**: Required procedure
pub fn prim_modulo(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("modulo requires exactly 2 arguments".to_string());
    }

    let n1 = match args[0] {
        Value::Integer(n) => n,
        _ => return Err(format!("modulo: not an integer: {:?}", args[0])),
    };

    let n2 = match args[1] {
        Value::Integer(n) => n,
        _ => return Err(format!("modulo: not an integer: {:?}", args[1])),
    };

    if n2 == 0 {
        return Err("modulo: division by zero".to_string());
    }

    // Euclidean modulo (always non-negative result)
    let result = ((n1 % n2) + n2) % n2;
    Ok(Value::integer(result))
}

/// (= number1 number2 number3 ...) → boolean
///
/// Returns #t if all arguments are numerically equal, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_num_eq(args: &[Value]) -> PrimitiveResult {
    if args.len() < 2 {
        return Err("=: requires at least 2 arguments".to_string());
    }

    let first_val = match args[0] {
        Value::Integer(n) => n as f64,
        Value::Real(r) => r,
        _ => return Err(format!("=: not a number: {:?}", args[0])),
    };

    for arg in &args[1..] {
        let val = match arg {
            Value::Integer(n) => *n as f64,
            Value::Real(r) => *r,
            _ => return Err(format!("=: not a number: {:?}", arg)),
        };

        if (first_val - val).abs() > f64::EPSILON {
            return Ok(Value::bool(false));
        }
    }

    Ok(Value::bool(true))
}

/// (< number1 number2 number3 ...) → boolean
///
/// Returns #t if arguments are in strictly increasing order, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_num_lt(args: &[Value]) -> PrimitiveResult {
    if args.len() < 2 {
        return Err("<: requires at least 2 arguments".to_string());
    }

    for i in 0..args.len() - 1 {
        let v1 = match args[i] {
            Value::Integer(n) => n as f64,
            Value::Real(r) => r,
            _ => return Err(format!("<: not a number: {:?}", args[i])),
        };

        let v2 = match args[i + 1] {
            Value::Integer(n) => n as f64,
            Value::Real(r) => r,
            _ => return Err(format!("<: not a number: {:?}", args[i + 1])),
        };

        if v1 >= v2 {
            return Ok(Value::bool(false));
        }
    }

    Ok(Value::bool(true))
}

/// (> number1 number2 number3 ...) → boolean
///
/// Returns #t if arguments are in strictly decreasing order, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_num_gt(args: &[Value]) -> PrimitiveResult {
    if args.len() < 2 {
        return Err(">: requires at least 2 arguments".to_string());
    }

    for i in 0..args.len() - 1 {
        let v1 = match args[i] {
            Value::Integer(n) => n as f64,
            Value::Real(r) => r,
            _ => return Err(format!(">: not a number: {:?}", args[i])),
        };

        let v2 = match args[i + 1] {
            Value::Integer(n) => n as f64,
            Value::Real(r) => r,
            _ => return Err(format!(">: not a number: {:?}", args[i + 1])),
        };

        if v1 <= v2 {
            return Ok(Value::bool(false));
        }
    }

    Ok(Value::bool(true))
}

/// (<= number1 number2 number3 ...) → boolean
///
/// Returns #t if arguments are in non-decreasing order, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_num_le(args: &[Value]) -> PrimitiveResult {
    if args.len() < 2 {
        return Err("<=: requires at least 2 arguments".to_string());
    }

    for i in 0..args.len() - 1 {
        let v1 = match args[i] {
            Value::Integer(n) => n as f64,
            Value::Real(r) => r,
            _ => return Err(format!("<=: not a number: {:?}", args[i])),
        };

        let v2 = match args[i + 1] {
            Value::Integer(n) => n as f64,
            Value::Real(r) => r,
            _ => return Err(format!("<=: not a number: {:?}", args[i + 1])),
        };

        if v1 > v2 {
            return Ok(Value::bool(false));
        }
    }

    Ok(Value::bool(true))
}

/// (>= number1 number2 number3 ...) → boolean
///
/// Returns #t if arguments are in non-increasing order, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_num_ge(args: &[Value]) -> PrimitiveResult {
    if args.len() < 2 {
        return Err(">=: requires at least 2 arguments".to_string());
    }

    for i in 0..args.len() - 1 {
        let v1 = match args[i] {
            Value::Integer(n) => n as f64,
            Value::Real(r) => r,
            _ => return Err(format!(">=: not a number: {:?}", args[i])),
        };

        let v2 = match args[i + 1] {
            Value::Integer(n) => n as f64,
            Value::Real(r) => r,
            _ => return Err(format!(">=: not a number: {:?}", args[i + 1])),
        };

        if v1 < v2 {
            return Ok(Value::bool(false));
        }
    }

    Ok(Value::bool(true))
}

/// (number? obj) → boolean
///
/// Returns #t if obj is a number, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_number_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("number?: requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::Integer(_) | Value::Real(_))))
}

/// (integer? obj) → boolean
///
/// Returns #t if obj is an integer, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_integer_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("integer?: requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::Integer(_))))
}

/// (real? obj) → boolean
///
/// Returns #t if obj is a real number, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_real_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("real?: requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::Real(_))))
}

/// (zero? number) → boolean
///
/// Returns #t if number is zero, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_zero_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("zero?: requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Integer(n) => Ok(Value::bool(n == 0)),
        Value::Real(r) => Ok(Value::bool(r.abs() < f64::EPSILON)),
        _ => Err(format!("zero?: not a number: {:?}", args[0])),
    }
}

/// (positive? number) → boolean
///
/// Returns #t if number is positive, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_positive_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("positive?: requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Integer(n) => Ok(Value::bool(n > 0)),
        Value::Real(r) => Ok(Value::bool(r > 0.0)),
        _ => Err(format!("positive?: not a number: {:?}", args[0])),
    }
}

/// (negative? number) → boolean
///
/// Returns #t if number is negative, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_negative_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("negative?: requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Integer(n) => Ok(Value::bool(n < 0)),
        Value::Real(r) => Ok(Value::bool(r < 0.0)),
        _ => Err(format!("negative?: not a number: {:?}", args[0])),
    }
}

/// (odd? integer) → boolean
///
/// Returns #t if integer is odd, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_odd_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("odd?: requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Integer(n) => Ok(Value::bool(n % 2 != 0)),
        _ => Err(format!("odd?: not an integer: {:?}", args[0])),
    }
}

/// (even? integer) → boolean
///
/// Returns #t if integer is even, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_even_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("even?: requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Integer(n) => Ok(Value::bool(n % 2 == 0)),
        _ => Err(format!("even?: not an integer: {:?}", args[0])),
    }
}

/// (abs number) → number
///
/// Returns the absolute value of number.
///
/// **R4RS**: Required procedure
pub fn prim_abs(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("abs: requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Integer(n) => Ok(Value::integer(n.abs())),
        Value::Real(r) => Ok(Value::real(r.abs())),
        _ => Err(format!("abs: not a number: {:?}", args[0])),
    }
}

/// (max number number* ...) → number
///
/// Returns the maximum of its arguments.
///
/// **R4RS**: Required procedure
pub fn prim_max(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() {
        return Err("max: requires at least 1 argument".to_string());
    }

    let mut max_val = match args[0] {
        Value::Integer(n) => n as f64,
        Value::Real(r) => r,
        _ => return Err(format!("max: not a number: {:?}", args[0])),
    };

    let mut has_real = matches!(args[0], Value::Real(_));

    for arg in &args[1..] {
        let val = match arg {
            Value::Integer(n) => *n as f64,
            Value::Real(r) => {
                has_real = true;
                *r
            }
            _ => return Err(format!("max: not a number: {:?}", arg)),
        };

        if val > max_val {
            max_val = val;
        }
    }

    if has_real {
        Ok(Value::real(max_val))
    } else {
        Ok(Value::integer(max_val as i64))
    }
}

/// (min number number* ...) → number
///
/// Returns the minimum of its arguments.
///
/// **R4RS**: Required procedure
pub fn prim_min(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() {
        return Err("min: requires at least 1 argument".to_string());
    }

    let mut min_val = match args[0] {
        Value::Integer(n) => n as f64,
        Value::Real(r) => r,
        _ => return Err(format!("min: not a number: {:?}", args[0])),
    };

    let mut has_real = matches!(args[0], Value::Real(_));

    for arg in &args[1..] {
        let val = match arg {
            Value::Integer(n) => *n as f64,
            Value::Real(r) => {
                has_real = true;
                *r
            }
            _ => return Err(format!("min: not a number: {:?}", arg)),
        };

        if val < min_val {
            min_val = val;
        }
    }

    if has_real {
        Ok(Value::real(min_val))
    } else {
        Ok(Value::integer(min_val as i64))
    }
}

/// (gcd n1 n2 ...) → integer
///
/// Returns the greatest common divisor of its arguments.
/// If called with no arguments, returns 0.
///
/// **R4RS**: Required procedure
pub fn prim_gcd(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() {
        return Ok(Value::integer(0));
    }

    // Helper function for Euclidean algorithm
    fn gcd_two(a: i64, b: i64) -> i64 {
        let mut a = a.abs();
        let mut b = b.abs();
        while b != 0 {
            let temp = b;
            b = a % b;
            a = temp;
        }
        a
    }

    let mut result = match args[0] {
        Value::Integer(n) => n,
        _ => return Err(format!("gcd: not an integer: {:?}", args[0])),
    };

    for arg in &args[1..] {
        let n = match arg {
            Value::Integer(n) => *n,
            _ => return Err(format!("gcd: not an integer: {:?}", arg)),
        };
        result = gcd_two(result, n);
    }

    Ok(Value::integer(result))
}

/// (lcm n1 n2 ...) → integer
///
/// Returns the least common multiple of its arguments.
/// If called with no arguments, returns 1.
///
/// **R4RS**: Required procedure
pub fn prim_lcm(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() {
        return Ok(Value::integer(1));
    }

    // Helper function for gcd (Euclidean algorithm)
    fn gcd_two(a: i64, b: i64) -> i64 {
        let mut a = a.abs();
        let mut b = b.abs();
        while b != 0 {
            let temp = b;
            b = a % b;
            a = temp;
        }
        a
    }

    // Helper function for lcm of two numbers
    fn lcm_two(a: i64, b: i64) -> i64 {
        if a == 0 || b == 0 {
            return 0;
        }
        (a.abs() / gcd_two(a, b)) * b.abs()
    }

    let mut result = match args[0] {
        Value::Integer(n) => n,
        _ => return Err(format!("lcm: not an integer: {:?}", args[0])),
    };

    for arg in &args[1..] {
        let n = match arg {
            Value::Integer(n) => *n,
            _ => return Err(format!("lcm: not an integer: {:?}", arg)),
        };
        result = lcm_two(result, n);
    }

    Ok(Value::integer(result))
}

/// (floor number) → integer
///
/// Returns the largest integer not greater than number.
///
/// **R4RS**: Required procedure
pub fn prim_floor(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("floor: requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Integer(n) => Ok(Value::integer(n)),
        Value::Real(r) => Ok(Value::integer(r.floor() as i64)),
        _ => Err(format!("floor: not a number: {:?}", args[0])),
    }
}

/// (ceiling number) → integer
///
/// Returns the smallest integer not less than number.
///
/// **R4RS**: Required procedure
pub fn prim_ceiling(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("ceiling: requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Integer(n) => Ok(Value::integer(n)),
        Value::Real(r) => Ok(Value::integer(r.ceil() as i64)),
        _ => Err(format!("ceiling: not a number: {:?}", args[0])),
    }
}

/// (truncate number) → integer
///
/// Returns the integer closest to number whose absolute value is not greater
/// than the absolute value of number.
///
/// **R4RS**: Required procedure
pub fn prim_truncate(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("truncate: requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Integer(n) => Ok(Value::integer(n)),
        Value::Real(r) => Ok(Value::integer(r.trunc() as i64)),
        _ => Err(format!("truncate: not a number: {:?}", args[0])),
    }
}

/// (round number) → integer
///
/// Returns the closest integer to number, rounding to even when number is halfway
/// between two integers.
///
/// **R4RS**: Required procedure
pub fn prim_round(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("round: requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Integer(n) => Ok(Value::integer(n)),
        Value::Real(r) => Ok(Value::integer(r.round() as i64)),
        _ => Err(format!("round: not a number: {:?}", args[0])),
    }
}

// =============================================================================
// String Primitives (R4RS)
// =============================================================================

/// (string-length string) → integer
///
/// Returns the number of characters in string.
///
/// **R4RS**: Required procedure
pub fn prim_string_length(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("string-length requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::String(s) => Ok(Value::integer(s.chars().count() as i64)),
        Value::Bool(false) => {
            // Treat #f as empty string (graceful handling)
            Ok(Value::integer(0))
        }
        Value::Unspecified => {
            // Treat unspecified as empty string (graceful handling)
            // This happens when case expressions have no matching clause
            Ok(Value::integer(0))
        }
        _ => Err(format!("string-length: not a string: {:?}", args[0])),
    }
}

/// (string-ref string k) → char
///
/// Returns the kth character of string (zero-indexed).
///
/// **R4RS**: Required procedure
pub fn prim_string_ref(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("string-ref requires exactly 2 arguments".to_string());
    }

    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(format!("string-ref: not a string: {:?}", args[0])),
    };

    let k = match args[1] {
        Value::Integer(n) if n >= 0 => n as usize,
        _ => return Err("string-ref: second argument must be a non-negative integer".to_string()),
    };

    let chars: Vec<char> = s.chars().collect();
    if k >= chars.len() {
        return Err("string-ref: index out of bounds".to_string());
    }

    Ok(Value::char(chars[k]))
}

/// (string-append string ...) → string
///
/// Returns a newly allocated string consisting of the concatenation of all arguments.
///
/// **R4RS**: Required procedure
pub fn prim_string_append(args: &[Value]) -> PrimitiveResult {
    let mut result = String::new();

    for arg in args {
        match arg {
            Value::String(s) => result.push_str(s),
            Value::Unspecified => {
                // Treat unspecified as empty string (graceful handling)
                // This happens when template functions don't explicitly return values
            }
            Value::Bool(false) => {
                // Treat #f as empty string (graceful handling)
                // Common pattern in templates where #f indicates "no value"
            }
            _ => return Err(format!("string-append: not a string: {:?}", arg)),
        }
    }

    Ok(Value::string(result))
}

/// (substring string start end) → string
///
/// Returns a newly allocated string formed from the characters of string
/// beginning with index start (inclusive) and ending with index end (exclusive).
///
/// **R4RS**: Required procedure
pub fn prim_substring(args: &[Value]) -> PrimitiveResult {
    if args.len() != 3 {
        return Err("substring requires exactly 3 arguments".to_string());
    }

    let s = match &args[0] {
        Value::String(s) => s,
        Value::Bool(false) => {
            return Err("substring: first argument is #f (expected string)".to_string());
        }
        Value::Unspecified => {
            // Treat unspecified as empty string (graceful handling)
            return Ok(Value::string(String::new()));
        }
        _ => return Err(format!("substring: not a string: {:?}", args[0])),
    };

    let start = match args[1] {
        Value::Integer(n) if n >= 0 => n as usize,
        _ => return Err("substring: start must be a non-negative integer".to_string()),
    };

    let end = match args[2] {
        Value::Integer(n) if n >= 0 => n as usize,
        _ => return Err("substring: end must be a non-negative integer".to_string()),
    };

    let chars: Vec<char> = s.chars().collect();
    if start > end || end > chars.len() {
        return Err("substring: invalid range".to_string());
    }

    let substring: String = chars[start..end].iter().collect();
    Ok(Value::string(substring))
}

/// (string=? string1 string2) → boolean
///
/// Returns #t if the two strings are equal, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_string_eq(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("string=? requires exactly 2 arguments".to_string());
    }

    let s1 = match &args[0] {
        Value::String(s) => s,
        _ => return Err(format!("string=?: not a string: {:?}", args[0])),
    };

    let s2 = match &args[1] {
        Value::String(s) => s,
        _ => return Err(format!("string=?: not a string: {:?}", args[1])),
    };

    Ok(Value::bool(s1 == s2))
}

/// (string<? string1 string2) → boolean
///
/// Returns #t if string1 is lexicographically less than string2, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_string_lt(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("string<? requires exactly 2 arguments".to_string());
    }

    let s1 = match &args[0] {
        Value::String(s) => s,
        _ => return Err(format!("string<?: not a string: {:?}", args[0])),
    };

    let s2 = match &args[1] {
        Value::String(s) => s,
        _ => return Err(format!("string<?: not a string: {:?}", args[1])),
    };

    Ok(Value::bool(s1 < s2))
}

/// (string>? string1 string2) → boolean
///
/// Returns #t if string1 is lexicographically greater than string2, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_string_gt(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("string>? requires exactly 2 arguments".to_string());
    }

    let s1 = match &args[0] {
        Value::String(s) => s,
        _ => return Err(format!("string>?: not a string: {:?}", args[0])),
    };

    let s2 = match &args[1] {
        Value::String(s) => s,
        _ => return Err(format!("string>?: not a string: {:?}", args[1])),
    };

    Ok(Value::bool(s1 > s2))
}

/// (string<=? string1 string2) → boolean
///
/// Returns #t if string1 is lexicographically less than or equal to string2, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_string_le(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("string<=? requires exactly 2 arguments".to_string());
    }

    let s1 = match &args[0] {
        Value::String(s) => s,
        _ => return Err(format!("string<=?: not a string: {:?}", args[0])),
    };

    let s2 = match &args[1] {
        Value::String(s) => s,
        _ => return Err(format!("string<=?: not a string: {:?}", args[1])),
    };

    Ok(Value::bool(s1 <= s2))
}

/// (string>=? string1 string2) → boolean
///
/// Returns #t if string1 is lexicographically greater than or equal to string2, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_string_ge(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("string>=? requires exactly 2 arguments".to_string());
    }

    let s1 = match &args[0] {
        Value::String(s) => s,
        _ => return Err(format!("string>=?: not a string: {:?}", args[0])),
    };

    let s2 = match &args[1] {
        Value::String(s) => s,
        _ => return Err(format!("string>=?: not a string: {:?}", args[1])),
    };

    Ok(Value::bool(s1 >= s2))
}

/// (string-ci=? string1 string2) → boolean
///
/// Returns #t if the two strings are equal ignoring case, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_string_ci_eq(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("string-ci=? requires exactly 2 arguments".to_string());
    }

    let s1 = match &args[0] {
        Value::String(s) => s.to_lowercase(),
        _ => return Err(format!("string-ci=?: not a string: {:?}", args[0])),
    };

    let s2 = match &args[1] {
        Value::String(s) => s.to_lowercase(),
        _ => return Err(format!("string-ci=?: not a string: {:?}", args[1])),
    };

    Ok(Value::bool(s1 == s2))
}

/// (string-ci<? string1 string2) → boolean
///
/// Returns #t if string1 is lexicographically less than string2 (ignoring case), otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_string_ci_lt(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("string-ci<? requires exactly 2 arguments".to_string());
    }

    let s1 = match &args[0] {
        Value::String(s) => s.to_lowercase(),
        _ => return Err(format!("string-ci<?: not a string: {:?}", args[0])),
    };

    let s2 = match &args[1] {
        Value::String(s) => s.to_lowercase(),
        _ => return Err(format!("string-ci<?: not a string: {:?}", args[1])),
    };

    Ok(Value::bool(s1 < s2))
}

/// (string-ci>? string1 string2) → boolean
///
/// Returns #t if string1 is lexicographically greater than string2 (ignoring case), otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_string_ci_gt(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("string-ci>? requires exactly 2 arguments".to_string());
    }

    let s1 = match &args[0] {
        Value::String(s) => s.to_lowercase(),
        _ => return Err(format!("string-ci>?: not a string: {:?}", args[0])),
    };

    let s2 = match &args[1] {
        Value::String(s) => s.to_lowercase(),
        _ => return Err(format!("string-ci>?: not a string: {:?}", args[1])),
    };

    Ok(Value::bool(s1 > s2))
}

/// (string-ci<=? string1 string2) → boolean
///
/// Returns #t if string1 is lexicographically less than or equal to string2 (ignoring case), otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_string_ci_le(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("string-ci<=? requires exactly 2 arguments".to_string());
    }

    let s1 = match &args[0] {
        Value::String(s) => s.to_lowercase(),
        _ => return Err(format!("string-ci<=?: not a string: {:?}", args[0])),
    };

    let s2 = match &args[1] {
        Value::String(s) => s.to_lowercase(),
        _ => return Err(format!("string-ci<=?: not a string: {:?}", args[1])),
    };

    Ok(Value::bool(s1 <= s2))
}

/// (string-ci>=? string1 string2) → boolean
///
/// Returns #t if string1 is lexicographically greater than or equal to string2 (ignoring case), otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_string_ci_ge(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("string-ci>=? requires exactly 2 arguments".to_string());
    }

    let s1 = match &args[0] {
        Value::String(s) => s.to_lowercase(),
        _ => return Err(format!("string-ci>=?: not a string: {:?}", args[0])),
    };

    let s2 = match &args[1] {
        Value::String(s) => s.to_lowercase(),
        _ => return Err(format!("string-ci>=?: not a string: {:?}", args[1])),
    };

    Ok(Value::bool(s1 >= s2))
}

/// (string? obj) → boolean
///
/// Returns #t if obj is a string, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_string_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("string? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::String(_))))
}

/// (make-string k [char]) → string
///
/// Returns a newly allocated string of length k. If char is given,
/// then all elements of the string are initialized to char, otherwise
/// the contents are unspecified (we use space).
///
/// **R4RS**: Required procedure
pub fn prim_make_string(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() || args.len() > 2 {
        return Err("make-string requires 1 or 2 arguments".to_string());
    }

    let k = match args[0] {
        Value::Integer(n) if n >= 0 => n as usize,
        _ => return Err("make-string: first argument must be a non-negative integer".to_string()),
    };

    let ch = if args.len() == 2 {
        match args[1] {
            Value::Char(c) => c,
            _ => return Err("make-string: second argument must be a character".to_string()),
        }
    } else {
        ' '
    };

    Ok(Value::string(ch.to_string().repeat(k)))
}

/// (string char ...) → string
///
/// Returns a newly allocated string composed of the arguments.
///
/// **R4RS**: Required procedure
pub fn prim_string(args: &[Value]) -> PrimitiveResult {
    let mut result = String::new();

    for arg in args {
        match arg {
            Value::Char(c) => result.push(*c),
            _ => return Err(format!("string: not a character: {:?}", arg)),
        }
    }

    Ok(Value::string(result))
}

/// (string->list string) → list
///
/// Returns a newly allocated list of the characters of string.
///
/// **R4RS**: Required procedure
pub fn prim_string_to_list(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("string->list requires exactly 1 argument".to_string());
    }

    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(format!("string->list: not a string: {:?}", args[0])),
    };

    let mut result = Value::Nil;
    for ch in s.chars().rev() {
        result = Value::cons(Value::char(ch), result);
    }

    Ok(result)
}

/// (list->string list) → string
///
/// Returns a newly allocated string formed from the characters in list.
///
/// **R4RS**: Required procedure
pub fn prim_list_to_string(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("list->string requires exactly 1 argument".to_string());
    }

    let mut result = String::new();
    let mut current = args[0].clone();

    loop {
        match current {
            Value::Nil => break,
            Value::Pair(ref p) => {
                let pair = p.borrow();
                match pair.car {
                    Value::Char(c) => result.push(c),
                    _ => return Err("list->string: list must contain only characters".to_string()),
                }
                let cdr = pair.cdr.clone();
                drop(pair);
                current = cdr;
            }
            _ => return Err("list->string: not a proper list".to_string()),
        }
    }

    Ok(Value::string(result))
}

/// (symbol->string symbol) → string
///
/// Returns the name of symbol as a string.
///
/// **R4RS**: Required procedure
pub fn prim_symbol_to_string(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("symbol->string requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Symbol(s) => Ok(Value::string(s.to_string())),
        _ => Err(format!("symbol->string: not a symbol: {:?}", args[0])),
    }
}

/// (string->symbol string) → symbol
///
/// Returns the symbol whose name is string.
///
/// **R4RS**: Required procedure
pub fn prim_string_to_symbol(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("string->symbol requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::String(s) => Ok(Value::symbol(s)),
        _ => Err(format!("string->symbol: not a string: {:?}", args[0])),
    }
}

/// (symbol? obj) → boolean
///
/// Returns #t if obj is a symbol, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_symbol_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("symbol? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::Symbol(_))))
}

/// (char? obj) → boolean
///
/// Returns #t if obj is a character, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("char? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::Char(_))))
}

/// (char=? char1 char2) → boolean
///
/// Returns #t if the two characters are equal, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_eq(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("char=? requires exactly 2 arguments".to_string());
    }

    let c1 = match args[0] {
        Value::Char(c) => c,
        _ => return Err(format!("char=?: not a character: {:?}", args[0])),
    };

    let c2 = match args[1] {
        Value::Char(c) => c,
        _ => return Err(format!("char=?: not a character: {:?}", args[1])),
    };

    Ok(Value::bool(c1 == c2))
}

/// (char<? char1 char2) → boolean
///
/// Returns #t if char1 is less than char2, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_lt(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("char<? requires exactly 2 arguments".to_string());
    }

    let c1 = match args[0] {
        Value::Char(c) => c,
        _ => return Err(format!("char<?: not a character: {:?}", args[0])),
    };

    let c2 = match args[1] {
        Value::Char(c) => c,
        _ => return Err(format!("char<?: not a character: {:?}", args[1])),
    };

    Ok(Value::bool(c1 < c2))
}

/// (char>? char1 char2) → boolean
///
/// Returns #t if char1 is greater than char2, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_gt(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("char>? requires exactly 2 arguments".to_string());
    }

    let c1 = match args[0] {
        Value::Char(c) => c,
        _ => return Err(format!("char>?: not a character: {:?}", args[0])),
    };

    let c2 = match args[1] {
        Value::Char(c) => c,
        _ => return Err(format!("char>?: not a character: {:?}", args[1])),
    };

    Ok(Value::bool(c1 > c2))
}

/// (char-upcase char) → char
///
/// Returns the uppercase equivalent of char.
///
/// **R4RS**: Required procedure
pub fn prim_char_upcase(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("char-upcase requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Char(c) => Ok(Value::char(c.to_ascii_uppercase())),
        _ => Err(format!("char-upcase: not a character: {:?}", args[0])),
    }
}

/// (char-downcase char) → char
///
/// Returns the lowercase equivalent of char.
///
/// **R4RS**: Required procedure
pub fn prim_char_downcase(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("char-downcase requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Char(c) => Ok(Value::char(c.to_ascii_lowercase())),
        _ => Err(format!("char-downcase: not a character: {:?}", args[0])),
    }
}

/// (char<=? char1 char2) → boolean
///
/// Returns #t if char1 is less than or equal to char2, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_le(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("char<=? requires exactly 2 arguments".to_string());
    }

    let c1 = match args[0] {
        Value::Char(c) => c,
        _ => return Err(format!("char<=?: not a character: {:?}", args[0])),
    };

    let c2 = match args[1] {
        Value::Char(c) => c,
        _ => return Err(format!("char<=?: not a character: {:?}", args[1])),
    };

    Ok(Value::bool(c1 <= c2))
}

/// (char>=? char1 char2) → boolean
///
/// Returns #t if char1 is greater than or equal to char2, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_ge(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("char>=? requires exactly 2 arguments".to_string());
    }

    let c1 = match args[0] {
        Value::Char(c) => c,
        _ => return Err(format!("char>=?: not a character: {:?}", args[0])),
    };

    let c2 = match args[1] {
        Value::Char(c) => c,
        _ => return Err(format!("char>=?: not a character: {:?}", args[1])),
    };

    Ok(Value::bool(c1 >= c2))
}

/// (char->integer char) → integer
///
/// Returns the Unicode/ASCII code point of the character.
///
/// **R4RS**: Required procedure
pub fn prim_char_to_integer(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("char->integer requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Char(c) => Ok(Value::integer(c as i64)),
        _ => Err(format!("char->integer: not a character: {:?}", args[0])),
    }
}

/// (integer->char n) → char
///
/// Returns the character with the given Unicode/ASCII code point.
///
/// **R4RS**: Required procedure
pub fn prim_integer_to_char(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("integer->char requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Integer(n) => {
            if n < 0 || n > 0x10FFFF {
                return Err(format!("integer->char: invalid code point: {}", n));
            }
            match char::from_u32(n as u32) {
                Some(c) => Ok(Value::char(c)),
                None => Err(format!("integer->char: invalid Unicode code point: {}", n)),
            }
        }
        _ => Err(format!("integer->char: not an integer: {:?}", args[0])),
    }
}

/// (char-alphabetic? char) → boolean
///
/// Returns #t if char is an alphabetic character, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_alphabetic_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("char-alphabetic? requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Char(c) => Ok(Value::bool(c.is_alphabetic())),
        _ => Err(format!("char-alphabetic?: not a character: {:?}", args[0])),
    }
}

/// (char-numeric? char) → boolean
///
/// Returns #t if char is a numeric character, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_numeric_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("char-numeric? requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Char(c) => Ok(Value::bool(c.is_numeric())),
        _ => Err(format!("char-numeric?: not a character: {:?}", args[0])),
    }
}

/// (char-whitespace? char) → boolean
///
/// Returns #t if char is a whitespace character, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_whitespace_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("char-whitespace? requires exactly 1 argument".to_string());
    }

    match args[0] {
        Value::Char(c) => Ok(Value::bool(c.is_whitespace())),
        _ => Err(format!("char-whitespace?: not a character: {:?}", args[0])),
    }
}

/// (char-ci=? char1 char2) → boolean
///
/// Returns #t if the two characters are equal ignoring case, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_ci_eq(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("char-ci=? requires exactly 2 arguments".to_string());
    }

    let c1 = match args[0] {
        Value::Char(c) => c.to_ascii_lowercase(),
        _ => return Err(format!("char-ci=?: not a character: {:?}", args[0])),
    };

    let c2 = match args[1] {
        Value::Char(c) => c.to_ascii_lowercase(),
        _ => return Err(format!("char-ci=?: not a character: {:?}", args[1])),
    };

    Ok(Value::bool(c1 == c2))
}

/// (char-ci<? char1 char2) → boolean
///
/// Returns #t if char1 is less than char2 ignoring case, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_ci_lt(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("char-ci<? requires exactly 2 arguments".to_string());
    }

    let c1 = match args[0] {
        Value::Char(c) => c.to_ascii_lowercase(),
        _ => return Err(format!("char-ci<?: not a character: {:?}", args[0])),
    };

    let c2 = match args[1] {
        Value::Char(c) => c.to_ascii_lowercase(),
        _ => return Err(format!("char-ci<?: not a character: {:?}", args[1])),
    };

    Ok(Value::bool(c1 < c2))
}

/// (char-ci>? char1 char2) → boolean
///
/// Returns #t if char1 is greater than char2 ignoring case, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_ci_gt(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("char-ci>? requires exactly 2 arguments".to_string());
    }

    let c1 = match args[0] {
        Value::Char(c) => c.to_ascii_lowercase(),
        _ => return Err(format!("char-ci>?: not a character: {:?}", args[0])),
    };

    let c2 = match args[1] {
        Value::Char(c) => c.to_ascii_lowercase(),
        _ => return Err(format!("char-ci>?: not a character: {:?}", args[1])),
    };

    Ok(Value::bool(c1 > c2))
}

/// (char-ci<=? char1 char2) → boolean
///
/// Returns #t if char1 is less than or equal to char2 ignoring case, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_ci_le(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("char-ci<=? requires exactly 2 arguments".to_string());
    }

    let c1 = match args[0] {
        Value::Char(c) => c.to_ascii_lowercase(),
        _ => return Err(format!("char-ci<=?: not a character: {:?}", args[0])),
    };

    let c2 = match args[1] {
        Value::Char(c) => c.to_ascii_lowercase(),
        _ => return Err(format!("char-ci<=?: not a character: {:?}", args[1])),
    };

    Ok(Value::bool(c1 <= c2))
}

/// (char-ci>=? char1 char2) → boolean
///
/// Returns #t if char1 is greater than or equal to char2 ignoring case, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_char_ci_ge(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("char-ci>=? requires exactly 2 arguments".to_string());
    }

    let c1 = match args[0] {
        Value::Char(c) => c.to_ascii_lowercase(),
        _ => return Err(format!("char-ci>=?: not a character: {:?}", args[0])),
    };

    let c2 = match args[1] {
        Value::Char(c) => c.to_ascii_lowercase(),
        _ => return Err(format!("char-ci>=?: not a character: {:?}", args[1])),
    };

    Ok(Value::bool(c1 >= c2))
}

// =============================================================================
// Boolean and Equality Primitives (R4RS)
// =============================================================================

/// (not obj) → boolean
///
/// Returns #t if obj is false, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_not(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("not requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(!args[0].is_true()))
}

/// (boolean? obj) → boolean
///
/// Returns #t if obj is a boolean, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_boolean_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("boolean? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::Bool(_))))
}

/// (equal? obj1 obj2) → boolean
///
/// Returns #t if obj1 and obj2 are structurally equal, otherwise #f.
/// This is a deep comparison that recursively compares pairs and other structures.
///
/// **R4RS**: Required procedure
pub fn prim_equal_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("equal? requires exactly 2 arguments".to_string());
    }

    Ok(Value::bool(values_equal(&args[0], &args[1])))
}

/// Helper function for deep structural equality
fn values_equal(v1: &Value, v2: &Value) -> bool {
    match (v1, v2) {
        (Value::Nil, Value::Nil) => true,
        (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
        (Value::Integer(n1), Value::Integer(n2)) => n1 == n2,
        (Value::Real(r1), Value::Real(r2)) => (r1 - r2).abs() < f64::EPSILON,
        (Value::Integer(n), Value::Real(r)) | (Value::Real(r), Value::Integer(n)) => {
            (*n as f64 - r).abs() < f64::EPSILON
        }
        (Value::Char(c1), Value::Char(c2)) => c1 == c2,
        (Value::String(s1), Value::String(s2)) => s1 == s2,
        (Value::Symbol(s1), Value::Symbol(s2)) => s1 == s2,
        (Value::Pair(p1), Value::Pair(p2)) => {
            let pair1 = p1.borrow();
            let pair2 = p2.borrow();
            values_equal(&pair1.car, &pair2.car) && values_equal(&pair1.cdr, &pair2.cdr)
        }
        _ => false,
    }
}

/// (eqv? obj1 obj2) → boolean
///
/// Returns #t if obj1 and obj2 are equivalent, otherwise #f.
/// For most types, this is the same as equal?, but symbols are compared by identity.
///
/// **R4RS**: Required procedure
pub fn prim_eqv_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("eqv? requires exactly 2 arguments".to_string());
    }

    let result = match (&args[0], &args[1]) {
        (Value::Nil, Value::Nil) => true,
        (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
        (Value::Integer(n1), Value::Integer(n2)) => n1 == n2,
        (Value::Real(r1), Value::Real(r2)) => (r1 - r2).abs() < f64::EPSILON,
        (Value::Char(c1), Value::Char(c2)) => c1 == c2,
        (Value::Symbol(s1), Value::Symbol(s2)) => s1 == s2,
        // For pairs, strings, and procedures, eqv? checks object identity
        (Value::Pair(p1), Value::Pair(p2)) => gc::Gc::ptr_eq(p1, p2),
        (Value::String(s1), Value::String(s2)) => gc::Gc::ptr_eq(s1, s2),
        (Value::Procedure(pr1), Value::Procedure(pr2)) => gc::Gc::ptr_eq(pr1, pr2),
        _ => false,
    };

    Ok(Value::bool(result))
}

/// (eq? obj1 obj2) → boolean
///
/// Returns #t if obj1 and obj2 are the same object (pointer equality), otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_eq_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("eq? requires exactly 2 arguments".to_string());
    }

    let result = match (&args[0], &args[1]) {
        (Value::Nil, Value::Nil) => true,
        (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
        (Value::Integer(n1), Value::Integer(n2)) => n1 == n2,
        (Value::Char(c1), Value::Char(c2)) => c1 == c2,
        (Value::Symbol(s1), Value::Symbol(s2)) => s1 == s2,
        // For heap-allocated objects, check pointer equality
        (Value::Pair(p1), Value::Pair(p2)) => gc::Gc::ptr_eq(p1, p2),
        (Value::String(s1), Value::String(s2)) => gc::Gc::ptr_eq(s1, s2),
        (Value::Procedure(pr1), Value::Procedure(pr2)) => gc::Gc::ptr_eq(pr1, pr2),
        _ => false,
    };

    Ok(Value::bool(result))
}

/// (procedure? obj) → boolean
///
/// Returns #t if obj is a procedure, otherwise #f.
///
/// **R4RS**: Required procedure
pub fn prim_procedure_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("procedure? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::Procedure(_))))
}

// =============================================================================
// I/O and Utility Primitives (R4RS / DSSSL)
// =============================================================================

/// (error message obj ...) → does not return
///
/// Signals an error with the given message and objects.
/// In DSSSL/OpenJade, this is used for runtime error reporting.
///
/// **DSSSL**: Extension (OpenJade primitive)
pub fn prim_error(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() {
        return Err("error: requires at least 1 argument".to_string());
    }

    let message = match &args[0] {
        Value::String(s) => s.to_string(),
        Value::Symbol(s) => s.to_string(),
        other => format!("{:?}", other),
    };

    // Collect additional objects
    let mut full_message = message;
    for arg in &args[1..] {
        full_message.push_str(&format!(" {:?}", arg));
    }

    Err(full_message)
}

/// (display obj) → unspecified
///
/// Writes obj to the current output port (stdout).
/// Used for debugging and output in DSSSL templates.
///
/// **R4RS**: Required procedure
pub fn prim_display(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("display requires exactly 1 argument".to_string());
    }

    let output = match &args[0] {
        Value::String(s) => s.to_string(),
        Value::Char(c) => c.to_string(),
        other => format!("{:?}", other),
    };

    print!("{}", output);
    Ok(Value::Unspecified)
}

/// (newline) → unspecified
///
/// Writes a newline to the current output port (stdout).
///
/// **R4RS**: Required procedure
pub fn prim_newline(args: &[Value]) -> PrimitiveResult {
    if !args.is_empty() {
        return Err("newline requires 0 arguments".to_string());
    }

    println!();
    Ok(Value::Unspecified)
}

/// (write obj) → unspecified
///
/// Writes obj to the current output port in a machine-readable format.
///
/// **R4RS**: Required procedure
pub fn prim_write(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("write requires exactly 1 argument".to_string());
    }

    print!("{:?}", args[0]);
    Ok(Value::Unspecified)
}

// =============================================================================
// Conversion Primitives (R4RS)
// =============================================================================

/// (number->string number) → string
///
/// Returns a string representation of number.
///
/// **R4RS**: Required procedure
pub fn prim_number_to_string(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("number->string requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Integer(n) => Ok(Value::string(n.to_string())),
        Value::Real(r) => Ok(Value::string(r.to_string())),
        _ => Err(format!("number->string: not a number: {:?}", args[0])),
    }
}

/// (string->number string) → number or #f
///
/// Returns a number parsed from string, or #f if the string is not a valid number.
///
/// **R4RS**: Required procedure
pub fn prim_string_to_number(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("string->number requires exactly 1 argument".to_string());
    }

    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(format!("string->number: not a string: {:?}", args[0])),
    };

    // Try parsing as integer first
    if let Ok(n) = s.parse::<i64>() {
        return Ok(Value::integer(n));
    }

    // Try parsing as real
    if let Ok(r) = s.parse::<f64>() {
        return Ok(Value::real(r));
    }

    // Return #f if not a valid number
    Ok(Value::bool(false))
}

// =============================================================================
// Keyword Primitives (DSSSL Extension)
// =============================================================================

/// (keyword? obj) → boolean
///
/// Returns #t if obj is a keyword, otherwise #f.
///
/// **DSSSL**: Extension (OpenJade primitive)
pub fn prim_keyword_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("keyword? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::Keyword(_))))
}

/// (keyword->string keyword) → string
///
/// Returns the name of keyword as a string.
///
/// **DSSSL**: Extension (OpenJade primitive)
pub fn prim_keyword_to_string(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("keyword->string requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Keyword(k) => Ok(Value::string(k.to_string())),
        _ => Err(format!("keyword->string: not a keyword: {:?}", args[0])),
    }
}

/// (string->keyword string) → keyword
///
/// Returns the keyword whose name is string.
///
/// **DSSSL**: Extension (OpenJade primitive)
pub fn prim_string_to_keyword(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("string->keyword requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::String(s) => Ok(Value::keyword(s)),
        _ => Err(format!("string->keyword: not a string: {:?}", args[0])),
    }
}

// =============================================================================
// Additional List Utilities (R4RS)
// =============================================================================

/// (memq obj list) → list or #f
///
/// Returns the first sublist of list whose car is eq? to obj.
/// If obj does not occur in list, returns #f.
///
/// **R4RS**: Required procedure
pub fn prim_memq(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("memq requires exactly 2 arguments".to_string());
    }

    let obj = &args[0];
    let mut current = args[1].clone();

    loop {
        match current {
            Value::Nil => return Ok(Value::bool(false)),
            Value::Pair(ref p) => {
                let pair = p.borrow();
                // Use eq? comparison
                if obj.eq(&pair.car) {
                    drop(pair);
                    return Ok(current);
                }
                let cdr = pair.cdr.clone();
                drop(pair);
                current = cdr;
            }
            _ => return Err("memq: not a proper list".to_string()),
        }
    }
}

/// (memv obj list) → list or #f
///
/// Returns the first sublist of list whose car is eqv? to obj.
/// If obj does not occur in list, returns #f.
///
/// **R4RS**: Required procedure
pub fn prim_memv(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("memv requires exactly 2 arguments".to_string());
    }

    let obj = &args[0];
    let mut current = args[1].clone();

    loop {
        match current {
            Value::Nil => return Ok(Value::bool(false)),
            Value::Pair(ref p) => {
                let pair = p.borrow();
                // Use eqv? comparison
                if obj.eqv(&pair.car) {
                    drop(pair);
                    return Ok(current);
                }
                let cdr = pair.cdr.clone();
                drop(pair);
                current = cdr;
            }
            _ => return Err("memv: not a proper list".to_string()),
        }
    }
}

/// (member obj list) → list or #f
///
/// Returns the first sublist of list whose car is equal? to obj.
/// If obj does not occur in list, returns #f.
///
/// **R4RS**: Required procedure
pub fn prim_member(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("member requires exactly 2 arguments".to_string());
    }

    let obj = &args[0];
    let mut current = args[1].clone();

    loop {
        match current {
            Value::Nil => return Ok(Value::bool(false)),
            Value::Pair(ref p) => {
                let pair = p.borrow();
                // Use equal? comparison
                if obj.equal(&pair.car) {
                    drop(pair);
                    return Ok(current);
                }
                let cdr = pair.cdr.clone();
                drop(pair);
                current = cdr;
            }
            _ => return Err("member: not a proper list".to_string()),
        }
    }
}

/// (assq obj alist) → pair or #f
///
/// Returns the first pair in alist whose car is eq? to obj.
/// If no pair is found, returns #f.
///
/// **R4RS**: Required procedure
pub fn prim_assq(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("assq requires exactly 2 arguments".to_string());
    }

    let obj = &args[0];
    let mut current = args[1].clone();

    loop {
        match current {
            Value::Nil => return Ok(Value::bool(false)),
            Value::Pair(ref p) => {
                let pair = p.borrow();
                // Check if car is a pair
                if let Value::Pair(ref inner_p) = pair.car {
                    let inner_pair = inner_p.borrow();
                    if obj.eq(&inner_pair.car) {
                        drop(inner_pair);
                        let result = pair.car.clone();
                        drop(pair);
                        return Ok(result);
                    }
                }
                let cdr = pair.cdr.clone();
                drop(pair);
                current = cdr;
            }
            _ => return Err("assq: not a proper list".to_string()),
        }
    }
}

/// (assv obj alist) → pair or #f
///
/// Returns the first pair in alist whose car is eqv? to obj.
/// If no pair is found, returns #f.
///
/// **R4RS**: Required procedure
pub fn prim_assv(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("assv requires exactly 2 arguments".to_string());
    }

    let obj = &args[0];
    let mut current = args[1].clone();

    loop {
        match current {
            Value::Nil => return Ok(Value::bool(false)),
            Value::Pair(ref p) => {
                let pair = p.borrow();
                // Check if car is a pair
                if let Value::Pair(ref inner_p) = pair.car {
                    let inner_pair = inner_p.borrow();
                    if obj.eqv(&inner_pair.car) {
                        drop(inner_pair);
                        let result = pair.car.clone();
                        drop(pair);
                        return Ok(result);
                    }
                }
                let cdr = pair.cdr.clone();
                drop(pair);
                current = cdr;
            }
            _ => return Err("assv: not a proper list".to_string()),
        }
    }
}

/// (assoc obj alist) → pair or #f
///
/// Returns the first pair in alist whose car is equal? to obj.
/// If no pair is found, returns #f.
///
/// **R4RS**: Required procedure
pub fn prim_assoc(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("assoc requires exactly 2 arguments".to_string());
    }

    let obj = &args[0];
    let mut current = args[1].clone();

    loop {
        match current {
            Value::Nil => return Ok(Value::bool(false)),
            Value::Pair(ref p) => {
                let pair = p.borrow();
                // Check if car is a pair
                if let Value::Pair(ref inner_p) = pair.car {
                    let inner_pair = inner_p.borrow();
                    if obj.equal(&inner_pair.car) {
                        drop(inner_pair);
                        let result = pair.car.clone();
                        drop(pair);
                        return Ok(result);
                    }
                }
                let cdr = pair.cdr.clone();
                drop(pair);
                current = cdr;
            }
            _ => return Err("assoc: not a proper list".to_string()),
        }
    }
}

// =============================================================================
// DSSSL Type Stub Primitives (Document Formatting - Not Needed for Code Gen)
// =============================================================================
//
// These primitives are part of DSSSL but are primarily used for document
// formatting (print/screen output). For code generation templates, they're
// not typically needed, so we implement them as stubs that return dummy values.
//
// If a template actually uses these, they can be properly implemented later.

/// (quantity? obj) → boolean
///
/// Returns #t if obj is a quantity (length/dimension), otherwise #f.
///
/// **DSSSL**: Type predicate (stub - quantities not implemented)
pub fn prim_quantity_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("quantity? requires exactly 1 argument".to_string());
    }
    // Quantities not implemented - always return #f
    Ok(Value::bool(false))
}

/// (color? obj) → boolean
///
/// Returns #t if obj is a color, otherwise #f.
///
/// **DSSSL**: Type predicate (stub - colors not implemented)
pub fn prim_color_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("color? requires exactly 1 argument".to_string());
    }
    // Colors not implemented - always return #f
    Ok(Value::bool(false))
}

/// (address? obj) → boolean
///
/// Returns #t if obj is an address, otherwise #f.
///
/// **DSSSL**: Type predicate (stub - addresses not implemented)
pub fn prim_address_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("address? requires exactly 1 argument".to_string());
    }
    // Addresses not implemented - always return #f
    Ok(Value::bool(false))
}

// =============================================================================
// Additional Utility Primitives
// =============================================================================

/// (cadr list) → value
///
/// Equivalent to (car (cdr list)). Returns the second element of a list.
///
/// **R4RS**: Library procedure
pub fn prim_cadr(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("cadr requires exactly 1 argument".to_string());
    }

    // Get cdr
    let cdr = prim_cdr(args)?;
    // Get car of result
    prim_car(&[cdr])
}

/// (caddr list) → value
///
/// Equivalent to (car (cdr (cdr list))). Returns the third element of a list.
///
/// **R4RS**: Library procedure
pub fn prim_caddr(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("caddr requires exactly 1 argument".to_string());
    }

    // Get cdr twice
    let cdr1 = prim_cdr(args)?;
    let cdr2 = prim_cdr(&[cdr1])?;
    // Get car of result
    prim_car(&[cdr2])
}

/// (cadddr list) → value
///
/// Equivalent to (car (cdr (cdr (cdr list)))). Returns the fourth element.
///
/// **R4RS**: Library procedure
pub fn prim_cadddr(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("cadddr requires exactly 1 argument".to_string());
    }

    // Get cdr three times
    let cdr1 = prim_cdr(args)?;
    let cdr2 = prim_cdr(&[cdr1])?;
    let cdr3 = prim_cdr(&[cdr2])?;
    // Get car of result
    prim_car(&[cdr3])
}

/// (caar list) → value
///
/// Equivalent to (car (car list)).
///
/// **R4RS**: Library procedure
pub fn prim_caar(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("caar requires exactly 1 argument".to_string());
    }

    let car = prim_car(args)?;
    prim_car(&[car])
}

/// (cddr list) → value
///
/// Equivalent to (cdr (cdr list)).
///
/// **R4RS**: Library procedure
pub fn prim_cddr(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("cddr requires exactly 1 argument".to_string());
    }

    let cdr1 = prim_cdr(args)?;
    prim_cdr(&[cdr1])
}

/// (cdar list) → value
///
/// Equivalent to (cdr (car list)).
///
/// **R4RS**: Library procedure
pub fn prim_cdar(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("cdar requires exactly 1 argument".to_string());
    }

    let car = prim_car(args)?;
    prim_cdr(&[car])
}

/// (caaar list) → value
///
/// Equivalent to (car (car (car list))).
///
/// **R4RS**: Library procedure
pub fn prim_caaar(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("caaar requires exactly 1 argument".to_string());
    }

    let car1 = prim_car(args)?;
    let car2 = prim_car(&[car1])?;
    prim_car(&[car2])
}

/// (cdaar list) → value
///
/// Equivalent to (cdr (car (car list))).
///
/// **R4RS**: Library procedure
pub fn prim_cdaar(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("cdaar requires exactly 1 argument".to_string());
    }

    let car1 = prim_car(args)?;
    let car2 = prim_car(&[car1])?;
    prim_cdr(&[car2])
}

/// (cadar list) → value
///
/// Equivalent to (car (cdr (car list))).
///
/// **R4RS**: Library procedure
pub fn prim_cadar(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("cadar requires exactly 1 argument".to_string());
    }

    let car = prim_car(args)?;
    let cdr = prim_cdr(&[car])?;
    prim_car(&[cdr])
}

/// (cddar list) → value
///
/// Equivalent to (cdr (cdr (car list))).
///
/// **R4RS**: Library procedure
pub fn prim_cddar(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("cddar requires exactly 1 argument".to_string());
    }

    let car = prim_car(args)?;
    let cdr1 = prim_cdr(&[car])?;
    prim_cdr(&[cdr1])
}

/// (caadr list) → value
///
/// Equivalent to (car (car (cdr list))).
///
/// **R4RS**: Library procedure
pub fn prim_caadr(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("caadr requires exactly 1 argument".to_string());
    }

    let cdr = prim_cdr(args)?;
    let car = prim_car(&[cdr])?;
    prim_car(&[car])
}

/// (cdadr list) → value
///
/// Equivalent to (cdr (car (cdr list))).
///
/// **R4RS**: Library procedure
pub fn prim_cdadr(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("cdadr requires exactly 1 argument".to_string());
    }

    let cdr = prim_cdr(args)?;
    let car = prim_car(&[cdr])?;
    prim_cdr(&[car])
}

// =============================================================================
// Additional List Utilities
// =============================================================================

/// (last pair) → value
///
/// Returns the last element of a list (or the last cdr for improper lists).
///
/// **Extension**: Useful utility
pub fn prim_last(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("last requires exactly 1 argument".to_string());
    }

    let mut current = args[0].clone();
    loop {
        match current {
            Value::Nil => return Err("last: empty list".to_string()),
            Value::Pair(ref p) => {
                let pair = p.borrow();
                match &pair.cdr {
                    Value::Nil => {
                        // This is the last pair, return its car
                        return Ok(pair.car.clone());
                    }
                    Value::Pair(_) => {
                        // Continue to next pair
                        let next = pair.cdr.clone();
                        drop(pair);
                        current = next;
                    }
                    _other => {
                        // Improper list - return the last element before the improper cdr
                        return Ok(pair.car.clone());
                    }
                }
            }
            _ => return Err(format!("last: not a list: {:?}", args[0])),
        }
    }
}

/// (zero? n) → boolean (alias check)
///
/// Returns #t if n is zero. Already implemented, but commonly used.
///
/// This is already in number primitives, just documenting it here.

/// (null? obj) → boolean (alias check)
///
/// Returns #t if obj is the empty list. Already implemented.
///
/// This is already in list primitives, just documenting it here.

// =============================================================================
// Format/Number Formatting Primitives (DSSSL)
// =============================================================================

/// (format-number n format) → string
///
/// Formats a number according to format string.
/// Simplified implementation for basic number formatting.
///
/// **DSSSL**: Processing primitive
pub fn prim_format_number(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("format-number requires exactly 2 arguments".to_string());
    }

    let n = match &args[0] {
        Value::Integer(i) => *i,
        Value::Real(r) => *r as i64,
        _ => return Err(format!("format-number: not a number: {:?}", args[0])),
    };

    let format = match &args[1] {
        Value::String(s) => s.as_str(),
        Value::Symbol(s) => s.as_ref(),
        _ => return Err(format!("format-number: invalid format: {:?}", args[1])),
    };

    // Simple format implementation
    let result = match format {
        "1" | "decimal" => n.to_string(),
        "I" | "roman-upper" => {
            // Simple Roman numeral conversion (up to 20 for simplicity)
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
                _ => return Ok(Value::string(n.to_string())),
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
                _ => return Ok(Value::string(n.to_string())),
            };
            roman_str.to_string()
        }
        "a" | "alpha-lower" => {
            // Convert to lowercase letter (1=a, 2=b, etc.)
            if n >= 1 && n <= 26 {
                ((b'a' + (n as u8 - 1)) as char).to_string()
            } else {
                n.to_string()
            }
        }
        "A" | "alpha-upper" => {
            // Convert to uppercase letter (1=A, 2=B, etc.)
            if n >= 1 && n <= 26 {
                ((b'A' + (n as u8 - 1)) as char).to_string()
            } else {
                n.to_string()
            }
        }
        _ => n.to_string(), // Default to decimal
    };

    Ok(Value::string(result))
}

/// (format-number-list numlist format) → string
///
/// Formats a list of numbers as a compound number (e.g., "1.2.3").
///
/// **DSSSL**: Processing primitive
pub fn prim_format_number_list(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() || args.len() > 2 {
        return Err("format-number-list requires 1 or 2 arguments".to_string());
    }

    let separator = if args.len() == 2 {
        match &args[1] {
            Value::String(s) => s.as_str(),
            Value::Symbol(s) => s.as_ref(),
            _ => ".",
        }
    } else {
        "."
    };

    let mut numbers = Vec::new();
    let mut current = args[0].clone();

    loop {
        match current {
            Value::Nil => break,
            Value::Pair(ref p) => {
                let pair = p.borrow();
                match &pair.car {
                    Value::Integer(n) => numbers.push(n.to_string()),
                    Value::Real(r) => numbers.push((*r as i64).to_string()),
                    _ => {
                        return Err("format-number-list: list must contain only numbers".to_string())
                    }
                }
                let cdr = pair.cdr.clone();
                drop(pair);
                current = cdr;
            }
            _ => return Err("format-number-list: not a proper list".to_string()),
        }
    }

    Ok(Value::string(numbers.join(separator)))
}

// =============================================================================
// Grove Query Primitives (DSSSL)
// =============================================================================
//
// These primitives provide XML tree navigation and querying.
// They form the core of DSSSL's grove model for document processing.
//
// **Implementation Status**: Basic API defined. Full grove integration
// with libxml2 will connect these to actual XML documents.

/// (current-node) → node
///
/// Returns the current node being processed.
///
/// In DSSSL, the current node is an implicit context variable that changes
/// as the evaluator processes the document tree. When processing children
/// or a node-list, the current-node changes to each node in turn.
///
/// **DSSSL**: Grove primitive (context-dependent)
pub fn prim_current_node(args: &[Value]) -> PrimitiveResult {
    if !args.is_empty() {
        return Err("current-node requires no arguments".to_string());
    }

    // Get the current evaluator context
    let ctx = crate::scheme::evaluator::get_evaluator_context()
        .ok_or_else(|| "current-node: no evaluator context available".to_string())?;

    // Get the current node from the context
    let node = ctx
        .current_node
        .ok_or_else(|| "current-node: no current node set".to_string())?;

    Ok(Value::Node(node))
}

/// (node-list? obj) → boolean
///
/// Returns #t if obj is a node-list.
///
/// **DSSSL**: Grove primitive
pub fn prim_node_list_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("node-list? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::NodeList(_))))
}

/// (empty-node-list) → node-list
///
/// Returns an empty node-list.
///
/// **DSSSL**: Grove primitive
pub fn prim_empty_node_list(args: &[Value]) -> PrimitiveResult {
    if !args.is_empty() {
        return Err("empty-node-list requires no arguments".to_string());
    }

    Ok(Value::node_list(Box::new(EmptyNodeList::new())))
}

/// (node-list-empty? nl) → boolean
///
/// Returns #t if the node-list is empty.
///
/// **DSSSL**: Grove primitive
pub fn prim_node_list_empty_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("node-list-empty? requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::NodeList(nl) => {
            // Now implemented with real grove support
            Ok(Value::bool(nl.is_empty()))
        }
        _ => Err(format!("node-list-empty?: not a node-list: {:?}", args[0])),
    }
}

/// (node-list-length nl) → integer
///
/// Returns the number of nodes in the node-list.
///
/// **DSSSL**: Grove primitive
pub fn prim_node_list_length(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("node-list-length requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::NodeList(nl) => {
            // Now implemented with real grove support
            Ok(Value::integer(nl.length() as i64))
        }
        _ => Err(format!("node-list-length: not a node-list: {:?}", args[0])),
    }
}

/// (node-list-first nl) → node
///
/// Returns the first node in a node-list.
/// Returns #f if the node-list is empty.
///
/// **DSSSL**: Grove primitive
pub fn prim_node_list_first(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("node-list-first requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::NodeList(nl) => {
            // Now implemented with real grove support
            if let Some(node) = nl.first() {
                Ok(Value::node(node))
            } else {
                Ok(Value::bool(false))
            }
        }
        Value::Node(_) => {
            // DSSSL: A single node can be treated as a single-element node-list
            // The first element is the node itself
            Ok(args[0].clone())
        }
        _ => Err(format!("node-list-first: not a node-list: {:?}", args[0])),
    }
}

/// (node-list->list nl) → list
///
/// Converts a node-list to a Scheme list of nodes.
/// This is a utility function (not in DSSSL standard).
///
/// **DSSSL**: Extension (utility)
pub fn prim_node_list_to_list(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("node-list->list requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::NodeList(nl) => {
            // Build list by recursively traversing node-list
            fn build_list(nl: &std::rc::Rc<Box<dyn crate::grove::NodeList>>) -> Value {
                if nl.is_empty() {
                    Value::Nil
                } else if let Some(first) = nl.first() {
                    let rest_nl = nl.rest();
                    let rest_list = build_list(&std::rc::Rc::new(rest_nl));
                    Value::cons(Value::node(first), rest_list)
                } else {
                    Value::Nil
                }
            }

            Ok(build_list(nl))
        }
        Value::Nil => {
            // Empty list treated as empty node-list
            Ok(Value::Nil)
        }
        Value::Pair(_) => {
            // Already a regular Scheme list - return as-is
            // This handles the case where node-list-map returns a regular list
            Ok(args[0].clone())
        }
        _ => Err(format!("node-list->list: not a node-list: {:?}", args[0])),
    }
}

/// (node-list-rest nl) → node-list
///
/// Returns a node-list containing all but the first node.
/// Returns an empty node-list if the input is empty or has only one element.
///
/// **DSSSL**: Grove primitive
pub fn prim_node_list_rest(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("node-list-rest requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::NodeList(_nl) => {
            // Now implemented with real grove support
            let rest = _nl.rest();
            Ok(Value::node_list(rest))
        }
        Value::Node(_) => {
            // DSSSL: A single node can be treated as a single-element node-list
            // The rest of a single-element list is an empty node-list
            Ok(Value::node_list(Box::new(EmptyNodeList::new())))
        }
        _ => Err(format!("node-list-rest: not a node-list: {:?}", args[0])),
    }
}

/// (node-list-ref nl index) → node
///
/// Returns the node at the given index in the node-list (0-based).
/// Returns #f if index is out of bounds.
///
/// **DSSSL**: Grove primitive
pub fn prim_node_list_ref(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("node-list-ref requires exactly 2 arguments".to_string());
    }

    match &args[0] {
        Value::NodeList(nl) => {
            // Check index is an integer
            match &args[1] {
                Value::Integer(idx) => {
                    // Now implemented with real grove support
                    if *idx < 0 {
                        return Err("node-list-ref: index must be non-negative".to_string());
                    }
                    if let Some(node) = nl.get(*idx as usize) {
                        Ok(Value::node(node))
                    } else {
                        Ok(Value::bool(false))
                    }
                }
                _ => Err(format!("node-list-ref: index not an integer: {:?}", args[1])),
            }
        }
        _ => Err(format!("node-list-ref: not a node-list: {:?}", args[0])),
    }
}

/// (node-list-reverse nl) → node-list
///
/// Returns a node-list with the nodes in reverse order.
///
/// **DSSSL**: Grove primitive
pub fn prim_node_list_reverse(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("node-list-reverse requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::NodeList(nl) => {
            // Collect all nodes into a Vec and reverse it
            let mut nodes: Vec<Box<dyn Node>> = Vec::new();
            let mut current = nl.clone();
            while let Some(node) = current.first() {
                nodes.push(node);
                current = std::rc::Rc::new(current.rest());
                if current.length() == 0 {
                    break;
                }
            }
            nodes.reverse();

            // Create a new node-list from the reversed Vec
            use crate::grove::VecNodeList;
            Ok(Value::node_list(Box::new(VecNodeList::new(nodes))))
        }
        _ => Err(format!("node-list-reverse: not a node-list: {:?}", args[0])),
    }
}

/// (node-list-remove-duplicates nl) → node-list
///
/// Returns a node-list with duplicate nodes removed.
/// Node identity is based on the underlying document node, not object equality.
///
/// **DSSSL**: Grove primitive
pub fn prim_node_list_remove_duplicates(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("node-list-remove-duplicates requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::NodeList(nl) => {
            let mut unique_nodes = Vec::new();

            // Iterate through the node-list and collect unique nodes
            let mut index = 0;
            loop {
                if let Some(node) = nl.get(index) {
                    // Check if this node is already in unique_nodes
                    let is_duplicate = unique_nodes.iter().any(|existing: &Box<dyn crate::grove::Node>| {
                        existing.node_eq(node.as_ref())
                    });

                    if !is_duplicate {
                        unique_nodes.push(node);
                    }
                    index += 1;
                } else {
                    break;
                }
            }

            Ok(Value::node_list(Box::new(crate::grove::VecNodeList::new(unique_nodes))))
        }
        _ => Err(format!("node-list-remove-duplicates: not a node-list: {:?}", args[0])),
    }
}

/// (node-list-count nl) → integer
///
/// Returns the number of unique nodes in the node-list.
/// Equivalent to (node-list-length (node-list-remove-duplicates nl))
///
/// **DSSSL**: Grove primitive (defined in DSSSL spec)
pub fn prim_node_list_count(args: &[Value]) -> PrimitiveResult {
    // Implementation: node-list-length(node-list-remove-duplicates(nl))
    let unique_nl = prim_node_list_remove_duplicates(args)?;
    prim_node_list_length(&[unique_nl])
}

/// (node-list-contains? node-list node) → boolean
///
/// Returns #t if the node is in the node-list, #f otherwise.
/// Uses node equality (same identity) for comparison.
///
/// **DSSSL**: Grove primitive (extension)
pub fn prim_node_list_contains_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("node-list-contains? requires exactly 2 arguments".to_string());
    }

    let search_node = match &args[1] {
        Value::Node(n) => n,
        _ => return Err(format!("node-list-contains?: second argument not a node: {:?}", args[1])),
    };

    match &args[0] {
        Value::NodeList(nl) => {
            // Iterate through the node-list checking for equality
            let mut index = 0;
            loop {
                if let Some(node) = nl.get(index) {
                    // Check if nodes are equal using node_eq
                    if node.node_eq(search_node.as_ref().as_ref()) {
                        return Ok(Value::bool(true));
                    }
                    index += 1;
                } else {
                    break;
                }
            }

            // Not found
            Ok(Value::bool(false))
        }
        Value::Nil => {
            // Empty list treated as empty node-list
            Ok(Value::bool(false))
        }
        Value::Pair(_) => {
            // Regular Scheme list - iterate through it
            let mut current = args[0].clone();
            loop {
                match current {
                    Value::Nil => return Ok(Value::bool(false)),
                    Value::Pair(ref p) => {
                        let (car, cdr) = {
                            let pair_data = p.borrow();
                            (pair_data.car.clone(), pair_data.cdr.clone())
                        };

                        // Check if car is the node we're looking for
                        if let Value::Node(ref n) = car {
                            if n.as_ref().node_eq(search_node.as_ref().as_ref()) {
                                return Ok(Value::bool(true));
                            }
                        }
                        // Continue with cdr
                        current = cdr;
                    }
                    _ => return Err(format!("node-list-contains?: malformed list: {:?}", current)),
                }
            }
        }
        _ => Err(format!("node-list-contains?: first argument not a node-list: {:?}", args[0])),
    }
}

/// (node? obj) → boolean
///
/// Returns #t if obj is a node.
///
/// **DSSSL**: Grove primitive
pub fn prim_node_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("node? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::Node(_))))
}

/// (gi node) → string | #f
///
/// Returns the generic identifier (element name) of a node.
/// Returns #f if the node is not an element node.
///
/// **DSSSL**: Grove primitive
pub fn prim_gi(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("gi requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Node(node) => {
            if let Some(gi) = node.gi() {
                Ok(Value::string(gi))
            } else {
                Ok(Value::bool(false))
            }
        }
        Value::NodeList(nl) => {
            // Handle single-element node-lists (OpenJade compatibility)
            if nl.length() == 1 {
                if let Some(node) = nl.first() {
                    if let Some(gi) = node.gi() {
                        Ok(Value::string(gi))
                    } else {
                        Ok(Value::bool(false))
                    }
                } else {
                    Ok(Value::bool(false))
                }
            } else {
                Err(format!("gi: node-list must have exactly 1 element, got {}", nl.length()))
            }
        }
        Value::Bool(false) => Ok(Value::bool(false)), // #f → #f (graceful handling)
        _ => Err(format!("gi: not a node: {:?}", args[0])),
    }
}

/// (data node) → string | #f
///
/// Returns the text content of a node.
/// For text nodes, returns the text. For elements, returns concatenated descendant text.
/// Returns #f if the node has no data.
///
/// **DSSSL**: Grove primitive
pub fn prim_data(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("data requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Node(node) => {
            if let Some(data) = node.data() {
                // OpenJade normalizes whitespace: replace sequences of whitespace
                // (spaces, tabs, newlines) with a single space, and trim
                let normalized = data
                    .split_whitespace()
                    .collect::<Vec<_>>()
                    .join(" ");
                if normalized.is_empty() {
                    Ok(Value::bool(false))
                } else {
                    Ok(Value::string(normalized))
                }
            } else {
                Ok(Value::bool(false))
            }
        }
        Value::NodeList(nl) => {
            // DSSSL: node property functions can be called on node-lists
            // OpenJade behavior: concatenates data from ALL nodes in the list
            // and normalizes whitespace (collapses sequences to single space, trims)
            let mut result = String::new();
            let mut current = nl.clone();

            loop {
                if let Some(node) = current.first() {
                    if let Some(data) = node.data() {
                        result.push_str(&data);
                    }
                    current = std::rc::Rc::new(current.rest());
                    if current.length() == 0 {
                        break;
                    }
                } else {
                    break;
                }
            }

            if result.is_empty() {
                Ok(Value::bool(false))
            } else {
                // OpenJade normalizes whitespace: replace sequences of whitespace
                // (spaces, tabs, newlines) with a single space, and trim
                let normalized = result
                    .split_whitespace()
                    .collect::<Vec<_>>()
                    .join(" ");
                Ok(Value::string(normalized))
            }
        }
        Value::Bool(false) => Ok(Value::bool(false)), // #f → #f (graceful handling)
        _ => Err(format!("data: not a node: {:?}", args[0])),
    }
}

/// (attribute-string name node) → string | #f
///
/// Returns the value of the attribute with the given name.
/// Returns #f if the attribute does not exist or if node is #f.
/// Includes DTD default values.
///
/// **DSSSL**: Grove primitive
/// **OpenJade**: Gracefully handles #f nodes (returns #f)
pub fn prim_attribute_string(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("attribute-string requires exactly 2 arguments".to_string());
    }

    let name = match &args[0] {
        Value::String(s) => s.as_str(),
        _ => return Err(format!("attribute-string: name not a string: {:?}", args[0])),
    };

    match &args[1] {
        Value::Node(node) => {
            if let Some(value) = node.attribute_string(name) {
                Ok(Value::string(value))
            } else {
                Ok(Value::bool(false))
            }
        }
        Value::NodeList(nl) => {
            // Handle single-element node-lists (OpenJade compatibility)
            // Empty node-lists return #f (no node to get attribute from)
            if nl.length() == 0 {
                Ok(Value::bool(false))
            } else if nl.length() == 1 {
                if let Some(node) = nl.first() {
                    if let Some(value) = node.attribute_string(name) {
                        Ok(Value::string(value))
                    } else {
                        Ok(Value::bool(false))
                    }
                } else {
                    Ok(Value::bool(false))
                }
            } else {
                Err(format!("attribute-string: node-list must have exactly 1 element, got {}", nl.length()))
            }
        }
        Value::Bool(false) => Ok(Value::bool(false)), // #f → #f (graceful handling)
        _ => Err(format!("attribute-string: not a node: {:?}", args[1])),
    }
}

/// (children node) → node-list
///
/// Returns the child nodes of a node.
/// In DSSSL, this returns only element children, not text nodes.
///
/// **DSSSL**: Grove primitive
pub fn prim_children(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("children requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Node(node) => {
            let children = node.children();
            Ok(Value::node_list(children))
        }
        Value::NodeList(nl) => {
            // DSSSL: node property functions can be called on node-lists
            // Operates on the first node of the list
            if let Some(node) = nl.first() {
                let children = node.children();
                Ok(Value::node_list(children))
            } else {
                // Empty node-list -> empty children
                Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new())))
            }
        }
        Value::Bool(false) | Value::Unspecified => {
            // Graceful handling: treat #f and unspecified as having no children
            Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new())))
        }
        _ => Err(format!("children: not a node or node-list: {:?}", args[0])),
    }
}

/// (select-children gi-string node) → node-list
///
/// Returns all child elements of node whose gi matches gi-string.
/// This is a filtered version of children that only returns elements with a specific tag name.
///
/// **DSSSL**: Grove primitive
pub fn prim_select_children(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("select-children requires exactly 2 arguments".to_string());
    }

    let gi_name = match &args[0] {
        Value::String(s) => s.as_str(),
        _ => return Err(format!("select-children: first argument not a string: {:?}", args[0])),
    };

    match &args[1] {
        Value::Node(node) => {
            // Get all children
            let children = node.children();

            // Filter by gi
            let mut matching = Vec::new();
            let mut current = children;
            while !current.is_empty() {
                if let Some(child) = current.first() {
                    if let Some(child_gi) = child.gi() {
                        if child_gi == gi_name {
                            matching.push(child);
                        }
                    }
                    current = current.rest();
                } else {
                    break;
                }
            }

            Ok(Value::node_list(Box::new(crate::grove::VecNodeList::new(matching))))
        }
        Value::NodeList(nl) => {
            // DSSSL: node property functions can be called on node-lists
            // Operates on the first node of the list
            if let Some(node) = nl.first() {
                // Recurse with the first node
                prim_select_children(&[args[0].clone(), Value::node(node)])
            } else {
                // Empty node-list -> empty result
                Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new())))
            }
        }
        Value::Bool(false) => {
            // #f → empty node-list (graceful handling)
            Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new())))
        }
        _ => Err(format!("select-children: second argument not a node or node-list: {:?}", args[1])),
    }
}

/// (parent node) → node | #f
///
/// Returns the parent node of a node.
/// Returns #f if the node has no parent (i.e., it's the root).
///
/// **DSSSL**: Grove primitive
pub fn prim_parent(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("parent requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Node(node) => {
            if let Some(parent) = node.parent() {
                Ok(Value::node(parent))
            } else {
                Ok(Value::bool(false))
            }
        }
        Value::Bool(false) => Ok(Value::bool(false)), // #f → #f (graceful handling)
        _ => Err(format!("parent: not a node: {:?}", args[0])),
    }
}

/// (id node) → string | #f
///
/// Returns the ID attribute value of a node.
/// Returns #f if the node has no ID.
///
/// **DSSSL**: Grove primitive
pub fn prim_id(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("id requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Node(node) => {
            if let Some(id) = node.id() {
                Ok(Value::string(id))
            } else {
                Ok(Value::bool(false))
            }
        }
        _ => Err(format!("id: not a node: {:?}", args[0])),
    }
}

/// (ancestor gi node) → node | #f
///
/// Returns the nearest ancestor element with the given generic identifier.
/// Walks up the parent chain from the starting node until a matching ancestor is found.
/// Returns #f if no such ancestor exists.
///
/// **DSSSL**: Grove primitive
pub fn prim_ancestor(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("ancestor requires exactly 2 arguments".to_string());
    }

    // First argument is the gi name
    let gi_name = match &args[0] {
        Value::String(s) => s.clone(),
        _ => return Err(format!("ancestor: first argument not a string: {:?}", args[0])),
    };

    // Second argument is the starting node
    let starting_node: Box<dyn crate::grove::Node> = match &args[1] {
        Value::Node(n) => n.clone_node(),
        Value::NodeList(nl) => {
            // DSSSL: node property functions can operate on node-lists (first node)
            if let Some(n) = nl.first() {
                n
            } else {
                return Ok(Value::bool(false)); // Empty node-list -> #f
            }
        }
        Value::Bool(false) => return Ok(Value::bool(false)), // #f → #f (graceful handling)
        _ => return Err(format!("ancestor: second argument not a node or node-list: {:?}", args[1])),
    };

    // Walk up the parent chain looking for an ancestor with matching gi
    let mut current = starting_node.parent();
    while let Some(parent_node) = current {
        if let Some(parent_gi) = parent_node.gi() {
            if parent_gi == gi_name.as_str() {
                return Ok(Value::node(parent_node));
            }
        }
        current = parent_node.parent();
    }

    // No matching ancestor found
    Ok(Value::bool(false))
}

/// (descendants node) → node-list
///
/// Returns all descendant nodes of the given node in document order.
/// In DSSSL, this returns only element descendants, not text nodes.
///
/// **DSSSL**: Grove primitive
pub fn prim_descendants(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("descendants requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Node(node) => {
            let mut descendants = Vec::new();
            // node is &Rc<Box<dyn Node>>
            // node.as_ref() gives &Box<dyn Node>
            // node.as_ref().as_ref() gives &dyn Node
            collect_descendants(node.as_ref().as_ref(), &mut descendants);
            Ok(Value::node_list(Box::new(crate::grove::VecNodeList::new(descendants))))
        }
        Value::NodeList(nl) => {
            // DSSSL: operate on first node of node-list
            if let Some(node) = nl.first() {
                let mut descendants = Vec::new();
                // node is Box<dyn Node>, so &*node gives &dyn Node
                collect_descendants(&*node, &mut descendants);
                Ok(Value::node_list(Box::new(crate::grove::VecNodeList::new(descendants))))
            } else {
                Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new())))
            }
        }
        _ => Err(format!("descendants: not a node or node-list: {:?}", args[0])),
    }
}

/// Helper function to recursively collect all descendant elements
fn collect_descendants(node: &dyn crate::grove::Node, result: &mut Vec<Box<dyn crate::grove::Node>>) {
    let children = node.children();
    let len = children.length();

    for i in 0..len {
        if let Some(child) = children.get(i) {
            // Add this child to results
            result.push(child.clone_node());

            // Recursively collect descendants of this child
            // child is Box<dyn Node>, so &*child gives us &dyn Node
            collect_descendants(&*child, result);
        }
    }
}

/// (follow node) → node-list
///
/// Returns all following sibling nodes.
///
/// **DSSSL**: Grove primitive (stub)
pub fn prim_follow(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("follow requires exactly 1 argument".to_string());
    }

    // TODO: Implement following siblings traversal
    // For now, return empty node-list (stub)
    Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new())))
}

/// (preced node) → node-list
///
/// Returns all preceding sibling nodes.
///
/// **DSSSL**: Grove primitive (stub)
pub fn prim_preced(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("preced requires exactly 1 argument".to_string());
    }

    // TODO: Implement preceding siblings traversal
    // For now, return empty node-list (stub)
    Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new())))
}

/// (attributes node) → node-list
///
/// Returns the attributes of the given node as a node-list.
///
/// **DSSSL**: Grove primitive (stub)
pub fn prim_attributes(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("attributes requires exactly 1 argument".to_string());
    }

    // TODO: Implement attribute node-list
    // For now, return empty node-list (stub)
    Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new())))
}

/// (select-elements node-list gi) → node-list
///
/// Returns a node-list containing only elements with the given gi.
///
/// **DSSSL**: Grove primitive (stub)
pub fn prim_select_elements(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("select-elements requires exactly 2 arguments".to_string());
    }

    let node_list = match &args[0] {
        Value::NodeList(nl) => nl,
        _ => return Err(format!("select-elements: first argument not a node-list: {:?}", args[0])),
    };

    let gi_name = match &args[1] {
        Value::String(s) => s.as_str(),
        _ => return Err(format!("select-elements: second argument not a string: {:?}", args[1])),
    };

    // Filter the node-list to only include elements with the specified gi
    let mut result_nodes = Vec::new();

    // Iterate through the node list and collect matching nodes
    let mut index = 0;
    loop {
        if let Some(node) = node_list.get(index) {
            // Check if this node has the matching gi
            if let Some(node_gi) = node.gi() {
                if node_gi == gi_name {
                    result_nodes.push(node);
                }
            }
            index += 1;
        } else {
            break;
        }
    }

    // Return filtered node-list
    Ok(Value::node_list(Box::new(crate::grove::VecNodeList::new(result_nodes))))
}

/// (element-with-id id) → node | #f
///
/// Returns the element with the given ID attribute.
/// Uses the DTD to determine which attributes are of type ID.
/// Returns #f if no element has that ID.
///
/// **DSSSL**: Grove primitive
pub fn prim_element_with_id(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() || args.len() > 2 {
        return Err("element-with-id requires 1 or 2 arguments".to_string());
    }

    // First argument is the ID string
    let id = match &args[0] {
        Value::String(s) => s.as_str(),
        Value::Bool(false) => return Ok(Value::bool(false)), // #f → #f (graceful handling)
        _ => return Err(format!("element-with-id: first argument not a string: {:?}", args[0])),
    };

    // Get the grove from evaluator context
    let ctx = crate::scheme::evaluator::get_evaluator_context()
        .ok_or_else(|| "element-with-id: no evaluator context available".to_string())?;

    let grove = ctx.grove
        .as_ref()
        .ok_or_else(|| "element-with-id: no grove available".to_string())?;

    // Use the grove's element_with_id method
    match grove.element_with_id(id) {
        Some(node) => Ok(Value::node(node)),
        None => Ok(Value::bool(false)),
    }
}

// =============================================================================
// Additional Math Primitives (R4RS/R5RS)
// =============================================================================

/// (expt base exponent) → number
///
/// Returns base raised to the power of exponent.
///
/// **R4RS**: Math primitive
pub fn prim_expt(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("expt requires exactly 2 arguments".to_string());
    }

    let base = match &args[0] {
        Value::Integer(n) => *n as f64,
        Value::Real(r) => *r,
        _ => return Err(format!("expt: not a number: {:?}", args[0])),
    };

    let exponent = match &args[1] {
        Value::Integer(n) => *n as f64,
        Value::Real(r) => *r,
        _ => return Err(format!("expt: not a number: {:?}", args[1])),
    };

    let result = base.powf(exponent);

    // Return integer if both inputs were integers and result is whole
    if matches!(args[0], Value::Integer(_)) && matches!(args[1], Value::Integer(_)) && result.fract() == 0.0 {
        Ok(Value::integer(result as i64))
    } else {
        Ok(Value::real(result))
    }
}

/// (sqrt n) → number
///
/// Returns the square root of n.
///
/// **R4RS**: Math primitive
pub fn prim_sqrt(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("sqrt requires exactly 1 argument".to_string());
    }

    let n = match &args[0] {
        Value::Integer(i) => *i as f64,
        Value::Real(r) => *r,
        _ => return Err(format!("sqrt: not a number: {:?}", args[0])),
    };

    if n < 0.0 {
        return Err("sqrt: negative argument".to_string());
    }

    Ok(Value::real(n.sqrt()))
}

/// (sin n) → number
///
/// Returns the sine of n (in radians).
///
/// **R5RS**: Math primitive
pub fn prim_sin(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("sin requires exactly 1 argument".to_string());
    }

    let n = match &args[0] {
        Value::Integer(i) => *i as f64,
        Value::Real(r) => *r,
        _ => return Err(format!("sin: not a number: {:?}", args[0])),
    };

    Ok(Value::real(n.sin()))
}

/// (cos n) → number
///
/// Returns the cosine of n (in radians).
///
/// **R5RS**: Math primitive
pub fn prim_cos(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("cos requires exactly 1 argument".to_string());
    }

    let n = match &args[0] {
        Value::Integer(i) => *i as f64,
        Value::Real(r) => *r,
        _ => return Err(format!("cos: not a number: {:?}", args[0])),
    };

    Ok(Value::real(n.cos()))
}

/// (tan n) → number
///
/// Returns the tangent of n (in radians).
///
/// **R5RS**: Math primitive
pub fn prim_tan(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("tan requires exactly 1 argument".to_string());
    }

    let n = match &args[0] {
        Value::Integer(i) => *i as f64,
        Value::Real(r) => *r,
        _ => return Err(format!("tan: not a number: {:?}", args[0])),
    };

    Ok(Value::real(n.tan()))
}

/// (atan n) → number
///
/// Returns the arctangent of n (in radians).
///
/// **R5RS**: Math primitive
pub fn prim_atan(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("atan requires exactly 1 argument".to_string());
    }

    let n = match &args[0] {
        Value::Integer(i) => *i as f64,
        Value::Real(r) => *r,
        _ => return Err(format!("atan: not a number: {:?}", args[0])),
    };

    Ok(Value::real(n.atan()))
}

/// (log n) → number
///
/// Returns the natural logarithm of n.
///
/// **R5RS**: Math primitive
pub fn prim_log(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("log requires exactly 1 argument".to_string());
    }

    let n = match &args[0] {
        Value::Integer(i) => *i as f64,
        Value::Real(r) => *r,
        _ => return Err(format!("log: not a number: {:?}", args[0])),
    };

    if n <= 0.0 {
        return Err("log: argument must be positive".to_string());
    }

    Ok(Value::real(n.ln()))
}

/// (exp n) → number
///
/// Returns e raised to the power of n.
///
/// **R5RS**: Math primitive
pub fn prim_exp(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("exp requires exactly 1 argument".to_string());
    }

    let n = match &args[0] {
        Value::Integer(i) => *i as f64,
        Value::Real(r) => *r,
        _ => return Err(format!("exp: not a number: {:?}", args[0])),
    };

    Ok(Value::real(n.exp()))
}

/// (asin n) → number
///
/// Returns the arcsine of n (in radians).
///
/// **R5RS**: Math primitive
pub fn prim_asin(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("asin requires exactly 1 argument".to_string());
    }

    let n = match &args[0] {
        Value::Integer(i) => *i as f64,
        Value::Real(r) => *r,
        _ => return Err(format!("asin: not a number: {:?}", args[0])),
    };

    if n < -1.0 || n > 1.0 {
        return Err("asin: argument must be in range [-1, 1]".to_string());
    }

    Ok(Value::real(n.asin()))
}

/// (acos n) → number
///
/// Returns the arccosine of n (in radians).
///
/// **R5RS**: Math primitive
pub fn prim_acos(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("acos requires exactly 1 argument".to_string());
    }

    let n = match &args[0] {
        Value::Integer(i) => *i as f64,
        Value::Real(r) => *r,
        _ => return Err(format!("acos: not a number: {:?}", args[0])),
    };

    if n < -1.0 || n > 1.0 {
        return Err("acos: argument must be in range [-1, 1]".to_string());
    }

    Ok(Value::real(n.acos()))
}

/// (exact? n) → boolean
///
/// Returns #t if n is exact (integer), #f otherwise.
///
/// **R5RS**: Number predicate
pub fn prim_exact_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("exact? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::Integer(_))))
}

/// (inexact? n) → boolean
///
/// Returns #t if n is inexact (real), #f otherwise.
///
/// **R5RS**: Number predicate
pub fn prim_inexact_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("inexact? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::Real(_))))
}

/// (exact->inexact n) → number
///
/// Converts an exact number to inexact.
///
/// **R5RS**: Number conversion
pub fn prim_exact_to_inexact(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("exact->inexact requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Integer(i) => Ok(Value::real(*i as f64)),
        Value::Real(r) => Ok(Value::real(*r)), // Already inexact
        _ => Err(format!("exact->inexact: not a number: {:?}", args[0])),
    }
}

/// (inexact->exact n) → number
///
/// Converts an inexact number to exact.
///
/// **R5RS**: Number conversion
pub fn prim_inexact_to_exact(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("inexact->exact requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Real(r) => {
            if r.fract() == 0.0 && r.is_finite() {
                Ok(Value::integer(*r as i64))
            } else {
                Err("inexact->exact: cannot convert non-integer to exact".to_string())
            }
        }
        Value::Integer(i) => Ok(Value::integer(*i)), // Already exact
        _ => Err(format!("inexact->exact: not a number: {:?}", args[0])),
    }
}

// =============================================================================
// Entity and Notation Primitives (DSSSL) - Stubs
// =============================================================================

/// (entity-system-id name) → string | #f
pub fn prim_entity_system_id(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (entity-public-id name) → string | #f
pub fn prim_entity_public_id(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (notation-system-id name) → string | #f
pub fn prim_notation_system_id(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (notation-public-id name) → string | #f
pub fn prim_notation_public_id(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

// =============================================================================
// DSSSL Type Stubs (Quantities, Colors, Addresses, Glyphs, Spacing)
// =============================================================================

/// (color color-space ...) → color
pub fn prim_color(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub - return placeholder color
}

/// (color-space name) → color-space
pub fn prim_color_space(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (color-space? obj) → boolean
pub fn prim_color_space_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("color-space? requires exactly 1 argument".to_string());
    }
    Ok(Value::bool(false)) // Stub - no color-space type yet
}

/// (display-space ...) → display-space
pub fn prim_display_space(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (display-space? obj) → boolean
pub fn prim_display_space_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("display-space? requires exactly 1 argument".to_string());
    }
    Ok(Value::bool(false)) // Stub
}

/// (inline-space ...) → inline-space
pub fn prim_inline_space(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (inline-space? obj) → boolean
pub fn prim_inline_space_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("inline-space? requires exactly 1 argument".to_string());
    }
    Ok(Value::bool(false)) // Stub
}

/// (glyph-id name) → glyph-id
pub fn prim_glyph_id(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (glyph-id? obj) → boolean
pub fn prim_glyph_id_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("glyph-id? requires exactly 1 argument".to_string());
    }
    Ok(Value::bool(false)) // Stub
}

/// (glyph-subst-table name) → glyph-subst-table
pub fn prim_glyph_subst_table(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (glyph-subst-table? obj) → boolean
pub fn prim_glyph_subst_table_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("glyph-subst-table? requires exactly 1 argument".to_string());
    }
    Ok(Value::bool(false)) // Stub
}

/// (glyph-subst table glyph) → glyph
pub fn prim_glyph_subst(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (current-node-address) → address
pub fn prim_current_node_address(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (address-local? addr) → boolean
pub fn prim_address_local_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("address-local? requires exactly 1 argument".to_string());
    }
    Ok(Value::bool(false)) // Stub
}

/// (address-visited? addr) → boolean
pub fn prim_address_visited_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("address-visited? requires exactly 1 argument".to_string());
    }
    Ok(Value::bool(false)) // Stub
}

// =============================================================================
// Node-list Utility Stubs
// =============================================================================

/// (node-list node ...) → node-list
pub fn prim_node_list(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new()))) // Stub
}

/// (node-list-map proc node-list) → list
///
/// Applies proc to each node in node-list and returns a list of the results.
/// Unlike map, this returns a regular list, not a node-list.
///
/// **DSSSL**: Grove primitive
/// **NOTE**: Implemented as a special form in the evaluator
pub fn prim_node_list_map(_args: &[Value]) -> PrimitiveResult {
    Err("node-list-map should be handled as a special form in the evaluator".to_string())
}

/// (node-property prop-name node) → value
pub fn prim_node_property(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (match-element? pattern node) → boolean
pub fn prim_match_element_p(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (named-node-list? obj) → boolean
pub fn prim_named_node_list_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("named-node-list? requires exactly 1 argument".to_string());
    }
    Ok(Value::bool(false)) // Stub
}

/// (node-list=? nl1 nl2) → boolean
///
/// Returns #t if nl1 and nl2 contain the same nodes in the same order.
/// A single node is treated as a singleton node-list.
/// Comparison is by node identity (pointer equality).
///
/// **DSSSL**: Grove primitive
pub fn prim_node_list_eq(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("node-list=? requires exactly 2 arguments".to_string());
    }

    // Handle all combinations of Node and NodeList
    match (&args[0], &args[1]) {
        // Two single nodes: compare by node identity
        (Value::Node(n1), Value::Node(n2)) => {
            Ok(Value::bool(n1.as_ref().as_ref().node_eq(n2.as_ref().as_ref())))
        }

        // Single node vs node-list: node-list must have length 1 and contain same node
        (Value::Node(n), Value::NodeList(nl)) | (Value::NodeList(nl), Value::Node(n)) => {
            if nl.length() != 1 {
                return Ok(Value::bool(false));
            }
            if let Some(nl_node) = nl.get(0) {
                // Compare by checking if the nodes are the same
                Ok(Value::bool(n.as_ref().as_ref().node_eq(nl_node.as_ref())))
            } else {
                Ok(Value::bool(false))
            }
        }

        // Two node-lists: compare element by element
        (Value::NodeList(nl1), Value::NodeList(nl2)) => {
            let len1 = nl1.length();
            let len2 = nl2.length();

            // Different lengths → not equal
            if len1 != len2 {
                return Ok(Value::bool(false));
            }

            // Compare each node
            for i in 0..len1 {
                let node1 = nl1.get(i);
                let node2 = nl2.get(i);

                match (node1, node2) {
                    (Some(n1), Some(n2)) => {
                        // Compare nodes by identity
                        if !n1.node_eq(n2.as_ref()) {
                            return Ok(Value::bool(false));
                        }
                    }
                    (None, None) => continue,
                    _ => return Ok(Value::bool(false)),
                }
            }

            Ok(Value::bool(true))
        }

        _ => Err(format!("node-list=?: arguments must be nodes or node-lists: {:?}, {:?}", args[0], args[1])),
    }
}

// =============================================================================
// Grove Extended Primitives - Stubs
// =============================================================================

/// (first-sibling? node) → boolean
///
/// Returns #t if the node is the first sibling with the same element name.
/// If no argument is provided, uses current-node.
///
/// **DSSSL**: Grove primitive
pub fn prim_first_sibling_p(args: &[Value]) -> PrimitiveResult {
    if args.len() > 1 {
        return Err("first-sibling? requires 0 or 1 arguments".to_string());
    }

    // Get the node to check
    let node = if args.is_empty() {
        // Use current-node if no argument provided
        let ctx = crate::scheme::evaluator::get_evaluator_context()
            .ok_or_else(|| "first-sibling?: no evaluator context available".to_string())?;
        ctx.current_node
            .ok_or_else(|| "first-sibling?: no current node set".to_string())?
    } else {
        match &args[0] {
            Value::Node(n) => n.clone(),
            Value::NodeList(nl) => {
                // Single-element node-list
                if nl.length() != 1 {
                    return Err("first-sibling?: argument must be a single node".to_string());
                }
                if let Some(n) = nl.first() {
                    std::rc::Rc::new(n)
                } else {
                    return Err("first-sibling?: empty node-list".to_string());
                }
            }
            _ => return Err(format!("first-sibling?: not a node: {:?}", args[0])),
        }
    };

    // Get the node's GI (element name)
    let gi = match node.gi() {
        Some(g) => g,
        None => return Ok(Value::bool(true)), // Non-elements are considered first siblings
    };

    // Get parent to find all siblings
    let parent = match node.parent() {
        Some(p) => p,
        None => return Ok(Value::bool(true)), // Root node is always first sibling
    };

    // Check all siblings before this node
    let siblings = parent.children();
    for i in 0..siblings.length() {
        if let Some(sibling) = siblings.get(i) {
            // If we found ourselves, we're the first sibling with this GI
            if sibling.node_eq(node.as_ref().as_ref()) {
                return Ok(Value::bool(true));
            }

            // If we found an earlier sibling with the same GI, we're not first
            if let Some(sibling_gi) = sibling.gi() {
                if sibling_gi == gi {
                    return Ok(Value::bool(false));
                }
            }
        }
    }

    // Should not reach here, but treat as first sibling
    Ok(Value::bool(true))
}

/// (last-sibling? node) → boolean
///
/// Returns #t if the node is the last sibling among nodes with the same element name.
/// If called with no arguments, uses the current node from the context.
///
/// **DSSSL**: Grove primitive
/// **OpenJade**: Checks if node is last among siblings with same GI
pub fn prim_last_sibling_p(args: &[Value]) -> PrimitiveResult {
    if args.len() > 1 {
        return Err("last-sibling? requires 0 or 1 arguments".to_string());
    }

    let node = if args.is_empty() {
        // Use current node (from context)
        return Err("last-sibling? with no arguments requires current-node context (not yet implemented)".to_string());
    } else {
        match &args[0] {
            Value::Node(n) => n.clone(),
            Value::NodeList(nl) => {
                if let Some(node) = nl.first() {
                    if nl.length() != 1 {
                        return Err(format!("last-sibling?: node-list must have exactly 1 element, got {}", nl.length()));
                    }
                    std::rc::Rc::new(node)
                } else {
                    return Ok(Value::bool(false)); // Empty node-list
                }
            }
            _ => return Err("last-sibling? requires node or singleton node-list".to_string()),
        }
    };

    // Get parent and check if this node is the last child with the same element name
    if let Some(parent) = node.parent() {
        let my_gi = node.gi();

        // Strategy: Find the last sibling with same GI, then check if it's us
        let siblings = parent.children();
        let mut current_nl = siblings;
        let mut last_with_same_gi: Option<Box<dyn crate::grove::Node>> = None;

        // Find the last sibling with matching GI
        loop {
            if let Some(child) = current_nl.first() {
                if child.gi() == my_gi {
                    last_with_same_gi = Some(child);
                }
                current_nl = current_nl.rest();
            } else {
                break;
            }
        }

        // Check if we are that last sibling using node_eq
        if let Some(last) = last_with_same_gi {
            Ok(Value::bool(node.node_eq(last.as_ref())))
        } else {
            Ok(Value::bool(false))
        }
    } else {
        // No parent - can't determine siblings
        Ok(Value::bool(false))
    }
}

/// (child-number node) → integer
///
/// Returns the 1-based position of the node among siblings with the same element name.
/// If called with no arguments, uses the current node.
///
/// **DSSSL**: Grove primitive (DSSSL §9)
/// **OpenJade**: Returns 1-based count (internal childNumber is 0-based, but adds 1)
pub fn prim_child_number(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() || args.len() > 1 {
        return Err("child-number requires 0 or 1 arguments".to_string());
    }

    let node = if args.is_empty() {
        // Use current node (from context - but we don't have context here, so error)
        return Err("child-number with no arguments requires current-node context (not yet implemented)".to_string());
    } else {
        match &args[0] {
            Value::Node(n) => n.clone(),
            Value::NodeList(nl) => {
                if let Some(node) = nl.first() {
                    if nl.length() != 1 {
                        return Err(format!("child-number: node-list must have exactly 1 element, got {}", nl.length()));
                    }
                    std::rc::Rc::new(node)
                } else {
                    return Err("child-number: empty node-list".to_string());
                }
            }
            _ => return Err(format!("child-number: not a node: {:?}", args[0])),
        }
    };

    // Get the element name of this node
    let gi = match node.gi() {
        Some(name) => name.to_string(),
        None => return Ok(Value::bool(false)), // Not an element
    };

    // Get the parent to find siblings
    let parent = match node.parent() {
        Some(p) => p,
        None => return Ok(Value::integer(1)), // Document element → child-number = 1
    };

    // Count siblings with the same GI that come before this node
    let mut count = 0;
    let mut current = parent.children();

    loop {
        if let Some(sibling) = current.first() {
            // Check if this is the target node (use node_eq for proper identity comparison)
            if sibling.node_eq(&**node) {
                // Found our node, return count + 1 (1-based)
                return Ok(Value::integer((count + 1) as i64));
            }

            // Not the target node, check if it has the same GI
            if let Some(sibling_gi) = sibling.gi() {
                if sibling_gi == gi {
                    count += 1;
                }
            }

            // Move to next sibling
            current = current.rest();
            if current.length() == 0 {
                break;
            }
        } else {
            break;
        }
    }

    // If we didn't find the node, something is wrong
    Err("child-number: node not found among siblings".to_string())
}

/// (element-number node) → integer
pub fn prim_element_number(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::integer(0)) // Stub
}

/// (inherited-attribute-string name node) → string | #f
pub fn prim_inherited_attribute_string(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (absolute-first-sibling? node) → boolean
pub fn prim_absolute_first_sibling_p(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (absolute-last-sibling? node) → boolean
pub fn prim_absolute_last_sibling_p(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (ancestor-child-number gi node) → integer
pub fn prim_ancestor_child_number(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::integer(0)) // Stub
}

/// (element-number-list gi-list node) → list
pub fn prim_element_number_list(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Nil) // Stub
}

/// (hierarchical-number gi-list node) → string
pub fn prim_hierarchical_number(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::string("1".to_string())) // Stub
}

/// (hierarchical-number-recursive gi-list node) → string
pub fn prim_hierarchical_number_recursive(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::string("1".to_string())) // Stub
}

/// (have-ancestor? gi node) → boolean
pub fn prim_have_ancestor_p(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (all-element-number node) → integer
pub fn prim_all_element_number(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::integer(0)) // Stub
}

// =============================================================================
// Character and Language Primitives
// =============================================================================

/// (char-property prop-name char) → value
pub fn prim_char_property(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (char-script-case char script-case-list) → char
pub fn prim_char_script_case(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Char('a')) // Stub - return dummy char
}

/// (language name country) → language
pub fn prim_language(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (with-language lang proc) → value
pub fn prim_with_language(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

// =============================================================================
// Basic Grove Extended
// =============================================================================

/// (first-child-gi node gi) → boolean
pub fn prim_first_child_gi(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (inherited-element-attribute-string gi name node) → string | #f
pub fn prim_inherited_element_attribute_string(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

// =============================================================================
// Debug and External Procedures
// =============================================================================

/// (debug obj) → obj
pub fn prim_debug(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() {
        return Err("debug requires at least 1 argument".to_string());
    }

    // OpenJade: (debug string) - outputs string to stdout and passes it through
    // Evaluate the argument and print it
    match &args[0] {
        Value::String(s) => {
            println!("{}", s);
        }
        other => {
            // For non-strings, print their representation
            println!("{:?}", other);
        }
    }

    Ok(args[0].clone())
}

/// (external-procedure name) → procedure
pub fn prim_external_procedure(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (read-entity name) → string
pub fn prim_read_entity(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::string("".to_string())) // Stub
}

// =============================================================================
// Entity/Notation Extended Stubs
// =============================================================================

/// (entity-address name) → address
pub fn prim_entity_address(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (entity-attribute-string attr-name entity-name node) → string | #f
pub fn prim_entity_attribute_string(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (entity-generated-system-id name node) → string | #f
pub fn prim_entity_generated_system_id(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (entity-name-normalize name node) → string
pub fn prim_entity_name_normalize(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::string("".to_string())) // Stub
}

/// (entity-notation name node) → string | #f
pub fn prim_entity_notation(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (entity-text name node) → string | #f
pub fn prim_entity_text(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (entity-type name node) → symbol
pub fn prim_entity_type(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::symbol("unknown")) // Stub
}

/// (notation-generated-system-id name node) → string | #f
pub fn prim_notation_generated_system_id(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (general-name-normalize name node) → string
pub fn prim_general_name_normalize(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::string("".to_string())) // Stub
}

/// (sgml-document-address doc-name node-id) → address
pub fn prim_sgml_document_address(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (declaration node) → node | #f
pub fn prim_declaration(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (dtd node) → node | #f
pub fn prim_dtd(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (sgml-declaration node) → node | #f
pub fn prim_sgml_declaration(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (document-element node) → node | #f
pub fn prim_document_element(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (prolog node) → node-list
pub fn prim_prolog(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new()))) // Stub
}

/// (epilog node) → node-list
pub fn prim_epilog(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new()))) // Stub
}

// define-language is now a special form in evaluator.rs

/// declare-default-language: Set default language (DSSSL declaration, no-op)
pub fn prim_declare_default_language(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified)
}

// declare-flow-object-class is now a special form in evaluator.rs

/// declare-characteristic: Declare a characteristic (DSSSL declaration, no-op)
pub fn prim_declare_characteristic(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified)
}

/// (origin-to-subnode-rel-forest-addr node subnode) → address
pub fn prim_origin_to_subnode_rel_forest_addr(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

// =============================================================================
// Node-list Extended Stubs
// =============================================================================

/// (named-node name named-node-list) → node | #f
pub fn prim_named_node(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (named-node-list-names named-node-list) → list
pub fn prim_named_node_list_names(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Nil) // Stub
}

/// (named-node-list-normalize named-node-list tree-root? node-list) → named-node-list
pub fn prim_named_node_list_normalize(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (node-list-address node-list) → address
pub fn prim_node_list_address(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (node-list-error msg node-list) → node-list
pub fn prim_node_list_error(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new()))) // Stub
}

/// (node-list-no-order node-list) → node-list
pub fn prim_node_list_no_order(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new()))) // Stub
}

/// (select-by-class class-name node-list) → node-list
pub fn prim_select_by_class(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new()))) // Stub
}

/// (node-list-union nl1 nl2 ...) → node-list
///
/// Returns a node-list containing all unique nodes from all argument node-lists.
/// Node identity (not equality) is used to determine uniqueness.
/// The order is the document order of the first occurrence of each node.
///
/// **DSSSL**: Grove primitive
pub fn prim_node_list_union(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() {
        return Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new())));
    }

    // Collect all nodes from all node-lists, maintaining document order
    let mut seen_ids = std::collections::HashSet::new();
    let mut result_nodes = Vec::new();

    for arg in args {
        match arg {
            Value::NodeList(nl) => {
                // Iterate through this node-list
                for i in 0..nl.length() {
                    if let Some(node) = nl.get(i) {
                        let node_id = node.node_id();
                        // Only add if we haven't seen this node before
                        if !seen_ids.contains(&node_id) {
                            seen_ids.insert(node_id);
                            result_nodes.push(node);
                        }
                    }
                }
            }
            Value::Node(n) => {
                // Single node treated as singleton node-list
                let node_id = n.as_ref().node_id();
                if !seen_ids.contains(&node_id) {
                    seen_ids.insert(node_id);
                    result_nodes.push(n.as_ref().clone_node());
                }
            }
            Value::Pair(_) => {
                // Regular Scheme list (from node-list-map) - iterate through it
                let mut current = arg.clone();
                loop {
                    match current {
                        Value::Nil => break,
                        Value::Pair(ref p) => {
                            let (car, cdr) = {
                                let pair_data = p.borrow();
                                (pair_data.car.clone(), pair_data.cdr.clone())
                            };

                            // Extract the node from car
                            match car {
                                Value::Node(ref n) => {
                                    let node_id = n.as_ref().node_id();
                                    if !seen_ids.contains(&node_id) {
                                        seen_ids.insert(node_id);
                                        result_nodes.push(n.as_ref().clone_node());
                                    }
                                }
                                Value::Bool(false) => {
                                    // #f in the list - skip it (graceful handling)
                                }
                                _ => return Err(format!("node-list-union: list contains non-node: {:?}", car)),
                            }

                            current = cdr;
                        }
                        _ => return Err(format!("node-list-union: malformed list: {:?}", current)),
                    }
                }
            }
            Value::Nil => {
                // Empty list - skip
            }
            _ => return Err(format!("node-list-union: argument not a node-list: {:?}", arg)),
        }
    }

    Ok(Value::node_list(Box::new(crate::grove::VecNodeList::new(result_nodes))))
}

/// (node-list-intersection nl1 nl2 ...) → node-list
pub fn prim_node_list_intersection(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new()))) // Stub
}

/// (node-list-difference nl1 nl2 ...) → node-list
pub fn prim_node_list_difference(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new()))) // Stub
}

/// (node-list-symmetrical-difference nl1 nl2) → node-list
pub fn prim_node_list_symmetrical_difference(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new()))) // Stub
}

/// (node-list-union-map proc nl) → node-list
pub fn prim_node_list_union_map(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::node_list(Box::new(crate::grove::EmptyNodeList::new()))) // Stub
}

// =============================================================================
// Processing Extended Stubs
// =============================================================================

/// (process-children-trim) → sosofo
pub fn prim_process_children_trim(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Sosofo) // Stub
}

/// (process-element-with-id id) → sosofo
pub fn prim_process_element_with_id(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Sosofo) // Stub
}

/// (process-first-descendant gi-list) → sosofo
pub fn prim_process_first_descendant(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Sosofo) // Stub
}

/// (process-matching-children gi-list) → sosofo
pub fn prim_process_matching_children(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Sosofo) // Stub
}

/// (merge-style styles...) → style
pub fn prim_merge_style(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (map-constructor constructor-name) → procedure
pub fn prim_map_constructor(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (with-mode mode proc) → sosofo
pub fn prim_with_mode(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Sosofo) // Stub
}

/// (current-mode) → symbol | #f
pub fn prim_current_mode(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

// =============================================================================
// Sosofo/Page Layout Extended Stubs
// =============================================================================

/// (current-node-page-number-sosofo) → sosofo
pub fn prim_current_node_page_number_sosofo(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Sosofo) // Stub
}

/// (page-number-sosofo) → sosofo
pub fn prim_page_number_sosofo(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Sosofo) // Stub
}

/// (sosofo-discard-labeled label sosofo) → sosofo
pub fn prim_sosofo_discard_labeled(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Sosofo) // Stub
}

/// (sosofo-label label sosofo) → sosofo
pub fn prim_sosofo_label(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Sosofo) // Stub
}

/// (idref-address string node) → address
pub fn prim_idref_address(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (hytime-linkend node) → string | #f
pub fn prim_hytime_linkend(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (if-first-page then-sosofo else-sosofo) → sosofo
pub fn prim_if_first_page(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Sosofo) // Stub
}

/// (if-front-page then-sosofo else-sosofo) → sosofo
pub fn prim_if_front_page(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Sosofo) // Stub
}

/// (sosofo-contains-node? sosofo node) → boolean
pub fn prim_sosofo_contains_node_p(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (set-visited! address) → unspecified
pub fn prim_set_visited(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (label-length label) → length
pub fn prim_label_length(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::integer(0)) // Stub
}

/// (label-distance label) → length
pub fn prim_label_distance(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::integer(0)) // Stub
}

// =============================================================================
// Quantity/Dimension Stubs
// =============================================================================

/// (quantity->number quantity) → number
pub fn prim_quantity_to_number(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::integer(0)) // Stub
}

/// (number->quantity number [unit]) → quantity
pub fn prim_number_to_quantity(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (quantity-convert quantity unit) → quantity
pub fn prim_quantity_convert(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (device-length quantity) → number
pub fn prim_device_length(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::integer(0)) // Stub
}

/// (quantity->string quantity) → string
pub fn prim_quantity_to_string(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::string("0pt".to_string())) // Stub
}

/// (table-unit n) → length
pub fn prim_table_unit(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

/// (display-size) → (width . height)
pub fn prim_display_size(_args: &[Value]) -> PrimitiveResult {
    // Return a cons pair (width . height)
    Ok(Value::Unspecified) // Stub
}

// =============================================================================
// SGML-specific Stubs
// =============================================================================

/// (sgml-parse system-id) → grove
pub fn prim_sgml_parse(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Unspecified) // Stub
}

// =============================================================================
// Time Utility Stubs
// =============================================================================

/// (time<? t1 t2) → boolean
pub fn prim_time_lt(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (time<=? t1 t2) → boolean
pub fn prim_time_le(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (time>? t1 t2) → boolean
pub fn prim_time_gt(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

/// (time>=? t1 t2) → boolean
pub fn prim_time_ge(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::bool(false)) // Stub
}

// =============================================================================
// Processing and Sosofo Extended Stubs
// =============================================================================

/// (next-match) → sosofo
pub fn prim_next_match(_args: &[Value]) -> PrimitiveResult {
    Ok(Value::Sosofo) // Stub
}

/// (style? obj) → boolean
pub fn prim_style_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("style? requires exactly 1 argument".to_string());
    }
    Ok(Value::bool(false)) // Stub
}

// =============================================================================
// Sosofo Primitives (DSSSL)
// =============================================================================
//
// Sosofo = Specification Of a Sequence Of Flow Objects
// Core abstraction for document transformation in DSSSL.
//
// **Implementation Status**: Basic API defined. Full implementation will
// generate actual output when connected to backends.

/// (sosofo? obj) → boolean
///
/// Returns #t if obj is a sosofo.
///
/// **DSSSL**: Processing primitive
pub fn prim_sosofo_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("sosofo? requires exactly 1 argument".to_string());
    }

    Ok(Value::bool(matches!(args[0], Value::Sosofo)))
}

/// (empty-sosofo) → sosofo
///
/// Returns an empty sosofo (generates no output).
///
/// **DSSSL**: Processing primitive
pub fn prim_empty_sosofo(args: &[Value]) -> PrimitiveResult {
    if !args.is_empty() {
        return Err("empty-sosofo requires no arguments".to_string());
    }

    Ok(Value::Sosofo)
}

/// (literal str) → sosofo
///
/// Creates a sosofo that outputs the given string.
/// Appends the string to the backend's current output buffer.
///
/// **DSSSL**: Processing primitive
pub fn prim_literal(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("literal requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::String(s) => {
            // Get backend from evaluator context and append text
            if let Some(ctx) = crate::scheme::evaluator::get_evaluator_context() {
                if let Some(ref backend) = ctx.backend {
                    backend.borrow_mut()
                        .formatting_instruction(s)
                        .map_err(|e| format!("literal: backend error: {}", e))?;
                }
            }
            Ok(Value::Sosofo)
        }
        _ => Err(format!("literal: not a string: {:?}", args[0])),
    }
}

/// (sosofo-append sosofo ...) → sosofo
///
/// Concatenates multiple sosofos into a single sosofo.
///
/// **DSSSL**: Processing primitive
pub fn prim_sosofo_append(args: &[Value]) -> PrimitiveResult {
    // Check all arguments are sosofos or unspecified (which we treat as empty sosofo)
    for arg in args {
        if !matches!(arg, Value::Sosofo | Value::Unspecified) {
            return Err(format!("sosofo-append: not a sosofo: {:?}", arg));
        }
    }

    // Return a sosofo (placeholder for now)
    // In practice, sosofos are just markers - the actual output happens via backend
    Ok(Value::Sosofo)
}

// =============================================================================
// Processing Primitives (DSSSL)
// =============================================================================
//
// These primitives handle document tree traversal and template application.
// They are central to DSSSL's processing model.

/// (process-children) → sosofo
///
/// Processes the children of the current node.
///
/// In DSSSL, this looks up the appropriate rule for each child node
/// and evaluates it, then appends all the resulting sosofos.
///
/// **Implementation Note**: This is a simplified stub implementation.
/// Full DSSSL requires rule/mode matching which will be added later.
/// For now, this returns empty-sosofo.
///
/// **DSSSL**: Processing primitive
pub fn prim_process_children(args: &[Value]) -> PrimitiveResult {
    if !args.is_empty() {
        return Err("process-children requires no arguments".to_string());
    }

    // TODO: Implement full processing model
    // This should:
    // 1. Get children of current node
    // 2. For each child, find matching rule/mode
    // 3. Evaluate the rule with child as current-node
    // 4. Append all resulting sosofos

    // For now, return empty sosofo
    Ok(Value::Sosofo)
}

/// (process-node-list node-list) → sosofo
///
/// Processes each node in the given node-list.
///
/// Similar to process-children, but processes an explicit node-list
/// rather than the current node's children.
///
/// **Implementation Note**: This is a simplified stub implementation.
/// Full DSSSL requires rule/mode matching which will be added later.
/// For now, this returns empty-sosofo.
///
/// **DSSSL**: Processing primitive
pub fn prim_process_node_list(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("process-node-list requires exactly 1 argument".to_string());
    }

    // Verify argument is a node-list
    if !matches!(args[0], Value::NodeList(_)) {
        return Err(format!("process-node-list: not a node-list: {:?}", args[0]));
    }

    // TODO: Implement full processing model
    // This should:
    // 1. For each node in the list
    // 2. Find matching rule/mode
    // 3. Evaluate the rule with node as current-node
    // 4. Append all resulting sosofos

    // For now, return empty sosofo
    Ok(Value::Sosofo)
}

/// (make flow-object-type ...) → sosofo
///
/// Creates a flow object (output element).
///
/// This is the primary way templates generate output. Common flow objects:
/// - `entity` - Creates an output file (system-id:, content)
/// - `formatting-instruction` - Outputs raw text (data:)
/// - `sequence` - Groups flow objects
/// - `literal` - Outputs text (handled separately as a function)
///
/// **Implementation Note**: This is a stub implementation.
/// Full implementation requires FotBuilder integration in the evaluator.
/// For now, this returns a sosofo marker.
///
/// **DSSSL**: Flow object construction primitive
///
/// **Example**:
/// ```scheme
/// (make entity
///   system-id: "Output.java"
///   (literal "public class Foo { }"))
/// ```
pub fn prim_make(args: &[Value]) -> PrimitiveResult {
    if args.is_empty() {
        return Err("make requires at least a flow object type".to_string());
    }

    // First argument should be a symbol (flow object type)
    match &args[0] {
        Value::Symbol(fo_type) => {
            // TODO: Implement actual flow object creation
            // This should:
            // 1. Parse keyword arguments (system-id:, data:, etc.)
            // 2. Evaluate body sosofo
            // 3. Call appropriate FotBuilder method
            // 4. Return sosofo

            // For now, just validate it's a known type and return sosofo
            match fo_type.as_ref() {
                "entity" | "formatting-instruction" | "sequence" | "paragraph" => {
                    Ok(Value::Sosofo)
                }
                _ => Err(format!("make: unknown flow object type: {}", fo_type)),
            }
        }
        _ => Err(format!("make: first argument must be a symbol: {:?}", args[0])),
    }
}

// =============================================================================
// String Utility Primitives (DSSSL Extensions)
// =============================================================================

/// (string-equiv? str1 str2) → boolean
///
/// Case-insensitive string comparison.
/// Returns #t if strings are equal ignoring case.
///
/// **DSSSL**: Extension primitive
pub fn prim_string_equiv_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("string-equiv? requires exactly 2 arguments".to_string());
    }

    let s1 = match &args[0] {
        Value::String(s) => s.to_lowercase(),
        _ => return Err(format!("string-equiv?: not a string: {:?}", args[0])),
    };

    let s2 = match &args[1] {
        Value::String(s) => s.to_lowercase(),
        _ => return Err(format!("string-equiv?: not a string: {:?}", args[1])),
    };

    Ok(Value::bool(s1 == s2))
}

// =============================================================================
// Time Primitives (DSSSL Extensions)
// =============================================================================
//
// Time values represent points in time. For now, these are stubs.

/// (time) → time
///
/// Returns the current time.
/// Stub implementation - returns unspecified for now.
///
/// **DSSSL**: Extension primitive
pub fn prim_time(args: &[Value]) -> PrimitiveResult {
    if !args.is_empty() {
        return Err("time requires no arguments".to_string());
    }

    // Stub: return unspecified
    // Real implementation would return a time object
    Ok(Value::Unspecified)
}

/// (time->string t format) → string
///
/// Converts time to string according to format.
/// Stub implementation.
///
/// **DSSSL**: Extension primitive
pub fn prim_time_to_string(args: &[Value]) -> PrimitiveResult {
    if args.len() != 2 {
        return Err("time->string requires exactly 2 arguments".to_string());
    }

    // Stub: return empty string
    Ok(Value::string("".to_string()))
}

// =============================================================================
// Language Primitives (DSSSL)
// =============================================================================
//
// Language objects represent natural languages (for i18n).
// Stub implementations for now.

/// (language? obj) → boolean
///
/// Returns #t if obj is a language.
/// Stub - no language type yet.
///
/// **DSSSL**: Type predicate
pub fn prim_language_p(args: &[Value]) -> PrimitiveResult {
    if args.len() != 1 {
        return Err("language? requires exactly 1 argument".to_string());
    }

    // Stub: no language type, always return #f
    Ok(Value::bool(false))
}

/// (current-language) → language
///
/// Returns the current language.
/// Stub implementation.
///
/// **DSSSL**: Extension primitive
pub fn prim_current_language(args: &[Value]) -> PrimitiveResult {
    if !args.is_empty() {
        return Err("current-language requires no arguments".to_string());
    }

    // Stub: return unspecified
    Ok(Value::Unspecified)
}

// =============================================================================
// Registration
// =============================================================================

/// Register all list primitives in an environment
pub fn register_list_primitives(env: &gc::Gc<crate::scheme::environment::Environment>) {
    env.define("car", Value::primitive("car", prim_car));
    env.define("cdr", Value::primitive("cdr", prim_cdr));
    env.define("cons", Value::primitive("cons", prim_cons));
    env.define("list", Value::primitive("list", prim_list));
    env.define("null?", Value::primitive("null?", prim_null_p));
    env.define("pair?", Value::primitive("pair?", prim_pair_p));
    env.define("list?", Value::primitive("list?", prim_list_p));
    env.define("length", Value::primitive("length", prim_length));
    env.define("append", Value::primitive("append", prim_append));
    env.define("reverse", Value::primitive("reverse", prim_reverse));
    env.define("list-tail", Value::primitive("list-tail", prim_list_tail));
    env.define("list-ref", Value::primitive("list-ref", prim_list_ref));
    env.define("memq", Value::primitive("memq", prim_memq));
    env.define("memv", Value::primitive("memv", prim_memv));
    env.define("member", Value::primitive("member", prim_member));
    env.define("assq", Value::primitive("assq", prim_assq));
    env.define("assv", Value::primitive("assv", prim_assv));
    env.define("assoc", Value::primitive("assoc", prim_assoc));
    // cXXr combinations
    env.define("cadr", Value::primitive("cadr", prim_cadr));
    env.define("caddr", Value::primitive("caddr", prim_caddr));
    env.define("cadddr", Value::primitive("cadddr", prim_cadddr));
    env.define("caar", Value::primitive("caar", prim_caar));
    env.define("cddr", Value::primitive("cddr", prim_cddr));
    env.define("cdar", Value::primitive("cdar", prim_cdar));
    env.define("caaar", Value::primitive("caaar", prim_caaar));
    env.define("cdaar", Value::primitive("cdaar", prim_cdaar));
    env.define("cadar", Value::primitive("cadar", prim_cadar));
    env.define("cddar", Value::primitive("cddar", prim_cddar));
    env.define("caadr", Value::primitive("caadr", prim_caadr));
    env.define("cdadr", Value::primitive("cdadr", prim_cdadr));
    // Additional list utilities
    env.define("last", Value::primitive("last", prim_last));
}

/// Register all number primitives in an environment
pub fn register_number_primitives(env: &gc::Gc<crate::scheme::environment::Environment>) {
    // Arithmetic
    env.define("+", Value::primitive("+", prim_add));
    env.define("-", Value::primitive("-", prim_subtract));
    env.define("*", Value::primitive("*", prim_multiply));
    env.define("/", Value::primitive("/", prim_divide));
    env.define("quotient", Value::primitive("quotient", prim_quotient));
    env.define("remainder", Value::primitive("remainder", prim_remainder));
    env.define("modulo", Value::primitive("modulo", prim_modulo));

    // Comparison
    env.define("=", Value::primitive("=", prim_num_eq));
    env.define("<", Value::primitive("<", prim_num_lt));
    env.define(">", Value::primitive(">", prim_num_gt));
    env.define("<=", Value::primitive("<=", prim_num_le));
    env.define(">=", Value::primitive(">=", prim_num_ge));

    // Type predicates
    env.define("number?", Value::primitive("number?", prim_number_p));
    env.define("integer?", Value::primitive("integer?", prim_integer_p));
    env.define("real?", Value::primitive("real?", prim_real_p));
    env.define("zero?", Value::primitive("zero?", prim_zero_p));
    env.define("positive?", Value::primitive("positive?", prim_positive_p));
    env.define("negative?", Value::primitive("negative?", prim_negative_p));
    env.define("odd?", Value::primitive("odd?", prim_odd_p));
    env.define("even?", Value::primitive("even?", prim_even_p));

    // Math functions
    env.define("abs", Value::primitive("abs", prim_abs));
    env.define("max", Value::primitive("max", prim_max));
    env.define("min", Value::primitive("min", prim_min));
    env.define("gcd", Value::primitive("gcd", prim_gcd));
    env.define("lcm", Value::primitive("lcm", prim_lcm));
    env.define("floor", Value::primitive("floor", prim_floor));
    env.define("ceiling", Value::primitive("ceiling", prim_ceiling));
    env.define("truncate", Value::primitive("truncate", prim_truncate));
    env.define("round", Value::primitive("round", prim_round));

    // Advanced math functions
    env.define("expt", Value::primitive("expt", prim_expt));
    env.define("sqrt", Value::primitive("sqrt", prim_sqrt));
    env.define("sin", Value::primitive("sin", prim_sin));
    env.define("cos", Value::primitive("cos", prim_cos));
    env.define("tan", Value::primitive("tan", prim_tan));
    env.define("atan", Value::primitive("atan", prim_atan));
    env.define("asin", Value::primitive("asin", prim_asin));
    env.define("acos", Value::primitive("acos", prim_acos));
    env.define("log", Value::primitive("log", prim_log));
    env.define("exp", Value::primitive("exp", prim_exp));

    // Number conversions
    env.define("exact?", Value::primitive("exact?", prim_exact_p));
    env.define("inexact?", Value::primitive("inexact?", prim_inexact_p));
    env.define("exact->inexact", Value::primitive("exact->inexact", prim_exact_to_inexact));
    env.define("inexact->exact", Value::primitive("inexact->exact", prim_inexact_to_exact));
}

/// Register all string primitives in an environment
pub fn register_string_primitives(env: &gc::Gc<crate::scheme::environment::Environment>) {
    // String operations
    env.define("string-length", Value::primitive("string-length", prim_string_length));
    env.define("string-ref", Value::primitive("string-ref", prim_string_ref));
    env.define("string-append", Value::primitive("string-append", prim_string_append));
    env.define("substring", Value::primitive("substring", prim_substring));
    env.define("make-string", Value::primitive("make-string", prim_make_string));
    env.define("string", Value::primitive("string", prim_string));

    // String comparison
    env.define("string=?", Value::primitive("string=?", prim_string_eq));
    env.define("string<?", Value::primitive("string<?", prim_string_lt));
    env.define("string>?", Value::primitive("string>?", prim_string_gt));
    env.define("string<=?", Value::primitive("string<=?", prim_string_le));
    env.define("string>=?", Value::primitive("string>=?", prim_string_ge));

    // Case-insensitive string comparison
    env.define("string-ci=?", Value::primitive("string-ci=?", prim_string_ci_eq));
    env.define("string-ci<?", Value::primitive("string-ci<?", prim_string_ci_lt));
    env.define("string-ci>?", Value::primitive("string-ci>?", prim_string_ci_gt));
    env.define("string-ci<=?", Value::primitive("string-ci<=?", prim_string_ci_le));
    env.define("string-ci>=?", Value::primitive("string-ci>=?", prim_string_ci_ge));

    // String conversions
    env.define("string->list", Value::primitive("string->list", prim_string_to_list));
    env.define("list->string", Value::primitive("list->string", prim_list_to_string));
    env.define("string->symbol", Value::primitive("string->symbol", prim_string_to_symbol));
    env.define("symbol->string", Value::primitive("symbol->string", prim_symbol_to_string));

    // Type predicates
    env.define("string?", Value::primitive("string?", prim_string_p));
    env.define("symbol?", Value::primitive("symbol?", prim_symbol_p));
    env.define("char?", Value::primitive("char?", prim_char_p));

    // Character operations
    env.define("char=?", Value::primitive("char=?", prim_char_eq));
    env.define("char<?", Value::primitive("char<?", prim_char_lt));
    env.define("char>?", Value::primitive("char>?", prim_char_gt));
    env.define("char<=?", Value::primitive("char<=?", prim_char_le));
    env.define("char>=?", Value::primitive("char>=?", prim_char_ge));
    env.define("char-upcase", Value::primitive("char-upcase", prim_char_upcase));
    env.define("char-downcase", Value::primitive("char-downcase", prim_char_downcase));
    env.define("char->integer", Value::primitive("char->integer", prim_char_to_integer));
    env.define("integer->char", Value::primitive("integer->char", prim_integer_to_char));
    env.define("char-alphabetic?", Value::primitive("char-alphabetic?", prim_char_alphabetic_p));
    env.define("char-numeric?", Value::primitive("char-numeric?", prim_char_numeric_p));
    env.define("char-whitespace?", Value::primitive("char-whitespace?", prim_char_whitespace_p));

    // Case-insensitive character operations
    env.define("char-ci=?", Value::primitive("char-ci=?", prim_char_ci_eq));
    env.define("char-ci<?", Value::primitive("char-ci<?", prim_char_ci_lt));
    env.define("char-ci>?", Value::primitive("char-ci>?", prim_char_ci_gt));
    env.define("char-ci<=?", Value::primitive("char-ci<=?", prim_char_ci_le));
    env.define("char-ci>=?", Value::primitive("char-ci>=?", prim_char_ci_ge));
}

/// Register all boolean and equality primitives in an environment
pub fn register_boolean_primitives(env: &gc::Gc<crate::scheme::environment::Environment>) {
    env.define("not", Value::primitive("not", prim_not));
    env.define("boolean?", Value::primitive("boolean?", prim_boolean_p));
    env.define("equal?", Value::primitive("equal?", prim_equal_p));
    env.define("eqv?", Value::primitive("eqv?", prim_eqv_p));
    env.define("eq?", Value::primitive("eq?", prim_eq_p));
    env.define("procedure?", Value::primitive("procedure?", prim_procedure_p));
}

/// Register all I/O and utility primitives in an environment
pub fn register_io_primitives(env: &gc::Gc<crate::scheme::environment::Environment>) {
    env.define("error", Value::primitive("error", prim_error));
    env.define("display", Value::primitive("display", prim_display));
    env.define("newline", Value::primitive("newline", prim_newline));
    env.define("write", Value::primitive("write", prim_write));
}

/// Register all conversion primitives in an environment
pub fn register_conversion_primitives(env: &gc::Gc<crate::scheme::environment::Environment>) {
    env.define("number->string", Value::primitive("number->string", prim_number_to_string));
    env.define("string->number", Value::primitive("string->number", prim_string_to_number));
}

/// Register all keyword primitives in an environment
pub fn register_keyword_primitives(env: &gc::Gc<crate::scheme::environment::Environment>) {
    env.define("keyword?", Value::primitive("keyword?", prim_keyword_p));
    env.define("keyword->string", Value::primitive("keyword->string", prim_keyword_to_string));
    env.define("string->keyword", Value::primitive("string->keyword", prim_string_to_keyword));
}

/// Register DSSSL type stub primitives in an environment
pub fn register_dsssl_type_primitives(env: &gc::Gc<crate::scheme::environment::Environment>) {
    env.define("quantity?", Value::primitive("quantity?", prim_quantity_p));
    env.define("color?", Value::primitive("color?", prim_color_p));
    env.define("address?", Value::primitive("address?", prim_address_p));

    // Color and color-space stubs
    env.define("color", Value::primitive("color", prim_color));
    env.define("color-space", Value::primitive("color-space", prim_color_space));
    env.define("color-space?", Value::primitive("color-space?", prim_color_space_p));

    // Spacing stubs
    env.define("display-space", Value::primitive("display-space", prim_display_space));
    env.define("display-space?", Value::primitive("display-space?", prim_display_space_p));
    env.define("inline-space", Value::primitive("inline-space", prim_inline_space));
    env.define("inline-space?", Value::primitive("inline-space?", prim_inline_space_p));

    // Glyph stubs
    env.define("glyph-id", Value::primitive("glyph-id", prim_glyph_id));
    env.define("glyph-id?", Value::primitive("glyph-id?", prim_glyph_id_p));
    env.define("glyph-subst-table", Value::primitive("glyph-subst-table", prim_glyph_subst_table));
    env.define("glyph-subst-table?", Value::primitive("glyph-subst-table?", prim_glyph_subst_table_p));
    env.define("glyph-subst", Value::primitive("glyph-subst", prim_glyph_subst));

    // Address stubs
    env.define("address-local?", Value::primitive("address-local?", prim_address_local_p));

    // Quantity/Dimension (stubs)
    env.define("quantity->number", Value::primitive("quantity->number", prim_quantity_to_number));
    env.define("number->quantity", Value::primitive("number->quantity", prim_number_to_quantity));
    env.define("quantity-convert", Value::primitive("quantity-convert", prim_quantity_convert));
    env.define("device-length", Value::primitive("device-length", prim_device_length));
}

/// Register format primitives in an environment
pub fn register_format_primitives(env: &gc::Gc<crate::scheme::environment::Environment>) {
    env.define("format-number", Value::primitive("format-number", prim_format_number));
    env.define("format-number-list", Value::primitive("format-number-list", prim_format_number_list));
}

/// Register grove query primitives in an environment
pub fn register_grove_primitives(env: &gc::Gc<crate::scheme::environment::Environment>) {
    // Context primitives
    env.define("current-node", Value::primitive("current-node", prim_current_node));

    // Node primitives
    env.define("node?", Value::primitive("node?", prim_node_p));
    env.define("gi", Value::primitive("gi", prim_gi));
    env.define("data", Value::primitive("data", prim_data));
    env.define("id", Value::primitive("id", prim_id));
    env.define("attribute-string", Value::primitive("attribute-string", prim_attribute_string));
    env.define("children", Value::primitive("children", prim_children));
    env.define("select-children", Value::primitive("select-children", prim_select_children));
    env.define("parent", Value::primitive("parent", prim_parent));

    // Node navigation (stubs for now)
    env.define("ancestor", Value::primitive("ancestor", prim_ancestor));
    env.define("descendants", Value::primitive("descendants", prim_descendants));
    env.define("follow", Value::primitive("follow", prim_follow));
    env.define("preced", Value::primitive("preced", prim_preced));
    env.define("attributes", Value::primitive("attributes", prim_attributes));
    env.define("select-elements", Value::primitive("select-elements", prim_select_elements));
    env.define("element-with-id", Value::primitive("element-with-id", prim_element_with_id));

    // Node-list primitives
    env.define("node-list?", Value::primitive("node-list?", prim_node_list_p));
    env.define("empty-node-list", Value::primitive("empty-node-list", prim_empty_node_list));
    env.define("node-list-empty?", Value::primitive("node-list-empty?", prim_node_list_empty_p));
    env.define("node-list-length", Value::primitive("node-list-length", prim_node_list_length));
    env.define("node-list-remove-duplicates", Value::primitive("node-list-remove-duplicates", prim_node_list_remove_duplicates));
    env.define("node-list-first", Value::primitive("node-list-first", prim_node_list_first));
    env.define("node-list-rest", Value::primitive("node-list-rest", prim_node_list_rest));
    env.define("node-list-ref", Value::primitive("node-list-ref", prim_node_list_ref));
    env.define("node-list-reverse", Value::primitive("node-list-reverse", prim_node_list_reverse));
    env.define("node-list->list", Value::primitive("node-list->list", prim_node_list_to_list));
    env.define("node-list-contains?", Value::primitive("node-list-contains?", prim_node_list_contains_p));

    // Node-list utilities (stubs)
    env.define("node-list", Value::primitive("node-list", prim_node_list));
    env.define("node-list-map", Value::primitive("node-list-map", prim_node_list_map));
    env.define("node-property", Value::primitive("node-property", prim_node_property));
    env.define("match-element?", Value::primitive("match-element?", prim_match_element_p));
    env.define("named-node-list?", Value::primitive("named-node-list?", prim_named_node_list_p));
    env.define("node-list=?", Value::primitive("node-list=?", prim_node_list_eq));

    // Grove extended (stubs)
    env.define("first-sibling?", Value::primitive("first-sibling?", prim_first_sibling_p));
    env.define("last-sibling?", Value::primitive("last-sibling?", prim_last_sibling_p));
    env.define("child-number", Value::primitive("child-number", prim_child_number));
    env.define("element-number", Value::primitive("element-number", prim_element_number));
    env.define("inherited-attribute-string", Value::primitive("inherited-attribute-string", prim_inherited_attribute_string));

    // Entity and notation (stubs)
    env.define("entity-system-id", Value::primitive("entity-system-id", prim_entity_system_id));
    env.define("entity-public-id", Value::primitive("entity-public-id", prim_entity_public_id));
    env.define("notation-system-id", Value::primitive("notation-system-id", prim_notation_system_id));
    env.define("notation-public-id", Value::primitive("notation-public-id", prim_notation_public_id));

    // Grove Position/Numbering (stubs)
    env.define("absolute-first-sibling?", Value::primitive("absolute-first-sibling?", prim_absolute_first_sibling_p));
    env.define("absolute-last-sibling?", Value::primitive("absolute-last-sibling?", prim_absolute_last_sibling_p));
    env.define("ancestor-child-number", Value::primitive("ancestor-child-number", prim_ancestor_child_number));
    env.define("element-number-list", Value::primitive("element-number-list", prim_element_number_list));
    env.define("hierarchical-number", Value::primitive("hierarchical-number", prim_hierarchical_number));
    env.define("hierarchical-number-recursive", Value::primitive("hierarchical-number-recursive", prim_hierarchical_number_recursive));
    env.define("have-ancestor?", Value::primitive("have-ancestor?", prim_have_ancestor_p));
    env.define("all-element-number", Value::primitive("all-element-number", prim_all_element_number));

    // Basic Grove Extended (stubs)
    env.define("first-child-gi", Value::primitive("first-child-gi", prim_first_child_gi));
    env.define("inherited-element-attribute-string", Value::primitive("inherited-element-attribute-string", prim_inherited_element_attribute_string));

    // Entity/Notation Extended (stubs)
    env.define("entity-address", Value::primitive("entity-address", prim_entity_address));
    env.define("entity-generated-system-id", Value::primitive("entity-generated-system-id", prim_entity_generated_system_id));
    env.define("entity-type", Value::primitive("entity-type", prim_entity_type));
    env.define("declaration", Value::primitive("declaration", prim_declaration));
    env.define("dtd", Value::primitive("dtd", prim_dtd));
    env.define("sgml-declaration", Value::primitive("sgml-declaration", prim_sgml_declaration));
    env.define("document-element", Value::primitive("document-element", prim_document_element));
    env.define("prolog", Value::primitive("prolog", prim_prolog));
    env.define("epilog", Value::primitive("epilog", prim_epilog));

    // DSSSL Language & Flow Object Class Declarations (define-language and declare-flow-object-class are special forms)
    env.define("declare-default-language", Value::primitive("declare-default-language", prim_declare_default_language));
    env.define("declare-characteristic", Value::primitive("declare-characteristic", prim_declare_characteristic));
    env.define("origin-to-subnode-rel-forest-addr", Value::primitive("origin-to-subnode-rel-forest-addr", prim_origin_to_subnode_rel_forest_addr));

    // Node-list Extended (stubs)
    env.define("named-node", Value::primitive("named-node", prim_named_node));
    env.define("named-node-list-names", Value::primitive("named-node-list-names", prim_named_node_list_names));
    env.define("node-list-union", Value::primitive("node-list-union", prim_node_list_union));
    env.define("node-list-intersection", Value::primitive("node-list-intersection", prim_node_list_intersection));
    env.define("node-list-difference", Value::primitive("node-list-difference", prim_node_list_difference));
    env.define("node-list-symmetrical-difference", Value::primitive("node-list-symmetrical-difference", prim_node_list_symmetrical_difference));
    env.define("node-list-union-map", Value::primitive("node-list-union-map", prim_node_list_union_map));

    // DSSSL: node-list-count = node-list-length(node-list-remove-duplicates(nl))
    env.define("node-list-count", Value::primitive("node-list-count", prim_node_list_count));
}

/// Register sosofo primitives in an environment
pub fn register_sosofo_primitives(env: &gc::Gc<crate::scheme::environment::Environment>) {
    // Sosofo construction
    env.define("sosofo?", Value::primitive("sosofo?", prim_sosofo_p));
    env.define("style?", Value::primitive("style?", prim_style_p));
    env.define("next-match", Value::primitive("next-match", prim_next_match));
    env.define("empty-sosofo", Value::primitive("empty-sosofo", prim_empty_sosofo));
    env.define("literal", Value::primitive("literal", prim_literal));
    env.define("sosofo-append", Value::primitive("sosofo-append", prim_sosofo_append));

    // Processing primitives
    env.define("process-children", Value::primitive("process-children", prim_process_children));
    env.define("process-node-list", Value::primitive("process-node-list", prim_process_node_list));
    env.define("make", Value::primitive("make", prim_make));

    // Processing Extended (stubs)
    env.define("process-children-trim", Value::primitive("process-children-trim", prim_process_children_trim));
    env.define("process-first-descendant", Value::primitive("process-first-descendant", prim_process_first_descendant));
    env.define("process-matching-children", Value::primitive("process-matching-children", prim_process_matching_children));
    env.define("process-element-with-id", Value::primitive("process-element-with-id", prim_process_element_with_id));
    env.define("with-mode", Value::primitive("with-mode", prim_with_mode));
    env.define("current-mode", Value::primitive("current-mode", prim_current_mode));

    // Sosofo/Page Layout Extended (stubs)
    env.define("current-node-page-number-sosofo", Value::primitive("current-node-page-number-sosofo", prim_current_node_page_number_sosofo));
    env.define("page-number-sosofo", Value::primitive("page-number-sosofo", prim_page_number_sosofo));
    env.define("sosofo-contains-node?", Value::primitive("sosofo-contains-node?", prim_sosofo_contains_node_p));
    env.define("current-node-address", Value::primitive("current-node-address", prim_current_node_address));
    env.define("address-visited?", Value::primitive("address-visited?", prim_address_visited_p));
    env.define("set-visited!", Value::primitive("set-visited!", prim_set_visited));
    env.define("label-length", Value::primitive("label-length", prim_label_length));
    env.define("label-distance", Value::primitive("label-distance", prim_label_distance));
}

/// Register utility primitives in an environment
pub fn register_utility_primitives(env: &gc::Gc<crate::scheme::environment::Environment>) {
    // String utilities
    env.define("string-equiv?", Value::primitive("string-equiv?", prim_string_equiv_p));

    // Time primitives (stubs)
    env.define("time", Value::primitive("time", prim_time));
    env.define("time->string", Value::primitive("time->string", prim_time_to_string));
    env.define("time<?", Value::primitive("time<?", prim_time_lt));
    env.define("time<=?", Value::primitive("time<=?", prim_time_le));
    env.define("time>?", Value::primitive("time>?", prim_time_gt));
    env.define("time>=?", Value::primitive("time>=?", prim_time_ge));

    // Language primitives (stubs)
    env.define("language?", Value::primitive("language?", prim_language_p));
    env.define("current-language", Value::primitive("current-language", prim_current_language));

    // Character/Language Extended (stubs)
    env.define("char-property", Value::primitive("char-property", prim_char_property));
    env.define("char-script-case", Value::primitive("char-script-case", prim_char_script_case));
    env.define("language", Value::primitive("language", prim_language));
    env.define("with-language", Value::primitive("with-language", prim_with_language));

    // Debug (actual implementation)
    env.define("debug", Value::primitive("debug", prim_debug));
    env.define("external-procedure", Value::primitive("external-procedure", prim_external_procedure));
    env.define("read-entity", Value::primitive("read-entity", prim_read_entity));

    // SGML-specific (stub)
    env.define("sgml-parse", Value::primitive("sgml-parse", prim_sgml_parse));
}

/// Register all primitives in an environment
///
/// This is a convenience function for testing and REPL use.
/// It registers all available primitives in the given environment.
pub fn register_all_primitives(env: &gc::Gc<crate::scheme::environment::Environment>) {
    register_list_primitives(env);
    register_number_primitives(env);
    register_string_primitives(env);
    register_boolean_primitives(env);
    register_io_primitives(env);
    register_conversion_primitives(env);
    register_keyword_primitives(env);
    register_dsssl_type_primitives(env);
    register_format_primitives(env);
    register_grove_primitives(env);
    register_sosofo_primitives(env);
    register_utility_primitives(env);
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_car() {
        let pair = Value::cons(Value::integer(1), Value::integer(2));
        let result = prim_car(&[pair]).unwrap();
        assert!(matches!(result, Value::Integer(1)));
    }

    #[test]
    fn test_cdr() {
        let pair = Value::cons(Value::integer(1), Value::integer(2));
        let result = prim_cdr(&[pair]).unwrap();
        assert!(matches!(result, Value::Integer(2)));
    }

    #[test]
    fn test_cons() {
        let result = prim_cons(&[Value::integer(1), Value::integer(2)]).unwrap();
        assert!(result.is_pair());
    }

    #[test]
    fn test_list() {
        let result = prim_list(&[Value::integer(1), Value::integer(2), Value::integer(3)]).unwrap();
        assert!(result.is_list());

        // Check length
        let len = prim_length(&[result]).unwrap();
        assert!(matches!(len, Value::Integer(3)));
    }

    #[test]
    fn test_null_p() {
        assert!(matches!(prim_null_p(&[Value::Nil]).unwrap(), Value::Bool(true)));
        assert!(matches!(
            prim_null_p(&[Value::integer(1)]).unwrap(),
            Value::Bool(false)
        ));
    }

    #[test]
    fn test_pair_p() {
        let pair = Value::cons(Value::integer(1), Value::integer(2));
        assert!(matches!(prim_pair_p(&[pair]).unwrap(), Value::Bool(true)));
        assert!(matches!(
            prim_pair_p(&[Value::Nil]).unwrap(),
            Value::Bool(false)
        ));
    }

    #[test]
    fn test_length() {
        let list = prim_list(&[Value::integer(1), Value::integer(2), Value::integer(3)]).unwrap();
        let result = prim_length(&[list]).unwrap();
        assert!(matches!(result, Value::Integer(3)));

        let empty = prim_length(&[Value::Nil]).unwrap();
        assert!(matches!(empty, Value::Integer(0)));
    }

    #[test]
    fn test_append() {
        let list1 = prim_list(&[Value::integer(1), Value::integer(2)]).unwrap();
        let list2 = prim_list(&[Value::integer(3), Value::integer(4)]).unwrap();
        let result = prim_append(&[list1, list2]).unwrap();

        let len = prim_length(&[result]).unwrap();
        assert!(matches!(len, Value::Integer(4)));
    }

    #[test]
    fn test_reverse() {
        let list = prim_list(&[Value::integer(1), Value::integer(2), Value::integer(3)]).unwrap();
        let result = prim_reverse(&[list]).unwrap();

        // Check first element is 3
        let first = prim_car(&[result]).unwrap();
        assert!(matches!(first, Value::Integer(3)));
    }

    #[test]
    fn test_list_ref() {
        let list = prim_list(&[Value::integer(10), Value::integer(20), Value::integer(30)]).unwrap();

        let result = prim_list_ref(&[list.clone(), Value::integer(0)]).unwrap();
        assert!(matches!(result, Value::Integer(10)));

        let result = prim_list_ref(&[list.clone(), Value::integer(1)]).unwrap();
        assert!(matches!(result, Value::Integer(20)));

        let result = prim_list_ref(&[list, Value::integer(2)]).unwrap();
        assert!(matches!(result, Value::Integer(30)));
    }

    #[test]
    fn test_list_tail() {
        let list = prim_list(&[Value::integer(1), Value::integer(2), Value::integer(3)]).unwrap();

        let result = prim_list_tail(&[list, Value::integer(2)]).unwrap();
        let len = prim_length(&[result]).unwrap();
        assert!(matches!(len, Value::Integer(1)));
    }

    // =========================================================================
    // Number primitive tests
    // =========================================================================

    #[test]
    fn test_add() {
        let result = prim_add(&[]).unwrap();
        assert!(matches!(result, Value::Integer(0)));

        let result = prim_add(&[Value::integer(1), Value::integer(2), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Integer(6)));

        let result = prim_add(&[Value::integer(1), Value::real(2.5)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 3.5).abs() < f64::EPSILON));
    }

    #[test]
    fn test_subtract() {
        let result = prim_subtract(&[Value::integer(5)]).unwrap();
        assert!(matches!(result, Value::Integer(-5)));

        let result = prim_subtract(&[Value::integer(10), Value::integer(3), Value::integer(2)]).unwrap();
        assert!(matches!(result, Value::Integer(5)));

        let result = prim_subtract(&[Value::real(10.5), Value::integer(2)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 8.5).abs() < f64::EPSILON));
    }

    #[test]
    fn test_multiply() {
        let result = prim_multiply(&[]).unwrap();
        assert!(matches!(result, Value::Integer(1)));

        let result = prim_multiply(&[Value::integer(2), Value::integer(3), Value::integer(4)]).unwrap();
        assert!(matches!(result, Value::Integer(24)));

        let result = prim_multiply(&[Value::integer(2), Value::real(1.5)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 3.0).abs() < f64::EPSILON));
    }

    #[test]
    fn test_divide() {
        let result = prim_divide(&[Value::integer(2)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 0.5).abs() < f64::EPSILON));

        let result = prim_divide(&[Value::integer(10), Value::integer(2)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 5.0).abs() < f64::EPSILON));

        // Division by zero should error
        assert!(prim_divide(&[Value::integer(1), Value::integer(0)]).is_err());
    }

    #[test]
    fn test_quotient() {
        let result = prim_quotient(&[Value::integer(10), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Integer(3)));

        let result = prim_quotient(&[Value::integer(-10), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Integer(-3)));
    }

    #[test]
    fn test_remainder() {
        let result = prim_remainder(&[Value::integer(10), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Integer(1)));

        let result = prim_remainder(&[Value::integer(-10), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Integer(-1)));
    }

    #[test]
    fn test_modulo() {
        let result = prim_modulo(&[Value::integer(10), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Integer(1)));

        let result = prim_modulo(&[Value::integer(-10), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Integer(2))); // Euclidean modulo
    }

    #[test]
    fn test_num_eq() {
        let result = prim_num_eq(&[Value::integer(5), Value::integer(5)]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_num_eq(&[Value::integer(5), Value::integer(6)]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        let result = prim_num_eq(&[Value::integer(5), Value::real(5.0)]).unwrap();
        assert!(matches!(result, Value::Bool(true)));
    }

    #[test]
    fn test_num_lt() {
        let result = prim_num_lt(&[Value::integer(1), Value::integer(2), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_num_lt(&[Value::integer(1), Value::integer(3), Value::integer(2)]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_num_gt() {
        let result = prim_num_gt(&[Value::integer(3), Value::integer(2), Value::integer(1)]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_num_gt(&[Value::integer(3), Value::integer(1), Value::integer(2)]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_num_le() {
        let result = prim_num_le(&[Value::integer(1), Value::integer(2), Value::integer(2)]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_num_le(&[Value::integer(2), Value::integer(1)]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_num_ge() {
        let result = prim_num_ge(&[Value::integer(3), Value::integer(2), Value::integer(2)]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_num_ge(&[Value::integer(1), Value::integer(2)]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_number_p() {
        assert!(matches!(prim_number_p(&[Value::integer(42)]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_number_p(&[Value::real(3.14)]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_number_p(&[Value::string("hello".to_string())]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_integer_p() {
        assert!(matches!(prim_integer_p(&[Value::integer(42)]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_integer_p(&[Value::real(3.14)]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_real_p() {
        assert!(matches!(prim_real_p(&[Value::real(3.14)]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_real_p(&[Value::integer(42)]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_zero_p() {
        assert!(matches!(prim_zero_p(&[Value::integer(0)]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_zero_p(&[Value::integer(1)]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_zero_p(&[Value::real(0.0)]).unwrap(), Value::Bool(true)));
    }

    #[test]
    fn test_positive_p() {
        assert!(matches!(prim_positive_p(&[Value::integer(5)]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_positive_p(&[Value::integer(-5)]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_positive_p(&[Value::integer(0)]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_negative_p() {
        assert!(matches!(prim_negative_p(&[Value::integer(-5)]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_negative_p(&[Value::integer(5)]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_negative_p(&[Value::integer(0)]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_odd_p() {
        assert!(matches!(prim_odd_p(&[Value::integer(3)]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_odd_p(&[Value::integer(4)]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_even_p() {
        assert!(matches!(prim_even_p(&[Value::integer(4)]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_even_p(&[Value::integer(3)]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_abs() {
        let result = prim_abs(&[Value::integer(-5)]).unwrap();
        assert!(matches!(result, Value::Integer(5)));

        let result = prim_abs(&[Value::real(-3.14)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 3.14).abs() < f64::EPSILON));
    }

    #[test]
    fn test_max() {
        let result = prim_max(&[Value::integer(1), Value::integer(5), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Integer(5)));

        let result = prim_max(&[Value::integer(1), Value::real(5.5), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 5.5).abs() < f64::EPSILON));
    }

    #[test]
    fn test_min() {
        let result = prim_min(&[Value::integer(5), Value::integer(1), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Integer(1)));

        let result = prim_min(&[Value::integer(5), Value::real(0.5), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 0.5).abs() < f64::EPSILON));
    }

    #[test]
    fn test_gcd() {
        // gcd of two numbers
        let result = prim_gcd(&[Value::integer(12), Value::integer(8)]).unwrap();
        assert!(matches!(result, Value::Integer(4)));

        // gcd of multiple numbers
        let result = prim_gcd(&[Value::integer(12), Value::integer(18), Value::integer(24)]).unwrap();
        assert!(matches!(result, Value::Integer(6)));

        // gcd with negative numbers
        let result = prim_gcd(&[Value::integer(-12), Value::integer(8)]).unwrap();
        assert!(matches!(result, Value::Integer(4)));

        // gcd with zero
        let result = prim_gcd(&[Value::integer(0), Value::integer(5)]).unwrap();
        assert!(matches!(result, Value::Integer(5)));

        // gcd with no arguments
        let result = prim_gcd(&[]).unwrap();
        assert!(matches!(result, Value::Integer(0)));

        // Non-integer should error
        assert!(prim_gcd(&[Value::real(12.5), Value::integer(8)]).is_err());
    }

    #[test]
    fn test_lcm() {
        // lcm of two numbers
        let result = prim_lcm(&[Value::integer(4), Value::integer(6)]).unwrap();
        assert!(matches!(result, Value::Integer(12)));

        // lcm of multiple numbers
        let result = prim_lcm(&[Value::integer(2), Value::integer(3), Value::integer(4)]).unwrap();
        assert!(matches!(result, Value::Integer(12)));

        // lcm with negative numbers
        let result = prim_lcm(&[Value::integer(-4), Value::integer(6)]).unwrap();
        assert!(matches!(result, Value::Integer(12)));

        // lcm with zero
        let result = prim_lcm(&[Value::integer(0), Value::integer(5)]).unwrap();
        assert!(matches!(result, Value::Integer(0)));

        // lcm with no arguments
        let result = prim_lcm(&[]).unwrap();
        assert!(matches!(result, Value::Integer(1)));

        // Non-integer should error
        assert!(prim_lcm(&[Value::real(4.5), Value::integer(6)]).is_err());
    }

    #[test]
    fn test_floor() {
        let result = prim_floor(&[Value::real(3.7)]).unwrap();
        assert!(matches!(result, Value::Integer(3)));

        let result = prim_floor(&[Value::real(-3.7)]).unwrap();
        assert!(matches!(result, Value::Integer(-4)));
    }

    #[test]
    fn test_ceiling() {
        let result = prim_ceiling(&[Value::real(3.2)]).unwrap();
        assert!(matches!(result, Value::Integer(4)));

        let result = prim_ceiling(&[Value::real(-3.2)]).unwrap();
        assert!(matches!(result, Value::Integer(-3)));
    }

    #[test]
    fn test_truncate() {
        let result = prim_truncate(&[Value::real(3.7)]).unwrap();
        assert!(matches!(result, Value::Integer(3)));

        let result = prim_truncate(&[Value::real(-3.7)]).unwrap();
        assert!(matches!(result, Value::Integer(-3)));
    }

    #[test]
    fn test_round() {
        let result = prim_round(&[Value::real(3.5)]).unwrap();
        assert!(matches!(result, Value::Integer(4)));

        let result = prim_round(&[Value::real(3.4)]).unwrap();
        assert!(matches!(result, Value::Integer(3)));
    }

    // =========================================================================
    // String primitive tests
    // =========================================================================

    #[test]
    fn test_string_length() {
        let s = Value::string("hello".to_string());
        let result = prim_string_length(&[s]).unwrap();
        assert!(matches!(result, Value::Integer(5)));

        let empty = Value::string(String::new());
        let result = prim_string_length(&[empty]).unwrap();
        assert!(matches!(result, Value::Integer(0)));
    }

    #[test]
    fn test_string_ref() {
        let s = Value::string("hello".to_string());
        let result = prim_string_ref(&[s.clone(), Value::integer(0)]).unwrap();
        assert!(matches!(result, Value::Char('h')));

        let result = prim_string_ref(&[s, Value::integer(4)]).unwrap();
        assert!(matches!(result, Value::Char('o')));
    }

    #[test]
    fn test_string_append() {
        let s1 = Value::string("hello".to_string());
        let s2 = Value::string(" ".to_string());
        let s3 = Value::string("world".to_string());
        let result = prim_string_append(&[s1, s2, s3]).unwrap();

        if let Value::String(ref s) = result {
            assert_eq!(&**s, "hello world");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_substring() {
        let s = Value::string("hello".to_string());
        let result = prim_substring(&[s, Value::integer(1), Value::integer(4)]).unwrap();

        if let Value::String(ref s) = result {
            assert_eq!(&**s, "ell");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_string_eq() {
        let s1 = Value::string("hello".to_string());
        let s2 = Value::string("hello".to_string());
        let s3 = Value::string("world".to_string());

        let result = prim_string_eq(&[s1.clone(), s2]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_eq(&[s1, s3]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_string_lt() {
        let s1 = Value::string("abc".to_string());
        let s2 = Value::string("def".to_string());

        let result = prim_string_lt(&[s1.clone(), s2.clone()]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_lt(&[s2, s1]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_string_ci_eq() {
        // Case-insensitive equal
        let result = prim_string_ci_eq(&[
            Value::string("Hello".to_string()),
            Value::string("hello".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_ci_eq(&[
            Value::string("WORLD".to_string()),
            Value::string("world".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_ci_eq(&[
            Value::string("hello".to_string()),
            Value::string("world".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_string_ci_lt() {
        // Case-insensitive less than
        let result = prim_string_ci_lt(&[
            Value::string("ABC".to_string()),
            Value::string("def".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_ci_lt(&[
            Value::string("abc".to_string()),
            Value::string("DEF".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_ci_lt(&[
            Value::string("def".to_string()),
            Value::string("ABC".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_string_ci_gt() {
        // Case-insensitive greater than
        let result = prim_string_ci_gt(&[
            Value::string("DEF".to_string()),
            Value::string("abc".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_ci_gt(&[
            Value::string("def".to_string()),
            Value::string("ABC".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_ci_gt(&[
            Value::string("ABC".to_string()),
            Value::string("def".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_string_ci_le() {
        // Case-insensitive less than or equal
        let result = prim_string_ci_le(&[
            Value::string("ABC".to_string()),
            Value::string("def".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_ci_le(&[
            Value::string("Hello".to_string()),
            Value::string("hello".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_ci_le(&[
            Value::string("def".to_string()),
            Value::string("ABC".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_string_ci_ge() {
        // Case-insensitive greater than or equal
        let result = prim_string_ci_ge(&[
            Value::string("DEF".to_string()),
            Value::string("abc".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_ci_ge(&[
            Value::string("Hello".to_string()),
            Value::string("hello".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_ci_ge(&[
            Value::string("ABC".to_string()),
            Value::string("def".to_string()),
        ]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_make_string() {
        let result = prim_make_string(&[Value::integer(5), Value::char('a')]).unwrap();

        if let Value::String(ref s) = result {
            assert_eq!(&**s, "aaaaa");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_string() {
        let result = prim_string(&[
            Value::char('h'),
            Value::char('i'),
        ]).unwrap();

        if let Value::String(ref s) = result {
            assert_eq!(&**s, "hi");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_string_to_list() {
        let s = Value::string("hi".to_string());
        let result = prim_string_to_list(&[s]).unwrap();

        // Should be a list of 2 characters
        let len = prim_length(&[result.clone()]).unwrap();
        assert!(matches!(len, Value::Integer(2)));

        let first = prim_car(&[result]).unwrap();
        assert!(matches!(first, Value::Char('h')));
    }

    #[test]
    fn test_list_to_string() {
        let list = prim_list(&[
            Value::char('h'),
            Value::char('i'),
        ]).unwrap();

        let result = prim_list_to_string(&[list]).unwrap();

        if let Value::String(ref s) = result {
            assert_eq!(&**s, "hi");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_symbol_to_string() {
        let sym = Value::symbol("foo");
        let result = prim_symbol_to_string(&[sym]).unwrap();

        if let Value::String(ref s) = result {
            assert_eq!(&**s, "foo");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_string_to_symbol() {
        let s = Value::string("foo".to_string());
        let result = prim_string_to_symbol(&[s]).unwrap();

        assert!(matches!(result, Value::Symbol(_)));
    }

    #[test]
    fn test_string_p() {
        assert!(matches!(prim_string_p(&[Value::string("hello".to_string())]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_string_p(&[Value::integer(42)]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_symbol_p() {
        assert!(matches!(prim_symbol_p(&[Value::symbol("foo")]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_symbol_p(&[Value::string("foo".to_string())]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_char_p() {
        assert!(matches!(prim_char_p(&[Value::char('a')]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_char_p(&[Value::integer(65)]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_char_eq() {
        let result = prim_char_eq(&[Value::char('a'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_eq(&[Value::char('a'), Value::char('b')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_char_lt() {
        let result = prim_char_lt(&[Value::char('a'), Value::char('b')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_lt(&[Value::char('b'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_char_upcase() {
        let result = prim_char_upcase(&[Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Char('A')));
    }

    #[test]
    fn test_char_downcase() {
        let result = prim_char_downcase(&[Value::char('Z')]).unwrap();
        assert!(matches!(result, Value::Char('z')));
    }

    #[test]
    fn test_char_le() {
        let result = prim_char_le(&[Value::char('a'), Value::char('b')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_le(&[Value::char('a'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_le(&[Value::char('b'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_char_ge() {
        let result = prim_char_ge(&[Value::char('b'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_ge(&[Value::char('a'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_ge(&[Value::char('a'), Value::char('b')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_char_to_integer() {
        let result = prim_char_to_integer(&[Value::char('A')]).unwrap();
        assert!(matches!(result, Value::Integer(65)));

        let result = prim_char_to_integer(&[Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Integer(97)));

        let result = prim_char_to_integer(&[Value::char('0')]).unwrap();
        assert!(matches!(result, Value::Integer(48)));

        // Non-char should error
        assert!(prim_char_to_integer(&[Value::integer(65)]).is_err());
    }

    #[test]
    fn test_integer_to_char() {
        let result = prim_integer_to_char(&[Value::integer(65)]).unwrap();
        assert!(matches!(result, Value::Char('A')));

        let result = prim_integer_to_char(&[Value::integer(97)]).unwrap();
        assert!(matches!(result, Value::Char('a')));

        let result = prim_integer_to_char(&[Value::integer(48)]).unwrap();
        assert!(matches!(result, Value::Char('0')));

        // Invalid code point should error
        assert!(prim_integer_to_char(&[Value::integer(-1)]).is_err());
        assert!(prim_integer_to_char(&[Value::integer(0x200000)]).is_err());

        // Non-integer should error
        assert!(prim_integer_to_char(&[Value::char('A')]).is_err());
    }

    #[test]
    fn test_char_alphabetic_p() {
        let result = prim_char_alphabetic_p(&[Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_alphabetic_p(&[Value::char('Z')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_alphabetic_p(&[Value::char('5')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        let result = prim_char_alphabetic_p(&[Value::char(' ')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        // Non-char should error
        assert!(prim_char_alphabetic_p(&[Value::integer(97)]).is_err());
    }

    #[test]
    fn test_char_numeric_p() {
        let result = prim_char_numeric_p(&[Value::char('5')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_numeric_p(&[Value::char('0')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_numeric_p(&[Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        let result = prim_char_numeric_p(&[Value::char(' ')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        // Non-char should error
        assert!(prim_char_numeric_p(&[Value::integer(5)]).is_err());
    }

    #[test]
    fn test_char_whitespace_p() {
        let result = prim_char_whitespace_p(&[Value::char(' ')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_whitespace_p(&[Value::char('\t')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_whitespace_p(&[Value::char('\n')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_whitespace_p(&[Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        let result = prim_char_whitespace_p(&[Value::char('5')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        // Non-char should error
        assert!(prim_char_whitespace_p(&[Value::integer(32)]).is_err());
    }

    #[test]
    fn test_char_ci_eq() {
        // Case-insensitive equal
        let result = prim_char_ci_eq(&[Value::char('a'), Value::char('A')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_ci_eq(&[Value::char('Z'), Value::char('z')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_ci_eq(&[Value::char('a'), Value::char('b')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        // Non-char should error
        assert!(prim_char_ci_eq(&[Value::integer(97), Value::char('a')]).is_err());
    }

    #[test]
    fn test_char_ci_lt() {
        // Case-insensitive less than
        let result = prim_char_ci_lt(&[Value::char('A'), Value::char('b')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_ci_lt(&[Value::char('a'), Value::char('B')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_ci_lt(&[Value::char('B'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        let result = prim_char_ci_lt(&[Value::char('A'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_char_ci_gt() {
        // Case-insensitive greater than
        let result = prim_char_ci_gt(&[Value::char('B'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_ci_gt(&[Value::char('b'), Value::char('A')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_ci_gt(&[Value::char('A'), Value::char('b')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        let result = prim_char_ci_gt(&[Value::char('A'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_char_ci_le() {
        // Case-insensitive less than or equal
        let result = prim_char_ci_le(&[Value::char('A'), Value::char('b')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_ci_le(&[Value::char('A'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_ci_le(&[Value::char('B'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_char_ci_ge() {
        // Case-insensitive greater than or equal
        let result = prim_char_ci_ge(&[Value::char('B'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_ci_ge(&[Value::char('A'), Value::char('a')]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_char_ci_ge(&[Value::char('A'), Value::char('b')]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    // =========================================================================
    // Boolean and equality primitive tests
    // =========================================================================

    #[test]
    fn test_not() {
        let result = prim_not(&[Value::bool(true)]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        let result = prim_not(&[Value::bool(false)]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        // #f is the only false value in Scheme
        let result = prim_not(&[Value::integer(0)]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_boolean_p() {
        assert!(matches!(prim_boolean_p(&[Value::bool(true)]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_boolean_p(&[Value::bool(false)]).unwrap(), Value::Bool(true)));
        assert!(matches!(prim_boolean_p(&[Value::integer(1)]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_boolean_p(&[Value::Nil]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_equal_p() {
        // Numbers
        let result = prim_equal_p(&[Value::integer(42), Value::integer(42)]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_equal_p(&[Value::integer(42), Value::integer(43)]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        // Strings
        let result = prim_equal_p(&[Value::string("hello".to_string()), Value::string("hello".to_string())]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        // Lists
        let list1 = prim_list(&[Value::integer(1), Value::integer(2)]).unwrap();
        let list2 = prim_list(&[Value::integer(1), Value::integer(2)]).unwrap();
        let result = prim_equal_p(&[list1, list2]).unwrap();
        assert!(matches!(result, Value::Bool(true)));
    }

    #[test]
    fn test_eqv_p() {
        // Numbers
        let result = prim_eqv_p(&[Value::integer(42), Value::integer(42)]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        // Symbols
        let sym1 = Value::symbol("foo");
        let sym2 = Value::symbol("foo");
        let result = prim_eqv_p(&[sym1, sym2]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        // Different types
        let result = prim_eqv_p(&[Value::integer(42), Value::string("42".to_string())]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_eq_p() {
        // Numbers
        let result = prim_eq_p(&[Value::integer(42), Value::integer(42)]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        // Symbols
        let sym1 = Value::symbol("foo");
        let sym2 = Value::symbol("foo");
        let result = prim_eq_p(&[sym1, sym2]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        // Same pair should be eq?
        let pair = Value::cons(Value::integer(1), Value::integer(2));
        let result = prim_eq_p(&[pair.clone(), pair]).unwrap();
        assert!(matches!(result, Value::Bool(true)));
    }

    #[test]
    fn test_procedure_p() {
        // Primitives are procedures
        let proc = Value::primitive("+", prim_add);
        assert!(matches!(prim_procedure_p(&[proc]).unwrap(), Value::Bool(true)));

        // Non-procedures
        assert!(matches!(prim_procedure_p(&[Value::integer(42)]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_procedure_p(&[Value::string("hello".to_string())]).unwrap(), Value::Bool(false)));
    }

    // =========================================================================
    // I/O and utility primitive tests
    // =========================================================================

    #[test]
    fn test_error() {
        // Error with string message
        let result = prim_error(&[Value::string("test error".to_string())]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "test error");

        // Error with symbol message
        let result = prim_error(&[Value::symbol("error-symbol")]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("error-symbol"));

        // Error with additional objects
        let result = prim_error(&[
            Value::string("error:".to_string()),
            Value::integer(42),
            Value::string("foo".to_string()),
        ]);
        assert!(result.is_err());
        let msg = result.unwrap_err();
        assert!(msg.contains("error:"));
        assert!(msg.contains("42"));
    }

    #[test]
    fn test_display() {
        // Display returns unspecified
        let result = prim_display(&[Value::string("test".to_string())]).unwrap();
        assert!(matches!(result, Value::Unspecified));

        let result = prim_display(&[Value::integer(42)]).unwrap();
        assert!(matches!(result, Value::Unspecified));

        let result = prim_display(&[Value::Char('x')]).unwrap();
        assert!(matches!(result, Value::Unspecified));
    }

    #[test]
    fn test_newline() {
        let result = prim_newline(&[]).unwrap();
        assert!(matches!(result, Value::Unspecified));

        // Should error with arguments
        let result = prim_newline(&[Value::integer(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn test_write() {
        let result = prim_write(&[Value::string("test".to_string())]).unwrap();
        assert!(matches!(result, Value::Unspecified));

        let result = prim_write(&[Value::integer(42)]).unwrap();
        assert!(matches!(result, Value::Unspecified));
    }

    // =========================================================================
    // Conversion primitive tests
    // =========================================================================

    #[test]
    fn test_number_to_string() {
        let result = prim_number_to_string(&[Value::integer(42)]).unwrap();
        if let Value::String(ref s) = result {
            assert_eq!(&***s, "42");
        } else {
            panic!("Expected string");
        }

        let result = prim_number_to_string(&[Value::real(3.14)]).unwrap();
        if let Value::String(ref s) = result {
            assert!(s.starts_with("3.14"));
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_string_to_number() {
        let result = prim_string_to_number(&[Value::string("42".to_string())]).unwrap();
        assert!(matches!(result, Value::Integer(42)));

        let result = prim_string_to_number(&[Value::string("3.14".to_string())]).unwrap();
        assert!(matches!(result, Value::Real(_)));

        // Invalid number returns #f
        let result = prim_string_to_number(&[Value::string("not-a-number".to_string())]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    // =========================================================================
    // Keyword primitive tests
    // =========================================================================

    #[test]
    fn test_keyword_p() {
        let kw = Value::keyword("test");
        assert!(matches!(prim_keyword_p(&[kw]).unwrap(), Value::Bool(true)));

        assert!(matches!(prim_keyword_p(&[Value::symbol("test")]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_keyword_p(&[Value::string("test".to_string())]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_keyword_to_string() {
        let kw = Value::keyword("test");
        let result = prim_keyword_to_string(&[kw]).unwrap();
        if let Value::String(ref s) = result {
            assert_eq!(&***s, "test");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_string_to_keyword() {
        let result = prim_string_to_keyword(&[Value::string("test".to_string())]).unwrap();
        assert!(matches!(result, Value::Keyword(_)));
    }

    // =========================================================================
    // List utility primitive tests
    // =========================================================================

    #[test]
    fn test_memq() {
        let list = prim_list(&[Value::integer(1), Value::integer(2), Value::integer(3)]).unwrap();
        let result = prim_memq(&[Value::integer(2), list.clone()]).unwrap();
        assert!(result.is_list());

        // Not found
        let result = prim_memq(&[Value::integer(99), list]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_memv() {
        let list = prim_list(&[Value::integer(1), Value::integer(2), Value::integer(3)]).unwrap();
        let result = prim_memv(&[Value::integer(2), list.clone()]).unwrap();
        assert!(result.is_list());

        // Not found
        let result = prim_memv(&[Value::integer(99), list]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_member() {
        let list = prim_list(&[
            Value::string("a".to_string()),
            Value::string("b".to_string()),
            Value::string("c".to_string()),
        ])
        .unwrap();
        let result = prim_member(&[Value::string("b".to_string()), list.clone()]).unwrap();
        assert!(result.is_list());

        // Not found
        let result = prim_member(&[Value::string("z".to_string()), list]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_assq() {
        // Create association list: ((a . 1) (b . 2) (c . 3))
        let pair1 = Value::cons(Value::symbol("a"), Value::integer(1));
        let pair2 = Value::cons(Value::symbol("b"), Value::integer(2));
        let pair3 = Value::cons(Value::symbol("c"), Value::integer(3));
        let alist = prim_list(&[pair1, pair2, pair3]).unwrap();

        let result = prim_assq(&[Value::symbol("b"), alist.clone()]).unwrap();
        assert!(result.is_pair());

        // Not found
        let result = prim_assq(&[Value::symbol("z"), alist]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_assv() {
        // Create association list: ((1 . a) (2 . b) (3 . c))
        let pair1 = Value::cons(Value::integer(1), Value::symbol("a"));
        let pair2 = Value::cons(Value::integer(2), Value::symbol("b"));
        let pair3 = Value::cons(Value::integer(3), Value::symbol("c"));
        let alist = prim_list(&[pair1, pair2, pair3]).unwrap();

        let result = prim_assv(&[Value::integer(2), alist.clone()]).unwrap();
        assert!(result.is_pair());

        // Not found
        let result = prim_assv(&[Value::integer(99), alist]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_assoc() {
        // Create association list: (("a" . 1) ("b" . 2) ("c" . 3))
        let pair1 = Value::cons(Value::string("a".to_string()), Value::integer(1));
        let pair2 = Value::cons(Value::string("b".to_string()), Value::integer(2));
        let pair3 = Value::cons(Value::string("c".to_string()), Value::integer(3));
        let alist = prim_list(&[pair1, pair2, pair3]).unwrap();

        let result = prim_assoc(&[Value::string("b".to_string()), alist.clone()]).unwrap();
        assert!(result.is_pair());

        // Not found
        let result = prim_assoc(&[Value::string("z".to_string()), alist]).unwrap();
        assert!(matches!(result, Value::Bool(false)));
    }

    // =========================================================================
    // cXXr combination tests
    // =========================================================================

    #[test]
    fn test_cadr() {
        // (cadr '(1 2 3)) => 2
        let list = prim_list(&[Value::integer(1), Value::integer(2), Value::integer(3)]).unwrap();
        let result = prim_cadr(&[list]).unwrap();
        assert!(matches!(result, Value::Integer(2)));
    }

    #[test]
    fn test_caddr() {
        // (caddr '(1 2 3 4)) => 3
        let list = prim_list(&[
            Value::integer(1),
            Value::integer(2),
            Value::integer(3),
            Value::integer(4),
        ])
        .unwrap();
        let result = prim_caddr(&[list]).unwrap();
        assert!(matches!(result, Value::Integer(3)));
    }

    #[test]
    fn test_cadddr() {
        // (cadddr '(1 2 3 4 5)) => 4
        let list = prim_list(&[
            Value::integer(1),
            Value::integer(2),
            Value::integer(3),
            Value::integer(4),
            Value::integer(5),
        ])
        .unwrap();
        let result = prim_cadddr(&[list]).unwrap();
        assert!(matches!(result, Value::Integer(4)));
    }

    #[test]
    fn test_caar() {
        // (caar '((1 2) 3)) => 1
        let inner = prim_list(&[Value::integer(1), Value::integer(2)]).unwrap();
        let outer = prim_list(&[inner, Value::integer(3)]).unwrap();
        let result = prim_caar(&[outer]).unwrap();
        assert!(matches!(result, Value::Integer(1)));
    }

    #[test]
    fn test_cddr() {
        // (cddr '(1 2 3 4)) => (3 4)
        let list = prim_list(&[
            Value::integer(1),
            Value::integer(2),
            Value::integer(3),
            Value::integer(4),
        ])
        .unwrap();
        let result = prim_cddr(&[list]).unwrap();
        assert!(result.is_list());
        // First element should be 3
        let first = prim_car(&[result.clone()]).unwrap();
        assert!(matches!(first, Value::Integer(3)));
    }

    #[test]
    fn test_cdar() {
        // (cdar '((1 2 3) 4)) => (2 3)
        let inner = prim_list(&[Value::integer(1), Value::integer(2), Value::integer(3)]).unwrap();
        let outer = prim_list(&[inner, Value::integer(4)]).unwrap();
        let result = prim_cdar(&[outer]).unwrap();
        assert!(result.is_list());
        let first = prim_car(&[result]).unwrap();
        assert!(matches!(first, Value::Integer(2)));
    }

    #[test]
    fn test_caaar() {
        // (caaar '(((1 2) 3) 4)) => 1
        let innermost = prim_list(&[Value::integer(1), Value::integer(2)]).unwrap();
        let middle = prim_list(&[innermost, Value::integer(3)]).unwrap();
        let outer = prim_list(&[middle, Value::integer(4)]).unwrap();
        let result = prim_caaar(&[outer]).unwrap();
        assert!(matches!(result, Value::Integer(1)));
    }

    #[test]
    fn test_cdaar() {
        // (cdaar '(((1 2) 3) 4)) => (2)
        let innermost = prim_list(&[Value::integer(1), Value::integer(2)]).unwrap();
        let middle = prim_list(&[innermost, Value::integer(3)]).unwrap();
        let outer = prim_list(&[middle, Value::integer(4)]).unwrap();
        let result = prim_cdaar(&[outer]).unwrap();
        assert!(result.is_list());
        let first = prim_car(&[result]).unwrap();
        assert!(matches!(first, Value::Integer(2)));
    }

    #[test]
    fn test_cadar() {
        // (cadar '((1 2 3) 4)) => 2
        let inner = prim_list(&[Value::integer(1), Value::integer(2), Value::integer(3)]).unwrap();
        let outer = prim_list(&[inner, Value::integer(4)]).unwrap();
        let result = prim_cadar(&[outer]).unwrap();
        assert!(matches!(result, Value::Integer(2)));
    }

    #[test]
    fn test_cddar() {
        // (cddar '((1 2 3 4) 5)) => (3 4)
        let inner = prim_list(&[Value::integer(1), Value::integer(2), Value::integer(3), Value::integer(4)]).unwrap();
        let outer = prim_list(&[inner, Value::integer(5)]).unwrap();
        let result = prim_cddar(&[outer]).unwrap();
        assert!(result.is_list());
        let first = prim_car(&[result]).unwrap();
        assert!(matches!(first, Value::Integer(3)));
    }

    #[test]
    fn test_caadr() {
        // (caadr '(1 (2 3) 4)) => 2
        let inner = prim_list(&[Value::integer(2), Value::integer(3)]).unwrap();
        let outer = prim_list(&[Value::integer(1), inner, Value::integer(4)]).unwrap();
        let result = prim_caadr(&[outer]).unwrap();
        assert!(matches!(result, Value::Integer(2)));
    }

    #[test]
    fn test_cdadr() {
        // (cdadr '(1 (2 3 4) 5)) => (3 4)
        let inner = prim_list(&[Value::integer(2), Value::integer(3), Value::integer(4)]).unwrap();
        let outer = prim_list(&[Value::integer(1), inner, Value::integer(5)]).unwrap();
        let result = prim_cdadr(&[outer]).unwrap();
        assert!(result.is_list());
        let first = prim_car(&[result]).unwrap();
        assert!(matches!(first, Value::Integer(3)));
    }

    #[test]
    fn test_last() {
        // (last '(1 2 3 4)) => 4
        let list = prim_list(&[Value::integer(1), Value::integer(2), Value::integer(3), Value::integer(4)]).unwrap();
        let result = prim_last(&[list]).unwrap();
        assert!(matches!(result, Value::Integer(4)));

        // (last '(1)) => 1
        let single = prim_list(&[Value::integer(1)]).unwrap();
        let result = prim_last(&[single]).unwrap();
        assert!(matches!(result, Value::Integer(1)));

        // Empty list should error
        assert!(prim_last(&[Value::Nil]).is_err());
    }

    // =========================================================================
    // DSSSL type stub tests
    // =========================================================================

    #[test]
    fn test_quantity_p() {
        // All types should return #f since quantities are not implemented
        assert!(matches!(prim_quantity_p(&[Value::integer(1)]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_quantity_p(&[Value::string("10pt".to_string())]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_quantity_p(&[Value::symbol("quantity")]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_color_p() {
        // All types should return #f since colors are not implemented
        assert!(matches!(prim_color_p(&[Value::string("red".to_string())]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_color_p(&[Value::integer(0)]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_color_p(&[Value::symbol("color")]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_address_p() {
        // All types should return #f since addresses are not implemented
        assert!(matches!(prim_address_p(&[Value::string("addr".to_string())]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_address_p(&[Value::integer(0)]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_address_p(&[Value::symbol("address")]).unwrap(), Value::Bool(false)));
    }

    // =========================================================================
    // Format function tests
    // =========================================================================

    #[test]
    fn test_format_number_decimal() {
        let result = prim_format_number(&[Value::integer(42), Value::string("1".to_string())]).unwrap();
        if let Value::String(ref s) = result {
            assert_eq!(&***s, "42");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_format_number_roman_upper() {
        let result = prim_format_number(&[Value::integer(5), Value::string("I".to_string())]).unwrap();
        if let Value::String(ref s) = result {
            assert_eq!(&***s, "V");
        } else {
            panic!("Expected string");
        }

        let result = prim_format_number(&[Value::integer(10), Value::string("I".to_string())]).unwrap();
        if let Value::String(ref s) = result {
            assert_eq!(&***s, "X");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_format_number_roman_lower() {
        let result = prim_format_number(&[Value::integer(3), Value::string("i".to_string())]).unwrap();
        if let Value::String(ref s) = result {
            assert_eq!(&***s, "iii");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_format_number_alpha_upper() {
        let result = prim_format_number(&[Value::integer(1), Value::string("A".to_string())]).unwrap();
        if let Value::String(ref s) = result {
            assert_eq!(&***s, "A");
        } else {
            panic!("Expected string");
        }

        let result = prim_format_number(&[Value::integer(26), Value::string("A".to_string())]).unwrap();
        if let Value::String(ref s) = result {
            assert_eq!(&***s, "Z");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_format_number_alpha_lower() {
        let result = prim_format_number(&[Value::integer(1), Value::string("a".to_string())]).unwrap();
        if let Value::String(ref s) = result {
            assert_eq!(&***s, "a");
        } else {
            panic!("Expected string");
        }

        let result = prim_format_number(&[Value::integer(3), Value::string("a".to_string())]).unwrap();
        if let Value::String(ref s) = result {
            assert_eq!(&***s, "c");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_format_number_list() {
        let nums = prim_list(&[Value::integer(1), Value::integer(2), Value::integer(3)]).unwrap();
        let result = prim_format_number_list(&[nums.clone()]).unwrap();
        if let Value::String(ref s) = result {
            assert_eq!(&***s, "1.2.3");
        } else {
            panic!("Expected string");
        }

        // With custom separator
        let result = prim_format_number_list(&[nums, Value::string("-".to_string())]).unwrap();
        if let Value::String(ref s) = result {
            assert_eq!(&***s, "1-2-3");
        } else {
            panic!("Expected string");
        }
    }

    // =========================================================================
    // Grove query primitive tests
    // =========================================================================

    #[test]
    fn test_node_list_p() {
        // NodeList is a node-list
        let nl = prim_empty_node_list(&[]).unwrap();
        assert!(matches!(prim_node_list_p(&[nl]).unwrap(), Value::Bool(true)));

        // Other types are not node-lists
        assert!(matches!(prim_node_list_p(&[Value::integer(1)]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_node_list_p(&[Value::string("test".to_string())]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_node_list_p(&[Value::Nil]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_empty_node_list() {
        let result = prim_empty_node_list(&[]).unwrap();
        assert!(matches!(result, Value::NodeList(_)));
    }

    #[test]
    fn test_node_list_empty_p() {
        // Empty node-list is empty
        let nl = prim_empty_node_list(&[]).unwrap();
        let result = prim_node_list_empty_p(&[nl]).unwrap();
        assert!(matches!(result, Value::Bool(true)));

        // Non-node-list should error
        assert!(prim_node_list_empty_p(&[Value::integer(1)]).is_err());
    }

    #[test]
    fn test_node_list_length() {
        // Empty node-list has length 0
        let nl = prim_empty_node_list(&[]).unwrap();
        let result = prim_node_list_length(&[nl]).unwrap();
        assert!(matches!(result, Value::Integer(0)));

        // Non-node-list should error
        assert!(prim_node_list_length(&[Value::integer(1)]).is_err());
    }

    #[test]
    fn test_node_list_first() {
        // Empty node-list returns #f
        let nl = prim_empty_node_list(&[]).unwrap();
        let result = prim_node_list_first(&[nl]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        // Non-node-list should error
        assert!(prim_node_list_first(&[Value::integer(1)]).is_err());
    }

    #[test]
    fn test_node_list_rest() {
        // Rest of empty node-list is empty node-list
        let nl = prim_empty_node_list(&[]).unwrap();
        let result = prim_node_list_rest(&[nl]).unwrap();
        assert!(matches!(result, Value::NodeList(_)));

        // Non-node-list should error
        assert!(prim_node_list_rest(&[Value::integer(1)]).is_err());
    }

    #[test]
    fn test_node_list_ref() {
        // Out-of-bounds on empty node-list returns #f
        let nl = prim_empty_node_list(&[]).unwrap();
        let result = prim_node_list_ref(&[nl, Value::integer(0)]).unwrap();
        assert!(matches!(result, Value::Bool(false)));

        // Non-node-list should error
        assert!(prim_node_list_ref(&[Value::integer(1), Value::integer(0)]).is_err());

        // Non-integer index should error
        let nl = prim_empty_node_list(&[]).unwrap();
        assert!(prim_node_list_ref(&[nl, Value::string("x".to_string())]).is_err());
    }

    #[test]
    fn test_node_list_reverse() {
        // Reverse of empty node-list is empty node-list
        let nl = prim_empty_node_list(&[]).unwrap();
        let result = prim_node_list_reverse(&[nl]).unwrap();
        assert!(matches!(result, Value::NodeList(_)));

        // Non-node-list should error
        assert!(prim_node_list_reverse(&[Value::integer(1)]).is_err());
    }

    // =========================================================================
    // Math primitive tests
    // =========================================================================

    #[test]
    fn test_expt() {
        // Integer exponentiation
        let result = prim_expt(&[Value::integer(2), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Integer(8)));

        // Real exponentiation
        let result = prim_expt(&[Value::real(2.0), Value::integer(3)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 8.0).abs() < 0.001));

        // Fractional exponent
        let result = prim_expt(&[Value::integer(4), Value::real(0.5)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 2.0).abs() < 0.001));

        // Non-number should error
        assert!(prim_expt(&[Value::string("x".to_string()), Value::integer(2)]).is_err());
    }

    #[test]
    fn test_sqrt() {
        // Square root of integer
        let result = prim_sqrt(&[Value::integer(16)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 4.0).abs() < 0.001));

        // Square root of real
        let result = prim_sqrt(&[Value::real(2.0)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 1.414).abs() < 0.01));

        // Negative should error
        assert!(prim_sqrt(&[Value::integer(-1)]).is_err());

        // Non-number should error
        assert!(prim_sqrt(&[Value::string("x".to_string())]).is_err());
    }

    #[test]
    fn test_sin() {
        // sin(0) = 0
        let result = prim_sin(&[Value::integer(0)]).unwrap();
        assert!(matches!(result, Value::Real(r) if r.abs() < 0.001));

        // sin(π/2) ≈ 1
        let result = prim_sin(&[Value::real(std::f64::consts::PI / 2.0)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 1.0).abs() < 0.001));

        // Non-number should error
        assert!(prim_sin(&[Value::string("x".to_string())]).is_err());
    }

    #[test]
    fn test_cos() {
        // cos(0) = 1
        let result = prim_cos(&[Value::integer(0)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 1.0).abs() < 0.001));

        // cos(π) ≈ -1
        let result = prim_cos(&[Value::real(std::f64::consts::PI)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r + 1.0).abs() < 0.001));

        // Non-number should error
        assert!(prim_cos(&[Value::string("x".to_string())]).is_err());
    }

    #[test]
    fn test_tan() {
        // tan(0) = 0
        let result = prim_tan(&[Value::integer(0)]).unwrap();
        assert!(matches!(result, Value::Real(r) if r.abs() < 0.001));

        // tan(π/4) ≈ 1
        let result = prim_tan(&[Value::real(std::f64::consts::PI / 4.0)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 1.0).abs() < 0.001));

        // Non-number should error
        assert!(prim_tan(&[Value::string("x".to_string())]).is_err());
    }

    #[test]
    fn test_atan() {
        // atan(0) = 0
        let result = prim_atan(&[Value::integer(0)]).unwrap();
        assert!(matches!(result, Value::Real(r) if r.abs() < 0.001));

        // atan(1) ≈ π/4
        let result = prim_atan(&[Value::integer(1)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - std::f64::consts::PI / 4.0).abs() < 0.001));

        // Non-number should error
        assert!(prim_atan(&[Value::string("x".to_string())]).is_err());
    }

    #[test]
    fn test_log() {
        // log(e) ≈ 1
        let result = prim_log(&[Value::real(std::f64::consts::E)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 1.0).abs() < 0.001));

        // log(1) = 0
        let result = prim_log(&[Value::integer(1)]).unwrap();
        assert!(matches!(result, Value::Real(r) if r.abs() < 0.001));

        // Negative should error
        assert!(prim_log(&[Value::integer(-1)]).is_err());

        // Non-number should error
        assert!(prim_log(&[Value::string("x".to_string())]).is_err());
    }

    #[test]
    fn test_exp() {
        // exp(0) = 1
        let result = prim_exp(&[Value::integer(0)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - 1.0).abs() < 0.001));

        // exp(1) ≈ e
        let result = prim_exp(&[Value::integer(1)]).unwrap();
        assert!(matches!(result, Value::Real(r) if (r - std::f64::consts::E).abs() < 0.001));

        // Non-number should error
        assert!(prim_exp(&[Value::string("x".to_string())]).is_err());
    }

    // =========================================================================
    // Sosofo primitive tests
    // =========================================================================

    #[test]
    fn test_sosofo_p() {
        // Sosofo is a sosofo
        assert!(matches!(prim_sosofo_p(&[Value::Sosofo]).unwrap(), Value::Bool(true)));

        // Other types are not sosofos
        assert!(matches!(prim_sosofo_p(&[Value::integer(1)]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_sosofo_p(&[Value::string("test".to_string())]).unwrap(), Value::Bool(false)));
        let nl = prim_empty_node_list(&[]).unwrap();
        assert!(matches!(prim_sosofo_p(&[nl]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_empty_sosofo() {
        let result = prim_empty_sosofo(&[]).unwrap();
        assert!(matches!(result, Value::Sosofo));
    }

    #[test]
    fn test_literal() {
        let result = prim_literal(&[Value::string("hello".to_string())]).unwrap();
        assert!(matches!(result, Value::Sosofo));

        // Non-string should error
        assert!(prim_literal(&[Value::integer(1)]).is_err());
    }

    #[test]
    fn test_sosofo_append() {
        // Append sosofos
        let s1 = prim_empty_sosofo(&[]).unwrap();
        let s2 = prim_empty_sosofo(&[]).unwrap();
        let result = prim_sosofo_append(&[s1, s2]).unwrap();
        assert!(matches!(result, Value::Sosofo));

        // Non-sosofo should error
        assert!(prim_sosofo_append(&[Value::Sosofo, Value::integer(1)]).is_err());
    }

    // =========================================================================
    // String utility tests
    // =========================================================================

    #[test]
    fn test_string_equiv_p() {
        // Case-insensitive comparison
        let result = prim_string_equiv_p(&[
            Value::string("Hello".to_string()),
            Value::string("hello".to_string()),
        ])
        .unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_equiv_p(&[
            Value::string("HELLO".to_string()),
            Value::string("hello".to_string()),
        ])
        .unwrap();
        assert!(matches!(result, Value::Bool(true)));

        let result = prim_string_equiv_p(&[
            Value::string("hello".to_string()),
            Value::string("world".to_string()),
        ])
        .unwrap();
        assert!(matches!(result, Value::Bool(false)));

        // Non-string should error
        assert!(prim_string_equiv_p(&[Value::integer(1), Value::string("test".to_string())]).is_err());
    }

    // =========================================================================
    // Time primitive tests
    // =========================================================================

    #[test]
    fn test_time() {
        // Stub: returns unspecified
        let result = prim_time(&[]).unwrap();
        assert!(matches!(result, Value::Unspecified));
    }

    #[test]
    fn test_time_to_string() {
        // Stub: returns empty string
        let result = prim_time_to_string(&[Value::Unspecified, Value::string("".to_string())]).unwrap();
        if let Value::String(ref s) = result {
            assert_eq!(&***s, "");
        } else {
            panic!("Expected string");
        }
    }

    // =========================================================================
    // Language primitive tests
    // =========================================================================

    #[test]
    fn test_language_p() {
        // Stub: no language type, always returns #f
        assert!(matches!(prim_language_p(&[Value::Unspecified]).unwrap(), Value::Bool(false)));
        assert!(matches!(prim_language_p(&[Value::string("en".to_string())]).unwrap(), Value::Bool(false)));
    }

    #[test]
    fn test_current_language() {
        // Stub: returns unspecified
        let result = prim_current_language(&[]).unwrap();
        assert!(matches!(result, Value::Unspecified));
    }
}
