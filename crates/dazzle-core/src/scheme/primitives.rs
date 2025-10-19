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
    env.define("floor", Value::primitive("floor", prim_floor));
    env.define("ceiling", Value::primitive("ceiling", prim_ceiling));
    env.define("truncate", Value::primitive("truncate", prim_truncate));
    env.define("round", Value::primitive("round", prim_round));
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
    env.define("char-upcase", Value::primitive("char-upcase", prim_char_upcase));
    env.define("char-downcase", Value::primitive("char-downcase", prim_char_downcase));
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
}
