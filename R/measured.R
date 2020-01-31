# CTOR
new_measure <- function(
    value = double(),
    quantity = NA_character_,
    unit = NA_character_) {
    new_vctr(
        vec_cast(value, double()),
        quantity = vec_assert(vec_cast(quantity, character()), size = 1L),
        unit = simplify_units(vec_assert(vec_cast(unit, character()), size = 1L)),
        class = "rastro_measure")
}

na_measure <- function() new_measure(NA_real_)

# METHODS


simplify_units <- function(
    units = "48 * erg * cm ^-2 * Hz^-1 * s^-1 * s ^ 1/ 3 * 3 ^ 2* erg ^ 1 / 2",
    power = c(1L, 1L)) {
    if (is_na(units))
        return(NA_character_)

    power <- vec_cast(power, integer())

    strsplit(units, "\\*")[[1]] -> split
    map(reg_sub(split, "([a-zA-Z]+|(?:\\d+(?:\\.\\d+)?))\\s*(?:\\^\\s*(?:([+-]?\\s*\\d+)\\s*(?:/\\s+(\\d+))?))?"),
        vec_slice, -1L) -> detections

    detections <- vec_rbind(!!!detections)

    num_rows <- grepl("^-?\\d+", detections$`2`)
    nums <- detections[num_rows,]
    units <- detections[!num_rows, ]

    if (!vec_is_empty(nums)) {
        nums <- proc_frac(nums, power)
        prod <- prod(vmap_pt(nums, ~ as.numeric(.x$key) ^ (.x$pw[[1]][1] / .x$pw[[1]][2])))
        if (prod %===% 1)
            num <- NA_character_
        else
            num <- glue_fmt_chr("{prod:%g}")
    }
    else
        num <- NA_character_

    if (!vec_is_empty(units)) {
        units <- proc_frac(units, power)
        units$pw <- map_chr(units$pw, format_frac)
        units <- units[!is.na(units$pw),]
        units <- units[order(units$key),] 
        unit <- paste(glue_fmt_chr("{units$key}{units$pw}"), collapse = " * ")
    }
    else
        unit <- NA_character_

    result <- vec_c(num, unit)
    result <- result[!is.na(result)]
    result <- paste(result, collapse = " * ")
    if (!nzchar(result))
        return(NA_character_)

    return(result)
}

combine_units <- function(x, y, propagate_na = TRUE) {
    if (propagate_na && (is_na(x) || is_na(y)))
        return(NA_character_)

    units <- vec_c(x, y)
    units <- units[!is.na(units)]
    if (vec_is_empty(units))
        return(NA_character_)

    simplify_units(paste(units, collapse = " * "))
}

get_unit_str <- function(x)
    x %|% "dimensionless"

get_quantity_str <- function(x)
    x %|% "?"

# METADATA
vec_ptype_abbr.rastro_measure <- function(x, ...)
    glue_fmt_chr("measure_of[{get_quantity_str(x %@% 'quantity')}]")
vec_ptype_full.rastro_measure <- function(x, ...)
    glue_fmt_chr("rastro_measure_of<{get_quantity_str(x %@% 'quantity')}>")

# FORMAT
format.rastro_measure <- function(
    x,
    format = "{measure:%.2f}",
    na_string = "NA_rastro_measure_",
    ...) {
    measure <- vec_data(x)
    result <- glue_fmt_chr(format)
    result[is.na(measure)] <- na_string

    return(result)
}

obj_print_footer.rastro_measure <- function(x, ...) {
    cat(glue_fmt_chr("Unit: [{get_unit_str(x %@% 'unit')}]"))
}

# PTYPE
vec_ptype2.rastro_measure <- function(x, y, ...) UseMethod("vec_ptype2.rastro_measure", y)
vec_ptype2.rastro_measure.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
        vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
vec_ptype2.rastro_measure.rastro_measure <- function(x, y, ..., x_arg = "x", y_arg = "y") {
    x_q <- x %@% "quantity"
    y_q <- y %@% "quantity"

    x_u <- x %@% "unit"
    y_u <- y %@% "unit"

    cnd_1 <- (x_q %===% y_q) || is_na(x_q) || is_na(y_q)
    cnd_2 <- (x_u %===% y_u) || is_na(x_u) || is_na(y_u)

    if (cnd_1 && cnd_2)
       return(new_measure(quantity = x_q %|% y_q, unit = x_u %|% y_u))
    stop_incompatible_type(x, y,
               details = vec_c(
               glue_fmt_chr("Quantity: `{x_arg}` is `{get_quantity_str(x_q)}`," %&%
                " `{y_arg}` is `{get_quantity_str(y_q)}`"),
               glue_fmt_chr("Unit: `{x_arg}` is [{get_unit_str(x_u)}]," %&%
                " `{y_arg}` is [{get_unit_str(y_u)}]")),
        x_arg = x_arg, y_arg = y_arg, ...)
}

# CAST
vec_cast.rastro_measure <- function(x, to, ..., x_arg = "x", to_arg = "to")
    UseMethod("vec_cast.rastro_measure")
vec_cast.rastro_measure.default <- function(x, to, ..., x_arg = "x", to_arg = "to")
    vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
vec_cast.rastro_measure.rastro_measure <- function(x, to, ..., x_arg = "x", to_arg = "to") {
    x_q <- x %@% "quantity"
    to_q <- to %@% "quantity"

    x_u <- x %@% "unit"
    to_u <- to %@% "unit"

    cnd_1 <- (x_q %===% to_q) || is_na(x_q)
    cnd_2 <- (x_u %===% to_u) || is_na(x_u)
    if (cnd_1 && cnd_2)
        return(new_measure(vec_data(x), to_q, to_u))

    maybe_lossy_cast(
        result = new_measure(vec_data(x), to_q, to_u),
        x = x, to = to,
        lossy = vec_repeat(TRUE, vec_size(x)),
        locations = vec_seq_along(x),
        details = vec_c(
            glue_fmt_chr("Quantity: `{x_arg}` is `{get_quantity_str(x_q)}`," %&%
                " `{to_arg}` is `{get_quantity_str(to_q)}`"),
            glue_fmt_chr("Unit: `{x_arg}` is [{get_unit_str(x_u)}]," %&%
                " `{to_arg}` is [{get_unit_str(to_u)}]")))
}

vec_cast.rastro_measure.numeric <- function(x, to, ...)
    new_measure(x, quantity = to %@% "quantity", unit = to %@% "unit")

vec_cast.numeric.rastro_measure <- function(x, to, ...) vec_data(x)

as_measure <- function(x, quantity = missing_arg(), unit = missing_arg()) {
    if (is_missing(quantity))
        quantity <- x %@% "quantity"
    if (is_missing(unit))
        unit <- x %@% "unit"

    new_measure(vec_data(x), quantity, unit)
}

# ARITHMETIC
vec_arith.rastro_measure <- function(op, x, y, ...) UseMethod("vec_arith.rastro_measure", y)
vec_arith.rastro_measure.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
vec_arith.rastro_measure.MISSING <- function(op, x, y, ...) {
    if (op %===% "-") {
        return(new_measure(-vec_data(x), x %@% "quantity", x %@% "unit"))
    } else if (op %===% "+")
        return(x)
    stop_incompatible_op(op, x, y)
}
vec_arith.rastro_measure.numeric <- function(op, x, y, ...) {
    switch(
        op,
        "+" = x + new_measure(y, format(y)),
        "-" = x - new_measure(y, format(y)),
        "*" = x * new_measure(y, format(y)),
        "/" = x / new_measure(y, format(y)),
        "^" = {
            y <- vec_assert(y, size = 1L)
            cast <- allow_lossy_cast(vec_cast(y, integer()))
            cast_inv <- allow_lossy_cast(vec_cast(1 / y, integer()))
            if (y %===% cast)
                power <- vec_c(cast, 1L)
            else if ((1 / y) %===% cast_inv)
                    power <- vec_c(1L, cast_inv)
            else
                stop_incompatible_op(op, x, y)
            new_measure(
                vec_data(x) ^ y,
                simplify_units(x %@% "quantity", power = power),
                simplify_units(x %@% "unit", power = power))
        },
        stop_incompatible_op(op, x, y))
}
vec_arith.numeric.rastro_measure <- function(op, x, y, ...) {
    switch(
        op,
        "+" = new_measure(x, format(x)) + y,
        "-" = new_measure(x, format(x)) - y,
        "*" = new_measure(x, format(x)) * y,
        stop_incompatible_op(op, x, y))
}
vec_arith.rastro_measure.rastro_measure <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    if ((op %===% "+") || (op %===% "-")) {
        vec_cast_common(x, y) %->% c(x, y)
        return(new_measure(vec_arith_base(op, x, y), x %@% "quantity", x %@% "unit"))
    }
    else if (op %===% "*") {
        unit <- combine_units(x %@% "unit", y %@% "unit", FALSE)

        quantity <- combine_units(x %@% "quantity", y %@% "quantity", TRUE)

        return(new_measure(vec_arith_base(op, x, y), quantity = quantity, unit = unit))
    }
    else if (op %===% "/") {
        unit <- combine_units(
            x %@% "unit",
            simplify_units(y %@% "unit", vec_c(-1L, 1L)),
            FALSE)

        quantity <- combine_units(
            x %@% "quantity",
            simplify_units(y %@% "quantity", vec_c(-1L, 1L)),
            TRUE)

        return(new_measure(vec_arith_base(op, x, y), quantity = quantity, unit = unit))
    }

    stop_incompatible_op(op, x, y)
}