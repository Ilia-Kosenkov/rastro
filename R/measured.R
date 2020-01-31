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

simplify_units <- function(units = "erg * cm ^-2 * Hz^-1 * s^-1 * s ^ 1/ 3 * erg ^ 1 / 2") {
    if (is_na(units))
        return(NA_character_)
    strsplit(units, "\\*")[[1]] -> split
    map(reg_sub(split, "([a-zA-Z]+)\\s*(?:\\^\\s*(?:([+-]?\\s*\\d+)\\s*(?:/\\s+(\\d+))?))?"),
        vec_slice, -1L) -> x

    x <- vec_rbind(!!!x)
    grps <- vec_group_loc(x$`2`)
    grps$pw <- map_chr(
        grps$loc,
        ~ format_frac(simplify_frac(as.integer(x[.x, "3"]) %|% 1L, as.integer(x[.x, "4"]) %|% 1L)))

    grps <- grps[!is.na(grps$pw),]
    grps <- grps[order(grps$key),]

    result <- paste(glue_fmt_chr("{grps$key}{grps$pw}"), collapse = " * ")
    if (!nzchar(result))
        return(NA_character_)

    return(result)
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
#vec_ptype2.rastro_measure.double <- function(x, y, ...)
    #new_measure(filter = x %@% "filter", zero_flux = x %@% "zero_flux")
#vec_ptype2.rastro_measure.integer <- function(x, y, ...)
    #new_measure(filter = x %@% "filter", zero_flux = x %@% "zero_flux")
#vec_ptype2.integer.rastro_measure <- function(x, y, ...)
    #new_measure(filter = y %@% "filter", zero_flux = y %@% "zero_flux")
#vec_ptype2.double.rastro_measure <- function(x, y, ...)
#new_measure(filter = y %@% "filter", zero_flux = y %@% "zero_flux")

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
    vec_recycle_common(x, y) %->% c(x, y)
    data_x <- vec_data(x)
    switch(
        op,
        "+" = new_measure(data_x + y, x %@% "quantity", x %@% "unit"),
        "-" = new_measure(data_x - y, x %@% "quantity", x %@% "unit"),
        "*" = new_measure(data_x * y, x %@% "quantity", x %@% "unit"),
        "/" = new_measure(data_x / y, x %@% "quantity", x %@% "unit"),
        stop_incompatible_op(op, x, y))
}
vec_arith.numeric.rastro_measure <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    data_y <- vec_data(y)
    switch(
        op,
        "+" = new_measure(x + data_y, y %@% "quantity", y %@% "unit"),
        "-" = new_measure(x - data_y, y %@% "quantity", y %@% "unit"),
        "*" = new_measure(x * data_y, y %@% "quantity", y %@% "unit"),
        stop_incompatible_op(op, x, y))
}
vec_arith.rastro_measure.rastro_measure <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    if ((op %===% "+") || (op %===% "-")) {
        vec_cast_common(x, y) %->% c(x, y)
        return(new_measure(vec_arith_base(op, x, y), x %@% "quantity", x %@% "unit"))
    }
    else if ((op %===% "*")) {
        unit <- vec_c(x %@% "unit", y %@% "unit")
        unit <- unit[!is.na(unit)]
        if (vec_is_empty(unit))
            unit <- NA_character_

        unit <- simplify_units(paste(unit, collapse = " * "))

        quantity <- vec_c(x %@% "quantity", y %@% "quantity")
        if (any(is.na(quantity)))
            quantity <- NA_character_
        quantity <- simplify_units(paste(quantity, collapse = " * "))

        return(new_measure(vec_arith_base(op, x, y), quantity = quantity, unit = unit))
    }

    stop_incompatible_op(op, x, y)
}