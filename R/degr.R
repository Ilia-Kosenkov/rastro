# CTOR
#' @title Degree
#' @param deg Degrees to store.
#' @param x,y \code{vec_ptype2*} arguments.
#' @param to \code{vec_cast} argument.
#' @param x_arg,y_arg \code{vec_ptype2*} error message variable names.
#' @param op,.fn Arithmetic functions/operators.
#' @param .x \code{vec_arith*} argument.
#' @param format,na_string \code{glue} flromat strings (support interpolation).
#' @param ... Additional parameters.
#' @rdname rastro_degr
#' @export
new_degr <- function(deg = double()) {
    new_vctr(vec_cast(deg, double(), x_arg = "deg") %% 360, class = "rastro_degr")
}


#' @rdname rastro_degr
#' @export
na_degr <- function() new_degr(NA_real_)

# FORMAT
#' @rdname rastro_degr
#' @export
format.rastro_degr <- function(
    x,
    format = "{deg}\u00B0",
    na_string = "NA_rastro_degr_",
    ...) {
    deg <- vec_data(x)
    result <- glue_fmt_chr(format)
    result[is.na(deg)] <- na_string

    return(result)
}

# METADATA
#' @rdname rastro_degr
#' @export
vec_ptype_abbr.rastro_degr <- function(x, ...) "degr"
#' @rdname rastro_degr
#' @export
vec_ptype_full.rastro_degr <- function(x, ...) "rastro_degr"

# PTYPE
#' @rdname rastro_degr
#' @export
vec_ptype2.rastro_degr <- function(x, y, ...) UseMethod("vec_ptype2.rastro_degr", y)

#' @rdname rastro_degr
#' @method vec_ptype2.rastro_degr default
#' @export
vec_ptype2.rastro_degr.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)

#' @rdname rastro_degr
#' @method vec_ptype2.rastro_degr rastro_degr
#' @export
vec_ptype2.rastro_degr.rastro_degr <- function(x, y, ...) new_degr()
#' @rdname rastro_degr
#' @method vec_ptype2.rastro_degr double
#' @export
vec_ptype2.rastro_degr.double <- function(x, y, ...) new_degr()
#' @rdname rastro_degr
#' @method vec_ptype2.rastro_degr integer
#' @export
vec_ptype2.rastro_degr.integer <- function(x, y, ...) new_degr()
#' @rdname rastro_degr
#' @method vec_ptype2.integer rastro_degr
#' @export
vec_ptype2.integer.rastro_degr <- function(x, y, ...) new_degr()
#' @rdname rastro_degr
#' @method vec_ptype2.double rastro_degr
#' @export
vec_ptype2.double.rastro_degr <- function(x, y, ...) new_degr()

#' @rdname rastro_degr
#' @method vec_ptype2.rastro_dec rastro_degr
#' @export
vec_ptype2.rastro_dec.rastro_degr <- function(x, y, ...) new_degr()
#' @rdname rastro_degr
#' @method vec_ptype2.rastro_ra rastro_degr
#' @export
vec_ptype2.rastro_ra.rastro_degr <- function(x, y, ...) new_degr()

#' @rdname rastro_degr
#' @method vec_ptype2.rastro_degr rastro_dec
#' @export
vec_ptype2.rastro_degr.rastro_dec <- function(x, y, ...) new_degr()
#' @rdname rastro_degr
#' @method vec_ptype2.rastro_degr rastro_ra
#' @export
vec_ptype2.rastro_degr.rastro_ra <- function(x, y, ...) new_degr()


 #LINKING PTYPE
#' @rdname rastro_degr
#' @method vec_ptype2.rastro_dec rastro_ra
#' @export
vec_ptype2.rastro_dec.rastro_ra <- function(x, y, ...) new_degr()
#' @rdname rastro_degr
#' @method vec_ptype2.rastro_ra rastro_dec
#' @export
vec_ptype2.rastro_ra.rastro_dec <- function(x, y, ...) new_degr()

#' @rdname rastro_degr
#' @export
is_degr <- function(x) vec_is(x, new_degr())


# CAST
#' @rdname rastro_degr
#' @export
vec_cast.rastro_degr <- function(x, to, ...) UseMethod("vec_cast.rastro_degr")
#' @rdname rastro_degr
#' @method vec_cast.rastro_degr default
#' @export
vec_cast.rastro_degr.default <- function(x, to, ...) vec_default_cast(x, to)
#' @rdname rastro_degr
#' @method vec_cast.rastro_degr rastro_degr
#' @export
vec_cast.rastro_degr.rastro_degr <- function(x, to, ...) x
#' @rdname rastro_degr
#' @method vec_cast.rastro_degr double
#' @export
vec_cast.rastro_degr.double <- function(x, to, ...) new_degr(x)
#' @rdname rastro_degr
#' @method vec_cast.rastro_degr integer
#' @export
vec_cast.rastro_degr.integer <- function(x, to, ...) new_degr(x)
#' @rdname rastro_degr
#' @method vec_cast.rastro_degr rastro_dec
#' @export
vec_cast.rastro_degr.rastro_dec <- function(x, to, ...) new_degr(dec_2_deg(x))
#' @rdname rastro_degr
#' @method vec_cast.rastro_degr rastro_ra
#' @export
vec_cast.rastro_degr.rastro_ra <- function(x, to, ...) new_degr(ra_2_hr(x) * 15)

#' @rdname rastro_degr
#' @method vec_cast.double rastro_degr
#' @export
vec_cast.double.rastro_degr <- function(x, to, ...) vec_data(x)
#' @rdname rastro_degr
#' @method vec_cast.integer rastro_degr
#' @export
vec_cast.integer.rastro_degr <- function(x, to, ...) vec_cast(vec_data(x), integer())
#' @rdname rastro_degr
#' @method vec_cast.rastro_dec rastro_degr
#' @export
vec_cast.rastro_dec.rastro_degr <- function(x, to, ...) new_dec_from_degr(vec_data(x))
#' @rdname rastro_degr
#' @method vec_cast.rastro_ra rastro_degr
#' @export
vec_cast.rastro_ra.rastro_degr <- function(x, to, ...) new_ra_from_hr(vec_data(x) / 15)

#' @rdname rastro_degr
#' @export
as_degr <- function(x) vec_cast(x, new_degr())

# EQUALITY
#' @rdname rastro_degr
#' @export
`%==%.rastro_degr` <- function(x, y) UseMethod("%==%.rastro_degr", y)
#' @rdname rastro_degr
#' @method %==%.rastro_degr default
#' @export
`%==%.rastro_degr.default` <- function(x, y) vec_equal(x, y) %|% FALSE

# ARITHMETIC
vec_arith.rastro_degr <- function(op, x, y, ...) UseMethod("vec_arith.rastro_degr", y)
#' @rdname rastro_degr
#' @method vec_arith.rastro_degr default
#' @export
vec_arith.rastro_degr.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
#' @rdname rastro_degr
#' @method vec_arith.rastro_degr MISSING
#' @export
vec_arith.rastro_degr.MISSING <- function(op, x, y, ...) {
    if (op %==% "-") {
        return(new_degr(-vec_data(x)))
    } else if (op %==% "+")
        return(x)
    stop_incompatible_op(op, x, y)
}
#' @rdname rastro_degr
#' @method vec_arith.rastro_degr numeric
#' @export
vec_arith.rastro_degr.numeric <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    data_x <- vec_data(x)
    switch(
        op,
        "+" = new_degr(data_x + y),
        "-" = new_degr(data_x - y),
        "*" = new_degr(data_x * y),
        "/" = new_degr(data_x / y),
        stop_incompatible_op(op, x, y))
}
#' @rdname rastro_degr
#' @method vec_arith.numeric rastro_degr
#' @export
vec_arith.numeric.rastro_degr <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    data_y <- vec_data(y)
    switch(
        op,
        "+" = new_degr(x + data_y),
        "-" = new_degr(x - data_y),
        "*" = new_degr(x * data_y),
        stop_incompatible_op(op, x, y))
}
#' @rdname rastro_degr
#' @method vec_arith.rastro_degr rastro_degr
#' @export
vec_arith.rastro_degr.rastro_degr <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    data_x <- vec_data(x)
    data_y <- vec_data(y)

    switch(
        op,
        "+" = new_degr(data_x + data_y),
        "-" = new_degr(data_x + data_y),
        stop_incompatible_op(op, x, y))
}

#' @rdname rastro_degr
#' @export
vec_math.rastro_degr <- function(.fn, .x, ...) {
    data_x <- vec_data(.x)
    switch(.fn,
           sin = sin(data_x / 180 * pi),
           cos = cos(data_x / 180 * pi),
           tan = tan(data_x / 180 * pi),
           abs = .x,
           sign = vec_repeat(1L, vec_size(data_x)),
           mean = new_degr(mean(data_x)),
           sum = new_degr(sum(data_x)),
           abort(glue_fmt_chr("`{.fn}` cannot be applied to <{vec_ptype_full(.x)}>.")))
}