# CTOR
#' @title Flux
#' @rdname rastro_flux
#'
#' @param flux,new_flux Flux values.
#' @param filter An optional filter.
#' @param unit Am optional measurement units.
#' @param x,y \code{vec_ptype2*} arguments.
#' @param to \code{vec_cast} argument.
#' @param x_arg,y_arg,to_arg \code{vec_ptype2*} and \code{vec_cast*} error message variable names.
#' @param op,.fn Arithmetic functions/operators.
#' @param .x \code{vec_arith*} argument.
#' @param format,na_string \code{glue} flromat strings (support interpolation).
#' @param ... Additional parameters.
#'
#' @export
new_flux <- function(flux = double(), filter = NA_character_, unit = NA_character_) {
    filter <- vec_assert(vec_cast(filter, character()), size = 1L)
    unit <- vec_assert(vec_cast(unit, character()), size = 1L)

    f <- vec_cast(flux, double())

    new_vctr(f, filter = filter, unit = unit, class = "rastro_flux")
}

#' @rdname rastro_flux
#' @export
na_flux <- function() new_flux(NA_real_)

# FORMAT
#' @rdname rastro_flux
#' @export
format.rastro_flux <- function(x,
    format = "{flux:%.3e}",
    na_string = "NA_rastro_flux_",
    ...) {
    flux <- vec_data(x)
    result <- glue_fmt_chr(format)
    result[is.na(flux)] <- na_string
    return(result)
}

#' @rdname rastro_flux
#' @export
obj_print_footer.rastro_flux <- function(x, ...) {
    cat(glue_fmt_chr("Unit: {(x %@% 'unit')}"))
}

# METADATA
#' @rdname rastro_flux
#' @export
vec_ptype_abbr.rastro_flux <- function(x, ...)
    glue_fmt_chr("flux<{(x %@% 'filter') %|% '?'}>")
#' @rdname rastro_flux
#' @export
vec_ptype_full.rastro_flux <- function(x, ...)
    glue_fmt_chr("rastro_flux<{(x %@% 'filter') %|% '?'}>")

# PTYPE
#' @rdname rastro_flux
#' @export
vec_ptype2.rastro_flux <- function(x, y, ...) UseMethod("vec_ptype2.rastro_flux", y)
#' @rdname rastro_flux
#' @method vec_ptype2.rastro_flux default
#' @export
vec_ptype2.rastro_flux.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
        vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
#' @rdname rastro_flux
#' @method vec_ptype2.rastro_flux rastro_flux
#' @export
vec_ptype2.rastro_flux.rastro_flux <- function(x, y, ..., x_arg = "x", y_arg = "y") {
    x_flt <- x %@% "filter"
    x_unt <- x %@% "unit"
    y_flt <- y %@% "filter"
    y_unt <- y %@% "unit"

    cnd <- ((x_flt %===% y_flt) || (is_na(x_flt) || is_na(y_flt))) &&
        ((x_unt %===% y_unt) || (is_na(x_unt) || is_na(y_unt)))

    if (cnd) {
        return(new_flux(filter = x_flt %|% y_flt, unit = x_unt %|% y_unt))
    }

    stop_incompatible_type(x, y,
        details = vec_c(
            glue_fmt_chr("Filter: `{x_arg}` has `{x_flt}`, `{y_arg}` has `{y_flt}`"),
            glue_fmt_chr("Unit: `{x_arg}` has `{x_unt}`, `{y_arg}` has `{y_unt}`")),
        x_arg = x_arg, y_arg = y_arg, ...)
}
#' @rdname rastro_flux
#' @method vec_ptype2.rastro_flux double
#' @export
vec_ptype2.rastro_flux.double <- function(x, y, ...)
    new_flux(filter = x %@% "filter", unit = x %@% "unit")
#' @rdname rastro_flux
#' @method vec_ptype2.rastro_flux integer
#' @export
vec_ptype2.rastro_flux.integer <- function(x, y, ...)
    new_flux(filter = x %@% "filter", unit = x %@% "unit")
#' @rdname rastro_flux
#' @method vec_ptype2.integer rastro_flux
#' @export
vec_ptype2.integer.rastro_flux <- function(x, y, ...)
    new_flux(filter = y %@% "filter", unit = y %@% "unit")
#' @rdname rastro_flux
#' @method vec_ptype2.double rastro_flux
#' @export
vec_ptype2.double.rastro_flux <- function(x, y, ...)
    new_flux(filter = y %@% "filter", unit = y %@% "unit")

#' @rdname rastro_flux
#' @export
is_flux <- function(x, filter = NA_character_, unit = NA_real_)
    vec_is(x, new_flux(filter = filter, unit = unit))

#' @rdname rastro_flux
#' @export
is.na.rastro_flux <- function(x) is.na(vec_data(x))

# CAST
#' @rdname rastro_flux
#' @export
vec_cast.rastro_flux <- function(x, to, ..., x_arg = "x", to_arg = "to")
    UseMethod("vec_cast.rastro_flux")
#' @rdname rastro_flux
#' @method vec_cast.rastro_flux default
#' @export
vec_cast.rastro_flux.default <- function(x, to, ..., x_arg = "x", to_arg = "to")
    vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
#' @rdname rastro_flux
#' @method vec_cast.rastro_flux rastro_flux
#' @export
vec_cast.rastro_flux.rastro_flux <- function(x, to, ..., x_arg = "x", to_arg = "to") {
    x_flt <- x %@% "filter"
    x_unt <- x %@% "unit"
    to_flt <- to %@% "filter"
    to_unt <- to %@% "unit"

    cnd1 <- ((to_flt %===% x_flt) || is_na(x_flt))
    cnd2 <- ((to_unt %===% x_unt) || is_na(x_unt))

    if (cnd1 && cnd2)
        return(new_flux(vec_data(x), filter = to_flt, unit = to_unt))

    maybe_lossy_cast(
        result = new_flux(vec_data(x), to_flt, to_unt),
        x = x, to = to,
        lossy = vec_repeat(TRUE, vec_size(x)),
        locations = vec_seq_along(x),
        details = vec_c(
            glue_fmt_chr("Filter: `{x_arg}` has `{x_flt}`, `{to_arg}` has `{to_flt}`"),
            glue_fmt_chr("Unit: `{x_arg}` has `{x_unt}`, `{to_arg}` has `{to_unt}`")))
}

#' @rdname rastro_flux
#' @method vec_cast.rastro_flux integer
#' @export
vec_cast.rastro_flux.integer <- function(x, to, ...)
    new_flux(x, filter = to %@% "filter", unit = to %@% "unit")
#' @rdname rastro_flux
#' @method vec_cast.rastro_flux double
#' @export
vec_cast.rastro_flux.double <- function(x, to, ...)
    new_flux(x, filter = to %@% "filter", unit = to %@% "unit")

#' @rdname rastro_flux
#' @method vec_cast.double rastro_flux
#' @export
vec_cast.double.rastro_flux <- function(x, to, ...) vec_data(x)
#' @rdname rastro_flux
#' @method vec_cast.integer rastro_flux
#' @export
vec_cast.integer.rastro_flux <- function(x, to, ...) vec_cast(vec_data(x), integer())

#' @rdname rastro_flux
#' @export
as_flux <- function(x, filter = NA_character_, unit = NA_real_, ...)
    vec_cast(x, new_flux(filter = filter, unit = unit))

# EQUALITY

#' @rdname rastro_flux
#' @export
`%==%.rastro_flux` <- function(x, y) UseMethod("%==%.rastro_flux", y)
#' @rdname rastro_flux
#' @method %==%.rastro_flux default
#' @export
`%==%.rastro_flux.default` <- function(x, y) vec_equal(x, y) %|% FALSE

# ARITHMETIC
#' @rdname rastro_flux
#' @export
vec_arith.rastro_flux <- function(op, x, y, ...) UseMethod("vec_arith.rastro_flux", y)
#' @rdname rastro_flux
#' @method vec_arith.rastro_flux default
#' @export
vec_arith.rastro_flux.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
#' @rdname rastro_flux
#' @method vec_arith.rastro_flux MISSING
#' @export
vec_arith.rastro_flux.MISSING <- function(op, x, y, ...) {
    if (op %===% "-") {
        return(new_flux(-vec_data(x), x %@% "filter", x %@% "unit"))
    } else if (op %===% "+")
        return(x)
    stop_incompatible_op(op, x, y)
}
#' @rdname rastro_flux
#' @method vec_arith.rastro_flux numeric
#' @export
vec_arith.rastro_flux.numeric <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    data_x <- vec_data(x)
    switch(
        op,
        "+" = new_flux(data_x + y, x %@% "filter", x %@% "unit"),
        "-" = new_flux(data_x - y, x %@% "filter", x %@% "unit"),
        "*" = new_flux(data_x * y, x %@% "filter", x %@% "unit"),
        "/" = new_flux(data_x / y, x %@% "filter", x %@% "unit"),
        stop_incompatible_op(op, x, y))
}
#' @rdname rastro_flux
#' @method vec_arith.numeric rastro_flux
#' @export
vec_arith.numeric.rastro_flux <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    data_y <- vec_data(y)
    switch(
        op,
        "+" = new_flux(x + data_y, y %@% "filter", y %@% "unit"),
        "-" = new_flux(x - data_y, y %@% "filter", y %@% "unit"),
        "*" = new_flux(x * data_y, y %@% "filter", y %@% "unit"),
        stop_incompatible_op(op, x, y))
}
#' @rdname rastro_flux
#' @method vec_arith.rastro_flux rastro_flux
#' @export
vec_arith.rastro_flux.rastro_flux <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    vec_ptype_common(x, y) -> ptype
    data_x <- vec_data(x)
    data_y <- vec_data(y)

    switch(
        op,
        "+" = new_flux(data_x + data_y, ptype %@% "filter", ptype %@% "unit"),
        "-" = new_flux(data_x - data_y, ptype %@% "filter", ptype %@% "unit"),
        stop_incompatible_op(op, x, y))
}
#' @rdname rastro_flux
#' @export
vec_math.rastro_flux <- function(.fn, .x, ...) {
    data_x <- vec_data(.x)
    print(.fn)
    switch(.fn,
           abs = new_flux(abs(data_x), .x %@% "filter", .x %@% "unit"),
           sign = vec_cast(sign(data_x), integer()),
           mean = new_flux(mean(data_x), .x %@% "filter", .x %@% "unit"),
           sum = new_flux(sum(data_x), .x %@% "filter", .x %@% "unit"),
           is.nan = is.nan(data_x),
           is.finite = is.finite(data_x),
           is.infinite = is.infinite(data_x),
           abort(glue_fmt_chr("`{.fn}` cannot be applied to <{vec_ptype_full(.x)}>.")))
}

# FLUX -> NAG conversion

#vec_cast.rastro_mag.rastro_flux <- function(x, to, ..., x_arg = "x", to_arg = "to") {
    #zf <- to %@% "zero_flux"
    #x <- vec_cast(x, zf)

    #new_mag(-2.5 * log10(vec_data(x) / vec_data(zf)), filter = zf %@% "filter", zero_flux = zf)
#}