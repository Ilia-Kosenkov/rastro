# CTOR
new_mag <- function(m = double(), filter = NA_character_, zero_flux = na_flux()) {
    filter <- vec_assert(vec_cast(filter, character()), size = 1L)
    zero_flux <- vec_assert(
        vec_cast(
            zero_flux,
            new_flux(
                filter = filter,
                unit = zero_flux %@% "unit" %||% NA_character_)),
        size = 1L)

    m <- vec_cast(m, double())

    new_vctr(m, filter = filter, zero_flux = zero_flux, class = "rastro_mag")
}

na_mag <- function() new_mag(NA_real_)

# FORMAT
format.rastro_mag <- function(x,
    format = "{mag:%.3f}",
    na_string = "NA_rastro_mag_",
    ...) {
    mag <- vec_data(x)
    result <- glue_fmt_chr(format)
    result[is.na(mag)] <- na_string
    return(result)
}

obj_print_footer.rastro_mag <- function(x, ...) {
    cat(glue_fmt_chr("Zero flux: {format(x %@% 'zero_flux')}"))
}

# METADATA
vec_ptype_abbr.rastro_mag <- function(x, ...)
    glue_fmt_chr("mag<{(x %@% 'filter') %|% '?'}>")
vec_ptype_full.rastro_mag <- function(x, ...)
    glue_fmt_chr("rastro_mag<{(x %@% 'filter') %|% '?'}>")

# PTYPE
vec_ptype2.rastro_mag <- function(x, y, ...) UseMethod("vec_ptype2.rastro_mag", y)
vec_ptype2.rastro_mag.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
        vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
vec_ptype2.rastro_mag.rastro_mag <- function(x, y, ..., x_arg = "x", y_arg = "y") {
    x_flt <- x %@% "filter"
    x_zf <- x %@% "zero_flux"
    y_flt <- y %@% "filter"
    y_zf <- y %@% "zero_flux"

    cnd <- ((x_flt %===% y_flt) || (is_na(x_flt) || is_na(y_flt))) &&
        ((x_zf %===% y_zf) || (is_na(x_zf) || is_na(y_zf)))

    if (cnd) {
        flt <- x_flt %|% y_flt
        zf <- x_zf %|% y_zf
        return(new_mag(filter = flt, zero_flux = zf))
    }

    stop_incompatible_type(x, y,
        details = vec_c(
            glue_fmt_chr("Filter: `{x_arg}` has `{x_flt}`, `{y_arg}` has `{y_flt}`"),
            glue_fmt_chr("Zero flux: `{x_arg}` has `{format(x_zf)}`, `{y_arg}` has `{format(y_zf)}`")),
        x_arg = x_arg, y_arg = y_arg, ...)
}
vec_ptype2.rastro_mag.double <- function(x, y, ...)
    new_mag(filter = x %@% "filter", zero_flux = x %@% "zero_flux")
vec_ptype2.rastro_mag.integer <- function(x, y, ...)
    new_mag(filter = x %@% "filter", zero_flux = x %@% "zero_flux")
vec_ptype2.integer.rastro_mag <- function(x, y, ...)
    new_mag(filter = y %@% "filter", zero_flux = y %@% "zero_flux")
vec_ptype2.double.rastro_mag <- function(x, y, ...)
    new_mag(filter = y %@% "filter", zero_flux = y %@% "zero_flux")


is_mag <- function(x, filter = NA_character_, zero_flux = NA_real_)
    vec_is(x, new_mag(filter = filter, zero_flux = zero_flux))

is.na.rastro_mag <- function(x) is.na(vec_data(x))

# CAST
vec_cast.rastro_mag <- function(x, to, ..., x_arg = "x", to_arg = "to")
    UseMethod("vec_cast.rastro_mag")
vec_cast.rastro_mag.default <- function(x, to, ..., x_arg = "x", to_arg = "to")
    vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
vec_cast.rastro_mag.rastro_mag <- function(x, to, ..., x_arg = "x", to_arg = "to") {
    x_flt <- x %@% "filter"
    x_zf <- x %@% "zero_flux"
    to_flt <- to %@% "filter"
    to_zf <- to %@% "zero_flux"

    cnd1 <- ((to_flt %===% x_flt) || is_na(x_flt))
    cnd2 <- ((to_zf %===% x_zf) || is_na(x_zf))

    if (cnd1 && cnd2)
        return(new_mag(vec_data(x), filter = to_flt, zero_flux = to_zf))

    maybe_lossy_cast(
        result = new_mag(vec_data(x), to_flt, to_zf),
        x = x, to = to,
        lossy = vec_repeat(TRUE, vec_size(x)),
        locations = vec_seq_along(x),
        details = vec_c(
            glue_fmt_chr("Filter: `{x_arg}` has `{x_flt}`, `{to_arg}` has `{to_flt}`"),
            glue_fmt_chr("Zero flux: `{x_arg}` has `{format(x_zf)}`, `{to_arg}` has `{format(to_zf)}`")))
}
vec_cast.rastro_mag.integer <- function(x, to, ...)
    new_mag(x, filter = to %@% "filter", zero_flux = to %@% "zero_flux")
vec_cast.rastro_mag.double <- function(x, to, ...)
    new_mag(x, filter = to %@% "filter", zero_flux = to %@% "zero_flux")

vec_cast.double.rastro_mag <- function(x, to, ...) vec_data(x)
vec_cast.integer.rastro_mag <- function(x, to, ...) vec_cast(vec_data(x), integer())

as_mag <- function(x, filter = NA_character_, zero_flux = NA_real_, ...)
    vec_cast(x, new_mag(filte = filter, zero_flux = zero_flux))

vec_restore.rastro_mag <- function(x, to, ..., i = NULL) {
    new_mag(x, to %@% "filter", to %@% "zero_flux")
}

vec_proxy.rastro_mag <- function(x, ...) {
    attributes(x) <- NULL
    x
}


# EQUALITY


`%==%.rastro_mag` <- function(x, y) UseMethod("%==%.rastro_mag", y)
`%==%.rastro_mag.default` <- function(x, y) vec_equal(x, y) %|% FALSE

# ARITHMETIC
vec_arith.rastro_mag <- function(op, x, y, ...) UseMethod("vec_arith.rastro_mag", y)
vec_arith.rastro_mag.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
vec_arith.rastro_mag.MISSING <- function(op, x, y, ...) {
    if (op %===% "-") {
        return(new_mag(-vec_data(x), x %@% "filter", x %@% "zero_flux"))
    } else if (op %===% "+")
        return(x)
    stop_incompatible_op(op, x, y)
}
vec_arith.rastro_mag.double <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    data_x <- vec_data(x)
    switch(
        op,
        "+" = new_mag(data_x + y, x %@% "filter", x %@% "zero_flux"),
        "-" = new_mag(data_x - y, x %@% "filter", x %@% "zero_flux"),
        "*" = new_mag(data_x * y, x %@% "filter", x %@% "zero_flux"),
        "/" = new_mag(data_x / y, x %@% "filter", x %@% "zero_flux"),
        stop_incompatible_op(op, x, y))
}
vec_arith.double.rastro_mag <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    data_y <- vec_data(y)
    switch(
        op,
        "+" = new_mag(x + data_y, y %@% "filter", y %@% "zero_flux"),
        "-" = new_mag(x - data_y, y %@% "filter", y %@% "zero_flux"),
        "*" = new_mag(x * data_y, y %@% "filter", y %@% "zero_flux"),
        stop_incompatible_op(op, x, y))
}
vec_arith.rastro_mag.rastro_mag <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    vec_ptype_common(x, y) -> ptype
    data_x <- vec_data(x)
    data_y <- vec_data(y)

    switch(
        op,
        "+" = new_mag(data_x + data_y, ptype %@% "filter", ptype %@% "zero_flux"),
        "-" = new_mag(data_x - data_y, ptype %@% "filter", ptype %@% "zero_flux"),
        stop_incompatible_op(op, x, y))
}

vec_arith.rastro_mag.integer <- function(op, x, y, ...)
    vec_arith.rastro_degr.double(op, x, vec_cast(y, double()), ...)
vec_arith.integer.rastro_mag <- function(op, x, y, ...)
    vec_arith.double.rastro_degr(op, vec_cast(x, double()), y, ...)


vec_math.rastro_mag <- function(.fn, .x, ...) {
    data_x <- vec_data(.x)
    switch(.fn,
           abs = new_mag(abs(data_x), .x %@% "filter", .x %@% "zero_flux"),
           sign = vec_cast(sign(data_x), integer()),
           mean = new_degr(mean(data_x), .x %@% "filter", .x %@% "zero_flux"),
           sum = new_degr(sum(data_x), .x %@% "filter", .x %@% "zero_flux"),
           is.nan = is.nan(data_x),
           is.finite = is.finite(data_x),
           is.infinite = is.infinite(data_x),
           abort(glue_fmt_chr("`{.fn}` cannot be applied to <{vec_ptype_full(.x)}>.")))
}


# MAG -> FLUX conversion

vec_cast.rastro_flux.rastro_mag <- function(x, to, ..., x_arg = "x", to_arg = "to") {
    zf <- vec_cast(x %@% "zero_flux", to)

    new_flux(zf * 10 ^ (-vec_data(x) / 2.5), zf %@% "filter", zf %@% "unit")
}