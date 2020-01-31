# CTOR
new_dec <- function(deg = integer(), min = integer(), sec = double()) {
    deg <- vec_cast(deg, integer())
    min <- vec_cast(min, integer())
    sec <- vec_cast(sec, double())

    if (vec_is_empty(deg) && vec_is_empty(min) && vec_is_empty(sec)) {
        return(new_rcrd(list(
                sign = integer(),
                deg = deg,
                min = min,
                sec = sec), class = "rastro_dec"))
    }

    vec_recycle_common(
        deg = deg %0% 0L,
        min = min %0% 0L,
        sec = sec %0% 0.0) -> tmp
    tmp %->% c(deg, min, sec)

    nas <- is.na(deg) | is.na(min) | is.na(sec)
    deg[nas] <- NA_integer_
    min[nas] <- NA_integer_
    sec[nas] <- NA_real_

    normalize_dec(deg, min, sec) -> fields
    new_rcrd(fields, class = "rastro_dec")
}

new_dec_from_degr <- function(deg) {
    if (vec_is_empty(deg))
        return(new_dec())

    i_deg <- vec_cast(round(deg), integer())
    sec <- 3600 * vec_cast(deg - i_deg, double())
    vec_recycle_common(i_deg, 0L, sec) %->% c(deg, min, sec)
    normalize_dec(deg, min, sec) -> fields

    new_rcrd(fields, class = "rastro_dec")
}

na_rastro_dec <- function() new_dec(NA)

# METHODS
normalize_dec_impl <- function(deg, min, sec) {

    mv <- vec_cast(sec %/% 60, integer())
    sec <- sec %% 60

    id <- mv %!=% 0
    min[id] <- min[id] + mv[id]
    mv <- min %/% 60L
    min <- min %% 60L

    id <- mv %!=% 0

    deg[id] <- deg[id] + mv[id]

    deg <- deg %% 360L

    return(list(deg = deg, min = min, sec = sec))
}

negate_dec <- function(deg, min, sec) {

    id <- sec %==% 0
    sec <- (60 - sec) %% 60

    min <- 60L - min - 1L
    min[id] <- min[id] + 1L
    id <- min %==% 60L
    min <- min %% 60L

    deg <- 360L - deg - 1L
    deg[id] <- deg[id] + 1L

    list(deg = deg, min = min, sec = sec)
}


normalize_dec <- function(deg, min, sec) {

    normalize_dec_impl(deg, min, sec) %->% c(deg, min, sec)

    nas <- is.na(deg) | is.na(min) | is.na(sec)

    sign <- vec_init(integer(), vec_size(deg))
    sign[vec_seq_along(sign)] <- 1L
    sign[nas] <- NA_integer_

    id <- !nas & ((deg > 180L) | ((deg %==% 180L) & ((min > 0) | (sec > 0))))

    if (any(id)) {
        negate_dec(deg[id], min[id], sec[id]) -> mod

        deg[id] <- mod$deg
        min[id] <- mod$min
        sec[id] <- mod$sec
        sign[id] <- -1L
    }

    id <- !nas & ((deg > 90L) | ((deg %==% 90L) & ((min > 0) | (sec > 0))))
    if (any(id)) {
        angle_add_impl(
            list(deg = 180L, min = 0L, sec = 0),
            list(deg = -deg[id], min = -min[id], sec = -sec[id])) -> mod

        deg[id] <- mod$deg
        min[id] <- mod$min
        sec[id] <- mod$sec
    }


    return(list(sign = sign, deg = deg, min = min, sec = sec))
}

dec_2_deg <- function(dec) {
    vec_assert(dec, new_dec())

    sign <- field(dec, "sign")
    deg <- field(dec, "deg")
    min <- field(dec, "min")
    sec <- field(dec, "sec")

    return(sign * (deg + min / 60 + sec / 3600))
}

angle_add_impl <- function(x, y) {
    x %->% c(x_deg, x_min, x_sec)
    y %->% c(y_deg, y_min, y_sec)

    vec_recycle_common(x_deg, y_deg) %->% c(x_deg, y_deg)
    vec_recycle_common(x_min, y_min) %->% c(x_min, y_min)
    vec_recycle_common(x_sec, y_sec) %->% c(x_sec, y_sec)

    normalize_dec_impl(x_deg + y_deg, x_min + y_min, x_sec + y_sec)
}



# FORMAT
format.rastro_dec <- function(
        x,
        format = "{sign:%1s}{deg:%02d}°{min:%02d}'{sec:%02d}\".{fff:%03.0f}",
        na_string = "NA_rastro_dec",
        ...) {
    sign_val <- field(x, "sign")
    sign <- vec_repeat("+", len(sign_val))
    sign[sign_val %==% -1L] <- "-"
    deg <- field(x, "deg")
    min <- field(x, "min")
    sec <- field(x, "sec")

    i_sec <- vec_cast(floor(sec), integer())
    fff <- 100 * (sec - i_sec)
    sec <- i_sec

    result <- glue_fmt_chr(format)
    nas <- is.na(deg) | is.na(min) | is.na(sec)
    result[nas] <- na_string

    return(result)
}


# METADATA
vec_ptype_abbr.rastro_dec <- function(x, ...) "dec"
vec_ptype_full.rastro_dec <- function(x, ...) "rastro_dec"

# PTYPE
vec_ptype2.rastro_dec <- function(x, y, ...) UseMethod("vec_ptype2.rastro_dec", y)
vec_ptype2.rastro_dec.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
        vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
vec_ptype2.rastro_dec.rastro_dec <- function(x, y, ...) new_dec()
vec_ptype2.rastro_dec.double <- function(x, y, ...) new_dec()
vec_ptype2.rastro_dec.integer <- function(x, y, ...) new_dec()
vec_ptype2.integer.rastro_dec <- function(x, y, ...) new_dec()
vec_ptype2.double.rastro_dec <- function(x, y, ...) new_dec()

is_dec <- function(x, ...) vec_is(x, new_dec())

# CAST
vec_cast.rastro_dec <- function(x, to, ...) UseMethod("vec_cast.rastro_dec")
vec_cast.rastro_dec.default <- function(x, to, ...) vec_default_cast(x, to)
vec_cast.rastro_dec.rastro_dec <- function(x, to, ...) x
vec_cast.rastro_dec.integer <- function(x, to, ...) new_dec_from_degr(x)
vec_cast.rastro_dec.double <- function(x, to, ...) new_dec_from_degr(x)
vec_cast.double.rastro_dec <- function(x, to, ...) dec_2_deg(x)

as_dec <- function(x, ...) vec_cast(x, new_dec())

# EQUALITY
`%==%.rastro_dec` <- function(x, y) UseMethod("%==%.rastro_dec", y)
`%==%.rastro_dec.default` <- function(x, y) vec_equal(x, y)
`%==%.rastro_dec.rastro_dec` <- function(x, y) {
    proxy_x <- vec_proxy_equal(x)
    proxy_y <- vec_proxy_equal(y)

    (proxy_x$sign == proxy_y$sign) &
        (proxy_x$deg == proxy_y$deg) &
        (proxy_x$min == proxy_y$min) &
        (are_equal_f(proxy_x$sec, proxy_y$sec))
}

vec_proxy_compare.rastro_dec <- function(x, ...) {
    sign <- field(x, "sign")
    data.frame(
        min = sign * (field(x, "deg") * 60L + field(x, "min")),
        sec = sign * field(x, "sec"))
}


# ARITHMETIC
vec_arith.rastro_dec <- function(op, x, y, ...) UseMethod("vec_arith.rastro_dec", y)
vec_arith.rastro_dec.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
vec_arith.rastro_dec.MISSING <- function(op, x, y, ...) {
    if (op %==% "-") {
        data <- vec_data(x)
        return(new_dec(-data$sign * data$deg, - data$sign * data$min, - data$sign * data$sec))
    } else if (op %==% "+")
        return(x)

    stop_incompatible_op(op, x, y)
}
vec_arith.rastro_dec.double <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    switch(
        op,
        "+" = x + new_dec_from_degr(y),
        "-" = x - new_dec_from_degr(y),
        "*" = new_dec_from_degr(dec_2_deg(x) * y),
        "/" = new_dec_from_degr(dec_2_deg(x) / y),
        stop_incompatible_op(op, x, y))
}
vec_arith.double.rastro_dec <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)

    switch(
        op,
        "+" = new_dec_from_degr(x) + y,
        "-" = new_dec_from_degr(x) - y,
        "*" = new_dec_from_degr(x * dec_2_deg(y)),
        stop_incompatible_op(op, x, y))
}
vec_arith.rastro_dec.rastro_dec <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    data_x <- vec_data(x)
    data_y <- vec_data(y)

    switch(
        op,
        "+" = new_dec(
            data_x$sign * data_x$deg + data_y$sign * data_y$deg,
            data_x$sign * data_x$min + data_y$sign * data_y$min,
            data_x$sign * data_x$sec + data_y$sign * data_y$sec),
        "-" = new_dec(
            data_x$sign * data_x$deg - data_y$sign * data_y$deg,
            data_x$sign * data_x$min - data_y$sign * data_y$min,
            data_x$sign * data_x$sec - data_y$sign * data_y$sec),
        stop_incompatible_op(op, x, y))
}

vec_arith.rastro_dec.integer <- function(op, x, y, ...)
    vec_arith.rastro_dec.double(op, x, vec_cast(y, double()), ...)
vec_arith.integer.rastro_dec <- function(op, x, y, ...)
    vec_arith.double.rastro_dec(op, ve_cast(x, double()), y, ...)


vec_math.rastro_dec <- function(.fn, .x, ...) {
    switch(.fn,
           sin = sin(dec_2_deg(.x) / 180 * pi),
           cos = cos(dec_2_deg(.x) / 180 * pi),
           tan = tan(dec_2_deg(.x) / 180 * pi),
           abs = cc(!!!vmap_if(.x, ~.x < new_dec(0), ~-.x)),
           vec_math_base(.fn, .x, ...))
}