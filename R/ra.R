# CTOR
new_ra <- function(hr = integer(), min = integer(), sec = double()) {
    hr <- vec_cast(hr, integer())
    min <- vec_cast(min, integer())
    sec <- vec_cast(sec, double())

    if (vec_is_empty(hr) && vec_is_empty(min) && vec_is_empty(sec)) {
        return(new_rcrd(list(
                hr = hr,
                min = min,
                sec = sec), class = "rastro_ra"))
    }

    vec_recycle_common(
        hr = hr %0% 0L,
        min = min %0% 0L,
        sec = sec %0% 0.0) -> tmp
    tmp %->% c(hr, min, sec)

    nas <- is.na(hr) | is.na(min) | is.na(sec)
    hr[nas] <- NA_integer_
    min[nas] <- NA_integer_
    sec[nas] <- NA_real_

    normalize_ra(hr, min, sec) -> fields
    new_rcrd(fields, class = "rastro_ra")
}

new_ra_from_hr <- function(hr) {
    if (vec_is_empty(hr))
        return(new_ra())

    i_hr <- vec_cast(round(hr), integer())
    sec <- 3600 * vec_cast(hr - i_hr, double())
    vec_recycle_common(i_hr, 0L, sec) %->% c(hr, min, sec)
    normalize_ra(hr, min, sec) -> fields

    new_rcrd(fields, class = "rastro_ra")
}

na_rastro_ra <- function() new_ra(NA)


# METHODS

ra_2_hr <- function(ra) {
    vec_assert(ra, new_ra())
    hr <- field(ra, "hr")
    min <- field(ra, "min")
    sec <- field(ra, "sec")

    return(hr + min / 60 + sec / 3600)
}

normalize_ra <- function(hr, min, sec) {

    mv <- vec_cast(sec %/% 60, integer())
    sec <- sec %% 60

    id <- mv %!=% 0
    min[id] <- min[id] + mv[id]
    mv <- min %/% 60L
    min <- min %% 60L

    id <- mv %!=% 0

    hr[id] <- hr[id] + mv[id]

    hr <- hr %% 24L

    return(list(hr = hr, min = min, sec = sec))
}


# FORMAT
format.rastro_ra <- function(
        x,
        format = "{hr:%02d}h{min:%02d}m{sec:%05.2f}s",
        na_string = "NA_rastro_ra_",
        ...) {
    hr <- field(x, "hr")
    min <- field(x, "min")
    sec <- field(x, "sec")

    result <- glue_fmt_chr(format)

    nas <- is.na(hr) | is.na(min) | is.na(sec)
    result[nas] <- na_string

    return(result)
}


# METADATA
vec_ptype_abbr.rastro_ra <- function(x, ...) "ra"
vec_ptype_full.rastro_ra <- function(x, ...) "rastro_ra"


# PTYPE
vec_ptype2.rastro_ra <- function(x, y, ...) UseMethod("vec_ptype2.rastro_ra", y)
vec_ptype2.rastro_ra.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
        vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
vec_ptype2.rastro_ra.rastro_ra <- function(x, y, ...) new_ra()
vec_ptype2.rastro_ra.double <- function(x, y, ...) new_ra()
vec_ptype2.rastro_ra.integer <- function(x, y, ...) new_ra()
vec_ptype2.integer.rastro_ra <- function(x, y, ...) new_ra()
vec_ptype2.double.rastro_ra <- function(x, y, ...) new_ra()

is_ra <- function(x, ...) vec_is(x, new_ra())

# CAST
vec_cast.rastro_ra <- function(x, to, ...) UseMethod("vec_cast.rastro_ra")
vec_cast.rastro_ra.default <- function(x, to, ...) vec_default_cast(x, to)
vec_cast.rastro_ra.rastro_ra <- function(x, to, ...) x
vec_cast.rastro_ra.integer <- function(x, to, ...) new_ra_from_hr(x)
vec_cast.rastro_ra.double <- function(x, to, ...) new_ra_from_hr(x)
vec_cast.double.rastro_ra <- function(x, to, ...) ra_2_hr(x)

as_ra <- function(x, ...) vec_cast(x, new_ra())


# EQUALITY
`%==%.rastro_ra` <- function(x, y) UseMethod("%==%.rastro_ra", y)
`%==%.rastro_ra.default` <- function(x, y) vec_equal(x, y)
`%==%.rastro_ra.rastro_ra` <- function(x, y) {
    proxy_x <- vec_proxy_equal(x)
    proxy_y <- vec_proxy_equal(y)

    (proxy_x$sign == proxy_y$sign) &
        (proxy_x$hr == proxy_y$hr) &
        (proxy_x$min == proxy_y$min) &
        (are_equal_f(proxy_x$sec, proxy_y$sec))
}

vec_proxy_compare.rastro_ra <- function(x, ...) {
    data.frame(
        min = (field(x, "hr") * 60L + field(x, "min")),
        sec = field(x, "sec"))
}

# ARITHMETIC
vec_arith.rastro_ra <- function(op, x, y, ...) UseMethod("vec_arith.rastro_ra", y)
vec_arith.rastro_ra.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
vec_arith.rastro_ra.MISSING <- function(op, x, y, ...) {
    if (op %==% "-") {
        data <- vec_data(x)
        return(new_ra(-data$hr, -data$min, -data$sec))
    } else if (op %==% "+")
        return(x)

    stop_incompatible_op(op, x, y)
}
vec_arith.rastro_ra.double <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    switch(
        op,
        "+" = x + new_ra_from_hr(y),
        "-" = x - new_ra_from_hr(y),
        "*" = new_ra_from_hr(ra_2_hr(x) * y),
        "/" = new_ra_from_hr(ra_2_hr(x) / y),
        stop_incompatible_op(op, x, y))
}
vec_arith.double.rastro_ra <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)

    switch(
        op,
        "+" = new_ra_from_hr(x) + y,
        "-" = new_ra_from_hr(x) - y,
        "*" = new_ra_from_hr(x * ra_2_hr(y)),
        stop_incompatible_op(op, x, y))
}
vec_arith.rastro_ra.rastro_ra <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    data_x <- vec_data(x)
    data_y <- vec_data(y)

    switch(
        op,
        "+" = new_ra(
            data_x$hr + data_y$hr,
            data_x$min + data_y$min,
            data_x$sec + data_y$sec),
        "-" = new_ra(
           data_x$hr - data_y$hr,
            data_x$min - data_y$min,
            data_x$sec - data_y$sec),
        stop_incompatible_op(op, x, y))
}

vec_arith.rastro_ra.integer <- function(op, x, y, ...)
    vec_arith.rastro_ra.double(op, x, vec_cast(y, double()), ...)
vec_arith.integer.rastro_ra <- function(op, x, y, ...)
    vec_arith.double.rastro_ra(op, vec_cast(x, double()), y, ...)

vec_math.rastro_ra <- function(.fn, .x, ...) {
    switch(.fn,
           sin = sin(ra_2_hr(.x) / 12 * pi),
           cos = cos(ra_2_hr(.x) / 12 * pi),
           tan = tan(ra_2_hr(.x) / 12 * pi),
           abs = .x,
           vec_math_base(.fn, .x, ...))
}