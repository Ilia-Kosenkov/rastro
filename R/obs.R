# prototyping
library(vctrs)
library(rlang)

new_obs <- function(
    x = double(),
    err = vec_recycle(vec_cast(0L, vec_ptype(x)), vec_size(x))) {

    result <- vec_recycle_common(!!!vec_cast_common(obs = x, err = err))

    nas <- is.na(result$obs) | is.na(result$err)
    ptype <- vec_ptype(result$obs)

    result$err <- abs(result$err)
    result$obs[nas] <- vec_cast(NA, ptype)
    result$err[nas] <- vec_cast(NA, ptype)

    new_rcrd(result, ptype = ptype, class = "rastro_obs")
}

default_obs <- function(ptype) {
    new_obs(vec_init(ptype, 0L), vec_init(ptype, 0L))
}

is_obs <- function(x)
    inherits(x, "rastro_obs")
as_obs <- function(x, ptype = double())
    vec_cast(x, default_obs(ptype))

as_tibble.rastro_obs <- function(x, ...) {
    ptype <- x %@% ptype
    vec_cast(x, tibble(obs = vec_init(ptype, 0L), err = vec_init(ptype, 0L)))
}

as.data.frame.rastro_obs <- function(x, ...) {
    ptype <- x %@% ptype
    print(ptype)
    vec_cast(x, data.frame(obs = vec_init(ptype, 0L), err = vec_init(ptype, 0L)))
}

as_list_of <- function(x, ...) {
    as_list_of(vec_data(x))
}

format.rastro_obs <- function(x, ...) {
    obs <- field(x, "obs")
    err <- field(x, "err")
    nas <- is.na(obs) | is.na(err)

    frmt <- paste0("(", format(obs), " ± ", format(err), ")")
    frmt[nas] <- "(NA)"

    frmt
}

vec_ptype_abbr.rastro_obs <- function(x, ...) paste0("r_obs<", vec_ptype_abbr(attr(x, "ptype")), ">")
vec_ptype_full.rastro_obs <- function(x, ...) paste0("rastro_obs<", vec_ptype_full(attr(x, "ptype")), ">")


# vec_ptype2 exports
vec_ptype2.rastro_obs.rastro_obs <- function(x, y, ...) {
    ptype <- vec_ptype2(attr(x, "ptype"), attr(y, "ptype"))

    default_obs(ptype)
}

.vec_ptype2.rastro_obs.impl <- function(x, y, ...) {
    ptype <- vec_ptype2(x %@% ptype, y)
    default_obs(ptype)
}


vec_ptype2.rastro_obs.integer <- function(x, y, ...)
    .vec_ptype2.rastro_obs.impl(x, y, ...)

vec_ptype2.rastro_obs.double <- function(x, y, ...)
    .vec_ptype2.rastro_obs.impl(x, y, ...)

vec_ptype2.rastro_obs.complex <- function(x, y, ...)
    .vec_ptype2.rastro_obs.impl(x, y, ...)

vec_ptype2.rastro_obs.data.frame <- function(x, y, ...) {
    df_ptype2(as.data.frame(vec_data(x)), y)
}

vec_ptype2.rastro_obs.tbl_df <- function(x, y, ...) {
    tib_ptype2(as_tibble(vec_data(x)), y)
}


vec_ptype2.rastro_obs.vctrs_list_of <- function(x, y, ...) {
    vec_ptype2(as_list_of(x), y)
}

# -------------------------------------------------------
vec_ptype2.integer.rastro_obs <- function(x, y, ...)
    .vec_ptype2.rastro_obs.impl(y, x, ...)

vec_ptype2.double.rastro_obs <- function(x, y, ...)
    .vec_ptype2.rastro_obs.impl(y, x, ...)

vec_ptype2.complex.rastro_obs <- function(x, y, ...)
    .vec_ptype2.rastro_obs.impl(y, x, ...)

vec_ptype2.rastro_obs.data.frame <- function(x, y, ...) {
    df_ptype2(x, as.data.frame(vec_data(y)))
}

vec_ptype2.rastro_obs.data.frame <- function(x, y, ...) {
    tib_ptype2(x, as_tibble(vec_data(y)))
}

vec_ptype2.vctrs_list_of.rastro_obs <- function(x, y, ...) {
    vec_ptype2(x, as_list_of(y))
}

# vec_cast exports

vec_cast.rastro_obs.rastro_obs <- function(x, to, ...) {
    new_obs(
        vec_cast(field(x, "obs"), to %@% ptype),
        vec_cast(field(x, "err"), to %@% ptype))
}

vec_cast.rastro_obs.impl <- function(x, to, ...) {
    ptype <- to %@% ptype

    new_obs(vec_cast(x, ptype), vec_cast(0L, ptype))
}

vec_cast.rastro_obs.integer <- function(x, to, ...)
    vec_cast.rastro_obs.impl(x, to, ...)

vec_cast.rastro_obs.double <- function(x, to, ...)
    vec_cast.rastro_obs.impl(x, to, ...)

vec_cast.rastro_obs.complex <- function(x, to, ...)
    vec_cast.rastro_obs.impl(x, to, ...)


vec_cast.data.frame.rastro_obs <- function(x, to, ...) {
    df_cast(as.data.frame(vec_data(x)), to, ...)
}

vec_cast.rastro_obs.data.frame <- function(x, to, ...) {
    result <- df_cast(x, as.data.frame(vec_data(to)))

    new_obs(result$obs, result$err)
}


vec_cast.tbl_df.rastro_obs <- function(x, to, ...) {
    tib_cast(as.data.frame(vec_data(x)), to, ...)
}

vec_cast.rastro_obs.tbl_df <- function(x, to, ...) {
    result <- tib_cast(x, as_tibble(vec_data(to)))

    new_obs(result$obs, result$err)
}


vec_cast.vctrs_list_of.rastro_obs <- function(x, to, ...) {
    list_of(obs = field(x, "obs"), err = field(x, "err"), .ptype = to %@% ptype)
}

# -------------------------------------------------------

# Order only using "obs" fields, without the error
vec_proxy_compare.rastro_obs <- function(x, ...)
    field(x, "obs")


# -------------------------------------------------------

vec_math.rastro_obs <- function(.fn, .x, ...) {
    obs <- field(.x, "obs")
    err <- field(.x, "err")
    args <- list2(...)
    switch(.fn,
        abs = new_obs(abs(obs), err),
        sign = sign(obs),
        exp = new_obs(exp(obs), exp(obs) * err),
        log = {
            base <- ifelse(vec_size(args) > 0, vec_cast(args[[1]], double()), exp(1))
            new_obs(log(obs, base), abs(err / obs / log(base)))
        },
        log10 = log(.x, 10),
        log2 = log(.x, 2),
        sqrt = .x ^ 0.5,
        sin = new_obs(sin(obs), abs(cos(obs) * err)),
        cos = new_obs(cos(obs), abs(sin(obs) * err)),
        tan = {
            tan_vl <- tan(obs)
            tan_err <- (1 + tan_vl ^ 2) * err
            new_obs(tan_vl, tan_err)
        },
        sum = new_obs(sum(obs), sqrt(sum(err ^ 2))),
        vec_math_base(.fn, .x, ...))
}

# -------------------------------------------------------
vec_arith.rastro_obs <- function(op, x, y, ...) 
    UseMethod("vec_arith.rastro_obs", y)

vec_arith.rastro_obs.default <- function(op, x, y, ...) {
    vec_arith.rastro_obs.rastro_obs(op, x, vec_cast(y, x), ...)
}

.vec_arith.rastro_obs.impl <- function(op, x, y, ...) {
    temp <- vec_recycle_common(x = x, y = y)
    y <- temp$y
    x <- temp$x
    obs <- field(x, "obs")
    err <- field(x, "err")

    switch(
        op,
        "+" = new_obs(obs + y, err),
        "-" = new_obs(obs - y, err),
        "*" = new_obs(y * obs, abs(y * err)),
        "/" = new_obs(obs / y, abs(err / y)),
        "^" = new_obs(obs ^ y, abs(y * obs ^ (y - 1L) * err)),
        stop_incompatible_op(op, x, y, ...))
}

.vec_arith.impl.rastro_obs <- function(op, x, y, ...) {
    temp <- vec_recycle_common(x = x, y = y)
    y <- temp$y
    x <- temp$x
    obs <- field(y, "obs")
    err <- field(y, "err")

    switch(
        op,
        "+" = new_obs(x + obs, err),
        "-" = new_obs(x - obs, err),
        "*" = new_obs(x * obs, abs(x * err)),
        stop_incompatible_op(op, x, y, ...))
}

vec_arith.rastro_obs.integer <- function(op, x, y, ...)
    .vec_arith.rastro_obs.impl(op, x, y, ...)

vec_arith.rastro_obs.double <- function(op, x, y, ...)
    .vec_arith.rastro_obs.impl(op, x, y, ...)

vec_arith.rastro_obs.numeric <- function(op, x, y, ...)
    .vec_arith.rastro_obs.impl(op, x, y, ...)

vec_arith.rastro_obs.complex <- function(op, x, y, ...)
    .vec_arith.rastro_obs.impl(op, x, y, ...)

vec_arith.integer.rastro_obs <- function(op, x, y, ...)
    .vec_arith.impl.rastro_obs(op, x, y, ...)

vec_arith.double.rastro_obs <- function(op, x, y, ...)
    .vec_arith.impl.rastro_obs(op, x, y, ...)

vec_arith.numeric.rastro_obs <- function(op, x, y, ...)
    .vec_arith.impl.rastro_obs(op, x, y, ...)

vec_arith.complex.rastro_obs <- function(op, x, y, ...)
    .vec_arith.impl.rastro_obs(op, x, y, ...)


vec_arith.rastro_obs.rastro_obs <- function(op, x, y, ...) {
    temp <- vec_cast_common(x = x, y = y)
    x_obs <- field(temp$x, "obs")
    y_obs <- field(temp$y, "obs")

    x_err <- field(temp$x, "err")
    y_err <- field(temp$y, "err")
    switch(
        op,
        "+" = new_obs(x_obs + y_obs, sqrt(x_err ^ 2 + y_err ^ 2)),
        "-" = new_obs(x_obs - y_obs, sqrt(x_err ^ 2 + y_err ^ 2)),
        "*" = new_obs(x_obs * y_obs, sqrt(y_obs ^ 2 * x_err ^ 2 + x_obs ^ 2 * y_err ^ 2)),
        "/" = new_obs(x_obs / y_obs, sqrt(x_err ^ 2 / y_obs ^ 2 + x_obs ^ 2 * y_err ^ 2 / y_obs ^ 4)),
        stop_incompatible_op(op, x, y, ...))
}

vec_arith.rastro_obs.MISSING <- function(op, x, y, ...) {
    switch(
        op,
        "+" = x,
        "-" = new_obs(-field(x, "obs"), field(x, "err")),
        stop_incompatible_op(op, x, y, ...))
}