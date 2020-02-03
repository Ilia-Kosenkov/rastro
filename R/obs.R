new_obs <- function(
    obs,
    err = missing_arg(),
    n_err = missing_arg(),
    p_err = missing_arg(),
    item_frmt = NA_character_) {

    ptype <- vec_ptype(obs)
    if (is_missing(n_err) || is_missing(p_err)) {
        if (is_missing(err))
            err <- vec_cast(0, ptype)

        vec_recycle_common(!!!vec_cast_common(obs, err)) %->% c(obs, err)
        n_err <- err
        p_err <- err
    }
    else {
        if (is_missing(err))
            err <- vec_cast(0, ptype)
        vec_recycle_common(!!!vec_cast_common(obs, n_err, p_err)) %->% c(obs, n_err, p_err)
    }

    item_frmt <- vec_assert(vec_cast(item_frmt, character()), size = 1L)


    item_ptype <- vec_item_ptype(obs)

    if (!vec_is(item_ptype, ptype)) {
        na_val <- vec_init(ptype, 1L)
        na_val[[1]] <- vec_cast(NA, item_ptype)
    }
    else
        na_val <- vec_cast(NA, item_ptype)

    nas <- is.na(obs)
    vec_slice(n_err, nas) <- na_val
    vec_slice(p_err, nas) <- na_val

    if (!vec_is_empty(obs))
        assert(((n_err[!nas] >= vec_cast(0, item_ptype)) %===% TRUE) && ((p_err[!nas] >= vec_cast(0, item_ptype)) %===% TRUE),
            "Errors should be strictly non-negative.")

    new_rcrd(list(obs = obs, n_err = n_err, p_err = p_err),
             item_frmt = item_frmt,
             item_ptype = item_ptype,
             class = "rastro_obs")
}

na_obs <- function(item_ptype) new_obs(vec_init(item_ptype, 1L))

# FORMAT
format.rastro_obs <- function(
        x,
        format = "{obs} (- {n_err}; + {p_err})",
        format_eq = "{obs} U+00B1 {err}",
        format_each = NULL,
        na_string = "NA_rastro_obs",
        ...) {

    obs <- field(x, "obs")
    n_err <- field(x, "n_err")
    p_err <- field(x, "p_err")
    err <- (n_err + p_err) / 2

    format_each <- (x %@% "item_frmt") %|% "{format(item)}"

    nas <- is.na(obs)
    eq <- vec_as_location(p_err %==% n_err, vec_size(obs))

    item <- obs
    obs <- glue_fmt_chr(format_each)

    item <- n_err
    n_err <- glue_fmt_chr(format_each)

    item <- p_err
    p_err <- glue_fmt_chr(format_each)

    item <- err
    err <- glue_fmt_chr(format_each)

    result <- glue_fmt_chr(format)
    result[eq] <- glue_fmt_chr(format_eq)[eq]

    result[nas] <- na_string

    return(result)
}

# METHODS

common_frmt <- function(x_frmt, y_frmt) {

    x_frmt <- vec_cast(x_frmt, character())
    y_frmt <- vec_cast(y_frmt, character())
    if (x_frmt %===% y_frmt)
        frmt <- x_frmt
    else if (is_na(x_frmt) && is_na(y_frmt))
        frmt <- NA_character_
    else
        frmt <- x_frmt %|% y_frmt

    return (frmt)
}

# METADATA
vec_ptype_abbr.rastro_obs <- function(x, ...) glue_fmt_chr("obs<{vec_ptype_abbr(x %@% 'item_ptype')}>")
vec_ptype_full.rastro_obs <- function(x, ...) glue_fmt_chr("rastro_obs<{vec_ptype_full(x %@% 'item_ptype')}>")

# PTYPE
vec_ptype2.rastro_obs <- function(x, y, ...) UseMethod("vec_ptype2.rastro_obs", y)
vec_ptype2.rastro_obs.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
        vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
vec_ptype2.rastro_obs.rastro_obs <- function(x, y, ...) {
    vec_ptype2(x %@% "item_ptype", y %@% "item_ptype") -> ptype

    frmt <- common_frmt(x %@% "item_frmt", y %@% "item_frmt")

    new_obs(vec_init(ptype, 0L), item_frmt = frmt)
}
vec_ptype2.rastro_obs.double <- function(x, y, ...) new_obs(double())
vec_ptype2.rastro_obs.integer <- function(x, y, ...) new_obs(integer())
vec_ptype2.integer.rastro_obs <- function(x, y, ...) new_obs(integer())
vec_ptype2.double.rastro_obs <- function(x, y, ...) new_obs(double())
vec_ptype2.rastro_obs.data.frame <- function(x, y, ...) {
    filler <- vec_init(x %@% "item_ptype", 0L)

    df_ptype_1 <- vec_ptype(data.frame(obs = filler, err = filler))
    df_ptype_2 <- vec_ptype(data.frame(obs = filler, n_err = filler, p_err = filler))

    if (vec_is(vec_ptype2(df_ptype_1, y), df_ptype_1)) {
        return(df_ptype_1)
    }
    else if (vec_is(vec_ptype2(df_ptype_2, y), df_ptype_2)) {
        return(df_ptype_2)
    }

    stop_incompatible_type(x, y)
}

vec_ptype2.data.frame.rastro_obs <- function(x, y, ...) {
    filler <- vec_init(y %@% "item_ptype", 0L)
    df_ptype_1 <- vec_ptype(data.frame(obs = filler, err = filler))
    df_ptype_2 <- vec_ptype(data.frame(obs = filler, n_err = filler, p_err = filler))

    if (vec_is(vec_ptype2(df_ptype_1, x), df_ptype_1)) {
        return(df_ptype_1)
    }
    else if (vec_is(vec_ptype2(df_ptype_2, x), df_ptype_2)) {
        return(df_ptype_2)
    }

    stop_incompatible_type(x, y)
}

is_obs <- function(x, item_ptype)
    vec_is(x, new_obs(vec_init(item_ptype, 0L)))


# CAST
vec_cast.rastro_obs <- function(x, to, ...) UseMethod("vec_cast.rastro_obs")
vec_cast.rastro_obs.default <- function(x, to, ...) vec_default_cast(x, to)
vec_cast.rastro_obs.rastro_obs <- function(x, to, ...) {
    vec_cast(x %@% "item_ptype", to %@% "item_ptype") -> ptype
    new_obs(
        vec_cast(field(x, "obs"), ptype),
        n_err = vec_cast(field(x, "n_err"), ptype),
        p_err = vec_cast(field(x, "p_err"), ptype),
        item_frmt = (to %@% "item_frmt") %|% (x %@% "item_frmt"))
}

vec_cast.rastro_obs.double <- function(x, to, ...) {
    item_ptype <- to %@% "item_ptype"
    new_obs(
        vec_cast(x, item_ptype),
        n_err = vec_cast(0, ptype),
        p_err = vec_cast(0, ptype),
        item_frmt = to %@% "item_frmt")
}

vec_cast.rastro_obs.double <- function(x, to, ...) {
    x <- vec_cast(x, vec_ptype(to %@% "item_ptype"))

    new_obs(x, item_frmt = to %@% "item_frmt")
}

vec_cast.rastro_obs.integer <- function(x, to, ...) {
    x <- vec_cast(x, vec_ptype(to %@% "item_ptype"))

    new_obs(x, item_frmt = to %@% "item_frmt")
}

vec_cast.data.frame.rastro_obs <- function(x, to, ...) {
    ptype <- vec_ptype2(x, to)
    filler <- vec_init(x %@% "item_ptype", 0L)

    df_ptype_1 <- vec_ptype(data.frame(obs = filler, err = filler))
    df_ptype_2 <- vec_ptype(data.frame(obs = filler, n_err = filler, p_err = filler))


    if (vec_is(vec_ptype2(ptype, df_ptype_1), ptype)) {
        proxy <- vec_proxy(x)
        return(vec_cast(data.frame(obs = proxy$obs, err = 0.5 * (proxy$n_err + proxy$p_err)), to))
    }
    else if (vec_is(vec_ptype2(ptype, df_ptype_2), ptype)) {
        proxy <- vec_proxy(x)
        return(vec_cast(proxy, to))
    }
    stop_incompatible_cast(x, to)
}

vec_cast.rastro_obs.data.frame <- function(x, to, ...) {
    ptype <- vec_ptype2(x, to)
    filler <- vec_init(to %@% "item_ptype", 0L)

    df_ptype_1 <- vec_ptype(data.frame(obs = filler, err = filler))
    df_ptype_2 <- vec_ptype(data.frame(obs = filler, n_err = filler, p_err = filler))

    if (vec_is(vec_ptype2(ptype, df_ptype_1), ptype)) {
        return(vec_cast(new_obs(x$obs, x$err), to))
    }
    else if (vec_is(vec_ptype2(ptype, df_ptype_2), ptype)) {
        return(vec_cast(new_obs(x$obs, n_err = x$n_err, p_err = x$p_err), to))
    }
    stop_incompatible_cast(x, to)

}

as_obs <- function(x) new_obs(x)

vec_restore.rastro_obs <- function(x, to, ...) {
    item_ptype <- to %@% "item_ptype"

    obs <- vec_restore(x$obs, item_ptype)
    n_err <- vec_restore(x$n_err, item_ptype)
    p_err <- vec_restore(x$p_err, item_ptype)
    new_obs(obs = obs, n_err = n_err, p_err = p_err, item_frmt = to %@% "item_frmt")
}


vec_proxy.rastro_obs <- function(x, ...) {
    obs <- vec_proxy(field(x, "obs"))
    n_err <- vec_proxy(field(x, "n_err"))
    p_err <- vec_proxy(field(x, "p_err"))
    vec_cbind(obs = obs, n_err = n_err, p_err = p_err)
}

# EQUALITY
vec_proxy_compare.rastro_obs <- function(x, ...) {
    vec_proxy_compare(field(x, "obs"))
}

`%==%.rastro_obs` <- function(x, y) UseMethod("%==%.rastro_obs", y)
`%==%.rastro_obs.default` <- function(x, y) vec_equal(x, y) %|% FALSE
`%==%.rastro_obs.rastro_obs` <- function(x, y) {
    vec_recycle_common(x, y) %->% c(x, y)
    if ((x %@% "item_ptype") %!==% (y %@% "item_ptype"))
        return(vec_repeat(FALSE, vec_size(x)))

    proxy_x <- vec_proxy_equal(x)
    proxy_y <- vec_proxy_equal(y)

    (proxy_x %==% proxy_y) %|% FALSE
}


# ARITHMETIC
vec_arith.rastro_obs <- function(op, x, y, ...) UseMethod("vec_arith.rastro_obs", y)
vec_arith.rastro_obs.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
vec_arith.rastro_obs.MISSING <- function(op, x, y, ...) {
    if (op %==% "-") {
        data <- vec_data(x)
        return(new_obs(
            obs = -data$obs,
            n_err = data$p_err,
            p_err = data$n_err,
            item_frmt = x %@% "item_frmt"))
    } else if (op %==% "+")
        return(x)

    stop_incompatible_op(op, x, y)
}
vec_arith.rastro_obs.numeric <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    data_x <- vec_data(x)
    item_ptype <- x %@% "item_ptype"

    if ((op %===% "*") || (op %===% "/"))
        return(
            new_obs(
                obs = vec_arith(op, data_x$obs, y),
                n_err = vec_arith(op, data_x$n_err, y),
                p_err = vec_arith(op, data_x$p_err, y),
                item_frmt = x %@% "item_frmt"))

    stop_incompatible_op(op, x, y)
}
vec_arith.numeric.rastro_obs <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    data_y <- vec_data(y)
    item_ptype <- y %@% "item_ptype"

    if ((op %===% "*") || (op %===% "/"))
        return(
            new_obs(
                obs = vec_arith(op, x, data_y$obs),
                n_err = vec_arith(op, x, data_y$n_err),
                p_err = vec_arith(op, x, data_y$p_err),
                item_frmt = y %@% "item_frmt"))

    stop_incompatible_op(op, x, y)
}
vec_arith.rastro_obs.rastro_obs <- function(op, x, y, ...) {
    vec_recycle_common(x, y) %->% c(x, y)
    data_x <- vec_data(x)
    data_y <- vec_data(y)
    item_ptype <- x %@% "item_ptype"
    switch(
        op,
        "+" = new_obs(
            data_x$obs + data_y$obs,
            n_err = data_x$n_err + data_y$n_err,
            p_err = data_x$p_err + data_y$p_err,
            item_frmt = common_frmt(x %@% "item_frmt", y %@% "item_frmt")),
        "-" = new_obs(
            data_x$obs + data_y$obs,
            n_err = data_x$n_err + data_y$n_err,
            p_err = data_x$p_err + data_y$p_err,
            item_frmt = common_frmt(x %@% "item_frmt", y %@% "item_frmt")),
        stop_incompatible_op(op, x, y))
}

#vec_math.rastro_obs <- function(.fn, .x, ...) {
    #switch(.fn,
           #sin = sin(dec_2_deg(.x) / 180 * pi),
           #cos = cos(dec_2_deg(.x) / 180 * pi),
           #tan = tan(dec_2_deg(.x) / 180 * pi),
           #abs = cc(!!!vmap_if(.x, ~ .x < new_obs(0), ~ -.x)),
           #vec_math_base(.fn, .x, ...))
#}