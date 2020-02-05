#' @title Observation
#' @rdname rastro_obs
#' @param obs Observations.
#' @param err,p_err,n_err Symmetrical (\code{err}) or a pair of positive (\code{p_err}) and
#' negative (\code{n_err}) errors.
#' @param item_frmt A desired format string to control the output.
#' @param x,y \code{vec_ptype2*} arguments.
#' @param to \code{vec_cast} argument.
#' @param x_arg,y_arg \code{vec_ptype2*} and \code{vec_cast*} error message variable names.
#' @param op Arithmetic functions/operators.
#' @param .x \code{vec_arith*} argument.
#' @param format,na_string,format_eq,format_each \code{glue} flromat strings (support interpolation).
#' @param item_ptype Ptype of the contained data.
#' @param ... Additional parameters.
#'
#' @export
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

#' @rdname rastro_obs
#' @export
na_obs <- function(item_ptype) new_obs(vec_init(item_ptype, 1L))

# FORMAT
#' @rdname rastro_obs
#' @export
format.rastro_obs <- function(
        x,
        format = "{obs} (- {n_err}; + {p_err})",
        format_eq = "{obs} \U00B1 {err}",
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
#' @rdname rastro_obs
#' @export
vec_ptype_abbr.rastro_obs <- function(x, ...) glue_fmt_chr("obs<{vec_ptype_abbr(x %@% 'item_ptype')}>")
#' @rdname rastro_obs
#' @export
vec_ptype_full.rastro_obs <- function(x, ...) glue_fmt_chr("rastro_obs<{vec_ptype_full(x %@% 'item_ptype')}>")

# PTYPE
#' @rdname rastro_obs
#' @export
vec_ptype2.rastro_obs <- function(x, y, ...) UseMethod("vec_ptype2.rastro_obs", y)
#' @rdname rastro_obs
#' @method vec_ptype2.rastro_obs default
#' @export
vec_ptype2.rastro_obs.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
        vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
#' @rdname rastro_obs
#' @method vec_ptype2.rastro_obs rastro_obs
#' @export
vec_ptype2.rastro_obs.rastro_obs <- function(x, y, ...) {
    vec_ptype2(x %@% "item_ptype", y %@% "item_ptype") -> ptype

    frmt <- common_frmt(x %@% "item_frmt", y %@% "item_frmt")

    new_obs(vec_init(ptype, 0L), item_frmt = frmt)
}
#' @rdname rastro_obs
#' @method vec_ptype2.rastro_obs double
#' @export
vec_ptype2.rastro_obs.double <- function(x, y, ...) new_obs(double())
#' @rdname rastro_obs
#' @method vec_ptype2.rastro_obs integer
#' @export
vec_ptype2.rastro_obs.integer <- function(x, y, ...) new_obs(integer())
#' @rdname rastro_obs
#' @method vec_ptype2.integer rastro_obs
#' @export
vec_ptype2.integer.rastro_obs <- function(x, y, ...) new_obs(integer())
#' @rdname rastro_obs
#' @method vec_ptype2.double rastro_obs
#' @export
vec_ptype2.double.rastro_obs <- function(x, y, ...) new_obs(double())
#' @rdname rastro_obs
#' @method vec_ptype2.rastro_obs data.frame
#' @export
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

#' @rdname rastro_obs
#' @method vec_ptype2.data.frame rastro_obs
#' @export
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

#' @rdname rastro_obs
#' @export
is_obs <- function(x, item_ptype)
    vec_is(x, new_obs(vec_init(item_ptype, 0L)))


# CAST
#' @rdname rastro_obs
#' @export
vec_cast.rastro_obs <- function(x, to, ...) UseMethod("vec_cast.rastro_obs")
#' @rdname rastro_obs
#' @method vec_cast.rastro_obs default
#' @export
vec_cast.rastro_obs.default <- function(x, to, ...) vec_default_cast(x, to)
#' @rdname rastro_obs
#' @method vec_cast.rastro_obs rastro_obs
#' @export
vec_cast.rastro_obs.rastro_obs <- function(x, to, ...) {
    vec_cast(x %@% "item_ptype", to %@% "item_ptype") -> ptype

    proxy <- vec_proxy(x)
    cast_rastro_obs(proxy$obs, ptype, proxy$p_err, proxy$n_err)

    #new_obs(
        #vec_cast(field(x, "obs"), ptype),
        #n_err = vec_cast(field(x, "n_err"), ptype),
        #p_err = vec_cast(field(x, "p_err"), ptype),
        #item_frmt = (to %@% "item_frmt") %|% (x %@% "item_frmt"))
}

#' @rdname rastro_obs
#' @method vec_cast.rastro_obs double
#' @export
vec_cast.rastro_obs.double <- function(x, to, ...) {
    item_ptype <- to %@% "item_ptype"
    new_obs(
        vec_cast(x, item_ptype),
        n_err = vec_cast(0, ptype),
        p_err = vec_cast(0, ptype),
        item_frmt = to %@% "item_frmt")
}

#' @rdname rastro_obs
#' @method vec_cast.rastro_obs double
#' @export
vec_cast.rastro_obs.double <- function(x, to, ...) {
    x <- vec_cast(x, vec_ptype(to %@% "item_ptype"))

    new_obs(x, item_frmt = to %@% "item_frmt")
}

#' @rdname rastro_obs
#' @method vec_cast.rastro_obs integer
#' @export
vec_cast.rastro_obs.integer <- function(x, to, ...) {
    x <- vec_cast(x, vec_ptype(to %@% "item_ptype"))

    new_obs(x, item_frmt = to %@% "item_frmt")
}

#' @rdname rastro_obs
#' @method vec_cast.data.frame rastro_obs
#' @export
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

#' @rdname rastro_obs
#' @method vec_cast.rastro_obs data.frame
#' @export
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
#' @rdname rastro_obs
#' @export
as_obs <- function(x) new_obs(x)

#' @rdname rastro_obs
#' @export
vec_restore.rastro_obs <- function(x, to, ...) {
    item_ptype <- to %@% "item_ptype"

    obs <- vec_restore(x$obs, item_ptype)
    n_err <- vec_restore(x$n_err, item_ptype)
    p_err <- vec_restore(x$p_err, item_ptype)
    new_obs(obs = obs, n_err = n_err, p_err = p_err, item_frmt = to %@% "item_frmt")
}

#' @rdname rastro_obs
#' @export
vec_proxy.rastro_obs <- function(x, ...) {
    obs <- vec_proxy(field(x, "obs"))
    n_err <- vec_proxy(field(x, "n_err"))
    p_err <- vec_proxy(field(x, "p_err"))
    vec_cbind(obs = obs, n_err = n_err, p_err = p_err)
}

# EQUALITY
#' @rdname rastro_obs
#' @export
vec_proxy_compare.rastro_obs <- function(x, ...) {
    vec_proxy_compare(field(x, "obs"))
}

#' @rdname rastro_obs
#' @export
`%==%.rastro_obs` <- function(x, y) UseMethod("%==%.rastro_obs", y)
#' @rdname rastro_obs
#' @method %==%.rastro_obs default
#' @export
`%==%.rastro_obs.default` <- function(x, y) vec_equal(x, y) %|% FALSE
#' @rdname rastro_obs
#' @method %==%.rastro_obs rastro_obs
#' @export
`%==%.rastro_obs.rastro_obs` <- function(x, y) {
    vec_recycle_common(x, y) %->% c(x, y)

    proxy_x <- vec_proxy_equal(x)
    proxy_y <- vec_proxy_equal(y)

    (proxy_x %==% proxy_y) %|% FALSE
}


# ARITHMETIC
#' @rdname rastro_obs
#' @export
vec_arith.rastro_obs <- function(op, x, y, ...) UseMethod("vec_arith.rastro_obs", y)
#' @rdname rastro_obs
#' @method vec_arith.rastro_obs default
#' @export
vec_arith.rastro_obs.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
#' @rdname rastro_obs
#' @method vec_arith.rastro_obs MISSING
#' @export
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
#' @rdname rastro_obs
#' @method vec_arith.rastro_obs numeric
#' @export
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
#' @rdname rastro_obs
#' @method vec_arith.numeric rastro_obs
#' @export
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
#' @rdname rastro_obs
#' @method vec_arith.rastro_obs rastro_obs
#' @export
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

# SPECIAL CASTS
cast_rastro_obs <- function(x, to, p_err, n_err, ...)
    UseMethod("cast_rastro_obs", to)

cast_rastro_obs.default <- function(x, to, p_err, n_err, ...)
    new_obs(vec_cast(x, to), p_err = vec_cast(p_err, to), n_err = vec_cast(n_err, to))

cast_rastro_obs.rastro_mag <- function(x, to, p_err, n_err, ...)
    UseMethod("cast_rastro_obs.rastro_mag", x)

cast_rastro_obs.rastro_flux <- function(x, to, p_err, n_err, ...)
    UseMethod("cast_rastro_obs.rastro_flux", x)

cast_rastro_obs.rastro_mag.default <- function(x, to, p_err, n_err, ...)
    new_obs(vec_cast(x, to), p_err = vec_cast(p_err, to), n_err = vec_cast(n_err, to))

cast_rastro_obs.rastro_flux.default <- function(x, to, p_err, n_err, ...)
    new_obs(vec_cast(x, to), p_err = vec_cast(p_err, to), n_err = vec_cast(n_err, to))

cast_rastro_obs.rastro_mag.rastro_flux <- function(x, to, p_err, n_err, ...) {
    new_obs(
        vec_cast(x, to),
        n_err = vec_cast(abs(-2.5 / log(10) * (p_err / x)), to),
        p_err = vec_cast(abs(-2.5 / log(10) * (n_err / x)), to))
}

cast_rastro_obs.rastro_flux.rastro_mag <- function(x, to, p_err, n_err, ...) {
    new_obs(
        vec_cast(x, to),
        n_err = vec_cast(abs(vec_cast(x, to) * log(10) / 2.5 * vec_cast(p_err, double())), to),
        p_err = vec_cast(abs(vec_cast(x, to) * log(10) / 2.5 * vec_cast(n_err, double())), to))
}
