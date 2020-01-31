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

    assert(((n_err >= 0) %===% TRUE) && ((p_err >= 0) %===% TRUE),
        "Errors should be strictly non-negative.")

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
        format_eq = "{obs} ± {err}",
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


# METADATA
vec_ptype_abbr.rastro_obs <- function(x, ...) glue_fmt_chr("obs<{vec_ptype_abbr(x %@% 'item_ptype')}>")
vec_ptype_full.rastro_obs <- function(x, ...) glue_fmt_chr("rastro_obs<{vec_ptype_full(x %@% 'item_ptype')}>")

# PTYPE
vec_ptype2.rastro_obs <- function(x, y, ...) UseMethod("vec_ptype2.rastro_obs", y)
vec_ptype2.rastro_obs.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
        vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
vec_ptype2.rastro_obs.rastro_obs <- function(x, y, ...) {
    vec_ptype2(x %@% "item_ptype", y %@% "item_ptype") -> ptype

    x_frmt <- vec_cast(x %@% "item_frmt", character())
    y_frmt <- vec_cast(y %@% "item_frmt", character())

    if (x_frmt %===% y_frmt)
        frmt <- x_frmt
    else if (is_na(x_frmt) && is_na(y_frmt))
        frmt <- NA_character_
    else
        frmt <- x_frmt %|% y_frmt


    new_obs(vec_init(ptype, 0L), item_frmt = frmt)
}
vec_ptype2.rastro_obs.double <- function(x, y, ...) new_obs(double())
vec_ptype2.rastro_obs.integer <- function(x, y, ...) new_obs(integer())
vec_ptype2.integer.rastro_obs <- function(x, y, ...) new_obs(integer())
vec_ptype2.double.rastro_obs <- function(x, y, ...) new_obs(double())

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
    x <- vec_cast(x, vec_ptype(to %@% "item_ptype"))

    new_obs(x, item_frmt = to %@% "item_frmt")
}

vec_cast.rastro_obs.integer <- function(x, to, ...) {
    x <- vec_cast(x, vec_ptype(to %@% "item_ptype"))

    new_obs(x, item_frmt = to %@% "item_frmt")
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