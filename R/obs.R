new_obs <- function(obs, err,
    m_err = missing_arg(), p_err = missing_arg(),
    item_frmt = NA_character_) {
    if (!is_missing(err)) {
        vec_recycle_common(!!!vec_cast_common(obs, err)) %->% c(obs, err)
        m_err <- err
        p_err <- err
    }
    else {
        vec_recycle_common(!!!vec_cast_common(obs, m_err, p_err)) %->% c(obs, m_err, p_err)
    }

    item_frmt <- vec_assert(vec_cast(item_frmt, character()), size = 1L)

    assert(((m_err >= 0) %===% TRUE) && ((p_err >= 0) %===% TRUE),
        "Errors should be strictly non-negative.")

    ptype <- vec_ptype(obs)
    item_ptype <- vec_item_ptype(obs)

    if (!vec_is(item_ptype, ptype)) {
        na_val <- vec_init(ptype, 1L)
        na_val[[1]] <- vec_cast(NA, item_ptype)
    }
    else
        na_val <- vec_cast(NA, item_ptype)

    nas <- is.na(obs)
    vec_slice(m_err, nas) <- na_val
    vec_slice(p_err, nas) <- na_val

    new_rcrd(list(obs = obs, m_err = m_err, p_err = p_err),
             item_frmt = item_frmt,
             class = "rastro_obs")
}

# FORMAT
format.rastro_obs <- function(
        x,
        format = "{obs} (-{m_err};+{p_err})",
        format_eq = "{obs} ± {err}",
        format_each = NULL,
        na_string = "NA_rastro_obs",
        ...) {

    vec_data(x) %->% c(obs, m_err, p_err)
    err <- sqrt(0.5 * (m_err ^ 2 + p_err ^ 2))

    format_each <- (x %@% "item_frmt") %|% "{format(item)}"

    nas <- is.na(obs)
    eq <- vec_as_location(p_err %==% m_err, vec_size(obs))

    item <- obs
    obs <- glue_fmt_chr(format_each)

    item <- m_err
    m_err <- glue_fmt_chr(format_each)

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
vec_ptype_abbr.rastro_obs <- function(x, ...) glue_fmt_chr("obs<{vec_ptype_abbr(field(x, 'obs'))}>")
vec_ptype_full.rastro_obs <- function(x, ...) glue_fmt_chr("rastro_obs<{vec_ptype_full(field(x, 'obs'))}>")