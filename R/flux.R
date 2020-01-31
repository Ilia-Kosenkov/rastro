# CTOR
new_flux <- function(flux = double(), filter = NA_character_, unit = NA_character_) {
    filter <- vec_assert(vec_cast(filter, character()), size = 1L)
    unit <- vec_assert(vec_cast(unit, character()), size = 1L)

    f <- vec_cast(flux, double())

    new_vctr(f, filter = filter, unit = unit, class = "rastro_flux")
}

na_flux <- function() new_flux(NA_real_)

# FORMAT
format.rastro_flux <- function(x,
    format = "{flux:%11.3e}",
    na_string = "NA_rastro_flux_",
    ...) {
    flux <- vec_data(x)
    result <- glue_fmt_chr(format)
    result[is.na(flux)] <- na_string
    return(result)
}

obj_print_footer.rastro_flux <- function(x, ...) {
    cat(glue_fmt_chr("Unit: {(x %@% 'unit')}"))
}

# METADATA
vec_ptype_abbr.rastro_flux <- function(x, ...)
    glue_fmt_chr("flux<{(x %@% 'filter') %|% '?'}>")
vec_ptype_full.rastro_flux <- function(x, ...)
    glue_fmt_chr("rastro_flux<{(x %@% 'filter') %|% '?'}>")

# PTYPE
vec_ptype2.rastro_flux <- function(x, y, ...) UseMethod("vec_ptype2.rastro_flux", y)
vec_ptype2.rastro_flux.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
        vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
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
vec_ptype2.rastro_flux.double <- function(x, y, ...)
    new_flux(filter = x %@% "filter", unit = x %@% "unit")
vec_ptype2.rastro_flux.integer <- function(x, y, ...)
    new_flux(filter = x %@% "filter", unit = x %@% "unit")
vec_ptype2.integer.rastro_flux <- function(x, y, ...)
    new_flux(filter = y %@% "filter", unit = y %@% "unit")
vec_ptype2.double.rastro_flux <- function(x, y, ...)
    new_flux(filter = y %@% "filter", unit = y %@% "unit")


is_flux <- function(x, filter = NA_character_, unit = NA_real_)
    vec_is(x, new_flux(filter = filter, unit = unit))

# CAST
vec_cast.rastro_flux <- function(x, to, ..., x_arg = "x", to_arg = "to")
    UseMethod("vec_cast.rastro_flux")
vec_cast.rastro_flux.default <- function(x, to, ..., x_arg = "x", to_arg = "to")
    vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
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

vec_cast.rastro_flux.integer <- function(x, to, ...)
    new_flux(x, filter = to %@% "filter", unit = to %@% "unit")
vec_cast.rastro_flux.double <- function(x, to, ...)
    new_flux(x, filter = to %@% "filter", unit = to %@% "unit")

vec_cast.double.rastro_flux <- function(x, to, ...) vec_data(x)
vec_cast.integer.rastro_flux <- function(x, to, ...) vec_cast(vec_data(x), integer())

as_flux <- function(x, filter = NA_character_, unit = NA_real_, ...)
    vec_cast(x, new_flux(filte = filter, unit = unit))

vec_restore.rastro_flux <- function(x, to, ..., i = NULL) {
    new_flux(x, to %@% "filter", to %@% "unit")
}

# EQUALITY
vec_proxy_equal.rastro_flux <- function(x, ...) {
    filter <- x %@% "filter"
    unit <- x %@% "unit"

    data.frame(
        mag = vec_data(x),
        filter = vec_repeat(filter %|% "", vec_size(x)),
        unit = vec_repeat(unit %|% "", vec_size(x)),
        flag = 10L * vec_cast(is.na(filter), integer()) + vec_cast(is.na(unit), integer()))
}


`%==%.rastro_flux` <- function(x, y) UseMethod("%==%.rastro_flux", y)
`%==%.rastro_flux.default` <- function(x, y) vec_equal(x, y)

