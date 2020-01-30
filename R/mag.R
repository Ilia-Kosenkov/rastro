# CTOR
new_mag <- function(m = double(), filter = NA_character_, zero_flux = NA_real_) {
    zero_flux <- vec_assert(vec_cast(zero_flux, double()), size = 1L)
    filter <- vec_assert(vec_cast(filter, character()), size = 1L)

    m <- vec_cast(m, double())

    new_vctr(m, filter = filter, zero_flux = zero_flux, class = "rastro_mag")
}

na_mag <- function() new_mag(NA_real_)

# FORMAT
format.rastro_mag <- function(x,
    format = "{mag:%.3f}m",
    na_string = "NA_rastro_mag_",
    ...) {
    mag <- vec_data(x)
    result <- glue_fmt_chr(format)
    result[is.na(mag)] <- na_string
    return(result)
}

obj_print_footer.rastro_mag <- function(x, ...) {
    cat(glue_fmt_chr("Zero flux: {(x %@% 'zero_flux')}"))
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

    stop_incompatible_cast(x, y,
        details = "Magnitudes' filter and/or zero-flux differ.", x_arg = x_arg, y_arg = y_arg, ...)
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

    cnd <- ((to_flt %===% x_flt) || is_na(x_flt)) &&
        ((to_zf %===% x_zf) || is_na(x_zf))

    if (cnd)
        return(new_mag(vec_data(x), filter = to_flt, zero_flux = to_zf))

    stop_incompatible_cast(x, to,
        details = "Magnitudes' filter and/or zero-flux differ.",
        x_arg = x_arg, to_arg = to_arg,
        ...)
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


# EQUALITY
vec_proxy_equal.rastro_mag <- function(x, ...) {
    filter <- x %@% "filter"
    zero_flux <- x %@% "zero_flux"

    data.frame(
        mag = vec_data(x),
        filter = vec_repeat(filter %|% "", vec_size(x)),
        zero_flux = vec_repeat(zero_flux %|% 0, vec_size(x)),
        flag = 10L * vec_cast(is.na(filter), integer()) + vec_cast(is.na(zero_flux), integer()))
}


`%==%.rastro_mag` <- function(x, y) UseMethod("%==%.rastro_mag", y)
`%==%.rastro_mag.default` <- function(x, y) vec_equal(x, y)
