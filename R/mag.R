# CTOR
new_mag <- function(m = double(), filter = NA_character_, zero_flux = NA_real_) {
    zero_flux <- vec_assert(vec_cast(zero_flux, double()), size = 1L)
    filter <- vec_assert(vec_cast(filter, character()), size = 1L)

    m <- vec_cast(m, double())

    new_vctr(m, filter = filter, zero_flux = zero_flux, class = "rastro_mag")
}

na_mag <- function() new_mag(NA_real_)

# FORMAT
format.rastro_mag <- function(x, format = "{m}m", ...) {
    m <- vec_data(x)
    glue_fmt_chr(format)
}

obj_print_footer.rastro_mag <- function(x, ...) {
    cat(glue_fmt_chr("Filter: {x %@% 'filter'}"))
}

# METADATA
vec_ptype_abbr.rastro_mag <- function(x, ...) "mag"
vec_ptype_full.rastro_mag <- function(x, ...) "rastro_mag"

# PTYPE
vec_ptype2.rastro_mag <- function(x, y, ...) UseMethod("vec_ptype2.rastro_mag", y)
vec_ptype2.rastro_mag.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
        vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
vec_ptype2.rastro_mag.rastro_mag <- function(x, y, ..., x_arg = "x", y_arg = "y") {
    x_flt <- x %@% "filter"
    x_zf <- x %@% "zero_flux"
    y_flt <- y %@% "filter"
    y_zf <- y %@% "zero_flux"

    cnd <- ((x_flt %===% y_flt) || (is_na(x_flt) && is_na(y_flt))) &&
        ((x_zf %===% y_zf) || (is_na(x_zf) && is_na(y_zf)))

    if (cnd)
        return(new_mag(filter = x_flt, zero_flux = x_zf))

    stop_incompatible_cast(x, y,
        details = "Magnitudes' filter and/or zero-flux differ.", x_arg = x_arg, y_arg = y_arg, ...)
}
vec_ptype2.rastro_mag.double <- function(x, y, ...)
    new_mag(filter = x %@% "filter", zero_flux = x %@% "zero_flux")
vec_ptype2.rastro_mag.integer <- function(x, y, ...)
    new_mag(filter = x %@% "filter", zero_flux = x %@% "zero_flux")
vec_ptype2.integer.rastro_mag <- function(x, y, ...)
    new_mag(filter = x %@% "filter", zero_flux = x %@% "zero_flux")
vec_ptype2.double.rastro_mag <- function(x, y, ...)
    new_mag(filter = x %@% "filter", zero_flux = x %@% "zero_flux")
