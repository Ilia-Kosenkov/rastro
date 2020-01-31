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