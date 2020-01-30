# CTOR
new_degr <- function(deg = double()) {
    new_vctr(vec_cast(deg, double(), x_arg = "deg") %% 360, class = "rastro_degr")
}

# FORMAT
format.rastro_degr <- function(x, format = "{x}°", ...) {
    x <- vec_data(x)
    glue_fmt_chr(format)
}

# METADATA
vec_ptype_abbr.rastro_degr <- function(x, ...) "degr"
vec_ptype_full.rastro_degr <- function(x, ...) "rastro_degr"

# PTYPE
vec_ptype2.rastro_degr <- function(x, y, ...) UseMethod("vec_ptype2.rastro_degr", y)
vec_ptype2.rastro_degr.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
        vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
vec_ptype2.rastro_degr.rastro_degr <- function(x, y, ...) new_degr()
vec_ptype2.rastro_degr.double <- function(x, y, ...) new_degr()
vec_ptype2.rastro_degr.integer <- function(x, y, ...) new_degr()
vec_ptype2.integer.rastro_degr <- function(x, y, ...) new_degr()
vec_ptype2.double.rastro_degr <- function(x, y, ...) new_degr()

vec_ptype2.rastro_dec.rastro_degr <- function(x, y, ...) new_degr()
vec_ptype2.rastro_ra.rastro_degr <- function(x, y, ...) new_degr()

vec_ptype2.rastro_degr.rastro_dec <- function(x, y, ...) new_degr()
vec_ptype2.rastro_degr.rastro_ra <- function(x, y, ...) new_degr()


# CAST
vec_cast.rastro_degr <- function(x, to, ...) UseMethod("vec_cast.rastro_degr")
vec_cast.rastro_degr.default <- function(x, to, ...) vec_default_cast(x, to)
vec_cast.rastro_degr.rastro_degr <- function(x, to, ...) x
vec_cast.rastro_degr.integer <- function(x, to, ...) new_degr(x)
vec_cast.rastro_degr.double <- function(x, to, ...) new_degr(x)
vec_cast.rastro_degr.rastro_dec <- function(x, to, ...) new_degr(dec_2_deg(x))
vec_cast.rastro_degr.rastro_ra <- function(x, to, ...) new_degr(ra_2_hr(x) * 15)

vec_cast.double.rastro_degr <- function(x, to, ...) vec_data(x)
vec_cast.integer.rastro_degr <- function(x, to, ...) vec_cast(vec_data(x), integer())

vec_cast.rastro_dec.rastro_degr <- function(x, to, ...) new_dec_from_degr(vec_data(x))
vec_cast.rastro_ra.rastro_degr <- function(x, to) new_ra_from_hr(vec_data(x) / 15)

as_degrstro_degr <- function(x, ...) vec_cast(x, new_degr())