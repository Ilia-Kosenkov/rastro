# CTOR
new_dec <- function(deg = integer(), min = integer(), sec = double()) {
    deg <- vec_cast(deg, integer())
    min <- vec_cast(min, integer())
    sec <- vec_cast(sec, double())

    if (vec_is_empty(deg) && vec_is_empty(min) && vec_is_empty(sec)) {
        return(new_rcrd(list(
                sign = integer(),
                deg = deg,
                min = min,
                sec = sec), class = "rastro_dec"))
    }

    vec_recycle_common(
        deg = deg %0% 0L,
        min = min %0% 0L,
        sec = sec %0% 0.0) -> tmp
    tmp %->% c(deg, min, sec)

    normalize_dec(deg, min, sec) -> fields
    new_rcrd(fields, class = "rastro_dec")
}

new_dec_from_degr <- function(deg) {
    deg <- vec_cast(deg, double())

    vec_recycle_common(deg, 0.0, 0.0) %->% c(deg, min, sec)

    normalize_dec(deg, min, sec) -> fields

    new_rcrd(fields, class = "rastro_dec")
}

normalize_dec <- function(deg, min, sec) {
    min <- min + vec_cast(sec %/% 60, integer())
    sec <- sec %% 60

    deg <- deg + min %/% 60L
    min <- min %% 60L

    deg <- deg %% 360L
    neg <- deg >= 180L

    nz_sec <- sec[neg] %!=% 0
    min[neg[nz_sec]] <- min[neg[nz_sec]] + 1L
    sec[neg[nz_sec]] <- 60 - sec[neg[nz_sec]]

    nz_min <- min[neg] != 0L
    deg[neg[nz_min]] <- deg[neg[nz_min]] + 1L
    min[neg[nz_min]] <- 60L - min[neg[nz_min]]

    nz_deg <- deg[neg] != 0L
    deg[nz_deg] <- 360L - deg[nz_deg]

    sign <- ifelse(neg, -1L, 1L)

    return(list(sign = sign, deg = deg, min = min, sec = sec))
}

dec_2_deg <- function(x) {
    vec_assert(x, new_dec())
    sign <- field(x, "sign")
    deg <- field(x, "deg")
    min <- field(x, "min")
    sec <- field(x, "sec")

    return(sign * (deg + min / 60 + sec / 3600))
}


# FORMAT
format.rastro_dec <- function(
        x,
        format = "{sign:%1s}{deg:%02d}:{min:%02d}:{sec:%06.3f}", ...) {
    sign_val <- field(x, "sign")
    sign <- vec_repeat("+", len(sign_val))
    sign[sign_val %==% -1] <- "-"
    deg <- field(x, "deg")
    min <- field(x, "min")
    sec <- field(x, "sec")

    rnd_err <- sec %==% 60
    min[rnd_err] <- min[rnd_err] + 1L
    sec[rnd_err] <- 0.0

    glue_fmt_chr(format)
}


# METADATA
vec_ptype_abbr.rastro_dec <- function(x, ...) "dec"
vec_ptype_full.rastro_dec <- function(x, ...) "rastro_dec"

# PTYPE
vec_ptype2.rastro_dec <- function(x, y, ...) UseMethod("vec_ptype2.rastro_dec", y)
vec_ptype2.rastro_dec.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
        vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
vec_ptype2.rastro_dec.rastro_dec <- function(x, y, ...) new_dec()
vec_ptype2.rastro_dec.double <- function(x, y, ...) new_dec()
vec_ptype2.rastro_dec.integer <- function(x, y, ...) new_dec()
vec_ptype2.integer.rastro_dec <- function(x, y, ...) new_dec()
vec_ptype2.double.rastro_dec <- function(x, y, ...) new_dec()

is_rastro_dec <- function(x, ...) vec_is(x, new_dec())

# CAST
vec_cast.rastro_dec <- function(x, to, ...) UseMethod("vec_cast.rastro_dec")
vec_cast.rastro_dec.default <- function(x, to, ...) vec_default_cast(x, to)
vec_cast.rastro_dec.rastro_dec <- function(x, to, ...) x
vec_cast.rastro_dec.integer <- function(x, to, ...) new_dec_from_degr(x)
vec_cast.rastro_dec.double <- function(x, to, ...) new_dec_from_degr(x)
vec_cast.double.rastro_dec <- function(x, to, ...) dec_2_deg(x)

as_rastro_dec <- function(x, ...) vec_cast(x, new_dec())

# EQUALITY
`%==%.rastro_dec` <- function(x, y) UseMethod("%==%.rastro_dec", y)
`%==%.rastro_dec.default` <- function(x, y) vec_equal(x, y)
`%==%.rastro_dec.rastro_dec` <- function(x, y) {
    proxy_x <- vec_proxy_equal(x)
    proxy_y <- vec_proxy_equal(y)

    (proxy_x$sign == proxy_y$sign) &
        (proxy_x$deg == proxy_y$deg) &
        (proxy_x$min == proxy_y$min) &
        (are_equal_f(proxy_x$sec, proxy_y$sec))
}

vec_proxy_compare.rastro_dec <- function(x, ...) {
    sign <- field(x, "sign")
    data.frame(
        min = sign * (field(x, "deg") * 60L + field(x, "min")),
        sec = sign * field(x, "sec"))
}