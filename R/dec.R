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
        sec = sec %0% 0.0) %->% c(deg, min, sec)

    adjust_dec(deg, min, sec) -> fields


    new_rcrd(fields, class = "rastro_dec")
}

adjust_dec <- function(deg, min, sec) {
    val <- deg + min / 60 + sec / 3600
    val <- val %% 360
    val[val >= 180] <- val - 360

    sign <- sign(val)
    val <- abs(val)
    deg <- as.integer(val)
    val <- abs(60 * (val - deg))
    min <- as.integer(val)
    sec <- abs(60 * (val - min))

    return(list(sign = sign, deg = deg, min = min, sec = sec))
}

new_dec_from_degr <- function(deg) {
    deg <- vec_cast(deg, double())

    vec_recycle_common(deg, 0.0, 0.0) %->% c(deg, min, sec)

    adjust_dec(deg, min, sec) -> fields

    new_rcrd(fields, class = "rastro_dec")
}

dec_2_deg <- function(x) {
    vec_assert(x, new_dec())
    sign <- field(x, "sign")
    deg <- field(x, "deg")
    min <- field(x, "min")
    sec <- field(x, "sec")

    return(sign * (deg + min / 60 + sec / 3600))
}


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

format.vctrs_rcrd <- function(x, ...) format.rastro_dec(x)

vec_ptype_abbr.rastro_dec <- function(x, ...) "dec"
vec_ptype_full.rastro_dec <- function(x, ...) "rastro_dec"

vec_ptype2.rastro_dec <- function(x, y, ...) UseMethod("vec_ptype2.rastro_dec", y)
vec_ptype2.rastro_dec.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
        vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
vec_ptype2.rastro_dec.rastro_dec <- function(x, y, ...) new_dec()
vec_ptype2.rastro_dec.double <- function(x, y, ...) new_dec()
vec_ptype2.rastro_dec.integer <- function(x, y, ...) new_dec()
vec_ptype2.integer.rastro_dec <- function(x, y, ...) new_dec()
vec_ptype2.double.rastro_dec <- function(x, y, ...) new_dec()


vec_cast.rastro_dec <- function(x, to, ...) UseMethod("vec_cast.rastro_dec")
vec_cast.rastro_dec.default <- function(x, to, ...) vec_default_cast(x, to)
vec_cast.rastro_dec.rastro_dec <- function(x, to, ...) x
vec_cast.rastro_dec.integer <- function(x, to, ...) new_dec_from_degr(x)
vec_cast.rastro_dec.double <- function(x, to, ...) new_dec_from_degr(x)
vec_cast.double.rastro_dec <- function(x, to, ...) dec_2_deg(x)
