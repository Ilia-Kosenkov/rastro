new_dec <- function(deg = integer(), min = integer(), sec = double()) {
    deg <- vec_cast(deg, integer())
    min <- vec_cast(min, integer())
    sec <- vec_cast(sec, double())

    vec_recycle_common(deg = deg, min = min, sec = sec) %->% c(deg, min, sec)

    adjust_dec(deg, min, sec) -> fields


    new_rcrd(fields, class = "rastro_dec")
}

adjust_dec <- function(deg, min, sec) {
    val <- deg + min / 60 + sec / 3600
    val <- val %% 360
    val[val >= 180] <- val - 360

    sign <- sign(val)

    deg <- as.integer(val)
    val <- abs(60 * (val - deg))
    min <- as.integer(val)
    sec <- abs(60 * (val - min))

    return(list(sign = sign, deg = abs(deg), min = min, sec = sec))
}

format.rastro_dec <- function(x, format = "{sign:%1s}{deg:%02d}:{min:%02d}:{sec:%06.3f}", ...) {
    sign_val <- field(x, "sign")
    sign <- vec_repeat("+", len(sign_val))
    sign[sign_val %==% -1] <- "-"
    deg <- field(x, "deg")
    min <- field(x, "min")
    sec <- field(x, "sec")

    glue_fmt_chr(format)
}


new_dec(30, -2, 90) %>% print
new_dec(-30, -5, 1) %>% print
new_dec(1, -61, 61) %>% print