gcd <- function(x, y) {
    r <- x %% y
    return(ifelse(r, gcd(y, r), y))
}

simplify_frac <- function(x, y, fn = sum) {
    denom <- vec_cast(prod(y), integer())
    num <- fn(map2_int(x, y, ~ .x * denom %/% .y))

    gcd <- gcd(num, denom)

    return(vec_c(num %/% gcd, denom %/% gcd))
}

format_frac <- function(x) {
    if (x[1] %===% 0L)
        return(NA_character_)
    if ((x[1] %===% 1L) && (x[2] %===% 1L))
        return("")
    if (x[2] %===% 1L)
        return(glue_fmt_chr("^{x[1]}"))
    return(glue_fmt_chr("^{x[1]}/{x[2]}"))
}

proc_frac <- function(x, power) {
    grps <- vec_group_loc(x$`2`)
    grps$pw <- map(
        grps$loc,
        ~ simplify_frac(
            power[1] * (as.integer(x[.x, "3"]) %|% 1L),
            power[2] * (as.integer(x[.x, "4"]) %|% 1L)))
    grps
}