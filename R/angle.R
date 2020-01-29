angle_add_impl <- function(x, y) {
    x %->% c(x_deg, x_min, x_sec)
    y %->% c(y_deg, y_min, y_sec)

    vec_recycle_common(x_deg, y_deg) %->% c(x_deg, y_deg)
    vec_recycle_common(x_min, y_min) %->% c(x_min, y_min)
    vec_recycle_common(x_sec, y_sec) %->% c(x_sec, y_sec)

    normalize_dec_impl(x_deg + y_deg, x_min + y_min, x_sec + y_sec)
}