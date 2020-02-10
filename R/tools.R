reg_sub <- function(text, pattern) {
    text <- vec_cast(text, character())
    pattern <- vec_assert(vec_cast(pattern, character()), size = 1L)

    matches <- vmap(
        regexec(pattern, text),
        ~ data.frame(start = .x, stop = .x + (.x %@% "match.length") - 1L))

    vmap_if(
        vmap2(text, matches, list)
            ~ !vec_is_empty(.x[[2]]),
            function(item) {
                vmap_pt(item[[2]], ~substr(item[[1]], .x$start, .x$stop))
            },
            .else = ~NA_character_)
}

#' @title Tibble methods for comaptibility
#' @rdname tbl_df
#' @param x,y Vectors to test type/cast.
#' @param ... Additional parameters
#' @export
vec_ptype2.tbl_df <- function(x, y, ...)
    UseMethod("vec_ptype2.tbl_df", y)

#' @rdname tbl_df
#' @method vec_ptype2.tbl_df default
#' @export
vec_ptype2.tbl_df.default <- function(x, y, ...)
    vec_default_ptype2(x, y, ...)