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

# TODO : Probaby should go to RLibs?

#' @title Tibble methods for comaptibility
#' @rdname tbl_df
#' @param x,y Vectors to test type/cast.
#' @param to To what type to cast.
#' @param ... Additional parameters
#' @export
vec_ptype2.tbl_df <- function(x, y, ...)
    UseMethod("vec_ptype2.tbl_df", y)

#' @rdname tbl_df
#' @method vec_ptype2.tbl_df default
#' @export
vec_ptype2.tbl_df.default <- function(x, y, ...)
    vec_default_ptype2(x, y, ...)

#' @rdname tbl_df
#' @method vec_ptype2.tbl_df tbl_df
#' @export
vec_ptype2.tbl_df.tbl_df <- function(x, y, ...) {
    as_tibble(vec_default_ptype2(as.data.frame(x), as.data.frame(y), ...))
}

#' @rdname tbl_df
#' @export
vec_cast.tbl_df <- function(x, to, ...)
    UseMethod("vec_cast.tbl_df")

#' @rdname tbl_df
#' @method vec_cast.tbl_df default
#' @export
vec_cast.tbl_df.default <- function(x, to, ...)
    vec_default_cast(x, to)

#' @rdname tbl_df
#' @method vec_cast.tbl_df tbl_df
#' @export
vec_cast.tbl_df.tbl_df <- function(x, to, ...) {
    x_ptype <- vec_ptype(x)
    to_ptype <- vec_ptype(to)

    if (vec_is(x_ptype, to_ptype))
        return(x)

    nms <- names(x_ptype)
    if (all(nms %vin% names(to))) {
        result <- vec_init(to_ptype, vec_size(x))
        for (nm in nms)
            result[[nm]] <- vec_cast(x[[nm]], to_ptype[[nm]], x_arg = nm, to_arg = "")
        return(result)
    }

    nms <- intersect(nms, names(to_ptype))
    if (vec_is_empty(nms) || !vec_is(x_ptype[nms], to_ptype[nms]))
        stop_incompatible_cast(x, to, "No common columns detected")

    result <- vec_init(to_ptype[nms], vec_size(x))
    for (nm in nms)
        result[[nm]] <- vec_cast(x[[nm]], to_ptype[[nm]], x_arg = nm, to_arg = "")

    return(maybe_lossy_cast(result, x, to, TRUE, vec_seq_along(result),
        details = glue_fmt_chr(
            "Narrowing cast.\n" %&% 
            "Only the following columns are present in the result: `{paste0(nms, collapse = '`, `')}`.")))
}