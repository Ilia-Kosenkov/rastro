reg_sub <- function(text, pattern) {
    text <- vec_cast(text, character())
    pattern <- vec_assert(vec_cast(pattern, character()), size = 1L)

    matches <- regexec(pattern, text) %>%
        vmap(~data.frame(start = .x, stop = .x +  (.x %@% "match.length") - 1L))

    vmap2(text, matches, list) %>%
        vmap_if(
            ~!vec_is_empty(.x[[2]]),
            function(item) {
                vmap_pt(item[[2]], ~substr(item[[1]], .x$start, .x$stop))
            },
            .else = ~NA_character_)
}