#' @importFrom rlang        eval_tidy parse_expr is_na is_empty %|% %@% %||% missing_arg
#' @importFrom rlang        abort is_missing
#'
#' @importFrom vctrs        vec_cast vec_recycle_common new_rcrd field %0% vec_equal vec_c
#' @importFrom vctrs        stop_incompatible_type stop_incompatible_op stop_incompatible_cast
#' @importFrom vctrs        vec_arith.numeric vec_cast.double vec_cast.integer vec_slice
#' @importFrom vctrs        vec_ptype2.double vec_ptype2.integer vec_data vec_assert vec_repeat
#' @importFrom vctrs        vec_size vec_is vec_init vec_is_empty new_vctr vec_ptype
#' @importFrom vctrs        vec_proxy_equal vec_arith vec_ptype_common allow_lossy_cast
#' @importFrom vctrs        vec_cast_common vec_arith_base vec_ptype2 maybe_lossy_cast
#' @importFrom vctrs        vec_as_location vec_cbind vec_default_cast vec_default_ptype2
#' @importFrom vctrs        vec_math_base vec_proxy vec_proxy_compare vec_rbind
#' @importFrom vctrs        vec_restore vec_seq_along vec_slice<- vec_cast.data.frame
#' @importFrom vctrs        vec_ptype2.data.frame vec_ptype_full vec_ptype_abbr
#'
#' @importFrom primitiveR   %->% glue_fmt_chr assert vmap_if cc %==% %===% %!=% %!==% are_equal_f
#' @importFrom primitiveR   vmap_pt vmap vmap2 %&% len vec_item_ptype
NULL