#' Pivot child tier annotations
#'
#' [Experimental]
#' Pivot ELAN annotations from child tiers to a wide format.
#' Note: Pivots empty values as NA! A better option may be to use
#' `dplyr::_join()` to join relevant child tier annotations to their
#' associated parents, or to `dplyr::rename()` columns before pivoting.
#'
#' @param data A tibble of annotations
#'
#' @return A tibble with dependent (child) annotations in wide format
#' @export
#' @importFrom rlang .data
#'
pivot_children_wider <- function(data) {

  # Subset independent (parent) tier annotations
  parent_annotations <-
    data |>
    dplyr::filter(is.na(.data$a_ref)) |>
    dplyr::select(-dplyr::all_of("a_ref"))

  # Subset dependent (child) tier annotations
  child_annotations <-
    data |>
    dplyr::filter(!is.na(.data$a_ref))

  child_tiers <- unique(child_annotations$tier)

  child_annotations <-
    child_annotations |>
    dplyr::select(dplyr::all_of(c("file", "parent_ref", "a_ref", "annotation", "tier"))) |>
    tidyr::pivot_wider(names_from = dplyr::all_of("tier"), values_from = dplyr::all_of("annotation"))

  # Pivot data into wide format, connecting child annotations with their parents
  all_annotations <-
    parent_annotations |>
    dplyr::left_join(child_annotations, by = dplyr::join_by("file" == "file",
                                                            "a" == "a_ref",
                                                            "tier" == "parent_ref"))

  return(all_annotations)

}
