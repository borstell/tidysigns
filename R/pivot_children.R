#' Pivot ELAN annotations from child tiers to a wide format
#'
#' @param data A tibble of annotations
#' @param long Whether the child tiers are pivoted back to long format (default = `FALSE`)
#'
#' @return A tibble with dependent (child) annotations in wide format
#' @export
#' @importFrom rlang .data
#'
pivot_children <- function(data, long = FALSE) {

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

  if (long) {

    long_children <-
      all_annotations |>
      dplyr::mutate(parent_ref = .data$tier) |>
      dplyr::select(-dplyr::all_of(c("tier", "annotation"))) |>
      tidyr::pivot_longer(cols = dplyr::all_of(child_tiers),
                          names_to = "tier",
                          values_to = "annotation") |>
      dplyr::filter(.data$tier %in% child_tiers)

    parent_annotations |>
      dplyr::bind_rows(long_children)

  } else {

    all_annotations

  }

}
