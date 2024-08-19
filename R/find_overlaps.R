#' Find overlaps in annotations within and across participants
#'
#' @param data A tibble of annotations
#'
#' @return A tibble with added columns of overlaps (self and other)
#' @export
#' @importFrom rlang .data
#'
find_overlaps <- function(data) {

  # Find overlap locations (partly based on: https://stackoverflow.com/a/62016952)

  # Extract interlocutor info
  data |>
    dplyr::group_by(.data$file) |>
    dplyr::mutate(interloc = list(unique(.data$participant))) |>
    dplyr::rowwise() |>
    dplyr::mutate(interloc = list(setdiff(.data$interloc, .data$participant))) |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::across(dplyr::all_of("interloc"), as.list)) |>

    # Check where overlaps happen with interlocutor
    dplyr::mutate(overlaps_with =
                    purrr::map2_vec(.data$start, .data$end,
                                    ~list(.data$participant[(.x >= .data$start & .x < .data$end) | (.y > .data$start & .y <= .data$end) | (.x == .data$start & .y == .data$end) | (.x < .data$start & .y > .data$end)])),
                  .by = c("file")) |>
    dplyr::mutate(overlap_other = dplyr::case_when(
      unlist(
        purrr::pmap(list(.data$interloc, .data$overlaps_with), function(x, y) any(x %in% y), .progress = T))==TRUE ~ "overlap",
      .default = "no overlap"
    )) |>

    # Check where overlaps happen with self
    dplyr::arrange(.data$file, .data$start, .data$end) |>
    dplyr::mutate(current_max_end = cummax(dplyr::lag(.data$end, default = dplyr::first(.data$start))), .by = c("file", "participant")) |>
    dplyr::mutate(overlap_self = dplyr::case_when(
      .data$start == dplyr::lead(.data$start) & .data$end == dplyr::lead(.data$end) ~ "overlap",
      .data$start == dplyr::lag(.data$start, default = -1) & .data$end == dplyr::lag(.data$end, default = -1) ~ "overlap",
      .data$end > dplyr::lead(.data$start) | .data$start < .data$current_max_end ~ "overlap",
      .default = "no overlap"
    ), .by = c("file", "participant")) |>

    # Clean up and rearrange tibble
    dplyr::select(-dplyr::all_of(c("interloc", "overlaps_with", "current_max_end"))) |>
    dplyr::arrange(.data$file, .data$start)

}
