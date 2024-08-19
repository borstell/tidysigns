#' Infer turns from data
#'
#' @param data A tibble of annotations
#' @param method Method for inferring turns, "interval" or "sequential" (default = "sequential")
#' @param interval Threshold for inferring turns with "interval" method (default = 500)
#' @param simplify Whether to collapse rows to turns only (default = `FALSE`)
#'
#' @return A tibble with inferred turns added
#' @export
#' @importFrom rlang .data
#'
find_turns <- function(data, method = "sequential", interval = 500, simplify = FALSE) {

  # Define function for turn segments within pause-based intervals
  turn_interval <- function(data, interval = interval, simplify = simplify) {

    df <- data |>

      # Arrange data in time-descending order (within file and participant)
      dplyr::arrange(.data$file, .data$participant, .data$start, .data$end) |>

      # Check what the highest end time seen is at each incremental point
      dplyr::mutate(longest_span = cummax(.data$end),
                    .by = c(.data$file, .data$participant)) |>

      # Compare highest end time seen with defined interval to identify pauses
      dplyr::mutate(gap = dplyr::if_else(
        is.na(dplyr::lag(.data$end)),
        TRUE,
        .data$start - dplyr::lag(.data$longest_span) >= interval),
        .by = c(.data$file, .data$participant)) |>

      # Create turns based on pauses
      dplyr::mutate(turn = cumsum(.data$gap),
                    .by = c(.data$file, .data$participant))

    # This step pivots annotations into turns
    if (simplify) {

      df |>
        dplyr::group_by(.data$file, .data$participant, .data$turn) |>
        dplyr::mutate(annotations = paste0(.data$annotation, collapse = " ")) |>
        dplyr::mutate(start = min(.data$start),
                      end = max(.data$end)) |>
        dplyr::slice(1) |>
        dplyr::select(-dplyr::all_of(c("annotation", "gap", "longest_span"))) |>
        dplyr::ungroup()

    } else {

      df |>
        dplyr::select(-dplyr::all_of(c("gap", "longest_span")))

    }

  }

  # Define function for strictly sequential turn segments by start time
  turn_seq <- function(data, simplify = TRUE) {

    # Define function for turn segments within pause-based intervals
    df <- data |>
      dplyr::arrange(.data$file, .data$start, .data$end) |>
      dplyr::mutate(turn = dplyr::consecutive_id(.data$participant),
                    .by = .data$file)

    # This step pivots annotations into turns
    if (simplify) {

      df |>
        dplyr::group_by(.data$file, .data$participant, .data$turn) |>
        dplyr::mutate(annotations = paste0(.data$annotation, collapse = " ")) |>
        dplyr::mutate(start = min(.data$start),
                      end = max(.data$end)) |>
        dplyr::slice(1) |>
        dplyr::select(-dplyr::all_of("annotation")) |>
        dplyr::ungroup()

    } else {

      df

    }

  }

  stopifnot('Error! Method must be either `"interval"` or `"sequential"`' = (method %in% c("i", "interval", "s", "seq", "sequential")))

  if (method %in% c("s", "seq", "sequential")) {

    turn_seq(data, simplify = simplify)

  } else {

    turn_interval(data, interval = interval, simplify = simplify)

  }

}
