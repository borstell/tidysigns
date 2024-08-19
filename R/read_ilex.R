#' Reads iLex (.ilex) file(s) from a path input
#'
#' @param path Path to iLex (.ilex) file(s)
#' @param recursive iLex (.ilex) files are read recursively in path (defaults to `TRUE`)
#'
#' @return Tibble of the iLex annotations
#' @export
#' @importFrom rlang .data
#'
read_ilex <- function(path, recursive = TRUE) {


  # Check that path is an existing file or directory
  stopifnot("Error: Path does not exist!" = (dir.exists(path) | file.exists(path) | (startsWith(path, "http") & endsWith(path, "ilex"))))

  # If path is an .ilex file, append to vector
  if (tools::file_ext(path) == "ilex") {
    filenames <- c(path)
  }

  # If path is directory, append all .ilex files within it to vector
  else {
    filenames = list.files(path = path,
                           pattern="*.ilex$",
                           full.names = TRUE,
                           recursive = recursive)
  }

  # Check that at least one .lex file is found
  stopifnot("Error: No .ilex files found!" = length(filenames) >= 1)

  # Function to read individual .ilex file
  read_ilex1 <- function(f) {

    # Read as .xml
    xml <- xml2::read_xml(f)

    # Get participant metadata
    participation <-
      xml |>
      xml2::xml_find_all("participation") |>
      xml2::xml_attrs() |>
      dplyr::bind_rows() |>
      dplyr::select(dplyr::all_of(c("id", "informant"))) |>
      dplyr::rename_with(~ c("participation", "id"), c("id", "informant"))

    # Get more participant metadata
    informants <-
      xml |>
      xml2::xml_find_all("informant") |>
      xml2::xml_attrs() |>
      dplyr::bind_rows() |>
      dplyr::select(dplyr::all_of(c("id", "name"))) |>
      dplyr::rename("participant" = dplyr::all_of("name")) |>
      dplyr::left_join(participation, by = dplyr::join_by("id")) |>
      dplyr::select(-dplyr::all_of("id"))

    # Get tier data
    tiers <-
      xml |>
      xml2::xml_find_all("tier") |>
      xml2::xml_attrs() |>
      dplyr::bind_rows()

    # Get all annotation tags
    tags <-
      xml |>
      xml2::xml_find_all("tag") |>
      purrr::map(
        \(x) {
          c(
            id = xml2::xml_attr(x, "id"),
            tier = xml2::xml_attr(x, "tier"),
            value = xml2::xml_attr(x, "value"),
            token_dom = xml2::xml_attr(x, "token_dom"),
            token_nondom = xml2::xml_attr(x, "token_nondom"),
            timecode_start = xml2::xml_attr(x, "timecode_start"),
            timecode_end = xml2::xml_attr(x, "timecode_end")
          )
        }
      ) |>
      dplyr::bind_rows() |>
      dplyr::filter(.data$tier != "1") |>
      dplyr::rename("annotation" = dplyr::all_of("value"))

    # Filter independent tier tags
    indep_tags <-
      tags |>
      dplyr::filter(!is.na(.data$annotation)) |>
      dplyr::select(-dplyr::all_of(c("token_dom", "token_nondom")))

    # Filter dependent tier tags for further processing
    dep_tags <-
      tags |>
      dplyr::filter(is.na(.data$annotation)) |>
      tidyr::pivot_longer(dplyr::all_of(c("token_dom", "token_nondom")),
                          names_to = "hand", values_to = "token_id") |>
      dplyr::filter(!is.na(.data$token_id))

    # Get sign type data
    types <-
      xml |>
      xml2::xml_find_all("type") |>
      xml2::xml_attrs() |>
      dplyr::bind_rows()

    # Get token data
    tokens <-
      xml |>
      xml2::xml_find_all("token") |>
      xml2::xml_attrs() |>
      dplyr::bind_rows() |>
      dplyr::select(-dplyr::all_of("hamnosys")) |>
      dplyr::left_join(types, by = dplyr::join_by("type" == "id")) |>
      dplyr::rename_with(~ c("lexeme", "token_id"), c("parent", "id"))

    # Get lexeme data
    lexemes <-
      types |>
      dplyr::select(dplyr::all_of(c("id", "name", "english")))

    # Join independent annotations with tier data
    indep_annotations <-
      indep_tags |>
      dplyr::left_join(tiers, by = dplyr::join_by("tier" == "id")) |>
      dplyr::select(-dplyr::all_of("tier")) |>
      dplyr::rename("tier" = dplyr::all_of("name")) |>
      dplyr::left_join(informants, by = dplyr::join_by("participation"))

    # Join dependent annotations with tier, token and lexeme data
    dep_annotations <-
      dep_tags |>
      dplyr::left_join(tiers, by = dplyr::join_by("tier" == "id")) |>
      dplyr::select(-dplyr::all_of("tier")) |>
      dplyr::rename("tier" = dplyr::all_of("name")) |>
      dplyr::left_join(informants, by = dplyr::join_by("participation")) |>
      dplyr::left_join(tokens, by = dplyr::join_by("token_id")) |>
      dplyr::mutate(annotation = .data$name) |>
      dplyr::select(-dplyr::all_of("name")) |>
      dplyr::rename("annotation_english" = dplyr::all_of("english")) |>
      dplyr::left_join(lexemes, by = dplyr::join_by("lexeme" == "id")) |>
      dplyr::select(-dplyr::all_of("lexeme")) |>
      dplyr::rename("lexeme" = dplyr::all_of("name"),
                    "lexeme_english" = dplyr::all_of("english")) |>
      dplyr::mutate(hand = gsub("token_", "", .data$hand)) |>
      dplyr::rowwise() |>
      dplyr::mutate(tier = paste0(.data$tier, "_", .data$hand), .data$hand) |>
      dplyr::ungroup() |>
      dplyr::select(-dplyr::all_of("hand"))

    # Bind independent and dependent annotations
    dplyr::bind_rows(indep_annotations, dep_annotations) |>
      dplyr::mutate(a = paste0("a", .data$id)) |>

      # Make time codes milliseconds from H:M:S:frames(max:50) format
      dplyr::mutate(start = gsub("(\\d{2}):(\\d{2}$)", "\\1", .data$timecode_start),
                    end = gsub("(\\d{2}):(\\d{2}$)", "\\1", .data$timecode_end)) |>
      dplyr::mutate(start_ms = as.numeric(gsub("(\\d{2}:\\d{2}:\\d{2}):(\\d{2}$)", "\\2", .data$timecode_start))*(1000/50),
                    end_ms = as.numeric(gsub("(\\d{2}:\\d{2}:\\d{2}):(\\d{2}$)", "\\2", .data$timecode_end))*(1000/50)) |>
      dplyr::mutate(start = as.numeric(lubridate::seconds(lubridate::hms(.data$start)))*1000+as.numeric(.data$start_ms),
                    end = as.numeric(lubridate::seconds(lubridate::hms(.data$end)))*1000+as.numeric(.data$end_ms)) |>
      dplyr::mutate(duration = .data$end - .data$start,
                    file = basename(f)) |>

      # Select relevant columns in the right order
      dplyr::select(dplyr::all_of(c("file", "a", "annotation", "tier", "tier_kind", "participant",
                                    "modification", "annotation_english", "lexeme", "lexeme_english",
                                    "hamnosys", "start", "end", "duration")))

  }

  # Read all files
  filenames |>
    purrr::map(read_ilex1, .progress = TRUE) |>
    purrr::list_rbind()

}
