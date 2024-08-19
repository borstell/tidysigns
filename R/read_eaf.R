#' Reads ELAN (.eaf) file(s) from a path input
#'
#' @param path Path to ELAN (.eaf) file(s)
#' @param tiers Specify tiers (TIER_ID or TIER_TYPE) to be read (default = `c()`)
#' @param xpath Specify a detailed XPath for tiers to be read
#' @param recursive ELAN (.eaf) files are read recursively in path (defaults to `TRUE`)
#'
#' @return Tibble of the ELAN annotations
#' @export
#' @importFrom rlang .data
#'
read_eaf <- function(path, tiers = c(), xpath = "", recursive = TRUE) {

  # Check that path is an existing file or directory
  stopifnot("Error: Path does not exist!" = (dir.exists(path) | file.exists(path) | (startsWith(path, "http") & endsWith(path, "eaf"))))

  # If path is an .eaf file, append to vector
  if (tools::file_ext(path) == "eaf") {
    filenames <- c(path)
  }

  # If path is directory, append all .eaf files within it to vector
  else {
    filenames = list.files(path = path,
                           pattern="*.eaf$",
                           full.names = TRUE,
                           recursive = recursive)
  }

  # Check that at least one .eaf file is found
  stopifnot("Error: No .eaf files found!" = length(filenames) >= 1)

  # Function to read individual .eaf file
  read_eaf1 <- function(f) {

    # Read as .xml
    eaf <- xml2::read_xml(f)

    # Make tibble from timestamp data
    ts <- xml2::xml_find_all(eaf, ".//TIME_SLOT")
    times <- dplyr::tibble(TIME_SLOT_ID = xml2::xml_attr(ts, "TIME_SLOT_ID"),
                           TIME_VALUE = xml2::xml_attr(ts, "TIME_VALUE"))

    # Restrict parse to custom selected tiers/tier types only
    if (all(is.vector(tiers) & rlang::is_named(tiers))) {
      # Translate input (expected labels and case of input)
      new_tiers <- tiers
      names(new_tiers) <- gsub("^TIER_TYPE$", "LINGUISTIC_TYPE_REF", gsub("^TIER$", "TIER_ID", toupper(names(tiers))))
      tier_attrs <- paste0(".//TIER[", paste0("@", names(new_tiers), "='", new_tiers, "'", collapse = " or "), "]")
    } else {
      if (xpath != "") {
        tier_attrs <- paste0(".//TIER", xpath)
      } else {
        tier_attrs <- ".//TIER"
      }
    }

    # Make tibble from annotations
    annotations <-
      eaf |>
      xml2::xml_find_all(tier_attrs) |>
      xml2::xml_children() |>
      xml2::xml_children()

    # Iterate through nodes and find parent attributes
    if (length(annotations) > 0) {
      annotations <-
        annotations |>
        purrr::map(
          \(x)
            c(
              a = xml2::xml_attr(x, "ANNOTATION_ID"),
              ts1 = xml2::xml_attr(x, "TIME_SLOT_REF1"),
              ts2 = xml2::xml_attr(x, "TIME_SLOT_REF2"),
              annotation = xml2::xml_text(x),
              tier = xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(x)), "TIER_ID"),
              tier_type = xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(x)), "LINGUISTIC_TYPE_REF"),
              participant = xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(x)), "PARTICIPANT"),
              annotator = xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(x)), "ANNOTATOR"),
              parent_ref = xml2::xml_attr(xml2::xml_parent(xml2::xml_parent(x)), "PARENT_REF"),
              a_ref = xml2::xml_attr(x, "ANNOTATION_REF")
            )
        ) |>
        dplyr::bind_rows() |>

        # Mutate columns and join with timestamp data
        dplyr::left_join(dplyr::rename(times, start = dplyr::all_of("TIME_VALUE")), by = dplyr::join_by("ts1" == "TIME_SLOT_ID")) |>
        dplyr::left_join(dplyr::rename(times, end = dplyr::all_of("TIME_VALUE")), by = dplyr::join_by("ts2" == "TIME_SLOT_ID")) |>
        dplyr::mutate(dplyr::across(dplyr::all_of("start"):dplyr::all_of("end"), as.double)) |>
        dplyr::mutate(file = basename(f),
                      duration = .data$end - .data$start) |>
        dplyr::relocate(dplyr::all_of("file"), .before = 1)
      }

    }

  # Read all files
  filenames |>
    purrr::map(read_eaf1, .progress = TRUE) |>
    purrr::list_rbind()

}
