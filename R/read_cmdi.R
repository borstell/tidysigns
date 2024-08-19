#' Reads CMDI (.cmdi) file(s) from a path input
#'
#' @param path Path to CMDI (.cmdi) metadata file(s)
#' @param meta Which metadata to be collected: "signers"/"content" (default = `"signers"`)
#' @param recursive CMDI (.cmdi) files are read recursively in path (defaults to `TRUE`)
#'
#' @return Tibble of the CMDI data
#' @export
#'
read_cmdi <- function(path, meta = "signers", recursive = TRUE) {

  # Check that path is an existing file or directory
  stopifnot("Error: Path does not exist!" = (dir.exists(path) | file.exists(path) | (startsWith(path, "http") & endsWith(path, "cmdi"))))

  # If path is an .cmdi file, append to vector
  if (tools::file_ext(path) == "cmdi") {
    filenames <- c(path)
  }

  # If path is directory, append all .cmdi files within it to vector
  else {
    filenames = list.files(path = path,
                           pattern="*.cmdi$",
                           full.names = TRUE,
                           recursive = recursive)
  }

  # Check that at least one .cmdi file is found
  stopifnot("Error: No .cmdi files found!" = length(filenames) >= 1)


  read_cmdi1 <- function(f) {
    # Parse XML structure of CMDI file
    xml <- xml2::read_xml(f)

    # Extract filename
    filename <-
      xml |>
      xml2::xml_find_first(".//*[local-name()='Name']") |>
      xml2::xml_text()

    if (tolower(meta) != "signers") {

      # Extract date
      filedate <-
        xml |>
        xml2::xml_find_first(".//*[local-name()='Date']") |>
        xml2::xml_text()

      # Collect file contents metadata in a tibble
      xml |>
        xml2::xml_find_all(".//*[local-name()='SL-Content' or local-name()='Content']") |>
        purrr::map(~ dplyr::tibble(
          metafile = filename,
          date = filedate,
          genre = .x |> xml2::xml_find_first(".//*[local-name()='Genre']") |> xml2::xml_text(),
          subgenre = .x |> xml2::xml_find_first(".//*[local-name()='SubGenre']") |> xml2::xml_text(),
          task = .x |> xml2::xml_find_first(".//*[local-name()='Task']") |> xml2::xml_text(),
          subject = .x |> xml2::xml_find_first(".//*[local-name()='Subject']") |> xml2::xml_text(),
          modality = .x |> xml2::xml_find_first(".//*[local-name()='Modalities']") |> xml2::xml_text(),
          interactivity = .x |> xml2::xml_find_first(".//*[local-name()='Interactivity']") |> xml2::xml_text(),
          planning_type = .x |> xml2::xml_find_first(".//*[local-name()='PlanningType']") |> xml2::xml_text(),
          involvement = .x |> xml2::xml_find_first(".//*[local-name()='Involvement']") |> xml2::xml_text(),
          social_context = .x |> xml2::xml_find_first(".//*[local-name()='SocialContext']") |> xml2::xml_text(),
          event_structure = .x |> xml2::xml_find_first(".//*[local-name()='EventStructure']") |> xml2::xml_text(),
          channel = .x |> xml2::xml_find_first(".//*[local-name()='Channel']") |> xml2::xml_text())
        ) |>
        purrr::list_rbind()

    } else {

      # Collect file signer metadata in a tibble
      xml |>
        xml2::xml_find_all(".//*[local-name()='SL-Actor' or local-name()='Actor']") |>
        purrr::map(~ dplyr::tibble(
          metafile = filename,
          participant = .x |>  xml2::xml_find_first(".//*[local-name()='Code']") |> xml2::xml_text(),
          role = .x |>  xml2::xml_find_first(".//*[local-name()='Role']") |> xml2::xml_text(),
          age = .x |> xml2::xml_find_first(".//*[local-name()='ExactAge']") |> xml2::xml_text(),
          estimated_age = .x |> xml2::xml_find_first(".//*[local-name()='EstimatedAge']") |> xml2::xml_text(),
          min_age = .x |> xml2::xml_find_first(".//*[local-name()='MinimumAge']") |> xml2::xml_text(),
          max_age = .x |> xml2::xml_find_first(".//*[local-name()='MaximumAge']") |> xml2::xml_text(),
          sex = .x |> xml2::xml_find_first(".//*[local-name()='Sex']") |> xml2::xml_text(),
          region = .x |> xml2::xml_find_first(".//*[local-name()='Region']") |> xml2::xml_text(),
          age_of_exposure = .x |> xml2::xml_find_first(".//*[local-name()='ExposureAge']") |> xml2::xml_text(),
          location_of_exposure = .x |> xml2::xml_find_first(".//*[local-name()='AcquisitionLocation']") |> xml2::xml_text(),
          family = .x |> xml2::xml_find_first(".//*[local-name()='Family']") |> xml2::xml_text(),
          family_social = .x |> xml2::xml_find_first(".//*[local-name()='FamilySocialRole']") |> xml2::xml_text(),
          education = .x |> xml2::xml_find_first(".//*[local-name()='Education']") |> xml2::xml_text(),
          ethnic_group = .x |> xml2::xml_find_first(".//*[local-name()='EthnicGroup']") |> xml2::xml_text(),
          l1 = .x |> xml2::xml_find_first(".//*[local-name()='MotherTongue']") |> xml2::xml_text(),
          primary_language = .x |> xml2::xml_find_first(".//*[local-name()='PrimaryLanguage']") |> xml2::xml_text(),
          deafness = .x |> xml2::xml_find_first(".//*[local-name()='SL_Deafness']") |> xml2::xml_text(),
          handedness = .x |> xml2::xml_find_first(".//*[local-name()='Handedness']") |> xml2::xml_text())
        ) |>
        purrr::list_rbind()

    }
  }

  # Read all files
  filenames |>
    purrr::map(read_cmdi1, .progress = TRUE) |>
    purrr::list_rbind()

}

