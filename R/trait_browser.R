# The trait browser: a zoomable tree of every trait, grouped by trait group and
# searchable by structure, characteristic and keyword. Published at
# https://traitecoevo.github.io/APD/trait_browser.html.
#
# `assets/trait_browser.html` is the page with an empty
# `<script type="application/json" id="trait-data">` block. `make site` fills
# that block from data/ and writes the result into docs/, so the browser is
# rebuilt with the rest of the site and goes live only at a release, like
# everything else on it. The page carries its data inline because it is also
# embedded elsewhere (the AusTraits website and data portal), and one file with
# no second request is the simplest thing to embed.

# The empty data block in the template, matched literally.
APD_TRAIT_BROWSER_BLOCK <- '<script type="application/json" id="trait-data"></script>'

# Split a `"; "`-delimited input field; NA and "" are empty lists, not `"NA"`.
apd_split_field <- function(value) {
  if (is.na(value) || value == "") {
    return(character(0))
  }
  stringr::str_trim(stringr::str_split(value, "; ")[[1]])
}

#' The trait groups a trait is drawn under
#'
#' Each entry in a trait's `category` field is a group it belongs to in its own
#' right, so a trait listed under two groups gets a leaf under each. An entry
#' that is an ancestor of another entry adds nothing once its descendant is
#' drawn, and is dropped.
#'
#' @param ids Group identifiers from one trait's `category` field.
#' @param paths Output of `apd_trait_group_paths()` (R/hierarchy.R).
#' @return A list of `list(path, resolved)`; `resolved` is `FALSE` for an
#'   identifier missing from the hierarchy, whose path is the identifier alone.
#'   The root group is in the hierarchy with an empty path, so it is resolved,
#'   and dropped as an ancestor whenever the trait names any other group.
apd_trait_end_groups <- function(ids, paths) {

  groups <- lapply(unique(sub("^APD:", "", ids)), function(id) {
    path <- paths[[id]]
    if (is.null(path)) {
      list(path = id, resolved = FALSE)
    } else {
      list(path = path, resolved = TRUE)
    }
  })

  is_prefix <- function(a, b) {
    length(b) > length(a) && identical(b[seq_along(a)], a)
  }
  keys <- lapply(groups, `[[`, "path")
  is_ancestor <- vapply(keys, function(key) {
    any(vapply(keys, function(other) is_prefix(key, other), logical(1)))
  }, logical(1))

  groups[!is_ancestor]
}

#' One record per trait, in the shape the browser reads
#'
#' Identifiers are resolved to labels the same way `apd_traits_table()` does,
#' against the published classes plus the glossary, but kept as lists rather
#' than pasted into one string: some labels contain `"; "` themselves
#' ("ratio; proportion"), so the flat CSV cannot be split back apart reliably.
#'
#' @param inputs Output of `apd_read_inputs()`.
#' @return A list of trait records, ordered by trait name.
apd_trait_browser_data <- function(inputs) {

  traits <- dplyr::arrange(inputs$traits, trait)
  paths <- apd_trait_group_paths(inputs$hierarchy)

  terms <- dplyr::bind_rows(
    dplyr::select(inputs$published_classes, identifier, label),
    dplyr::select(inputs$glossary, identifier, label)
  )

  # An identifier with no label is shown as itself rather than dropped, so the
  # gap stays visible in the browser.
  labels_for <- function(value) {
    ids <- apd_split_field(value)
    found <- apd_lookup(ids, terms, "identifier", "label")
    ifelse(is.na(found), ids, found)
  }

  text <- function(value) if (is.na(value)) "" else value

  lapply(seq_len(nrow(traits)), function(i) {
    trait <- traits[i, ]
    structure <- labels_for(trait$structure)
    keywords <- labels_for(trait$keywords)

    # I() keeps one-element vectors as JSON arrays under `auto_unbox`.
    list(
      id = trait$trait,
      uri = paste0(trait$inScheme, "/traits/", trait$identifier),
      label = text(trait$label),
      description = text(trait$description),
      type = text(dplyr::coalesce(
        apd_lookup(trait$type, terms, "identifier", "label"), trait$type
      )),
      units = text(trait$units),
      structure_measured = I(structure),
      characteristic_measured = I(labels_for(trait$measured_characteristic)),
      trait_groups = lapply(
        apd_trait_end_groups(apd_split_field(trait$category), paths),
        function(group) list(path = I(group$path), resolved = group$resolved)
      ),
      keywords = I(keywords),
      terms_core = I(unique(c(structure, keywords)))
    )
  })
}

#' Write the trait browser, with its data filled in
#'
#' @param inputs Output of `apd_read_inputs()`.
#' @param template The page with an empty data block.
#' @param out Where to write the filled-in page.
#' @param version The APD version the page reports.
#' @param built The build date the page reports.
#' @return The number of traits written, invisibly.
apd_write_trait_browser <- function(inputs,
                                    template = file.path("assets",
                                                         "trait_browser.html"),
                                    out = file.path("docs", "trait_browser.html"),
                                    version = apd_version(),
                                    built = Sys.Date()) {

  page <- readr::read_file(template)

  n_blocks <- lengths(regmatches(page, gregexpr(APD_TRAIT_BROWSER_BLOCK, page,
                                                fixed = TRUE)))
  if (n_blocks != 1) {
    stop(template, " has ", n_blocks, " empty trait-data blocks; expected one.",
         call. = FALSE)
  }

  traits <- apd_trait_browser_data(inputs)
  json <- jsonlite::toJSON(
    list(version = version, built = format(built), traits = traits),
    auto_unbox = TRUE
  )
  # Every `<` is written as `\u003c`, which JSON.parse reads back as `<`. A
  # literal `</script` inside the block would end the element early, and a
  # `<!--` followed by `<script` would stop the real `</script>` ending it at
  # all, so the page's own code would be read as data. Escaping `</` alone
  # covered only the first.
  json <- stringi::stri_replace_all_fixed(json, "<", "\\u003c")

  at <- regexpr(APD_TRAIT_BROWSER_BLOCK, page, fixed = TRUE)
  filled <- paste0(
    substr(page, 1, at - 1),
    '<script type="application/json" id="trait-data">', json, "</script>",
    substr(page, at + nchar(APD_TRAIT_BROWSER_BLOCK), nchar(page))
  )

  readr::write_file(filled, out)
  invisible(length(traits))
}
