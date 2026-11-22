# The trait hierarchy, data/APD_trait_hierarchy.csv, records each group's place
# twice: in `Parent`, the one link the RDF publishes (as skos:broader), and as
# the full path from the top, written out in `tier_1`, `tier_2`, ... and again as
# text in `hierarchy` ("leaf size trait < leaf morphology trait < ...").
#
# Only `Parent` is edited by hand. The path columns are derived from it by
# `make sync-hierarchy`, and `make check` fails when they have drifted -- before
# that check existed they had, for 38 of 81 groups, so nothing that read them
# saw the hierarchy the RDF publishes. They are kept rather than dropped because
# austraits.build reads `hierarchy` to build the `trait_group` field of its
# traits.yml.

#' Root-to-leaf label path for every trait group
#'
#' Follows each group's `Parent` link up the hierarchy. The root, "plant trait",
#' is left off: every path would start with it.
#'
#' @param hierarchy The `hierarchy` table from `apd_read_inputs()`.
#' @return A named list of character vectors of labels, keyed by group
#'   identifier; the root's is empty.
apd_trait_group_paths <- function(hierarchy) {

  parent <- stats::setNames(sub(".*/", "", hierarchy$Parent),
                            hierarchy$identifier)
  label <- stats::setNames(hierarchy$label, hierarchy$identifier)

  path_to <- function(id) {
    path <- character(0)
    while (parent[[id]] %in% names(parent)) {
      if (label[[id]] %in% path) {
        stop("The trait hierarchy has a cycle at ", id, call. = FALSE)
      }
      path <- c(label[[id]], path)
      id <- parent[[id]]
    }
    path
  }

  stats::setNames(lapply(hierarchy$identifier, path_to), hierarchy$identifier)
}

#' The path columns, derived from `Parent`
#'
#' @param hierarchy The `hierarchy` table from `apd_read_inputs()`.
#' @param n_tiers How many `tier_*` columns to fill; at least the depth of the
#'   deepest group.
#' @return A tibble of `identifier`, `tier_1` ... `tier_<n_tiers>` and
#'   `hierarchy`, with "" where a group has no value.
apd_hierarchy_path_columns <- function(hierarchy,
                                       n_tiers = apd_hierarchy_depth(hierarchy)) {

  paths <- apd_trait_group_paths(hierarchy)

  tiers <- lapply(seq_len(n_tiers), function(i) {
    vapply(paths, function(path) if (i <= length(path)) path[[i]] else "",
           character(1), USE.NAMES = FALSE)
  })
  names(tiers) <- paste0("tier_", seq_len(n_tiers))

  tibble::tibble(
    identifier = hierarchy$identifier,
    !!!tiers,
    hierarchy = vapply(paths, function(path) paste(rev(path), collapse = " < "),
                       character(1), USE.NAMES = FALSE)
  )
}

#' Depth of the deepest trait group, not counting the root
apd_hierarchy_depth <- function(hierarchy) {
  max(lengths(apd_trait_group_paths(hierarchy)))
}

#' Groups whose path columns disagree with `Parent`
#'
#' @param hierarchy The `hierarchy` table from `apd_read_inputs()`.
#' @return The identifiers of the groups that `make sync-hierarchy` would
#'   change, including every group if a needed `tier_*` column is missing.
apd_stale_hierarchy_paths <- function(hierarchy) {

  expected <- apd_hierarchy_path_columns(hierarchy)
  columns <- setdiff(names(expected), "identifier")

  if (!all(columns %in% names(hierarchy))) {
    return(hierarchy$identifier)
  }

  actual <- dplyr::mutate(hierarchy[, columns],
                          dplyr::across(dplyr::everything(),
                                        ~ dplyr::coalesce(as.character(.x), "")))
  differs <- Reduce(`|`, lapply(columns, function(column) {
    actual[[column]] != expected[[column]]
  }))

  hierarchy$identifier[differs]
}
