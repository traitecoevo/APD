# The browse table on the landing page: one row per trait concept, filterable in
# the browser, each row linking to that trait's own page.
#
# This is what replaces a 9 MB single page as the front door. The whole dictionary
# is still published as one document -- full.html -- but you no longer download it
# to look up one trait.

#' Build the browse table for the landing page
#'
#' @param traits The trait table, as written to `APD_traits.csv`.
#' @return A tibble of `trait`, `label`, `groups`, `type`, `units` and `path`.
apd_browse_table <- function(traits) {

  # Strip the "[identifier]" suffix the published table carries: on screen the
  # label is what you scan for, and the identifier is on the trait's own page.
  plain <- function(x) trimws(gsub("\\s*\\[[^]]*\\]", "", x))

  tibble::tibble(
    trait = traits$trait,
    label = traits$label,
    groups = plain(traits$trait_groupings),
    type = plain(traits$trait_type),
    units = ifelse(is.na(traits$units), "", traits$units),
    path = apd_entity_path(traits$Entity)
  )
}

#' Render the browse table as filterable HTML
#'
#' No JavaScript framework: a `<table>` plus about 20 lines of vanilla JS that
#' hides rows whose text does not match. 559 rows is small enough that filtering
#' in the DOM is instant, and it degrades to a plain sortable-by-eye table with
#' JavaScript off.
#'
#' @param browse Output of `apd_browse_table()`.
#' @return HTML as a single string.
apd_browse_html <- function(browse) {

  rows <- paste0(
    "<tr>",
    "<td><a href=\"", browse$path, "\">", apd_escape(browse$trait), "</a></td>",
    "<td>", apd_escape(browse$label), "</td>",
    "<td>", apd_escape(browse$groups), "</td>",
    "<td>", apd_escape(browse$units), "</td>",
    "</tr>",
    collapse = "\n"
  )

  paste0(
    '<p><label for="apd-filter">Filter ', nrow(browse), ' traits</label></p>\n',
    '<input type="search" id="apd-filter" autocomplete="off"\n',
    '       placeholder="leaf nitrogen, seed mass, fire response...">\n',
    '<p id="apd-filter-count" aria-live="polite"></p>\n',
    '<div class="apd-browse-wrap">\n',
    '<table class="apd-browse" id="apd-browse">\n',
    "<thead><tr><th>Trait</th><th>Label</th><th>Trait groups</th>",
    "<th>Units</th></tr></thead>\n<tbody>\n",
    rows,
    "\n</tbody>\n</table>\n</div>\n",
    apd_browse_script(nrow(browse))
  )
}

#' The filter script for the browse table
apd_browse_script <- function(total) {
  paste0(
    "<script>\n",
    "(function () {\n",
    "  var input = document.getElementById('apd-filter');\n",
    "  var count = document.getElementById('apd-filter-count');\n",
    "  var rows = Array.prototype.slice.call(\n",
    "    document.querySelectorAll('#apd-browse tbody tr'));\n",
    "  var haystacks = rows.map(function (row) {\n",
    "    return row.textContent.toLowerCase();\n",
    "  });\n",
    "  var total = ", total, ";\n",
    "\n",
    "  function apply() {\n",
    "    var terms = input.value.toLowerCase().split(/[^a-z0-9_.-]+/)\n",
    "      .filter(function (t) { return t.length > 0; });\n",
    "    var shown = 0;\n",
    "    for (var i = 0; i < rows.length; i++) {\n",
    "      var hay = haystacks[i];\n",
    "      var match = terms.every(function (t) { return hay.indexOf(t) !== -1; });\n",
    "      rows[i].hidden = !match;\n",
    "      if (match) shown++;\n",
    "    }\n",
    "    count.textContent = terms.length === 0 ? '' :\n",
    "      shown + ' of ' + total + ' traits match';\n",
    "  }\n",
    "\n",
    "  input.addEventListener('input', apply);\n",
    "  apply();\n",
    "})();\n",
    "</script>\n"
  )
}

#' A shim redirecting legacy `index.html#slug` bookmarks to the entity's page
#'
#' Google has indexed those fragments for years, and every trait link published
#' before this change was `index.html#trait_0000012`. The single page they pointed
#' into is now `full.html`, so without this they land on a browse table with no
#' explanation.
#'
#' Runs synchronously in `<head>` so the redirect happens before anything renders.
#'
#' @return HTML as a single string.
apd_legacy_fragment_shim <- function() {
  paste0(
    "<script>\n",
    "// Legacy bookmarks and search results point at index.html#<slug>, where\n",
    "// <slug> is a trait, trait group, categorical value or glossary term. Those\n",
    "// now have pages of their own. Runs before render, so there is no flash of\n",
    "// the wrong page.\n",
    "(function () {\n",
    "  var slug = window.location.hash.replace(/^#/, '');\n",
    "  if (!slug || !/^[A-Za-z][A-Za-z0-9_.-]*$/.test(slug)) return;\n",
    "  var dir = /^glossary_/.test(slug) ? 'glossary/' : 'traits/';\n",
    "  window.location.replace(dir + slug + '.html');\n",
    "})();\n",
    "</script>\n"
  )
}
