#' Handle Species argument input for miEAA endpoints
#'
#' This internal function allows users to supply a supported species as an
#'   abbreviation, NCBI taxonomy identifier, or scientific name. Invalid or
#'   unsupported values produce an informative error message.
#'
#' @param sp Character or Numeric: A supported species abbreviation, NCBI
#'   taxonomy identifier, or scientific name.
#' @param to_name Logical: (default = \code{FALSE}) Convert a supplied species
#'   abbreviation to its scientific name.
#'
#' @return If \code{to_name = FALSE}, a three-letter abbreviation for a
#'   supported miEAA species. Otherwise, the corresponding scientific name.
#'
#' @examples
#' \donttest{
#' .rba_mieaa_species(9606)
#' }
#' \donttest{
#' .rba_mieaa_species("hsa", to_name = TRUE)
#' }
#'
#' @family "miEAA"
#' @noRd
.rba_mieaa_species <- function(sp, to_name = FALSE) {

  diagnostics <- get0("diagnostics", envir = parent.frame(1),
                      ifnotfound = getOption("rba_diagnostics"))

  sp_df <- data.frame(
    abbreviation = c("hsa", "mmu", "rno", "ath", "bta",
                     "cel", "dme", "dre", "gga", "ssc"),
    ncbi_taxid  = c(9606L, 10090L, 10116L, 3702L, 9913L,
                    6239L, 7227L, 7955L, 9031L, 9823L),
    species_name = c("Homo sapiens", "Mus musculus",
                     "Rattus norvegicus", "Arabidopsis thaliana",
                     "Bos taurus", "Caenorhabditis elegans",
                     "Drosophila melanogaster", "Danio rerio",
                     "Gallus gallus", "Sus scrofa"),
    stringsAsFactors = FALSE
  )

  if (isTRUE(to_name)) {

    return(sp_df$species_name[[which(sp_df$abbreviation == sp)]])

  } else {

    sp_table <- c(
      "hsa" = "hsa", "hsa" = 9606L,  "hsa" = "Homo sapiens",
      "mmu" = "mmu", "mmu" = 10090L, "mmu" = "Mus musculus",
      "rno" = "rno", "rno" = 10116L, "rno" = "Rattus norvegicus",
      "ath" = "ath", "ath" = 3702L,  "ath" = "Arabidopsis thaliana",
      "bta" = "bta", "bta" = 9913L,  "bta" = "Bos taurus",
      "cel" = "cel", "cel" = 6239L,  "cel" = "Caenorhabditis elegans",
      "dme" = "dme", "dme" = 7227L,  "dme" = "Drosophila melanogaster",
      "dre" = "dre", "dre" = 7955L,  "dre" = "Danio rerio",
      "gga" = "gga", "gga" = 9031L,  "gga" = "Gallus gallus",
      "ssc" = "ssc", "ssc" = 9823L,  "ssc" = "Sus scrofa"
    )

    sp_match <- match(
      x = tolower(sp),
      table = tolower(sp_table),
      nomatch = 0L
    )

    if (sp_match != 0L) {

      return(names(sp_table)[[sp_match]])

    } else {

      stop(
        "Species should be one of the following values:\n",
        paste(utils::capture.output(print(sp_df)), collapse = "\n"),
        call. = diagnostics
      )
    }
  }
}

#' Get Supported Enrichment Categories for a Species and miRNA Type
#'
#' Each combination of species and miRNA type supports a predefined set of
#'   enrichment categories. This function retrieves the categories available
#'   for a given combination.
#'
#' @param mirna_type Character: Type of the miRNA identifiers; either "mature"
#'   or "precursor".
#' @param species Character or Numeric: Scientific name, abbreviation, or NCBI
#'   taxon ID of one of the following species: \enumerate{
#'   \item "Homo sapiens", "hsa" or 9606
#'   \item "Mus musculus", "mmu" or 10090
#'   \item "Rattus norvegicus", "rno" or 10116
#'   \item "Arabidopsis thaliana", "ath" or 3702
#'   \item "Bos taurus", "bta" or 9913
#'   \item "Caenorhabditis elegans", "cel" or 6239
#'   \item "Drosophila melanogaster", "dme" or 7227
#'   \item "Danio rerio", "dre" or 7955
#'   \item "Gallus gallus", "gga" or 9031
#'   \item "Sus scrofa", "ssc" or  9823}
#' @param mode Character: (default = \code{"all"}) Category subset to retrieve.
#'   One of: "all" to include default and expert categories, "default" to
#'   include only default categories, or "expert" to include only expert
#'   categories.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET https://ccb-compute2.cs.uni-saarland.de/mieaa/api/v1/enrichment_categories/\{species\}/\{mirna_type\}/\{mode\}"
#'
#' @return A named character vector whose values are the supported category
#'   identifiers and whose names are their descriptions. If the selected subset
#'   has no categories, returns \code{character(0)}.
#'
#' @references \itemize{
#'   \item Ernesto Aparicio-Puerta, Pascal Hirsch, Georges P. Schmartz,
#'   Fabian Kern, Tobias Fehlmann, Andreas Keller, miEAA 2023: updates,
#'   new functional microRNA sets and improved enrichment visualizations,
#'   Nucleic Acids Research, Volume 51, Issue W1, 5 July 2023,
#'   Pages W319–W325, https://doi.org/10.1093/nar/gkad392
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/tutorial/api/}{miEAA
#'   browsable API tutorial}
#'   \item \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/}{Citation note
#'   on miEAA website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_mieaa_cats("mature", "Homo sapiens")
#' }
#'
#' @family "miEAA"
#' @export
rba_mieaa_cats <- function(mirna_type, species, mode = "all", ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "mirna_type",
        class = "character",
        val = c("mature", "precursor"),
        len = 1L
      ),
      list(
        arg = "species",
        class = c("character", "numeric", "integer"),
        len = 1L
      ),
      list(
        arg = "mode",
        class = "character",
        val = c("all", "default", "expert"),
        len = 1L,
        no_null = TRUE
      )
    )
  )

  # convert species input to abbreviation
  species <- .rba_mieaa_species(species, to_name = FALSE)

  .msg(
    "Retrieving %s enrichment categories of %s for %s.",
    mode,
    switch(
      mirna_type,
      "mature" = "miRNA",
      "precursor" = "miRNA precursor"
    ),
    .rba_mieaa_species(species, to_name = TRUE)
  )

  ## Build Function-Specific Call
  parser_input <- list(
    "json->df",
    function(x) {
      if (nrow(x) == 0L) {
        return(character())
      }

      return(stats::setNames(x[[1]], x[[2]]))
    }
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("mieaa", "url"),
    path = sprintf(
      "%senrichment_categories/%s/%s/%s",
      .rba_stg("mieaa", "pth"),
      species,
      switch(
        mirna_type,
        "mature" = "mirna",
        "precursor" = "precursor"
      ),
      mode
    ),
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("rba_mieaa_cats.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Convert miRNA Identifiers Between Different miRBase Versions
#'
#' miEAA uses miRBase v22 identifiers. This function converts a set of mature
#'   or precursor miRNA identifiers between two supported miRBase versions.
#'
#' @param mirna Character vector: miRNA identifiers to convert.
#' @param mirna_type Character: Type of the supplied miRNA identifiers; either
#'   "mature" or "precursor".
#' @param input_version Numeric: miRBase version of the supplied identifiers.
#' @param output_version Numeric: miRBase version to which the identifiers
#'   should be converted.
#' @param simple_output Logical: (default = \code{FALSE}) If \code{FALSE},
#'   return a two-column data frame containing the input and output identifier
#'   mappings. If \code{TRUE}, return only the converted identifiers without
#'   their association with the supplied identifiers.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST https://ccb-compute2.cs.uni-saarland.de/mieaa/api/v1/mirbase_converter/"
#'
#' @return Depending on \code{simple_output}, a data frame or character vector
#'   containing the mappings returned by miEAA. Unrecognized or unmapped
#'   supplied identifiers can be omitted from the output.
#'
#' @references \itemize{
#'   \item Ernesto Aparicio-Puerta, Pascal Hirsch, Georges P. Schmartz,
#'   Fabian Kern, Tobias Fehlmann, Andreas Keller, miEAA 2023: updates,
#'   new functional microRNA sets and improved enrichment visualizations,
#'   Nucleic Acids Research, Volume 51, Issue W1, 5 July 2023,
#'   Pages W319–W325, https://doi.org/10.1093/nar/gkad392
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/tutorial/api/}{miEAA
#'   browsable API tutorial}
#'   \item \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/}{Citation note
#'   on miEAA website}
#'   }
#'
#' @examples
#' \donttest{
#' Sys.sleep(1) # to prevent 429 error during R CMD check
#' rba_mieaa_convert_version(mirna = c("hsa-miR-20b-5p", "hsa-miR-144-5p"),
#'     mirna_type = "mature", input_version = 22, output_version =  16)
#' }
#'
#' @family "miEAA"
#' @export
rba_mieaa_convert_version <- function(mirna,
                                      mirna_type,
                                      input_version,
                                      output_version,
                                      simple_output = FALSE,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "mirna", class = "character", min_len = 1L),
      list(
        arg = "mirna_type",
        class = "character",
        val = c("mature", "precursor"),
        len = 1L
      ),
      list(
        arg = "input_version",
        class = c("numeric", "integer"),
        val = c(9.1, 10, 12:22),
        len = 1L
      ),
      list(
        arg = "output_version",
        class = c("numeric", "integer"),
        val = c(9.1, 10, 12:22),
        len = 1L
      ),
      list(
        arg = "simple_output",
        class = "logical",
        len = 1L,
        no_null = TRUE
      )
    )
  )

  .msg(
    "Converting %s %s miRNA IDs from miRBase v%s to v%s.",
    length(mirna),
    mirna_type,
    input_version, output_version
  )

  ## Build POST API Request's body
  call_body <- list(
    mirnas = paste(mirna, collapse = "\n"),
    mirbase_input_version = paste0("v", input_version),
    mirbase_output_version = paste0("v", output_version),
    input_type = ifelse(mirna_type == "mature", yes = "mirna", no = "precursor"),
    output_format = ifelse(isTRUE(simple_output), yes = "oneline", no = "tabsep")
  )

  ## Build Function-Specific Call
  if (isTRUE(simple_output)) {

    parser_input <- list(
      "text->df",
      function(x) { x[, 1] }
    )
    file_extension <- "txt"

  } else {

    parser_input <- list(
      "text->df",
      function(x) {
        colnames(x) <- x[1, ]
        x <- x[-1, , drop = FALSE]
        rownames(x) <- NULL
        return(x)
      }
    )
    file_extension <- "tsv"

  }

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("mieaa", "url"),
    path = sprintf("%smirbase_converter/", .rba_stg("mieaa", "pth")),
    encode = "multipart",
    body = call_body,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file(sprintf("rba_mieaa_convert_version.%s", file_extension))
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Convert Between Mature and Precursor miRNA Identifiers
#'
#' miRBase identifiers can refer to either mature or precursor miRNAs.
#'   (see: \href{https://rnajournal.cshlp.org/content/9/3/277}{A uniform system
#'   for microRNA annotation}). Use this function to convert mature miRNA
#'   identifiers to precursor identifiers or vice versa.
#'
#' @param mirna Character vector: miRNA identifiers to convert.
#' @param input_type Character: Type of the supplied miRNA identifiers; either
#'   "mature" or "precursor".
#' @param only_unique Logical: (default = \code{FALSE}) Mature and precursor
#'   miRNA identifiers do not always map uniquely. If \code{TRUE}, do not
#'   return mappings for inputs with multiple matches. In tabular output, these
#'   inputs remain as rows with \code{"-"} in the output column.
#' @param simple_output Logical: (default = \code{FALSE}) If \code{FALSE},
#'   return a two-column data frame containing the input and output identifier
#'   mappings; multiple output identifiers are separated by semicolons. If
#'   \code{TRUE}, expand one-to-many mappings into a flat character vector of
#'   converted identifiers without their association with the supplied
#'   identifiers.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST https://ccb-compute2.cs.uni-saarland.de/mieaa/api/v1/mirna_precursor_converter/"
#'
#' @return Depending on \code{simple_output}, a data frame or character vector
#'   containing the mappings returned by miEAA. Unrecognized or unmapped
#'   supplied identifiers can be omitted from the output.
#'
#' @references \itemize{
#'   \item Ernesto Aparicio-Puerta, Pascal Hirsch, Georges P. Schmartz,
#'   Fabian Kern, Tobias Fehlmann, Andreas Keller, miEAA 2023: updates,
#'   new functional microRNA sets and improved enrichment visualizations,
#'   Nucleic Acids Research, Volume 51, Issue W1, 5 July 2023,
#'   Pages W319–W325, https://doi.org/10.1093/nar/gkad392
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/tutorial/api/}{miEAA
#'   browsable API tutorial}
#'   \item \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/}{Citation note
#'   on miEAA website}
#'   }
#'
#' @examples
#' \donttest{
#' Sys.sleep(1) # to prevent 429 error during R CMD check
#' rba_mieaa_convert_type(mirna = c("hsa-miR-20b-5p", "hsa-miR-144-5p"),
#'     input_type = "mature")
#' }
#'
#' @family "miEAA"
#' @export
rba_mieaa_convert_type <- function(mirna,
                                   input_type,
                                   only_unique = FALSE,
                                   simple_output = FALSE,
                                   ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "mirna", class = "character", min_len = 1L),
      list(
        arg = "input_type",
        class = "character",
        val = c("mature", "precursor"),
        len = 1L
      ),
      list(
        arg = "only_unique",
        class = "logical",
        len = 1L,
        no_null = TRUE
      ),
      list(
        arg = "simple_output",
        class = "logical",
        len = 1L,
        no_null = TRUE
      )
    )
  )

  .msg(
    "Converting %s %s miRNA IDs to %s IDs.",
    length(mirna),
    input_type,
    ifelse(input_type == "mature", yes = "precursor", no = "mature")
  )

  ## Build POST API Request's body
  call_body <- list(
    mirnas = paste(mirna, collapse = "\n"),
    input_type = ifelse(input_type == "mature", yes = "to_precursor", no = "to_mirna"),
    output_format = ifelse(isTRUE(simple_output), yes = "newline", no = "tabsep"),
    conversion_type = ifelse(isTRUE(only_unique), yes = "unique", no = "all")
  )

  ## Build Function-Specific Call
  if (isTRUE(simple_output)) {

    parser_input <- list(
      "text->df",
      function(x) { x[, 1] }
    )
    file_extension <- "txt"

  } else {
    parser_input <- list(
      "text->df",
      function(x) {
        names(x) <- c(
          input_type,
          setdiff(c("mature", "precursor"), input_type)
        )
        return(x)
      }
    )
    file_extension <- "tsv"
  }

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("mieaa", "url"),
    path = sprintf("%smirna_precursor_converter/", .rba_stg("mieaa", "pth")),
    encode = "multipart",
    body = call_body,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file(sprintf("rba_mieaa_convert_type.%s", file_extension))
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Submit miEAA miRNA Enrichment Analysis Request
#'
#' Submit a request to the miEAA server to perform over-representation analysis
#'   or gene set enrichment analysis for a set of miRNA identifiers.
#'
#' Note that using \code{\link{rba_mieaa_enrich}} is a more convenient way to
#'   automatically perform this and other required function calls to
#'   perform enrichment analysis on your input miRNA-set using miEAA.
#'
#' @param test_set Character vector: Mature or precursor miRBase miRNA
#'   identifiers. Note that \enumerate{
#'   \item Only miRBase v22 identifiers are accepted. You can use
#'   \code{\link{rba_mieaa_convert_version}} to convert older identifiers to
#'   miRBase v22.
#'   \item The list must contain either mature or precursor miRNA identifiers,
#'   not a mixture of both.
#'   }
#' @param mirna_type Character: Type of the supplied miRNA identifiers; either
#'   "mature" or "precursor".
#' @param test_type Character: Analysis to perform; either "ORA" for
#'   over-representation analysis or "GSEA" for miRNA gene set enrichment
#'   analysis. For GSEA, the input list must already be ranked by an appropriate
#'   criterion.
#' @param species Character or Numeric: Scientific name, abbreviation, or NCBI
#'   taxon ID of one of the following species:
#'   \enumerate{
#'  \item "Homo sapiens", "hsa" or 9606
#'  \item "Mus musculus", "mmu" or 10090
#'  \item "Rattus norvegicus", "rno" or 10116
#'  \item "Arabidopsis thaliana", "ath" or 3702
#'  \item "Bos taurus", "bta" or 9913
#'  \item "Caenorhabditis elegans", "cel" or 6239
#'  \item "Drosophila melanogaster", "dme" or 7227
#'  \item "Danio rerio", "dre" or 7955
#'  \item "Gallus gallus", "gga" or 9031
#'  \item "Sus scrofa", "ssc" or  9823
#'  }
#' @param categories Character vector: (default = \code{NULL}) One or more
#'   category identifiers to use for miRNA set enrichment analysis. Note that
#'   \itemize{
#'   \item Available categories vary with the selected species and whether
#'    the supplied miRNAs are mature or precursor. Use
#'    \code{\link{rba_mieaa_cats}} to retrieve a list of available category
#'    identifiers for a given species and miRNA type.
#'   \item If \code{NULL}, the analysis is performed using all available
#'    categories.}
#' @param p_adj_method Character: (default = \code{"fdr"}) P-value adjustment
#'   method to use. One of: "none", "fdr", "bonferroni", "BY", "hochberg",
#'   "holm", or "hommel".
#' @param independent_p_adj Logical: (default = \code{TRUE}) The scope of
#'   p-value adjustment. If \code{TRUE}, p-values are adjusted separately
#'   within each category. If \code{FALSE}, p-values are adjusted collectively
#'   over all categories.
#' @param sig_level Numeric: (default = \code{0.05}) Significance threshold for
#'   adjusted p-values. Values equal to or greater than this threshold are
#'   omitted from the results. Must be greater than 0 and at most 1.
#' @param min_hits Numeric: (default = \code{2}) Minimum number of miRNAs from
#'   the test set that a subcategory must contain to be included in the
#'   results. Must be a positive integer.
#' @param ref_set Character vector: (default = \code{NULL}) Only applicable
#'   when \code{test_type = "ORA"}. Used as the reference (background or
#'   universe) set for p-value calculations.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST https://ccb-compute2.cs.uni-saarland.de/mieaa/api/v1/enrichment_analysis/\{species\}/\{type\}/\{test\}/"
#'
#' @return A list that contains your submitted job's ID and a URL to
#'   manually check for your job status.
#'
#' @references \itemize{
#'   \item Ernesto Aparicio-Puerta, Pascal Hirsch, Georges P. Schmartz,
#'   Fabian Kern, Tobias Fehlmann, Andreas Keller, miEAA 2023: updates,
#'   new functional microRNA sets and improved enrichment visualizations,
#'   Nucleic Acids Research, Volume 51, Issue W1, 5 July 2023,
#'   Pages W319–W325, https://doi.org/10.1093/nar/gkad392
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/tutorial/api/}{miEAA
#'   browsable API tutorial}
#'   \item \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/}{Citation note
#'   on miEAA website}
#'   }
#'
#' @examples
#' \donttest{
#' Sys.sleep(1) # to prevent 429 error during R CMD check
#' rba_mieaa_enrich_submit(test_set = c("hsa-miR-20b-5p", "hsa-miR-144-5p"),
#'     mirna_type = "mature",
#'     test_type = "GSEA",
#'     species = 9606,
#'     categories = NULL)
#' }
#'
#' @family "miEAA"
#' @export
rba_mieaa_enrich_submit <- function(test_set,
                                    mirna_type,
                                    test_type,
                                    species,
                                    categories = NULL,
                                    p_adj_method = "fdr",
                                    independent_p_adj = TRUE,
                                    sig_level = 0.05,
                                    min_hits = 2,
                                    ref_set = NULL,
                                    ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "test_type",
        class = "character",
        val = c("GSEA", "ORA"),
        len = 1L
      ),
      list(arg = "test_set", class = "character", min_len = 1L),
      list(
        arg = "mirna_type",
        class = "character",
        val = c("mature", "precursor"),
        len = 1L
      ),
      list(
        arg = "species",
        class = c("character", "numeric", "integer"),
        len = 1L,
        no_null = TRUE
      ),
      list(arg = "categories", class = "character", min_len = 1L),
      list(
        arg = "p_adj_method", class = "character",
        val = c("none", "fdr", "bonferroni", "BY", "hochberg", "holm", "hommel"),
        len = 1L,
        no_null = TRUE
      ),
      list(
        arg = "independent_p_adj",
        class = "logical",
        len = 1L,
        no_null = TRUE
      ),
      list(
        arg = "sig_level",
        class = c("numeric", "integer"),
        len = 1L,
        no_null = TRUE
      ),
      list(
        arg = "min_hits",
        class = c("numeric", "integer"),
        len = 1L,
        integerish = TRUE,
        min_val = 1,
        no_null = TRUE
      ),
      list(arg = "ref_set", class = "character", min_len = 1L)
    ),
    cond = list(
      list(
        quote(sig_level <= 0 || sig_level > 1),
        "`sig_level` must be greater than 0 and at most 1."
      )
    )
  )

  ## handle function-specific inputs
  #species
  species <- .rba_mieaa_species(sp = species, to_name = FALSE)
  #categories
  all_cats <- rba_mieaa_cats(
    mirna_type = mirna_type,
    species = species,
    mode = "all",
    verbose = FALSE,
    save_file = FALSE,
    ...
  )

  if (
    !is.character(all_cats) ||
    length(all_cats) == 0L ||
    is.null(names(all_cats)) ||
    anyNA(all_cats)
  ) {
    categories_error <- paste0(
      "Could not retrieve the supported miEAA categories before submitting ",
      "the enrichment request. The response was: ",
      paste(all_cats, collapse = "\n")
    )

    if (isTRUE(get("skip_error"))) {
      return(categories_error)
    } else {
      stop(categories_error, call. = get("diagnostics"))
    }
  }

  all_cats <- unique(all_cats)

  if (is.null(categories)) {

    categories <- all_cats
    .msg(
      "No categories were supplied, Requesting enrichment using all of the %s available categories for species '%s'.",
      length(categories),
      .rba_mieaa_species(species, to_name = TRUE)
    )

  } else {

    categories <- unique(categories)
    cats_dif <- setdiff(categories, all_cats)
    if (length(cats_dif) != 0) {
      invalid_cats_msg <- sprintf(
        "Invalid categories! The following requested categories do not match your supplied species and miRNA type:\n%s",
        .paste2(cats_dif, last = " and ")
      )
      if (isTRUE(get("skip_error"))) {
        return(invalid_cats_msg)
      } else {
        stop(invalid_cats_msg, call. = FALSE)
      }
    }

  }

  names(categories) <- rep("categories", length(categories))

  .msg(
    "Submitting %s enrichment request for %s miRNA IDs of species %s to miEAA servers.",
    test_type,
    length(test_set),
    .rba_mieaa_species(species, to_name = TRUE)
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      testset = paste(test_set, collapse = "\n"),
      p_value_adjustment = p_adj_method,
      independent_p_adjust = ifelse(independent_p_adj, yes = "True", no = "False"),
      significance_level = sig_level,
      threshold_level = min_hits
    ),
    list("reference_set", test_type == "ORA" && !is.null(ref_set), paste(ref_set, collapse = "\n"))
  )

  call_body <- append(call_body, categories)

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("mieaa", "url"),
    path = sprintf(
      "%senrichment_analysis/%s/%s/%s/",
      .rba_stg("mieaa", "pth"),
      species,
      switch(mirna_type, "mature" = "mirna", "precursor" = "precursor"),
      test_type
    ),
    encode = "multipart",
    body = call_body,
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("rba_mieaa_info.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Check the Status of a Submitted miEAA Enrichment Analysis
#'
#' After you have submitted your enrichment analysis (using
#'    \code{\link{rba_mieaa_enrich_submit}}) and retrieved a job-id,
#'   you can use this function to check the status of the job. The status is
#'   either a numeric completion percentage or \code{"FAILED"}. A status value
#'   equal to 100 means that the requested analysis has finished and you may
#'   retrieve the results using \code{\link{rba_mieaa_enrich_results}}.
#'
#' Note that using \code{\link{rba_mieaa_enrich}} is a more convenient way to
#'   automatically perform this and other required function calls to
#'   perform enrichment analysis on your input miRNA-set using miEAA.
#'
#' @param job_id Character: Job ID of a submitted enrichment analysis.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET https://ccb-compute2.cs.uni-saarland.de/mieaa/api/v1/job_status/\{job_id\}/"
#'
#' @return A list containing a \code{status} element with either the numeric
#'   completion percentage or \code{"FAILED"} for the supplied job ID. A
#'   completed job also includes its results URL.
#'
#' @references \itemize{
#'   \item Ernesto Aparicio-Puerta, Pascal Hirsch, Georges P. Schmartz,
#'   Fabian Kern, Tobias Fehlmann, Andreas Keller, miEAA 2023: updates,
#'   new functional microRNA sets and improved enrichment visualizations,
#'   Nucleic Acids Research, Volume 51, Issue W1, 5 July 2023,
#'   Pages W319–W325, https://doi.org/10.1093/nar/gkad392
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/tutorial/api/}{miEAA
#'   browsable API tutorial}
#'   \item \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/}{Citation note
#'   on miEAA website}
#'   }
#'
#' @examples
#' \dontrun{
#' Sys.sleep(1) # to prevent 429 error during R CMD check
#' rba_mieaa_enrich_status("f52d1aef-6d3d-4d51-9020-82e68fe99012")
#' }
#'
#' @family "miEAA"
#' @export
rba_mieaa_enrich_status <- function(job_id, ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "job_id", class = "character", len = 1L)
    )
  )

  .msg(
    "Retrieving status of submitted enrichment request with ID: %s",
    job_id
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("mieaa", "url"),
    path = sprintf("%sjob_status/%s/", .rba_stg("mieaa", "pth"), job_id),
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("rba_mieaa_info.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Retrieve the Results of a Finished miEAA Enrichment Analysis
#'
#' After your submitted enrichment analysis request has finished (check
#'   using \code{\link{rba_mieaa_enrich_status}}), you can retrieve the results
#'   using this function.
#'
#' Note that using \code{\link{rba_mieaa_enrich}} is a more convenient way to
#'   automatically perform this and other required function calls to
#'   perform enrichment analysis on your input miRNA-set using miEAA.
#'
#' @param job_id Character: Job ID of a submitted enrichment analysis.
#' @param sort_by Character: (default = \code{"p_adjusted"}) Result column to
#'   sort by. One of: "category", "subcategory", "enrichment", "p_value",
#'   "p_adjusted", "q_value", or "observed".
#' @param sort_asc Logical: (default = \code{TRUE}) If \code{TRUE}, sort the
#'   results in ascending order. If \code{FALSE}, sort them in descending order.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET https://ccb-compute2.cs.uni-saarland.de/mieaa/api/v1/enrichment_analysis/results/\{job_id\}/"
#'
#' @return A data frame with your enrichment analysis results.
#'
#' @references \itemize{
#'   \item Ernesto Aparicio-Puerta, Pascal Hirsch, Georges P. Schmartz,
#'   Fabian Kern, Tobias Fehlmann, Andreas Keller, miEAA 2023: updates,
#'   new functional microRNA sets and improved enrichment visualizations,
#'   Nucleic Acids Research, Volume 51, Issue W1, 5 July 2023,
#'   Pages W319–W325, https://doi.org/10.1093/nar/gkad392
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/tutorial/api/}{miEAA
#'   browsable API tutorial}
#'   \item \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/}{Citation note
#'   on miEAA website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_mieaa_enrich_results("f52d1aef-6d3d-4d51-9020-82e68fe99012")
#' }
#'
#' @family "miEAA"
#' @export
rba_mieaa_enrich_results <- function(job_id,
                                     sort_by = "p_adjusted",
                                     sort_asc = TRUE,
                                     ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "job_id", class = "character", len = 1L),
      list(
        arg = "sort_by", class = "character", no_null = TRUE,
        val = c("category",
                "subcategory",
                "enrichment",
                "p_value",
                "p_adjusted",
                "q_value",
                "observed"),
        len = 1L
      ),
      list(
        arg = "sort_asc",
        class = "logical",
        len = 1L,
        no_null = TRUE
      )
    )
  )

  .msg(
    "Retrieving results of submitted enrichment request with ID: %s",
    job_id
  )

  ## Build Function-Specific Call
  sort_column <- switch(
    sort_by,
    category = "Category",
    subcategory = "Subcategory",
    enrichment = "Enrichment",
    p_value = "P-value",
    p_adjusted = "P-adjusted",
    q_value = "Q-value",
    observed = "Observed"
  )

  parser_input <- list(
    "json->df",
    function(x) {
      if (ncol(x) == 0L) {
        return(x)
      } else if (ncol(x) == 9L) {
        colnames(x) <- c("Category", "Subcategory",
                         "Enrichment", "P-value",
                         "P-adjusted", "Q-value",
                         "Expected", "Observed",
                         "miRNAs/precursors")
      } else if (ncol(x) == 8L) {
        colnames(x) <- c("Category", "Subcategory",
                         "Enrichment", "P-value",
                         "P-adjusted", "Q-value",
                         "Observed", "miRNAs/precursors")
      } else {
        stop(
          "Unexpected miEAA results format: expected 8 or 9 columns.",
          call. = FALSE
        )
      }

      sort_value <- x[[sort_column]]

      if (sort_by %in% c("p_value", "p_adjusted", "q_value", "observed")) {
        sort_value <- suppressWarnings(as.numeric(sort_value))
      }

      x <- x[
        order(
          sort_value,
          decreasing = !isTRUE(sort_asc),
          na.last = TRUE
        ),
        ,
        drop = FALSE
      ]
      rownames(x) <- NULL

      return(x)
    }
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("mieaa", "url"),
    path = sprintf(
      "%senrichment_analysis/results/%s/",
      .rba_stg("mieaa", "pth"),
      job_id
    ),
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("rba_mieaa_info.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' A One-step Wrapper for miRNA Enrichment Using miEAA
#'
#' This function is a wrapper for the multiple function calls necessary to
#'   perform enrichment analysis on a given miRNA list using miEAA. See Details
#'   section for more information.
#'
#' This function will call other rba_mieaa_*** functions with the following
#'   order:
#'   \enumerate{
#'   \item Call \code{\link{rba_mieaa_enrich_submit}} to submit an enrichment
#'     analysis request to the miEAA server using the supplied miRNA list and
#'     other arguments.
#'   \item Once the job is successfully submitted, call
#'     \code{\link{rba_mieaa_enrich_status}} at the selected polling interval
#'     to check whether the server-side analysis has finished.
#'   \item Call \code{\link{rba_mieaa_enrich_results}} to retrieve the results
#'     of your enrichment analysis.
#'   }
#'   See each function's manual for more details.
#'   The \code{save_file} rbioapi option applies only to the final enrichment
#'   results and not to the intermediate submission or status responses.
#'
#' @inheritParams rba_mieaa_enrich_submit
#' @inheritParams rba_mieaa_enrich_results
#' @param poll_interval Numeric: (default = \code{5}) Number of seconds to wait
#'   between job-status requests; must be between 5 and 300 seconds.
#' @param poll_timeout Numeric: (default = \code{300}) Maximum number of seconds
#'   to wait for the enrichment analysis to finish. Use \code{Inf} to wait
#'   without a time limit.
#'
#' @section Corresponding API Resources:
#'  "https://ccb-compute2.cs.uni-saarland.de/mieaa/api/"
#'
#' @return A data frame with your enrichment analysis results.
#'
#' @references \itemize{
#'   \item Ernesto Aparicio-Puerta, Pascal Hirsch, Georges P. Schmartz,
#'   Fabian Kern, Tobias Fehlmann, Andreas Keller, miEAA 2023: updates,
#'   new functional microRNA sets and improved enrichment visualizations,
#'   Nucleic Acids Research, Volume 51, Issue W1, 5 July 2023,
#'   Pages W319–W325, https://doi.org/10.1093/nar/gkad392
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/tutorial/api/}{miEAA
#'   browsable API tutorial}
#'   \item \href{https://ccb-compute2.cs.uni-saarland.de/mieaa/}{Citation note
#'   on miEAA website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_mieaa_enrich(test_set = c("hsa-miR-20b-5p", "hsa-miR-144-5p",
#'  "hsa-miR-17-5p", "hsa-miR-20a-5p"),
#'      mirna_type = "mature",
#'      test_type = "ORA",
#'      species = 9606,
#'      categories = "miRPathDB_GO_Biological_process_mature")
#' }
#'
#' @family "miEAA"
#' @family "Enrichment/Over-representation"
#' @export
rba_mieaa_enrich <- function(test_set,
                             mirna_type,
                             test_type,
                             species,
                             categories = NULL,
                             p_adj_method = "fdr",
                             independent_p_adj = TRUE,
                             sig_level = 0.05,
                             min_hits = 2,
                             ref_set = NULL,
                             sort_by = "p_adjusted",
                             sort_asc = TRUE,
                             poll_interval = 5,
                             poll_timeout = 300,
                             ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "poll_interval", class = c("numeric", "integer"),
        len = 1L, min_val = 5, max_val = 300
      ),
      list(
        arg = "poll_timeout", class = c("numeric", "integer"),
        len = 1L, min_val = 1
      )
    ),
    cond = list(
      list(
        quote(!is.finite(poll_interval)), "`poll_interval` must be finite."
      ),
      list(
        quote(poll_timeout < poll_interval),
        "`poll_timeout` must be equal to or greater than `poll_interval`."
      )
    )
  )

  ## 1 Submit Enrichment Request
  .msg(
    " -- Step 1/3: Submitting Enrichment analysis request:"
  )

  step1 <- rba_mieaa_enrich_submit(
    test_set = test_set,
    mirna_type = mirna_type,
    species = species,
    test_type = test_type,
    categories = categories,
    p_adj_method = p_adj_method,
    independent_p_adj = independent_p_adj,
    sig_level = sig_level,
    min_hits = min_hits,
    ref_set = ref_set,
    save_file = FALSE,
    ...
  )

  ### 1.1 Check Submission Response
  if (!utils::hasName(step1, "job_id")) {
    no_job_id_msg <- paste0(
      "Error: Couldn't submit analysis request to miEAA. ",
      "Please retry or manually run the required steps as demonstrated in the `miEAA & rbioapi` vignette article, section `Approach 2: Going step-by-step`. ",
      "If the problem persists, kindly report this issue to us. The error message was: ",
      try(step1),
      collapse = "\n"
    )

    if (isTRUE(get("skip_error"))) {
      return(no_job_id_msg)
    } else {
      stop(no_job_id_msg, call. = get("diagnostics"))
    }
  }

  .msg(
    paste0(
      "\n -- Step 2/3: Checking for Submitted enrichment analysis's status ",
      "every %s seconds.\n",
      "    Your submitted job ID is: %s"
    ),
    poll_interval,
    step1$job_id
  )

  ## 2 Poll Job Status
  poll_state <- "pending"
  step2 <- list(status = 0L, `results-URL` = NULL)
  poll_started <- Sys.time()

  repeat {
    Sys.sleep(poll_interval)

    if (isTRUE(get("verbose"))) {
      cat(".")
    }
    step2 <- rba_mieaa_enrich_status(
      job_id = step1$job_id,
      verbose = FALSE,
      save_file = FALSE,
      ...
    )

    ### 2.1 Classify Polling Response
    if (
      !is.list(step2) ||
      !utils::hasName(step2, "status") ||
      !is.atomic(step2$status) ||
      length(step2$status) != 1L
    ) {
      poll_state <- "invalid"
      break
    }

    if (identical(step2$status, "FAILED")) {
      poll_state <- "failed"
      break
    }

    if (
      !is.numeric(step2$status) ||
      !is.finite(step2$status) ||
      step2$status < 0
    ) {
      poll_state <- "invalid"
      break
    }

    if (
      isTRUE(step2$status >= 100L)
    ) {
      poll_state <- "completed"
      break
    }

    if (
      as.numeric(difftime(Sys.time(), poll_started, units = "secs")) >=
      poll_timeout
    ) {
      poll_state <- "timeout"
      break
    }
  }

  ### 2.2 Handle Polling Failure
  if (!identical(poll_state, "completed")) {
    step2_error <- if (is.list(step2)) {
      if (utils::hasName(step2, "status")) {
        step2$status
      } else {
        "No valid job status was returned."
      }
    } else {
      step2
    }
    if (length(step2_error) == 0L || anyNA(step2_error)) {
      step2_error <- "No valid job status was returned."
    } else {
      step2_error <- paste(step2_error, collapse = "\n")
    }

    poll_error_msg <- switch(
      poll_state,
      failed = "The miEAA server reported that the analysis failed",
      timeout = sprintf(
        "The miEAA server did not complete the analysis within %s seconds",
        poll_timeout
      ),
      invalid = "The miEAA server returned an invalid job-status response",
      "The miEAA server did not complete the analysis"
    )

    job_stuck_msg <- paste0(
      "Error: ",
      poll_error_msg,
      " for job ID `",
      step1$job_id,
      "`. ",
      "Please retry or manually run the required steps as demonstrated in the `miEAA & rbioapi` vignette article, section `Approach 2: Going step-by-step`. ",
      "If the problem persists, kindly report this issue to us. The last status response was: ",
      step2_error
    )

    if (isTRUE(get("skip_error"))) {
      return(job_stuck_msg)
    } else {
      stop(job_stuck_msg, call. = get("diagnostics"))
    }
  }

  ## 3 Retrieve Enrichment Results
  .msg(
    "\n -- Step 3/3: Retrieving the results."
  )

  Sys.sleep(1)

  step3 <- rba_mieaa_enrich_results(
    job_id = step1$job_id,
    sort_by = sort_by,
    sort_asc = sort_asc,
    ...
  )
  return(step3)

}
