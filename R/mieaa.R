#' Handle Species argument input for miEAA endpoints
#'
#' This internal function will make it possible for the users to supply
#'   variety of specie's name type or if they have entered a wrongly-formatted
#'   or not supported species, to produce an informative error message
#'
#' @param sp specie input.
#' @param to_name (logical) (default = FALSE) to convert a supplied species
#'   abbreviation to specie's scientific name.
#'
#' @return If to_name = FALSE, a three-lettered character string of
#'   a supported miEAA species. otherwise, if to_name = TRUE, a character
#'   string with the scientific name of that specie.
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
    specie_name = c("Homo sapiens", "Mus musculus",
                    "Rattus norvegicus", "Arabidopsis thaliana",
                    "Bos taurus", "Caenorhabditis elegans",
                    "Drosophila melanogaster", "Danio rerio",
                    "Gallus gallus", "Sus scrofa"),
    stringsAsFactors = FALSE)
  if (isTRUE(to_name)) {
    return(sp_df$specie_name[[which(sp_df$abbreviation == sp)]])
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
      "ssc" = "ssc", "ssc" = 9823L,  "ssc" = "Sus scrofa")

    sp_match <- pmatch(x = tolower(sp), table = tolower(sp_table),
                       nomatch = 0, duplicates.ok = FALSE)
    if (sp_match != 0) {
      return(names(sp_table)[[sp_match]])
    } else {
      stop("Species should be or partially match one the following values:\n",
           paste(utils::capture.output(print(sp_df)), collapse = "\n"),
           call. = diagnostics)
    }
  }
}

#' Get Supported Enrichment Categories for a Species and miRNA Type
#'
#' For each Combination of species and miRNA type, Only a pre-defined
#'   categories groups are supported. Use this function to retrieve a list
#'   of supported categories for a given combination of Species and miRNA type.
#'
#' @param mirna_type Type of your miRNA accession. either "mature" or
#'   "precursor".
#' @param species Fully or partially matching Scientific name, abbreviation
#'   or NCBI taxon ID of one of the following species: \enumerate{
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
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET  "https://ccb-compute2.cs.uni-saarland.de/mieaa2/api/v1/enrichment_categories/{species}/{mirna_type}/"
#'
#' @return a named character vector with the supported categories for
#'   your supplied input combination.
#'
#' @references \itemize{
#'   \item Fabian Kern, Tobias Fehlmann, Jeffrey Solomon, Louisa Schwed,
#'   Nadja Grammes, Christina Backes, Kendall Van Keuren-Jensen,
#'   David Wesley Craig,Eckart Meese, Andreas Keller, miEAA 2.0:
#'   integrating multi-species microRNA enrichment analysis and workflow
#'   management systems, Nucleic Acids Research, Volume 48, Issue W1,
#'   02 July 2020, Pages W521–W528, https://doi.org/10.1093/nar/gkaa309
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa2/tutorial/api/}{miEAA
#'   browsable API tutorial}
#'   }
#'
#' @examples
#' \donttest{
#' rba_mieaa_cats("mature", "Homo sapiens")
#' }
#'
#' @family "miEAA"
#' @export
rba_mieaa_cats <- function(mirna_type, species, ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(cons = list(list(arg = "mirna_type",
                             class = "character",
                             val = c("mature",
                                     "precursor")),
                        list(arg = "species",
                             class = c("character",
                                       "numeric"),
                             len = 1)))
  # convert species input to abbreviation
  species <- .rba_mieaa_species(species, to_name = FALSE)

  .msg("Retrieving available enrichment categories of %s for %s.",
       switch(mirna_type,
              "mature" = "miRNA",
              "precursor" = "miRNA precursor"),
       .rba_mieaa_species(species, to_name = TRUE))

  ## Build Function-Specific Call
  parser_input <- list("json->df",
                       function(x) {
                         y <- x[[1]]
                         names(y) <- x[[2]]
                         return(y)})

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("mieaa", "url"),
                          path = sprintf("%senrichment_categories/%s/%s",
                                         .rba_stg("mieaa", "pth"),
                                         species,
                                         switch(mirna_type,
                                                "mature" = "mirna",
                                                "precursor" = "precursor")),
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("rba_mieaa_cats.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Convert miRNA accession Between Different miRBase Versions
#'
#' miEAA works with miRBASE v22 accession. Using This function you can convert
#'   a set of mature or precursor miRNA accession between two given miRBase
#'   versions.
#'
#' @param mirna A vector of miRNA accessions to be converted.
#' @param mirna_type Type of your supplied miRNA accession. either "mature"
#'   or "precursor".
#' @param input_version (numeric) miRBase version of your supplied miRNA
#'   accessions.
#' @param output_version (numeric) To what version should your miRNA accessions
#'   be converted?
#' @param simple_output (logical) If FALSE (default), the result will be a
#'   two-columned data frame with your input and output accessions. Otherwise,
#'   if TRUE, only the output miRNA accessions will be returned.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST https://ccb-compute2.cs.uni-saarland.de/mieaa2/api/v1/mirbase_converter/"
#'
#' @return Depending on the arguments, a data frame or a character vectors
#'   containing the miRNA accessions in your output version.
#'
#' @references \itemize{
#'   \item Fabian Kern, Tobias Fehlmann, Jeffrey Solomon, Louisa Schwed,
#'   Nadja Grammes, Christina Backes, Kendall Van Keuren-Jensen,
#'   David Wesley Craig,Eckart Meese, Andreas Keller, miEAA 2.0:
#'   integrating multi-species microRNA enrichment analysis and workflow
#'   management systems, Nucleic Acids Research, Volume 48, Issue W1,
#'   02 July 2020, Pages W521–W528, https://doi.org/10.1093/nar/gkaa309
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa2/tutorial/api/}{miEAA
#'   browsable API tutorial}
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
  .rba_args(cons = list(list(arg = "mirna",
                             class = "character"),
                        list(arg = "mirna_type",
                             class = "character",
                             val = c("mature",
                                     "precursor")),
                        list(arg = "input_version",
                             class = "numeric",
                             val = c(9.1, 10, 12:22)),
                        list(arg = "output_version",
                             class = "numeric",
                             val = c(9.1, 10, 12:22)),
                        list(arg = "simple_output",
                             class = "logical")))

  .msg("Converting %s %s miRNA IDs from mirbase v%s to v%s.",
       length(mirna),
       mirna_type,
       input_version, output_version)
  ## Build POST API Request's body
  call_body <- list(mirnas = paste(mirna, collapse = "\n"),
                    mirbase_input_version = paste0("v", input_version),
                    mirbase_output_version = paste0("v", output_version),
                    input_type = ifelse(mirna_type == "mature",
                                        yes = "mirna", no = "precursor"),
                    output_format = ifelse(isTRUE(simple_output),
                                           yes = "oneline",
                                           no = "tabsep"))

  ## Build Function-Specific Call
  if (isTRUE(simple_output)) {
    parser_input <- list("text->df", function(x) {x[, 1]})
  } else {
    parser_input <- list("text->df", function(x) {
      colnames(x) <- x[1, ]; x <- x[-1, ]  })
  }

  input_call <- .rba_httr(httr = "post",
                          url = .rba_stg("mieaa", "url"),
                          path = sprintf("%smirbase_converter/",
                                         .rba_stg("mieaa", "pth")),
                          encode = "multipart",
                          body = call_body,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("rba_mieaa_convert_version.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Convert Between Mature and precursor miRNA Accession
#'
#' miRBase miRNA accession could refer to either mature or precursor miRNAs.
#'   (see: \href{http://www.mirbase.org/help/nomenclature.shtml}{miRNA naming
#'   conventions}). Use this function to mature miRNA accession to
#'   corresponding miRNA accessions or vice versa.
#'
#' @param mirna A vector of miRNA accessions to be converted.
#' @param input_type Type of your supplied miRNA accession. either "mature"
#'   or "precursor".
#' @param only_unique (logical) miRBase precursor and mature miRNA accessions
#'   are not uniquely mapped. (i.e. you may get more than one results for
#'   a given accession). set this to TRUE to only retrieve the unique mappings.
#'   (default = FALSE)
#' @param simple_output (logical) If FALSE (default), the result will be a
#'   two-columned data frame with your input and output accessions. Otherwise,
#'   if TRUE, only the output miRNA accessions will be returned.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST https://ccb-compute2.cs.uni-saarland.de/mieaa2/api/v1/mirna_precursor_converter/"
#'
#' @return Depending on the arguments, a data frame or a character vectors
#'   containing the miRNA accessions in your output version.
#'
#' @references \itemize{
#'   \item Fabian Kern, Tobias Fehlmann, Jeffrey Solomon, Louisa Schwed,
#'   Nadja Grammes, Christina Backes, Kendall Van Keuren-Jensen,
#'   David Wesley Craig,Eckart Meese, Andreas Keller, miEAA 2.0:
#'   integrating multi-species microRNA enrichment analysis and workflow
#'   management systems, Nucleic Acids Research, Volume 48, Issue W1,
#'   02 July 2020, Pages W521–W528, https://doi.org/10.1093/nar/gkaa309
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa2/tutorial/api/}{miEAA
#'   browsable API tutorial}
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
  .rba_args(cons = list(list(arg = "mirna",
                             class = "character"),
                        list(arg = "input_type",
                             class = "character",
                             val = c("mature",
                                     "precursor")),
                        list(arg = "only_unique",
                             class = "logical"),
                        list(arg = "simple_output",
                             class = "logical")))

  .msg("Converting %s %s miRNA IDs to %s IDs.",
       length(mirna),
       input_type,
       ifelse(input_type == "mature",
              yes = "precursor", no = "mature"))
  ## Build POST API Request's body
  call_body <- list(mirnas = paste(mirna, collapse = "\n"),
                    input_type = ifelse(input_type == "mature",
                                        yes = "to_precursor", no = "to_mirna"),
                    output_format = ifelse(isTRUE(simple_output),
                                           yes = "newline",
                                           no = "tabsep"),
                    conversion_type = ifelse(isTRUE(only_unique),
                                             yes = "unique",
                                             no = "all"))

  ## Build Function-Specific Call
  if (isTRUE(simple_output)) {
    parser_input <- list("text->df", function(x) {x[, 1]})
  } else {
    parser_input <- list("text->df",
                         function(x) {
                           names(x) <- c(input_type,
                                         setdiff(c("mature", "precursor"),
                                                 input_type))
                           return(x)
                         })
  }

  input_call <- .rba_httr(httr = "post",
                          url = .rba_stg("mieaa", "url"),
                          path = sprintf("%smirna_precursor_converter/",
                                         .rba_stg("mieaa", "pth")),
                          encode = "multipart",
                          body = call_body,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("rba_mieaa_convert_type.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Submit miEAA miRNA Enrichment Analysis Request
#'
#' Using This function you can submit a request in miEAA servers to perform
#'   Over-representation or GSEA Analysis for a given set of miRNA identifiers.
#'   see "arguments" section for more information.
#'
#' Note that using \code{\link{rba_mieaa_enrich}} is a more convenient way to
#'   automatically perform this and other required function calls to enrich
#'   your input miRNA-set using miEAA.
#'
#' @param test_set a character vector with your mature or precursor miRBase
#'   miRNA accessions. Note that \enumerate{
#'   \item Only miRBase v22 miRNA accession are accepted. You can use
#'   \code{\link{rba_mieaa_convert_version}} to convert your accessions to
#'   miRBase v22.
#'   \item Your list should be entirely consisted of either mature or
#'   precursor miRNA accession. A mixture of both is not accepted.
#'   }
#' @param mirna_type Type of your supplied miRNA accession. either "mature"
#'   or "precursor".
#' @param test_type The analysis to perform. can be either "ORA" for 'Over
#'   Representation Analysis' or "GSEA" for miRNA (Gene)
#'   'Set Enrichment Analysis'. Note that in GSEA, your list should be sorted
#'   beforehand based on some criterion.
#' @param species Fully or partially matching Scientific name, abbreviation
#' or NCBI taxon ID of one of the following species: \enumerate{
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
#' @param categories one or multiple Category names to be used for miRNA set
#'   enrichment analysis. Note that \itemize{
#'   \item Available categories varies based on your chosen specie and if
#'    your supplied miRNA type is mature or precursor. Use
#'    \code{\link{rba_mieaa_cats}} to retrieve a list of available category
#'    names for a given specie and miRNA type.
#'   \item If you supply NULL, the analysis will be performed on all of the
#'    available categories.}
#' @param p_adj_method P-value adjustment method to be used. Should be one of:
#'   "none", "fdr" (default), "bonferroni", "BY", "hochberg", "holm" or "hommel"
#' @param independent_p_adj (logical) The scope and level of p-value adjustment;
#'   if TRUE (default), the categories will be considered independent from
#'   each other and the p-value will be adjusted separately for each category.
#'   if FALSE, the p-value will be adjusted collectively over all categories.
#' @param sig_level (numeric) The significance threshold of adjusted P-value.
#'   values equal to or greater than this threshold will be dropped from the
#'   results.
#' @param min_hits (numeric) How many miRNA should a sub-category have from
#'   your supplied test-list to be included in the results? (default is 2)
#' @param ref_set (Optional) Only applicable when test_type is "ORA".
#'   This character vector will be used as your reference (background or
#'   universe) set for p-value calculations.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "POST https://ccb-compute2.cs.uni-saarland.de/mieaa2/api/v1/enrichment_analysis/{species}/{type}/{test}/"
#'
#' @return A list that contains your submitted job's ID and a URL to
#'   manually check for your job status.
#'
#' @references \itemize{
#'   \item Fabian Kern, Tobias Fehlmann, Jeffrey Solomon, Louisa Schwed,
#'   Nadja Grammes, Christina Backes, Kendall Van Keuren-Jensen,
#'   David Wesley Craig,Eckart Meese, Andreas Keller, miEAA 2.0:
#'   integrating multi-species microRNA enrichment analysis and workflow
#'   management systems, Nucleic Acids Research, Volume 48, Issue W1,
#'   02 July 2020, Pages W521–W528, https://doi.org/10.1093/nar/gkaa309
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa2/tutorial/api/}{miEAA
#'   browsable API tutorial}
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
                                    species = "hsa",
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
  .rba_args(cons = list(list(arg = "test_type",
                             class = "character",
                             val = c("GSEA",
                                     "ORA")),
                        list(arg = "test_set",
                             class = "character",
                             no_null = TRUE),
                        list(arg = "mirna_type",
                             class = "character",
                             no_null = TRUE,
                             val = c("mature",
                                     "precursor")),
                        list(arg = "species",
                             class = c("character",
                                       "numeric"),
                             no_null = TRUE,
                             len = 1),
                        list(arg = "categories",
                             class = "character"),
                        list(arg = "p_adj_method",
                             class = "character",
                             val = c("none",
                                     "fdr",
                                     "bonferroni",
                                     "BY",
                                     "hochberg",
                                     "holm",
                                     "hommel")),
                        list(arg = "independent_p_adj",
                             class = "logical"),
                        list(arg = "sig_level",
                             class = "numeric",
                             ran = c(0, 1)),
                        list(arg = "min_hits",
                             class = "numeric")
  ))
  ## handle function-specific inputs
  #species
  species <- .rba_mieaa_species(sp = species, to_name = FALSE)
  #categories
  all_cats <- rba_mieaa_cats(mirna_type = mirna_type,
                             species = species,
                             verbose = FALSE)
  if (is.null(categories)) {
    categories <- all_cats
    .msg("No categories were supplied, Requesting enrichment using all of the %s available categories for species '%s'.",
         length(categories),
         .rba_mieaa_species(species, to_name = TRUE))
  } else {
    cats_dif <- setdiff(categories, all_cats)
    if (length(cats_dif) != 0) {
      invalid_cats_msg <- sprintf("Invalid categories! The following requested categories do not match your supplied specie and miRNA type:\n%s",
                                  .paste2(cats_dif, last = " and "))
      if (isTRUE(get("skip_error"))) {
        return(invalid_cats_msg)
      } else {
        stop(invalid_cats_msg,
             call. = FALSE)
      }

    }
  }
  names(categories) <- rep("categories", length(categories))

  .msg("Submitting %s enrichment request for %s miRNA IDs of species %s to miEAA servers.",
       test_type, length(test_set), .rba_mieaa_species(species, to_name = TRUE))

  ## Build POST API Request's body
  call_body <- .rba_query(init = list(testset = paste(test_set, collapse = "\n"),
                                      p_value_adjustment = p_adj_method,
                                      independent_p_adjust = ifelse(independent_p_adj,
                                                                    yes = "True",
                                                                    no = "False"),
                                      significance_level = sig_level,
                                      threshold_level = min_hits),
                          list("reference_set",
                               test_type == "ORA" && !is.null(ref_set),
                               paste(ref_set, collapse = "\n")))
  call_body <- append(call_body, categories)

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "post",
                          url = .rba_stg("mieaa", "url"),
                          path = sprintf("%senrichment_analysis/%s/%s/%s/",
                                         .rba_stg("mieaa", "pth"),
                                         species,
                                         switch(mirna_type,
                                                "mature" = "mirna",
                                                "precursor" = "precursor"),
                                         test_type),
                          encode = "multipart",
                          body = call_body,
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("rba_mieaa_info.json"))
  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Check Status of a Submitted Enrichment Analysis in miEAA
#'
#' After you have submitted your enrichment analysis (using
#'    \code{\link{rba_mieaa_enrich_submit}}) and retrieved a job-id,
#'   you can use this function to check the status of your job. Status value
#'   equal to 100 means that your requested analysis has finished and you may
#'   retrieve the results using \code{\link{rba_mieaa_enrich_results}}.
#'
#' Note that using \code{\link{rba_mieaa_enrich}} is a more convenient way to
#'   automatically perform this and other required function calls to enrich
#'   your input miRNA-set using miEAA.
#'
#' @param job_id The job-id (a character string) of a submitted enrichment
#'   analysis.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET https://ccb-compute2.cs.uni-saarland.de/mieaa2/api/v1/job_status/{job_id}"
#'
#' @return A list containing the status value for a analysis that corresponds
#'   to your supplied job-id.
#'
#' @references \itemize{
#'   \item Fabian Kern, Tobias Fehlmann, Jeffrey Solomon, Louisa Schwed,
#'   Nadja Grammes, Christina Backes, Kendall Van Keuren-Jensen,
#'   David Wesley Craig,Eckart Meese, Andreas Keller, miEAA 2.0:
#'   integrating multi-species microRNA enrichment analysis and workflow
#'   management systems, Nucleic Acids Research, Volume 48, Issue W1,
#'   02 July 2020, Pages W521–W528, https://doi.org/10.1093/nar/gkaa309
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa2/tutorial/api/}{miEAA
#'   browsable API tutorial}
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
  .rba_args(cons = list(list(arg = "job_id",
                             class = "character",
                             len = 1)))

  .msg("Retrieving status of submitted enrichment request with ID: %s",
       job_id)

  ## Build Function-Specific Call
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("mieaa", "url"),
                          path = sprintf("%sjob_status/%s/",
                                         .rba_stg("mieaa", "pth"),
                                         job_id),
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("rba_mieaa_info.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Retrieve Results of a finished Enrichment Analysis from miEAA
#'
#' After your submitted enrichment analysis request has finished (check
#'   using \code{\link{rba_mieaa_enrich_status}}), you can retrieve the results
#'   using this function.
#'
#' Note that using \code{\link{rba_mieaa_enrich}} is a more convenient way to
#'   automatically perform this and other required function calls to enrich
#'   your input miRNA-set using miEAA.
#'
#' @param job_id The job-id (a character string) of a submitted enrichment
#'   analysis.
#' @param sort_by A column name to the result's table based on that. one of:
#'   "category", "subcategory", "enrichment", "p_value", "p_adjusted" (default),
#'   "q_value" or "observed" .
#' @param sort_asc (logical) If TRUE, the results will be sorted in ascending
#'   order. If FALSE, the results will be sorted in descending order.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @section Corresponding API Resources:
#'  "GET https://ccb-compute2.cs.uni-saarland.de/mieaa2/api/v1/results/{job_id}"
#'
#' @return A data frame with your enrichment analysis results.
#'
#' @references \itemize{
#'   \item Fabian Kern, Tobias Fehlmann, Jeffrey Solomon, Louisa Schwed,
#'   Nadja Grammes, Christina Backes, Kendall Van Keuren-Jensen,
#'   David Wesley Craig,Eckart Meese, Andreas Keller, miEAA 2.0:
#'   integrating multi-species microRNA enrichment analysis and workflow
#'   management systems, Nucleic Acids Research, Volume 48, Issue W1,
#'   02 July 2020, Pages W521–W528, https://doi.org/10.1093/nar/gkaa309
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa2/tutorial/api/}{miEAA
#'   browsable API tutorial}
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
  .rba_args(cons = list(list(arg = "job_id",
                             class = "character",
                             len = 1),
                        list(arg = "sort_by",
                             class = "character",
                             no_null = TRUE,
                             val = c("category",
                                     "subcategory",
                                     "enrichment",
                                     "p_value",
                                     "p_adjusted",
                                     "q_value",
                                     "observed"))))

  .msg("Retrieving results of submitted enrichment request with ID: %s",
       job_id)

  ## Build Function-Specific Call
  parser_input <- list("json->df",
                       function(x) {
                         if (ncol(x) == 9) {
                           colnames(x) <- c("Category", "Subcategory",
                                            "Enrichment", "P-value",
                                            "P-adjusted", "Q-value",
                                            "Expected", "Observed",
                                            "miRNAs/precursors")
                         }
                         if (ncol(x) == 8) {
                           colnames(x) <- c("Category", "Subcategory",
                                            "Enrichment", "P-value",
                                            "P-adjusted", "Q-value",
                                            "Observed", "miRNAs/precursors")

                         }
                         return(x)
                       }
  )

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("mieaa", "url"),
                          path = sprintf("%s/enrichment_analysis/results/%s/",
                                         .rba_stg("mieaa", "pth"),
                                         job_id),
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("rba_mieaa_info.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' A One-step Wrapper for miRNA Enrichment Using miEAA
#'
#' This function is a wrapper for the multiple function calls necessary to
#'   enrich a given miRNA list using miEAA. see details section
#'   for more information.
#'
#' This function will call other rba_mieaa_*** functions with the following
#'   order:
#'   \enumerate{
#'   \item Call \code{\link{rba_mieaa_enrich_submit}} to Submit an enrichment
#'     analysis request to miEAA servers, using your supplied miRNA lists and
#'     other arguments.
#'   \item Once your job was successfully submitted, it will call
#'     \code{\link{rba_mieaa_enrich_status}} every 5 seconds, to check the
#'     status of your running server-side job and whether your analysis job is
#'     finished and the results are available.
#'   \item Call \code{\link{rba_mieaa_enrich_results}} to retrieve the results
#'     of your enrichment analysis.
#'   }
#'   See each function's manual for more details.
#'
#' @inheritParams rba_mieaa_enrich_submit
#' @inheritParams rba_mieaa_enrich_results
#'
#' @section Corresponding API Resources:
#'  "GET https://ccb-compute2.cs.uni-saarland.de/mieaa2/api/"
#'
#' @return A data frame with your enrichment analysis results.
#'
#' @references \itemize{
#'   \item Fabian Kern, Tobias Fehlmann, Jeffrey Solomon, Louisa Schwed,
#'   Nadja Grammes, Christina Backes, Kendall Van Keuren-Jensen,
#'   David Wesley Craig,Eckart Meese, Andreas Keller, miEAA 2.0:
#'   integrating multi-species microRNA enrichment analysis and workflow
#'   management systems, Nucleic Acids Research, Volume 48, Issue W1,
#'   02 July 2020, Pages W521–W528, https://doi.org/10.1093/nar/gkaa309
#'   \item
#'   \href{https://ccb-compute2.cs.uni-saarland.de/mieaa2/tutorial/api/}{miEAA
#'   browsable API tutorial}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_mieaa_enrich(test_set = c("hsa-miR-20b-5p", "hsa-miR-144-5p"),
#'      mirna_type = "mature",
#'      test_type = "GSEA",
#'      species = 9606,
#'      categories = NULL)
#' }
#'
#' @family "miEAA"
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
                             ...) {
  ## Load Global Options
  .rba_ext_args(...)
  .msg(" -- Step 1/3: Submitting Enrichment request:")
  step1 <- rba_mieaa_enrich_submit(test_set = test_set,
                                   mirna_type = mirna_type,
                                   species = species,
                                   test_type = test_type,
                                   categories = categories,
                                   p_adj_method = p_adj_method,
                                   independent_p_adj = independent_p_adj,
                                   sig_level = sig_level,
                                   min_hits = min_hits,
                                   ref_set = ref_set,
                                   ...)
  if (utils::hasName(step1, "job_id")) {
    .msg("\n -- Step 2/3: Checking for Submitted enrichment job's status every 5 seconds.\n",
         "    Your submitted job ID is: ", step1$job_id)
    step2 <- 0L
    while (step2 != 100L) {
      if (isTRUE(get("verbose"))) { cat(".") }
      Sys.sleep(5)
      step2 <- rba_mieaa_enrich_status(job_id = step1$job_id,
                                       verbose = FALSE, ...)[["status"]]
    }
    .msg("\n -- Step 3/3: Retrieving the results of the finished enrichment job.")
    step3 <- rba_mieaa_enrich_results(job_id = step1$job_id,
                                      sort_by = sort_by,
                                      sort_asc = sort_asc,
                                      ...)
    return(step3)
  } else {
    if (isTRUE(get("skip_error"))) {
      return(step1)
    } else {
      stop("Step 1 returned invalid results, ",
           "maybe skip_error is TRUE and the connection encontered errors.",
           "\n Please run this function again or run the steps manually.",
           call. = get("diagnostics"))
    }
  }

}
