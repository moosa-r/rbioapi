#' Retrieve a List of available libraries from Enrichr
#'
#' This function will retrieve a list of available libraries in Enrichr with
#'   their statistics. And by default, will save those names as a global option
#'   ("rba_enrichr_libs") to be available for other Enrichr functions that
#'   internally require the names of Enrichr libraries.
#'
#' You should call this function once per R session with the argument
#'   'store_in_options = TRUE' before using \code{\link{rba_enrichr_enrich}}
#'   or \code{\link{rba_enrichr}}.\cr
#'   Nevertheless, rbioapi will do this for you in the background at the first
#'   time you call any function which requires this.\cr
#'  Note that using \code{\link{rba_enrichr}} is a more convenient way to
#'    automatically perform this and other required function calls to enrich
#'    your input gene-set.
#'
#' @section Corresponding API Resources:
#'  "GET http://maayanlab.cloud/Enrichr/datasetStatistics"
#'
#' @param store_in_options logical: (default = TRUE) Should a list of available
#' Enrichr libraries be saved as a global option?
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#'
#' @return A data frame with the names of available library in Enrichr and their
#'   statistics.
#'
#' @references \itemize{
#'   \item Chen EY, Tan CM, Kou Y, Duan Q, Wang Z, Meirelles GV, Clark NR,
#'   Ma'ayan A. Enrichr: interactive and collaborative HTML5 gene list
#'   enrichment analysis tool. BMC Bioinformatics. 2013;128(14).
#'   \item Kuleshov MV, Jones MR, Rouillard AD, Fernandez NF, Duan Q, Wang Z,
#'   Koplev S, Jenkins SL, Jagodnik KM, Lachmann A, McDermott MG, Monteiro CD,
#'   Gundersen GW, Ma'ayan A. Enrichr: a comprehensive gene set enrichment
#'   analysis web server 2016 update. Nucleic Acids Research. 2016; gkw377.
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_enrichr_info()
#'
#' @family "Enrichr API"
#' @seealso \code{\link{rba_enrichr}}
#' @export
rba_enrichr_info = function(store_in_options = TRUE,
                            ...){
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "store_in_options",
                               class = "logical")))

  v_msg("Retrieving List of available libraries and statistics from Enrichr.")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("enrichr", "url"),
                           path = paste0(rba_ba_stg("enrichr", "pth"),
                                         "datasetStatistics"),
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("enrichr_info.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  ## Save Library Names as Global Options
  if (store_in_options == TRUE) {
    options(rba_enrichr_libs = final_output[["statistics.libraryName"]])
  }
  return(final_output)
}

#' Upload Your Gene-List to Enrichr
#'
#' Prior to perform enrichment, Enrichr requires you to upload your gene-list
#'   and retrieve a 'user list ID'.
#'
#'  Note that using \code{\link{rba_enrichr}} is a more convenient way to
#'    automatically perform this and other required function calls to enrich
#'    your input gene-set.
#'
#' @section Corresponding API Resources:
#'  "POST http://maayanlab.cloud/Enrichr/addList"
#'
#' @param gene_list A vector with Entrez gene symbols.
#' @param description (optional) A name or description to be associated with your
#'   uploaded gene-set to Enrichr servers.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#'
#' @return A list with two unique IDs for your uploaded gene sets.
#'
#' @references \itemize{
#'   \item Chen EY, Tan CM, Kou Y, Duan Q, Wang Z, Meirelles GV, Clark NR,
#'   Ma'ayan A. Enrichr: interactive and collaborative HTML5 gene list
#'   enrichment analysis tool. BMC Bioinformatics. 2013;128(14).
#'   \item Kuleshov MV, Jones MR, Rouillard AD, Fernandez NF, Duan Q, Wang Z,
#'   Koplev S, Jenkins SL, Jagodnik KM, Lachmann A, McDermott MG, Monteiro CD,
#'   Gundersen GW, Ma'ayan A. Enrichr: a comprehensive gene set enrichment
#'   analysis web server 2016 update. Nucleic Acids Research. 2016; gkw377.
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_enrichr_add_list(gene_list = c("TP53", "TNF", "EGFR"),
#'   description = "tumoral genes")
#'
#' @family "Enrichr API"
#' @seealso \code{\link{rba_enrichr}}
#' @export
rba_enrichr_add_list = function(gene_list,
                                description = NA,
                                ...){
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "gene_list",
                               class = "character"),
                          list(arg = "description",
                               class = "character")))

  v_msg("Uploading %s gene symbols to Enrichr.", length(gene_list))

  ## Build POST API Request's URL
  call_body = rba_ba_query(init = list("format" = "text",
                                       "list" = paste(unique(gene_list),
                                                      collapse = "\n")),
                           list("description",
                                !is.na(description),
                                description))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("enrichr", "url"),
                           path = paste0(rba_ba_stg("enrichr", "pth"),
                                         "addList"),
                           body = call_body,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("enrichr_add_list.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' View an Uploaded Gene List
#'
#' Retrieve a list of uploaded genes under a 'user list ID'.
#'
#' @section Corresponding API Resources:
#'  "GET http://maayanlab.cloud/Enrichr/view"
#
#' @param user_list_id a user_list_id returned to you after uploading a gene
#'   list using \code{\link{rba_enrichr_add_list}}
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#'
#' @return A list containing the genes and description available under the
#'   provided user_list_id
#'
#' @references \itemize{
#'   \item Chen EY, Tan CM, Kou Y, Duan Q, Wang Z, Meirelles GV, Clark NR,
#'   Ma'ayan A. Enrichr: interactive and collaborative HTML5 gene list
#'   enrichment analysis tool. BMC Bioinformatics. 2013;128(14).
#'   \item Kuleshov MV, Jones MR, Rouillard AD, Fernandez NF, Duan Q, Wang Z,
#'   Koplev S, Jenkins SL, Jagodnik KM, Lachmann A, McDermott MG, Monteiro CD,
#'   Gundersen GW, Ma'ayan A. Enrichr: a comprehensive gene set enrichment
#'   analysis web server 2016 update. Nucleic Acids Research. 2016; gkw377.
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   }
#'
#' @examples
#' \dontrun{rba_enrichr_view_list(user_list_id = 11111)}
#'
#' @family "Enrichr API"
#' @export
rba_enrichr_view_list = function(user_list_id,
                                 ...){
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "user_list_id",
                               class = c("numeric", "integer"),
                               len = 1)))

  v_msg("Retrieving the gene list under the ID %s.", user_list_id)

  ## Build GET API Request's query
  call_query = list("userListId" = user_list_id)

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("enrichr", "url"),
                           path = paste0(rba_ba_stg("enrichr", "pth"),
                                         "view"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file(sprintf("enrichr_view_list_%s.json",
                                                         user_list_id)))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Internal function for rba_enrichr_enrich
#'
#' This is an internal helper function which will retrieve the enrichment
#'   results of one_user_list id against one library name
#'
#' The function will be called within \code{\link{rba_enrichr_enrich}} and will
#' handle api requests to the server.
#'
#' @section Corresponding API Resources:
#'  "GET http://maayanlab.cloud/Enrichr/enrich"
#'
#' @param user_list_id An ID returned to you after uploading a gene
#'   list using \code{\link{rba_enrichr_add_list}}
#' @param gene_set_library a valid gene-set library name which exists
#' in the results retrieved via \code{\link{rba_enrichr_info}}.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#'
#' @return A data frame with the enrichment results of the provided user_list_id
#'   against the gene_set_library
#'
#' @references \itemize{
#'   \item Chen EY, Tan CM, Kou Y, Duan Q, Wang Z, Meirelles GV, Clark NR,
#'   Ma'ayan A. Enrichr: interactive and collaborative HTML5 gene list
#'   enrichment analysis tool. BMC Bioinformatics. 2013;128(14).
#'   \item Kuleshov MV, Jones MR, Rouillard AD, Fernandez NF, Duan Q, Wang Z,
#'   Koplev S, Jenkins SL, Jagodnik KM, Lachmann A, McDermott MG, Monteiro CD,
#'   Gundersen GW, Ma'ayan A. Enrichr: a comprehensive gene set enrichment
#'   analysis web server 2016 update. Nucleic Acids Research. 2016; gkw377.
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   }
#'
#' @export
rba_enrichr_enrich_internal = function(user_list_id,
                                       gene_set_library,
                                       save_name,
                                       ...){
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Build GET API Request's query
  call_query = list("userListId" = user_list_id,
                    "backgroundType" = gene_set_library)

  ## Build Function-Specific Call
  parser_input = quote(httr::content(response,
                                     as = "text",
                                     type = "text/tab-separated-values",
                                     encoding = "UTF-8"))

  input_call = rba_ba_httr(httr = "get",
                           rba_ba_stg("enrichr", "url"),
                           path = paste0(rba_ba_stg("enrichr", "pth"),
                                         "export"),
                           query = call_query,
                           httr::accept("text/tab-separated-values"),
                           parser = parser_input,
                           save_to = rba_ba_file(save_name))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  final_output = utils::read.delim(textConnection(final_output),
                                   sep = "\t", header = TRUE,
                                   stringsAsFactors = FALSE)
  return(final_output)
}

#' Get Enrichr Enrichment Results
#'
#' This function which will retrieve the enrichment results of your
#'   provided gene-list id against one or multiple Enrichr libraries.
#'
#' Note that using \code{\link{rba_enrichr}} is a more convenient way to
#'   automatically perform this and other required function calls to enrich
#'   your input gene-set.
#'
#' @section Corresponding API Resources:
#'  "GET http://maayanlab.cloud/Enrichr/enrich"
#'
#' @param user_list_id An ID returned to you after uploading a gene
#'   list using \code{\link{rba_enrichr_add_list}}
#' @param gene_set_library One of the:
#'   \enumerate{
#'   \item "all" to select all of the available Enrichr gene-set libraries.
#'   \item A gene-set library name existed in the results
#'   retrieved via \code{\link{rba_enrichr_info}}
#'   \item If regex_library_name = TRUE, A partially-matching name a regex
#'   pattern that correspond to one or more of Enrichr library names.
#'   }
#' @param regex_library_name logical: if TRUE (default) the provided
#'   gene_set_library will be regarded as a regex or partially matching name. if
#'   FALSE, gene_set_library will be considered exact match.
#' @param multi_libs_progress_bar logical: In case of selecting multiple Enrichr
#'   libraries, should a progress bar be displayed?
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#'
#' @return A list containing data frames of the enrichment results of your
#'   provided gene-list against the selected Enrichr libraries.
#'
#' @references \itemize{
#'   \item Chen EY, Tan CM, Kou Y, Duan Q, Wang Z, Meirelles GV, Clark NR,
#'   Ma'ayan A. Enrichr: interactive and collaborative HTML5 gene list
#'   enrichment analysis tool. BMC Bioinformatics. 2013;128(14).
#'   \item Kuleshov MV, Jones MR, Rouillard AD, Fernandez NF, Duan Q, Wang Z,
#'   Koplev S, Jenkins SL, Jagodnik KM, Lachmann A, McDermott MG, Monteiro CD,
#'   Gundersen GW, Ma'ayan A. Enrichr: a comprehensive gene set enrichment
#'   analysis web server 2016 update. Nucleic Acids Research. 2016; gkw377.
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   }
#'
#' @examples
#' \dontrun{rba_enrichr_enrich(user_list_id = "11111")},
#' \dontrun{rba_enrichr_enrich(user_list_id = "11111",
#'          gene_set_library = "GO_Molecular_Function_2017",
#'          regex_library_name = FALSE)}
#' \dontrun{rba_enrichr_enrich(user_list_id = "11111",
#'          gene_set_library = "go",
#'          regex_library_name = TRUE)}
#'
#' @family "Enrichr API"
#' @seealso \code{\link{rba_enrichr}}
#' @export
rba_enrichr_enrich = function(user_list_id,
                              gene_set_library = "all",
                              regex_library_name = TRUE,
                              multi_libs_progress_bar = TRUE,
                              ...){
  ## Load Global Options
  rba_ba_ext_args(...)
  ## get a list of available libraries
  if (is.null(getOption("rba_enrichr_libs"))){
    v_msg("Calling rba_enrichr_info() to get the names of available Enricr libraries.")
    invisible(rba_enrichr_info(store_in_options = TRUE))
  }
  ## handle different gene_set_library input situations
  if (length(gene_set_library) > 1) {
    run_mode = "multiple"
  } else if (gene_set_library == "all") {
    run_mode = "multiple"
    gene_set_library = getOption("rba_enrichr_libs")
  } else {
    if (regex_library_name == FALSE) {
      run_mode = "single"
    } else {
      gene_set_library = grep(gene_set_library,
                              getOption("rba_enrichr_libs"),
                              ignore.case = TRUE, value = TRUE, perl = TRUE)
      #check the results of regex
      if (length(gene_set_library) == 0) {
        stop("Your regex pattern did not match any Enrichr library name.",
             call. = get("diagnostics"))
      } else if (length(gene_set_library) == 1) {
        run_mode = "single"
      } else if (length(gene_set_library) > 1) {
        run_mode = "multiple"
      }
    }
  } #end of if length(gene_set_library) > 1
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "user_list_id",
                               class = c("numeric", "integer"),
                               len = 1),
                          list(arg = "gene_set_library",
                               class = "character",
                               val = getOption("rba_enrichr_libs")),
                          list(arg = "multi_libs_progress_bar",
                               class = "logical")))
  ## call Enrichr API
  if (run_mode == "single") {
    v_msg("Enriching gene-list %s against Enrichr library: %s.",
          user_list_id, gene_set_library)
    final_output = rba_enrichr_enrich_internal(user_list_id = user_list_id,
                                               gene_set_library = gene_set_library,
                                               save_name = sprintf("enrichr_%s_%s.json",
                                                                   user_list_id,
                                                                   gene_set_library),
                                               ...)
    return(final_output)

  } else {
    v_msg("Enriching gene-list %s using multiple Enrichr libraries.",
          user_list_id)
    v_msg(paste0("Note: You have selected '%s' Enrichr libraries. Note that for ",
                 "each library, a separate call should be sent to Enrichr server. ",
                 "Thus, this could take a while depending on the number of selected ",
                 "libraries and your network connection."),
          length(gene_set_library))
    ## initiate progress bar
    if (multi_libs_progress_bar == TRUE) {
      pb = utils::txtProgressBar(min = 0,
                                 max = length(gene_set_library),
                                 style = 3)
    }
    final_output = lapply(gene_set_library,
                          function(x){
                            lib_enrich_res = rba_enrichr_enrich_internal(user_list_id = user_list_id,
                                                                         gene_set_library = x,
                                                                         save_name = sprintf("enrichr_%s_%s.json",
                                                                                             user_list_id,
                                                                                             x),
                                                                         ...)
                            #advance the progress bar
                            if (multi_libs_progress_bar == TRUE) {
                              utils::setTxtProgressBar(pb, which(gene_set_library == x))
                            }
                            return(lib_enrich_res)
                          })
    close(pb)
    names(final_output) = gene_set_library
    return(final_output)
  }
}


#' Find Enrichr Terms That Contain a Given Gene
#'
#' This function will search your provided gene and retrieve a list of Enrichr
#'   Terms that contains that gene.
#'
#' @section Corresponding API Resources:
#'  "GET http://maayanlab.cloud/Enrichr/genemap"
#'
#' @param gene character: An Entrez gene symbol.
#' @param catagorize logical: Should the catagory informations be included?
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#'
#' @return a list containing the search results of your provided gene.
#'
#' @references \itemize{
#'   \item Chen EY, Tan CM, Kou Y, Duan Q, Wang Z, Meirelles GV, Clark NR,
#'   Ma'ayan A. Enrichr: interactive and collaborative HTML5 gene list
#'   enrichment analysis tool. BMC Bioinformatics. 2013;128(14).
#'   \item Kuleshov MV, Jones MR, Rouillard AD, Fernandez NF, Duan Q, Wang Z,
#'   Koplev S, Jenkins SL, Jagodnik KM, Lachmann A, McDermott MG, Monteiro CD,
#'   Gundersen GW, Ma'ayan A. Enrichr: a comprehensive gene set enrichment
#'   analysis web server 2016 update. Nucleic Acids Research. 2016; gkw377.
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_enrichr_gene_map(gene = "p53")
#' rba_enrichr_gene_map(gene = "p53", catagorize = TRUE)
#'
#' @family "Enrichr API"
#' @export
rba_enrichr_gene_map = function(gene,
                                catagorize = FALSE,
                                ...){
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "gene",
                               class = "character",
                               len = 1),
                          list(arg = "catagorize",
                               class = "logical")))

  v_msg("Finding terms that contain gene: %s.", gene)

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list("gene" = gene,
                                        "json" = "true"),
                            list("setup",
                                 catagorize == TRUE,
                                 "true"))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("enrichr", "url"),
                           path = paste0(rba_ba_stg("enrichr", "pth"),
                                         "genemap"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("enrichr_gene_map.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' A One-step Wrapper for Gene-list Enrichment Using Enrichr
#'
#' This function is an easy-to-use wrapper for the multiple function calls
#'   necessary to enrich a given gene-list using Enrichr. see details section
#'   for more information.
#'
#' This function will call other rba_enrichr_*** functions with the following
#'   order:
#'   \enumerate{
#'   \item (If neccessary) Call \code{\link{rba_enrichr_info}} to obtain a list
#'     of available libraries in Enrichr.
#'   \item Call \code{\link{rba_enrichr_add_list}} to upload your gene-list
#'     and obtain a 'user list ID'.
#'   \item Call \code{\link{rba_enrichr_enrich}} to enrich the gene-list
#'     against one or multiple Enrichr libraries
#'   }
#' @section Corresponding API Resources:
#'  "GET http://maayanlab.cloud/Enrichr/datasetStatistics"\cr
#'  "POST http://maayanlab.cloud/Enrichr/addList"\cr
#'  "GET http://maayanlab.cloud/Enrichr/enrich"
#'
#' @inheritParams rba_enrichr_add_list
#' @inheritParams rba_enrichr_enrich
#'
#' @return A list containing data frames of the enrichment results of your
#'   provided gene-list against the selected Enrichr libraries.
#'
#' @references \itemize{
#'   \item Chen EY, Tan CM, Kou Y, Duan Q, Wang Z, Meirelles GV, Clark NR,
#'   Ma'ayan A. Enrichr: interactive and collaborative HTML5 gene list
#'   enrichment analysis tool. BMC Bioinformatics. 2013;128(14).
#'   \item Kuleshov MV, Jones MR, Rouillard AD, Fernandez NF, Duan Q, Wang Z,
#'   Koplev S, Jenkins SL, Jagodnik KM, Lachmann A, McDermott MG, Monteiro CD,
#'   Gundersen GW, Ma'ayan A. Enrichr: a comprehensive gene set enrichment
#'   analysis web server 2016 update. Nucleic Acids Research. 2016; gkw377.
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_enrichr_enrich(gene_list = c("TP53", "TNF", "EGFR")),
#' rba_enrichr_enrich(gene_list = c("TP53", "TNF", "EGFR")",
#'          gene_set_library = "GO_Molecular_Function_2017",
#'          regex_library_name = FALSE)
#' rba_enrichr_enrich(gene_list = c("TP53", "TNF", "EGFR"),
#'          gene_set_library = "go",
#'          regex_library_name = TRUE)
#'
#' @family "Enrichr API"
#' @export
rba_enrichr = function(gene_list,
                       description = NA,
                       gene_set_library = "all",
                       regex_library_name = TRUE,
                       multi_libs_progress_bar = TRUE,
                       ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "gene_list",
                               class = "character"),
                          list(arg = "description",
                               class = "character"),
                          list(arg = "regex_library_name",
                               class = "logical"),
                          list(arg = "multi_libs_progress_bar",
                               class = "logical")))
  v_msg("--Step 1/3:")
  invisible(rba_enrichr_info(store_in_options = TRUE,
                             ...))
  v_msg("--Step 2/3:")
  list_id = rba_enrichr_add_list(gene_list = gene_list,
                                 description = description,
                                 ...)
  v_msg("--Step 3/3:")
  enriched = rba_enrichr_enrich(user_list_id = list_id$userListId,
                                gene_set_library = gene_set_library,
                                regex_library_name = regex_library_name,
                                multi_libs_progress_bar = multi_libs_progress_bar,
                                ...)
  return(enriched)
}
