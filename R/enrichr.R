#' Internal function to validate rba_enrichr steps
#'
#' This is an internal helper function that after each main step of rba_enrichr,
#'   checks wether the operation sucedded and stops or returns a character
#'   string based on the skip_error in the parent environment.
#'
#' @param assertion logical. A test to vaildate the previous step output.
#' @param msg_type Character. Chooses which predefined message template to use.
#'   Valid values:
#'   - `"no_lib"`: failed to fetch Enrichr libraries
#'   - `"invalid_lib_regex"`: regex did not match any library names
#'   - `"invalid_lib_chr"`: one or more explicit library names are invalid
#'   - `"no_gene_upload"`: failed to upload the gene list
#'   - `"no_background_upload"`: failed to upload the background list
#'   - `"no_result"`: failed to retrieve enrichment results
#' @param msg_detail Character. Additional information to add to the error
#'   message template.
#'
#' @return If
#'   - `assertion` is `TRUE`: returns `TRUE`
#'   Otherwise, if
#'   - `skip_error = FALSE`: invokes `stop()` with a detailed error message
#'   - `skip_error = TRUE`: calls `.msg()` and returns the message string
#'
#' @noRd
.rba_enrichr_validate <- function(assertion,
                                  msg_type,
                                  msg_detail,
                                  skip_error) {

  if (assertion) {
    return(TRUE)
  } else {
    error_msg <- paste0(
      switch(
        msg_type,
        no_lib = c(
          "Error: Couldn't fetch available Enrichr libraries. Please retry or manually run `rba_enrichr_libs(store_in_options = TRUE)`. ",
          "The internal `rba_enrichr_libs()` call did not return a non-empty data.frame with a character column `libraryName`",
          "The error message was: ",
          msg_detail
        ),
        invalid_lib_regex = c(
          "Error: ",
          "Your supplied regex didn't match any Enrichr libraries.",
          "Please use `rba_enrichr_libs()` to get a list of valid Enrichr libraries. ",
          "input regex: ",
          msg_detail
        ),
        invalid_lib_chr = c(
          "Error: ",
          "The following supplied Enrichr libraries are invalid. ",
          "Please use `rba_enrichr_libs()` to get a list of valid Enrichr libraries. ",
          paste0(msg_detail, collapse = ", ")
        ),
        no_gene_upload = c(
          "Error: Couldn't upload your genes list to Enrichr. ",
          "Please retry or manually run the required steps as demonstrated in the `Enrichr & rbioapi` vignette article, section `Approach 2: Going step-by-step`. ",
          "If the problem persists, kindly report this issue to us. The error message was: ",
          msg_detail
        ),
        no_background_upload = c(
          "Error: Couldn't upload your background gene list to Enrichr. ",
          "Please retry or manually run the required steps as demonstrated in the `Enrichr & rbioapi` vignette article, section `Approach 2: Going step-by-step`. ",
          "If the problem persists, kindly report this issue to us. The error message was: ",
          msg_detail
        ),
        no_result = c(
          "Error: Couldn't retrieve the submitted Enrichr analysis request. ",
          "Please retry or manually run the required steps as demonstrated in the `Enrichr & rbioapi` vignette article, section `Approach 2: Going step-by-step`. ",
          "If the problem persists, kindly report this issue to us. The error message was:",
          msg_detail
        )
      ),
      collapse = "\n"
    )

    if (skip_error) {
      .msg(error_msg)
      return(error_msg)
    } else {
      stop(error_msg, call. = FALSE)
    }
  }

}


#' Retrieve a List of available libraries from Enrichr
#'
#' This function retrieves a list of libraries available in Enrichr along with
#'   their associated statistics. Each library represents a collection of gene sets
#'   that can be used for enrichment analysis.
#'
#' By default, this function will save the library names as a global option
#'   ("rba_enrichr_libs") for other Enrichr functions that internally require
#'   the names of Enrichr libraries. You should call this function once per
#'   R session with the argument 'store_in_options = TRUE' before
#'   using \code{\link{rba_enrichr_enrich}} or \code{\link{rba_enrichr}}.
#'   However, if you do not explicitly call it, rbioapi will automatically
#'   execute this function in the background the when it is needed.
#'
#' Please note that \code{\link{rba_enrichr}} provides a one-step and
#'   more convenient way to automatically handle this and other required
#'   function calls needed to perform gene set enrichment analysis with Enrichr.
#'
#' @section Corresponding API Resources:
#'  "GET https://maayanlab.cloud/Enrichr/datasetStatistics"
#'
#' @param organism (default = "human") Which model organism version of Enrichr
#'   to use? Available options are: "human", (H. sapiens & M. musculus),
#'   "fly" (D. melanogaster), "yeast" (S. cerevisiae), "worm" (C. elegans)
#'   and "fish" (D. rerio).
#' @param store_in_options logical: (default = TRUE) Should a list of available
#' Enrichr libraries be saved as a global option?
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame with the names of available library in Enrichr and their
#'   statistics.
#'
#' @references \itemize{
#'   \item Chen, E.Y., Tan, C.M., Kou, Y. et al. Enrichr: interactive and
#'   collaborative HTML5 gene list enrichment analysis tool. Bioinformatics
#'   14, 128 (2013). https://doi.org/10.1186/1471-2105-14-128
#'   \item Maxim V. Kuleshov, Matthew R. Jones, Andrew D. Rouillard, Nicolas
#'   F. Fernandez, Qiaonan Duan, Zichen Wang, Simon Koplev, Sherry L. Jenkins,
#'   Kathleen M. Jagodnik, Alexander Lachmann, Michael G. McDermott,
#'   Caroline D. Monteiro, Gregory W. Gundersen, Avi Ma’ayan, Enrichr: a
#'   comprehensive gene set enrichment analysis web server 2016 update,
#'   Nucleic Acids Research, Volume 44, Issue W1, 8 July 2016, Pages W90–W97,
#'   https://doi.org/10.1093/nar/gkw377
#'   \item Xie, Z., Bailey, A., Kuleshov, M. V., Clarke, D. J. B.,
#'   Evangelista, J. E., Jenkins, S. L., Lachmann, A., Wojciechowicz, M. L.,
#'   Kropiwnicki, E., Jagodnik, K. M., Jeon, M., & Ma’ayan, A. (2021). Gene
#'   set knowledge discovery with Enrichr. Current Protocols, 1, e90.
#'   doi: 10.1002/cpz1.90
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   \item \href{https://maayanlab.cloud/Enrichr/help#terms}{Citations note
#'   on Enrichr website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_enrichr_libs()
#' }
#'
#' @family "Enrichr"
#' @seealso \code{\link{rba_enrichr}}
#' @export
rba_enrichr_libs <- function(organism = "human",
                             store_in_options = TRUE,
                             ...){
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "store_in_options", class = "logical"),
      list(
        arg = "organism", class = "character", no_null = TRUE,
        val = c("human", "fly", "yeast", "worm", "fish")
      )
    )
  )

  .msg(
    "Retrieving List of available libraries and statistics from Enrichr %s.",
    organism
  )

  ## Build Function-Specific Call
  parser_input <- list(
    "json->list_simp",
    function(x) { x[[1]] }
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("enrichr", "url"),
    path = paste0(.rba_stg("enrichr", "pth", organism), "datasetStatistics"),
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("enrichr_info.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)

  ## Save Library Names as Global Options
  if (isTRUE(store_in_options) && utils::hasName(final_output, "libraryName")) {

    enrichr_libs <- getOption("rba_enrichr_libs", default = list())
    enrichr_libs[[organism]] <- final_output[["libraryName"]]
    options("rba_enrichr_libs" = enrichr_libs)

  }

  return(final_output)
}

#' Upload Your Gene-List to Enrichr
#'
#' This function uploads your gene list to Enrichr and retrieves a unique
#'   'user list ID' required for performing enrichment analysis.
#'
#' Enrichr uses separate APIs for analysis with or without a background gene
#'   list. Set `speedrichr = TRUE` if this gene list will be used with a
#'   background gene list; otherwise, set it to FALSE. Gene lists submitted with
#'   `speedrichr = TRUE` can only be analyzed with a background set, and those
#'    submitted with `speedrichr = FALSE` can only be analyzed without one.
#'    Currently, background-based enrichment is supported only for human
#'    libraries.
#'
#' Please note that \code{\link{rba_enrichr}} provides a one-step and
#'   more convenient way to automatically handle this and other required
#'   function calls needed to perform gene set enrichment analysis with Enrichr.
#'
#' @section Corresponding API Resources:
#'  "POST https://maayanlab.cloud/Enrichr/addList"
#'  \cr "POST https://maayanlab.cloud/speedrichr/api/addList"
#'
#' @param gene_list A character vector with Entrez gene symbols of test genes.
#' @param description (optional) A description to be associated with your
#'   uploaded gene-set to Enrichr servers.
#' @param organism (default = "human") Which model organism version of Enrichr
#'   to use? Available options are: "human", (H. sapiens & M. musculus),
#'   "fly" (D. melanogaster), "yeast" (S. cerevisiae), "worm" (C. elegans)
#'   and "fish" (D. rerio).
#' @param speedrichr logical (default = FALSE) Set to TRUE if you will use this
#'   gene list with a background list; otherwise, set to FALSE. Only available
#'   for human libraries. Refer to the details section for more information.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list with the unique IDs for your uploaded gene list. `userListId`
#'   is the ID required for later steps.
#'
#' @references \itemize{
#'   \item Chen, E.Y., Tan, C.M., Kou, Y. et al. Enrichr: interactive and
#'   collaborative HTML5 gene list enrichment analysis tool. Bioinformatics
#'   14, 128 (2013). https://doi.org/10.1186/1471-2105-14-128
#'   \item Maxim V. Kuleshov, Matthew R. Jones, Andrew D. Rouillard, Nicolas
#'   F. Fernandez, Qiaonan Duan, Zichen Wang, Simon Koplev, Sherry L. Jenkins,
#'   Kathleen M. Jagodnik, Alexander Lachmann, Michael G. McDermott,
#'   Caroline D. Monteiro, Gregory W. Gundersen, Avi Ma’ayan, Enrichr: a
#'   comprehensive gene set enrichment analysis web server 2016 update,
#'   Nucleic Acids Research, Volume 44, Issue W1, 8 July 2016, Pages W90–W97,
#'   https://doi.org/10.1093/nar/gkw377
#'   \item Xie, Z., Bailey, A., Kuleshov, M. V., Clarke, D. J. B.,
#'   Evangelista, J. E., Jenkins, S. L., Lachmann, A., Wojciechowicz, M. L.,
#'   Kropiwnicki, E., Jagodnik, K. M., Jeon, M., & Ma’ayan, A. (2021). Gene
#'   set knowledge discovery with Enrichr. Current Protocols, 1, e90.
#'   doi: 10.1002/cpz1.90
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   \item \href{https://maayanlab.cloud/Enrichr/help#terms}{Citations note
#'   on Enrichr website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_enrichr_add_list(gene_list = c("TP53", "TNF", "EGFR"),
#'      description = "tumoral genes",
#'      speedrichr = FALSE)
#' }
#' \donttest{
#' rba_enrichr_add_list(gene_list = c("RAG1", "RAG2", "DNTT", "LIG4", "ARTEMIS"),
#'      description = "TCR rearrangment",
#'      speedrichr = TRUE)
#' }
#'
#' @family "Enrichr"
#' @seealso \code{\link{rba_enrichr}}
#' @export
rba_enrichr_add_list <- function(gene_list,
                                 description = NULL,
                                 organism = "human",
                                 speedrichr = FALSE,
                                 ...){
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "gene_list", class = "character", min_len = 1),
      list(arg = "description", class = "character"),
      list(
        arg = "organism", class = "character", no_null = TRUE,
        val = c("human", "fly", "yeast", "worm", "fish")
      ),
      list(arg = "speedrichr", class = "logical", no_null = TRUE)
    ),
    cond = list(
      list(
        quote((isTRUE(speedrichr)) && organism != "human"),
        "Using speedrichr (to provide background gene list later) is only available for `human`."
      )
    )
  )

  .msg(
    "Uploading %s gene symbols to Enrichr %s.",
    length(gene_list), organism
  )

  ## Build POST API Request's URL
  input_path <- paste0(
    .rba_stg("enrichr", "pth", ifelse(speedrichr, "speedrichr", organism)),
    "addList"
  )

  # Description is mandatory for speedrichr
  if (isTRUE(speedrichr) && is.null(description)) {
    description = "Submitted to speedrichr"
  }

  call_body <- .rba_query(
    init = list(
      "format" = "text",
      "list" = paste(unique(gene_list), collapse = "\n")
    ),
    list("description", !is.null(description), description)
  )


  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("enrichr", "url"),
    path = input_path,
    body = call_body,
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("enrichr_add_list.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' View an Uploaded Gene List
#'
#' Retrieve the list of uploaded genes with a given 'user list ID'.
#'
#' @section Corresponding API Resources:
#'  "GET https://maayanlab.cloud/Enrichr/view"
#
#' @param user_list_id a user list ID returned after uploading a gene
#'   list using \code{\link{rba_enrichr_add_list}}
#' @param organism (default = "human") Which model organism version of Enrichr
#'   to use? Available options are: "human", (H. sapiens & M. musculus),
#'   "fly" (D. melanogaster), "yeast" (S. cerevisiae), "worm" (C. elegans)
#'   and "fish" (D. rerio).
#' @param speedrichr logical (default = FALSE) Did you upload your gene list
#'   to speedrichr API? (i.e. did you intend to use this gene list along with
#'   a background gene list?)
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the genes and description associated to the
#'   supplied user_list_id.
#'
#' @references \itemize{
#'   \item Chen, E.Y., Tan, C.M., Kou, Y. et al. Enrichr: interactive and
#'   collaborative HTML5 gene list enrichment analysis tool. Bioinformatics
#'   14, 128 (2013). https://doi.org/10.1186/1471-2105-14-128
#'   \item Maxim V. Kuleshov, Matthew R. Jones, Andrew D. Rouillard, Nicolas
#'   F. Fernandez, Qiaonan Duan, Zichen Wang, Simon Koplev, Sherry L. Jenkins,
#'   Kathleen M. Jagodnik, Alexander Lachmann, Michael G. McDermott,
#'   Caroline D. Monteiro, Gregory W. Gundersen, Avi Ma’ayan, Enrichr: a
#'   comprehensive gene set enrichment analysis web server 2016 update,
#'   Nucleic Acids Research, Volume 44, Issue W1, 8 July 2016, Pages W90–W97,
#'   https://doi.org/10.1093/nar/gkw377
#'   \item Xie, Z., Bailey, A., Kuleshov, M. V., Clarke, D. J. B.,
#'   Evangelista, J. E., Jenkins, S. L., Lachmann, A., Wojciechowicz, M. L.,
#'   Kropiwnicki, E., Jagodnik, K. M., Jeon, M., & Ma’ayan, A. (2021). Gene
#'   set knowledge discovery with Enrichr. Current Protocols, 1, e90.
#'   doi: 10.1002/cpz1.90
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   \item \href{https://maayanlab.cloud/Enrichr/help#terms}{Citations note
#'   on Enrichr website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_enrichr_view_list(user_list_id = 11111)
#' }
#'
#' @family "Enrichr"
#' @export
rba_enrichr_view_list <- function(user_list_id,
                                  organism = "human",
                                  speedrichr = FALSE,
                                  ...){
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "user_list_id", class = c("numeric"), len = 1),
      list(
        arg = "organism", class = "character", no_null = TRUE,
        val = c("human", "fly", "yeast", "worm", "fish")
      ),
      list(arg = "speedrichr", class = "logical", no_null = TRUE)
    ),
    cond = list(
      list(
        quote((isTRUE(speedrichr)) && organism != "human"),
        "Using speedrichr (to provide background gene list later) is only available for `human`."
      )
    )
  )

  .msg(
    "Retrieving the gene list under the ID %s from Enrichr %s.",
    user_list_id, organism
  )

  ## Build GET API Request's query
  call_query <- list("userListId" = user_list_id)

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("enrichr", "url"),
    path = paste0(.rba_stg("enrichr", "pth", organism), "view"),
    query = call_query,
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file(sprintf("enrichr_view_list_%s.json", user_list_id))
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Upload Background Gene-List to Enrichr
#'
#' In addition to the main gene list, you can also submit a background gene
#'   list to Enrichr. This gene list can be used later to compute the
#'   statistics of the enrichment analysis.
#'
#' Please note that \code{\link{rba_enrichr}} provides a one-step and
#'   more convenient way to automatically handle this and other required
#'   function calls needed to perform gene set enrichment analysis with Enrichr.
#'
#' @section Corresponding API Resources:
#'  "POST https://maayanlab.cloud/speedrichr/api/addbackground"
#'
#' @param background_genes A character vector of Entrez gene symbols of the
#'   background genes.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list with the unique IDs for your uploaded background gene list.
#'
#' @references \itemize{
#'   \item Chen, E.Y., Tan, C.M., Kou, Y. et al. Enrichr: interactive and
#'   collaborative HTML5 gene list enrichment analysis tool. Bioinformatics
#'   14, 128 (2013). https://doi.org/10.1186/1471-2105-14-128
#'   \item Maxim V. Kuleshov, Matthew R. Jones, Andrew D. Rouillard, Nicolas
#'   F. Fernandez, Qiaonan Duan, Zichen Wang, Simon Koplev, Sherry L. Jenkins,
#'   Kathleen M. Jagodnik, Alexander Lachmann, Michael G. McDermott,
#'   Caroline D. Monteiro, Gregory W. Gundersen, Avi Ma’ayan, Enrichr: a
#'   comprehensive gene set enrichment analysis web server 2016 update,
#'   Nucleic Acids Research, Volume 44, Issue W1, 8 July 2016, Pages W90–W97,
#'   https://doi.org/10.1093/nar/gkw377
#'   \item Xie, Z., Bailey, A., Kuleshov, M. V., Clarke, D. J. B.,
#'   Evangelista, J. E., Jenkins, S. L., Lachmann, A., Wojciechowicz, M. L.,
#'   Kropiwnicki, E., Jagodnik, K. M., Jeon, M., & Ma’ayan, A. (2021). Gene
#'   set knowledge discovery with Enrichr. Current Protocols, 1, e90.
#'   doi: 10.1002/cpz1.90
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   \item \href{https://maayanlab.cloud/Enrichr/help#terms}{Citations note
#'   on Enrichr website}
#'   }
#'
#' @examples
#' \donttest{
#' my_background_genes <- c(
#' "NSUN3", "POLRMT", "NLRX1", "SFXN5", "ZC3H12C", "SLC25A39", "ARSG",
#' "DEFB29", "PCMTD2", "ACAA1A", "LRRC1", "2810432D09RIK", "SEPHS2",
#' "SAC3D1", "TMLHE", "LOC623451", "TSR2", "PLEKHA7", "GYS2", "ARHGEF12",
#' "HIBCH", "LYRM2", "ZBTB44", "ENTPD5", "RAB11FIP2", "LIPT1",
#' "INTU", "ANXA13", "KLF12", "SAT2", "GAL3ST2", "VAMP8", "FKBPL",
#' "AQP11", "TRAP1", "PMPCB", "TM7SF3", "RBM39", "BRI3", "KDR", "ZFP748",
#' "NAP1L1", "DHRS1", "LRRC56", "WDR20A", "STXBP2", "KLF1", "UFC1",
#' "CCDC16", "9230114K14RIK", "RWDD3", "2610528K11RIK")
#'
#' rba_enrichr_add_background(background_genes = my_background_genes)
#' }
#'
#' @family "Enrichr"
#' @seealso \code{\link{rba_enrichr}}
#' @export
rba_enrichr_add_background <- function(background_genes,
                                       ...){
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "background_genes", class = "character", min_len = 1)
    )
  )

  .msg(
    "Uploading %s background gene symbols to Enrichr.",
    length(background_genes)
  )

  ## Build POST API Request's URL
  call_body <- list(background = paste(background_genes, collapse = "\n"))

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("enrichr", "url"),
    path = paste0(.rba_stg("enrichr", "pth", "speedrichr"), "addbackground"),
    body = call_body,
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("enrichr_add_background.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Internal function for rba_enrichr_enrich
#'
#' This is an internal helper function which will retrieve the enrichment
#'   results of one_user_list id against one library name
#'
#' The function will be called within \code{\link{rba_enrichr_enrich}} and will
#' handle API requests to the server.
#'
#' @section Corresponding API Resources:
#'  "GET https://maayanlab.cloud/Enrichr/enrich"
#'  \cr "POST https://maayanlab.cloud/speedrichr/api/backgroundenrich"
#'
#' @param user_list_id An ID returned to you after uploading a gene
#'   list using \code{\link{rba_enrichr_add_list}}
#' @param gene_set_library a valid gene-set library name which exists
#' in the results retrieved via \code{\link{rba_enrichr_libs}}.
#' @param save_name default raw file name
#' @param organism Which model organism version of Enrichr
#'   to use? Available options are: "human", (H. sapiens & M. musculus),
#'   "fly" (D. melanogaster), "yeast" (S. cerevisiae), "worm" (C. elegans)
#'   and "fish" (D. rerio).
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame with the enrichment results of the supplied user_list_id
#'   against the gene_set_library
#'
#' @references \itemize{
#'   \item Chen, E.Y., Tan, C.M., Kou, Y. et al. Enrichr: interactive and
#'   collaborative HTML5 gene list enrichment analysis tool. Bioinformatics
#'   14, 128 (2013). https://doi.org/10.1186/1471-2105-14-128
#'   \item Maxim V. Kuleshov, Matthew R. Jones, Andrew D. Rouillard, Nicolas
#'   F. Fernandez, Qiaonan Duan, Zichen Wang, Simon Koplev, Sherry L. Jenkins,
#'   Kathleen M. Jagodnik, Alexander Lachmann, Michael G. McDermott,
#'   Caroline D. Monteiro, Gregory W. Gundersen, Avi Ma’ayan, Enrichr: a
#'   comprehensive gene set enrichment analysis web server 2016 update,
#'   Nucleic Acids Research, Volume 44, Issue W1, 8 July 2016, Pages W90–W97,
#'   https://doi.org/10.1093/nar/gkw377
#'   \item Xie, Z., Bailey, A., Kuleshov, M. V., Clarke, D. J. B.,
#'   Evangelista, J. E., Jenkins, S. L., Lachmann, A., Wojciechowicz, M. L.,
#'   Kropiwnicki, E., Jagodnik, K. M., Jeon, M., & Ma’ayan, A. (2021). Gene
#'   set knowledge discovery with Enrichr. Current Protocols, 1, e90.
#'   doi: 10.1002/cpz1.90
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   \item \href{https://maayanlab.cloud/Enrichr/help#terms}{Citations note
#'   on Enrichr website}
#'   }
#'
#' @noRd
.rba_enrichr_enrich_internal <- function(user_list_id,
                                         background_id = NULL,
                                         gene_set_library,
                                         save_name,
                                         organism,
                                         sleep_time = 0,
                                         ...){
  ## Load Global Options
  .rba_ext_args(...)

  ## Build GET API Request's query
  if (is.null(background_id)) {
    httr_verb <- "get"
    httr_accept <- "text/tab-separated-values"
    call_query <- list(
      "userListId" = user_list_id,
      "backgroundType" = gene_set_library
    )
    call_body = NULL

    path_input <- paste0(
      .rba_stg("enrichr", "pth", organism),
      "export"
    )

    parser_input <- function(httr_response) {
      parsed_response <- httr::content(
        httr_response,
        as = "text",
        type = "text/tab-separated-values",
        encoding = "UTF-8"
      )

      try(
        utils::read.delim(
          textConnection(parsed_response),
          sep = "\t",
          header = TRUE,
          stringsAsFactors = FALSE),
        silent = !get("diagnostics")
      )
    }

  } else {
    httr_verb <- "post"
    httr_accept <- "application/json"
    call_query <- NULL
    call_body <- list(
      "userListId" = user_list_id,
      "backgroundid" = background_id,
      "backgroundType" = gene_set_library
    )

    path_input <- paste0(
      .rba_stg("enrichr", "pth", "speedrichr"),
      "backgroundenrich"
    )

    parser_input <- function(httr_response) {

      parsed_response <- httr::content(httr_response,
                                       as = "text",
                                       encoding = "UTF-8")

      # To prevent possible lexical errors with fromJSON
      parsed_response <- gsub("Infinity", "\"Inf\"", parsed_response)
      parsed_response <- gsub("NaN", "\"NaN\"", parsed_response)

      parsed_response <- jsonlite::fromJSON(parsed_response)[[1]]

      parsed_response <- lapply(
        parsed_response,
        function(response_row) {
          names(response_row) <- c("Rank", "Term",
                                   "P.value", "Odds.Ratio",
                                   "Combined.Score", "Genes",
                                   "Adjusted.P.value",
                                   "Old.P.value", "Old.adjusted.P.value")
          response_row$Overlapping.genes <- paste0(
            response_row$Overlapping.genes,
            collapse = ";")

          return(response_row)
        }
      )

      if (length(parsed_response) == 0) {
        parsed_response <- data.frame(
          Rank = integer(),
          Term = character(),
          P.value = numeric(),
          Odds.Ratio = numeric(),
          Combined.Score = numeric(),
          Genes = character(),
          Adjusted.P.value = numeric(),
          Old.P.value = numeric(),
          Old.adjusted.P.value = numeric()
        )
      } else {
        parsed_response <- do.call(
          rbind,
          lapply(
            parsed_response,
            function(response_row) {
              as.data.frame(response_row, stringsAsFactors = FALSE)
            }
          )
        )

        numeric_cols <- c("Rank", "P.value", "Odds.Ratio", "Combined.Score",
                          "Adjusted.P.value", "Old.P.value", "Old.adjusted.P.value")
        parsed_response[numeric_cols] <- lapply(parsed_response[numeric_cols], as.numeric)
      }
      return(parsed_response)
    }
  }

  input_call <- .rba_httr(
    httr = httr_verb,
    url = .rba_stg("enrichr", "url"),
    path = path_input,
    query = call_query,
    body = call_body,
    httr::accept(httr_accept),
    parser = parser_input,
    save_to = .rba_file(save_name)
  )

  ## Call API
  Sys.sleep(sleep_time)
  final_output <- .rba_skeleton(input_call)

  if (is.data.frame(final_output)) {
    return(final_output)
  } else {
    error_message <- paste0(
      "Error: Couldn't parse the server response for the requested Enrichr analysis. ",
      "Please try again. If the problem persists, kindly report the issue to us. ",
      "The server's raw response was: ",
      as.character(final_output),
      collapse = "\n"
    )
    if (isTRUE(get("skip_error"))) {
      return(error_message)
    } else {
      stop(error_message, call. = get("diagnostics"))
    }
  }

}

#' Get Enrichr Enrichment Results
#'
#' This function retrieves enrichment analysis results for your supplied
#'   `user_list_id` against one or multiple Enrichr libraries.
#'
#' If `background_id` is supplied, this function will interact with the
#'   speedrichr API. In this case, `user_list_id` must have been obtained
#'   from a \code{\link{rba_enrichr_add_list}} call with the `speedrichr`
#'   parameter set to `TRUE`. Additionally, this feature is only available
#'   for "human" organism.
#'
#' Please note that \code{\link{rba_enrichr}} provides a one-step and
#'   more convenient way to automatically handle this and other required
#'   function calls needed to perform gene set enrichment analysis with Enrichr.
#'
#' @section Corresponding API Resources:
#'  "GET https://maayanlab.cloud/Enrichr/enrich"
#'  \cr "POST https://maayanlab.cloud/speedrichr/api/backgroundenrich"
#'
#' @param user_list_id An ID returned after uploading a gene list
#'   using \code{\link{rba_enrichr_add_list}}, with the `speedrichr` set to
#'   TRUE or FALSE depending on whether you intend to analyze this gene list
#'   with or without a background gene list, respectively.
#' @param background_id An ID returned after uploading a background gene
#'   list using \code{\link{rba_enrichr_add_background}}
#' @param gene_set_library One of the:
#'   \enumerate{
#'   \item "all" to select all of the available Enrichr gene-set libraries.
#'   \item A gene-set library name. You can retrieve the available options
#'   for a given species using \code{\link{rba_enrichr_libs}}.
#'   \item If regex_library_name = TRUE, A partially-matching name a regex
#'   pattern that correspond to one or more of Enrichr library names.
#'   }
#' @param regex_library_name logical: (default = FALSE) if TRUE the supplied
#'   gene_set_library will be considered as a regex pattern.
#'   If FALSE, gene_set_library will be considered as an exact match.
#' @param organism (default = "human") Which model organism version of Enrichr
#'   to use? Available options are: "human", (H. sapiens & M. musculus),
#'   "fly" (D. melanogaster), "yeast" (S. cerevisiae), "worm" (C. elegans)
#'   and "fish" (D. rerio). If `background_id` is provided, the only available
#'   option is "human".
#' @param progress_bar logical: (default = TRUE) if multiple Enrichr libraries
#'   are selected, should a progress bar be displayed?
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing data frames of the enrichment results of your
#'   supplied gene-list against the selected Enrichr libraries.
#'
#' @references \itemize{
#'   \item Chen, E.Y., Tan, C.M., Kou, Y. et al. Enrichr: interactive and
#'   collaborative HTML5 gene list enrichment analysis tool. Bioinformatics
#'   14, 128 (2013). https://doi.org/10.1186/1471-2105-14-128
#'   \item Maxim V. Kuleshov, Matthew R. Jones, Andrew D. Rouillard, Nicolas
#'   F. Fernandez, Qiaonan Duan, Zichen Wang, Simon Koplev, Sherry L. Jenkins,
#'   Kathleen M. Jagodnik, Alexander Lachmann, Michael G. McDermott,
#'   Caroline D. Monteiro, Gregory W. Gundersen, Avi Ma’ayan, Enrichr: a
#'   comprehensive gene set enrichment analysis web server 2016 update,
#'   Nucleic Acids Research, Volume 44, Issue W1, 8 July 2016, Pages W90–W97,
#'   https://doi.org/10.1093/nar/gkw377
#'   \item Xie, Z., Bailey, A., Kuleshov, M. V., Clarke, D. J. B.,
#'   Evangelista, J. E., Jenkins, S. L., Lachmann, A., Wojciechowicz, M. L.,
#'   Kropiwnicki, E., Jagodnik, K. M., Jeon, M., & Ma’ayan, A. (2021). Gene
#'   set knowledge discovery with Enrichr. Current Protocols, 1, e90.
#'   doi: 10.1002/cpz1.90
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   \item \href{https://maayanlab.cloud/Enrichr/help#terms}{Citations note
#'   on Enrichr website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_enrichr_enrich(user_list_id = 11111)
#' }
#' \dontrun{
#' rba_enrichr_enrich(user_list_id = 11111,
#'     gene_set_library = "GO_Molecular_Function_2017",
#'     regex_library_name = FALSE)
#' }
#' \dontrun{
#' rba_enrichr_enrich(user_list_id = 11111,
#'     gene_set_library = "go",
#'     regex_library_name = TRUE)
#' }
#'
#' @family "Enrichr"
#' @seealso \code{\link{rba_enrichr}}
#' @export
rba_enrichr_enrich <- function(user_list_id,
                               gene_set_library = "all",
                               regex_library_name = FALSE,
                               organism = "human",
                               background_id = NULL,
                               progress_bar = TRUE,
                               ...){
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "user_list_id", class = c("numeric", "integer"), len = 1),
      list(arg = "gene_set_library", class = "character", min_len = 1),
      list(arg = "regex_library_name", class = "logical"),
      list(arg = "progress_bar", class = "logical"),
      list(
        arg = "organism", class = "character", no_null = TRUE,
        val = c("human", "fly", "yeast", "worm", "fish")
      ),
      list(arg = "background_id", class = "character", len = 1)
    ),
    cond = list(
      list(
        quote((!is.null(background_id)) && organism != "human"),
        "Providing background gene set is only available for `human`."
      ),
      list(
        quote(length(gene_set_library) > 1 && "all" %in% gene_set_library),
        "In `gene_set_library`, `all` cannot be combined with other library names."
      ),
      list(
        quote(isTRUE(regex_library_name) && length(gene_set_library) != 1),
        "You should supply a character string of length one for `gene_set_library` if `regex_library_name` is `TRUE`."
      )
    )
  )

  ## get a list of available libraries and check user inputs
  if (is.null(getOption("rba_enrichr_libs")[[organism]])) {
    .msg(
      "Calling rba_enrichr_libs() to get the names of available Enrichr %s libraries.",
      organism
    )

    enrichr_libs <- rba_enrichr_libs(
      organism = organism,
      store_in_options = TRUE,
      ...
    )

    if (utils::hasName(enrichr_libs, "libraryName")) {
      enrichr_libs <- enrichr_libs[["libraryName"]]
    }

  } else {
    enrichr_libs <- getOption("rba_enrichr_libs")[[organism]]
  }

  if (isTRUE(regex_library_name)) {
    valid_lib_check <- .rba_enrichr_validate(
      assertion = any(
        grepl(
          pattern = gene_set_library,
          x = enrichr_libs,
          perl = TRUE, ignore.case = TRUE
        )
      ),
      msg_type = "invalid_lib_regex",
      msg_detail = gene_set_library,
      skip_error = isTRUE(get("skip_error"))
    )
  } else {
    valid_lib_check <- .rba_enrichr_validate(
      assertion = all(gene_set_library %in% c("all", enrichr_libs)),
      msg_type = "invalid_lib_chr",
      msg_detail = gene_set_library[!gene_set_library %in% c("all", enrichr_libs)],
      skip_error = isTRUE(get("skip_error"))
    )
  }

  if (!isTRUE(valid_lib_check)) {
    return(valid_lib_check)
  }

  if (isTRUE(regex_library_name)) {
    gene_set_library <- grep(
      pattern = gene_set_library,
      x = enrichr_libs,
      value = TRUE, perl = TRUE, ignore.case = TRUE
    )
  } else if (identical(gene_set_library, "all")) {
    gene_set_library <- enrichr_libs
  }

  is_multi_libs <- length(gene_set_library) > 1

  ## call Enrichr API
  .msg(
    "Performing Enrichr analysis on gene-list %s %s Enrichr %s %s.",
    user_list_id,
    ifelse(is_multi_libs, yes = "using multiple", no = "against"),
    organism,
    ifelse(is_multi_libs, yes = "libraries", no = paste0("library: ", gene_set_library))
  )

  if (is_multi_libs) {
    .msg(
      paste0(
        "Note: You have selected '%s' Enrichr %s libraries. Note that for ",
        "each library, a separate call should be sent to Enrichr server. ",
        "Thus, this could take a while depending on the number of selected ",
        "libraries."
      ),
      length(gene_set_library),
      organism
    )
  }

  if (is_multi_libs && progress_bar) {
    pb <- utils::txtProgressBar(min = 0, max = length(gene_set_library), style = 3)
  }

  final_output <- lapply(
    seq_along(gene_set_library),
    function(i) {
      lib <- gene_set_library[i]
      out <- .rba_enrichr_enrich_internal(
        user_list_id = user_list_id,
        background_id = background_id,
        gene_set_library = lib,
        organism = organism,
        save_name = sprintf("enrichr_%s_%s.json", user_list_id, lib),
        sleep_time  = if (is_multi_libs) { 1 } else { 0 },
        ...
      )
      if (is_multi_libs && progress_bar) { utils::setTxtProgressBar(pb = pb, value = i) }
      return(out)
    }
  )

  names(final_output) <- gene_set_library
  if (is_multi_libs && progress_bar) { close(pb) }


  if (!is_multi_libs) { final_output <- final_output[[1]] }
  return(final_output)
}


#' Find Enrichr Terms That Contain a Given Gene
#'
#' This function will search the gene and retrieve a list of Enrichr
#'   Terms that contains that gene.
#'
#' @section Corresponding API Resources:
#'  "GET https://maayanlab.cloud/Enrichr/genemap"
#'
#' @param gene character: An Entrez gene symbol.
#' @param categorize logical: Should the category information be included?
#' @param organism (default = "human") Which model organism version of Enrichr
#'   to use? Available options are: "human", (H. sapiens & M. musculus),
#'   "fly" (D. melanogaster), "yeast" (S. cerevisiae), "worm" (C. elegans)
#'   and "fish" (D. rerio).
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#' arguments manual for more information on available options.
#'
#' @return A nested list containing the search results of your supplied gene.
#'
#' @references \itemize{
#'   \item Chen, E.Y., Tan, C.M., Kou, Y. et al. Enrichr: interactive and
#'   collaborative HTML5 gene list enrichment analysis tool. Bioinformatics
#'   14, 128 (2013). https://doi.org/10.1186/1471-2105-14-128
#'   \item Maxim V. Kuleshov, Matthew R. Jones, Andrew D. Rouillard, Nicolas
#'   F. Fernandez, Qiaonan Duan, Zichen Wang, Simon Koplev, Sherry L. Jenkins,
#'   Kathleen M. Jagodnik, Alexander Lachmann, Michael G. McDermott,
#'   Caroline D. Monteiro, Gregory W. Gundersen, Avi Ma’ayan, Enrichr: a
#'   comprehensive gene set enrichment analysis web server 2016 update,
#'   Nucleic Acids Research, Volume 44, Issue W1, 8 July 2016, Pages W90–W97,
#'   https://doi.org/10.1093/nar/gkw377
#'   \item Xie, Z., Bailey, A., Kuleshov, M. V., Clarke, D. J. B.,
#'   Evangelista, J. E., Jenkins, S. L., Lachmann, A., Wojciechowicz, M. L.,
#'   Kropiwnicki, E., Jagodnik, K. M., Jeon, M., & Ma’ayan, A. (2021). Gene
#'   set knowledge discovery with Enrichr. Current Protocols, 1, e90.
#'   doi: 10.1002/cpz1.90
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   \item \href{https://maayanlab.cloud/Enrichr/help#terms}{Citations note
#'   on Enrichr website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_enrichr_gene_map(gene = "p53")
#' }
#' \donttest{
#' rba_enrichr_gene_map(gene = "p53", categorize = TRUE)
#' }
#'
#' @family "Enrichr"
#' @export
rba_enrichr_gene_map <- function(gene,
                                 categorize = FALSE,
                                 organism = "human",
                                 ...){
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "gene", class = "character", len = 1),
      list(arg = "categorize", class = "logical"),
      list(
        arg = "organism", class = "character", no_null = TRUE,
        val = c("human", "fly", "yeast", "worm", "fish")
      )
    )
  )

  .msg(
    "Finding terms that contain %s gene: %s.",
    organism, gene
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("gene" = gene, "json" = "true"),
    list("setup", isTRUE(categorize), "true")
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("enrichr", "url"),
    path = paste0(.rba_stg("enrichr", "pth", organism), "genemap"),
    query = call_query,
    accept = "application/json",
    parser = "json->list_simp_flt_df",
    save_to = .rba_file("enrichr_gene_map.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}


#' A One-step Wrapper for Gene-list Enrichment Using Enrichr
#'
#' This function provides a convenient one-step wrapper for performing
#'   enrichment analysis on a given gene list using Enrichr. It simplifies the
#'   process by internally calling the necessary functions in the correct order.
#'   See the details section for more information.
#'
#' This function will call other rba_enrichr_*** functions with the following
#'   order:
#'   \enumerate{
#'   \item (If necessary) Call \code{\link{rba_enrichr_libs}} to obtain a list
#'     of available libraries in Enrichr for the given organism.
#'   \item Call \code{\link{rba_enrichr_add_list}} to upload your gene-list
#'     and obtain a 'user list ID'.
#'   \item (If necessary) Call \code{\link{rba_enrichr_add_background}} to
#'     upload your background gene-list and obtain a 'background list ID'.
#'   \item Call \code{\link{rba_enrichr_enrich}} to perform enrichment analysis
#'     on the gene-list against one or multiple Enrichr libraries
#'   }
#' @section Corresponding API Resources:
#'  "GET https://maayanlab.cloud/Enrichr/datasetStatistics"
#'  \cr "POST https://maayanlab.cloud/Enrichr/addList"
#'  \cr "POST https://maayanlab.cloud/speedrichr/api/addList"
#'  \cr "POST https://maayanlab.cloud/speedrichr/api/addbackground"
#'  \cr "GET https://maayanlab.cloud/Enrichr/enrich"
#'  \cr "POST https://maayanlab.cloud/speedrichr/api/backgroundenrich"
#'
#' @inheritParams rba_enrichr_add_list
#' @inheritParams rba_enrichr_add_background
#' @inheritParams rba_enrichr_enrich
#'
#' @return A list containing data frames of the enrichment results of your
#'   supplied gene-list against the selected Enrichr libraries.
#'
#' @references \itemize{
#'   \item Chen, E.Y., Tan, C.M., Kou, Y. et al. Enrichr: interactive and
#'   collaborative HTML5 gene list enrichment analysis tool. Bioinformatics
#'   14, 128 (2013). https://doi.org/10.1186/1471-2105-14-128
#'   \item Maxim V. Kuleshov, Matthew R. Jones, Andrew D. Rouillard, Nicolas
#'   F. Fernandez, Qiaonan Duan, Zichen Wang, Simon Koplev, Sherry L. Jenkins,
#'   Kathleen M. Jagodnik, Alexander Lachmann, Michael G. McDermott,
#'   Caroline D. Monteiro, Gregory W. Gundersen, Avi Ma’ayan, Enrichr: a
#'   comprehensive gene set enrichment analysis web server 2016 update,
#'   Nucleic Acids Research, Volume 44, Issue W1, 8 July 2016, Pages W90–W97,
#'   https://doi.org/10.1093/nar/gkw377
#'   \item Xie, Z., Bailey, A., Kuleshov, M. V., Clarke, D. J. B.,
#'   Evangelista, J. E., Jenkins, S. L., Lachmann, A., Wojciechowicz, M. L.,
#'   Kropiwnicki, E., Jagodnik, K. M., Jeon, M., & Ma’ayan, A. (2021). Gene
#'   set knowledge discovery with Enrichr. Current Protocols, 1, e90.
#'   doi: 10.1002/cpz1.90
#'   \item \href{https://maayanlab.cloud/Enrichr/help#api}{Enrichr API
#'   Documentation}
#'   \item \href{https://maayanlab.cloud/Enrichr/help#terms}{Citations note
#'   on Enrichr website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_enrichr(gene_list = c("TP53", "TNF", "EGFR"))
#' }
#' \donttest{
#' rba_enrichr(gene_list = c("TP53", "TNF", "EGFR"),
#'     gene_set_library = "GO_Molecular_Function_2025",
#'     regex_library_name = FALSE)
#' }
#' \donttest{
#' rba_enrichr(gene_list = c("TP53", "TNF", "EGFR"),
#'     gene_set_library = "go",
#'     regex_library_name = TRUE)
#' }
#'
#' @family "Enrichr"
#' @family "Enrichment/Over-representation"
#' @export
rba_enrichr <- function(gene_list,
                        description = NULL,
                        gene_set_library = "all",
                        regex_library_name = FALSE,
                        organism = "human",
                        background_genes = NULL,
                        progress_bar = TRUE,
                        ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "gene_list", class = "character", min_len = 1),
      list(arg = "description", class = "character"),
      list(arg = "gene_set_library", class = "character", min_len = 1),
      list(arg = "regex_library_name", class = "logical"),
      list(
        arg = "organism", class = "character", no_null = TRUE,
        val = c("human", "fly", "yeast", "worm", "fish")
      ),
      list(arg = "background_genes", class = "character", min_len = 1),
      list(arg = "progress_bar", class = "logical")
    ),
    cond = list(
      list(
        quote((!is.null(background_genes)) && organism != "human"),
        "Providing background gene set is only available for `human`."
      ),
      list(
        quote(length(gene_set_library) > 1 && "all" %in% gene_set_library),
        "In `gene_set_library`, `all` cannot be combined with other library names."
      ),
      list(
        quote(isTRUE(regex_library_name) && length(gene_set_library) != 1),
        "You should supply a character string of length one for `gene_set_library` if `regex_library_name` is `TRUE`."
      ),
      list(
        quote(!is.null(background_genes) && (!all(gene_list %in% background_genes))),
        "Some of the `gene_list` elements are not present in `background_genes`."
      )
    )
  )

  # Step 1, Get available Enrichr libraries
  .msg("--Step 1/3:")
  enrichr_libs <- rba_enrichr_libs(
    organism = organism,
    store_in_options = TRUE,
    ...)

  get_libs_check <- .rba_enrichr_validate(
    assertion = utils::hasName(enrichr_libs, "libraryName") &&
      is.character(enrichr_libs[["libraryName"]]) &&
      length(enrichr_libs[["libraryName"]]) > 0,
    msg_type = "no_lib",
    msg_detail = try(enrichr_libs),
    skip_error = isTRUE(get("skip_error"))
  )


  if (!isTRUE(get_libs_check)) {
    return(get_libs_check)
  } else {
    enrichr_libs <- enrichr_libs[["libraryName"]]
  }

  if (isTRUE(regex_library_name)) {
    valid_lib_check <- .rba_enrichr_validate(
      assertion = any(
        grepl(
          pattern = gene_set_library,
          x = enrichr_libs,
          perl = TRUE, ignore.case = TRUE
        )
      ),
      msg_type = "invalid_lib_regex",
      msg_detail = gene_set_library,
      skip_error = isTRUE(get("skip_error"))
    )
  } else {
    valid_lib_check <- .rba_enrichr_validate(
      assertion = all(gene_set_library %in% c("all", enrichr_libs)),
      msg_type = "invalid_lib_chr",
      msg_detail = gene_set_library[!gene_set_library %in% c("all", enrichr_libs)],
      skip_error = isTRUE(get("skip_error"))
    )
  }

  if (!isTRUE(valid_lib_check)) {
    return(valid_lib_check)
  }

  # Step 2.1 Submit gene list to Enrich
  .msg("--Step 2/3:")
  Sys.sleep(2)

  list_id <- rba_enrichr_add_list(
    gene_list = gene_list,
    description = description,
    organism = organism,
    speedrichr = !is.null(background_genes),
    ...
  )

  gene_upload_check <- .rba_enrichr_validate(
    assertion = exists("list_id") && utils::hasName(list_id, "userListId"),
    msg_type = "no_gene_upload",
    msg_detail = try(list_id),
    skip_error = isTRUE(get("skip_error"))
  )


  if (!isTRUE(gene_upload_check)) {
    return(gene_upload_check)
  }

  # Step 2.2 Submit background gene list if requested
  if (!is.null(background_genes)) {
    Sys.sleep(2)
    background_id <- rba_enrichr_add_background(
      background_genes = background_genes,
      ...
    )

    background_upload_check <- .rba_enrichr_validate(
      assertion = utils::hasName(background_id, "backgroundid"),
      msg_type = "no_background_upload",
      msg_detail = try(background_id),
      skip_error = isTRUE(get("skip_error"))
    )

    if (!isTRUE(background_upload_check)) {
      return(background_upload_check)
    } else {
      background_id <- background_id$backgroundid
    }

  } else {
    background_id <- NULL
  }

  # Step 3 Send enrich requests to Enrichr
  .msg("--Step 3/3:")
  Sys.sleep(2)
  enriched <- rba_enrichr_enrich(
    user_list_id = list_id$userListId,
    gene_set_library = gene_set_library,
    regex_library_name = regex_library_name,
    background_id = background_id,
    organism = organism,
    progress_bar = progress_bar,
    ...
  )

  results_check <- .rba_enrichr_validate(
    assertion = is.list(enriched) || is.data.frame(enriched),
    msg_type = "no_result",
    msg_detail = try(enriched),
    skip_error = isTRUE(get("skip_error"))
  )

  if (!isTRUE(results_check)) {
    return(results_check)
  }

  # Return
  return(enriched)
}
