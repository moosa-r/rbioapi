#### Proteomes Endpoints ####
#' Search proteomes in UniProt
#'
#' @param accession
#' @param reviewed
#' @param isoform
#' @param go_term
#' @param keyword
#' @param ec
#' @param gene
#' @param exact_gene
#' @param protein
#' @param organism
#' @param taxid
#' @param pubmed
#' @param seq_length
#' @param md5
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_proteomes_search = function(upid = NA,
                                        name = NA,
                                        taxid = NA,
                                        keyword = NA,
                                        xref = NA,
                                        genome_acc = NA,
                                        is_ref_proteome = NA,
                                        is_redundant = NA,
                                        verbose = TRUE,
                                        progress_bar = FALSE,
                                        diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = upid,
                                         name = "upid",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = name,
                                         name = "name",
                                         class = "character"),
                                    list(arg = taxid,
                                         name = "taxid",
                                         class = "numeric",
                                         max_len = 20),
                                    list(arg = keyword,
                                         name = "keyword",
                                         class = "character"),
                                    list(arg = xref,
                                         name = "xref",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = genome_acc,
                                         name = "genome_acc",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = is_ref_proteome,
                                         name = "is_ref_proteome",
                                         class = "logical"),
                                    list(arg = is_redundant,
                                         name = "is_redundant",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /proteomes Search proteomes in UniProt")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(upid)),
                              list("upid" = paste0(upid,
                                                   collapse = ","))),
                         list(!is.na(name),
                              list("name" = name)),
                         list(any(!is.na(taxid)),
                              list("taxid" = paste0(taxid,
                                                    collapse = ","))),
                         list(!is.na(keyword),
                              list("keyword" = keyword)),
                         list(any(!is.na(xref)),
                              list("xref" = paste0(xref,
                                                   collapse = ","))),
                         list(any(!is.na(genome_acc)),
                              list("genome_acc" = paste0(genome_acc,
                                                         collapse = ","))),
                         list(!is.na(is_ref_proteome),
                              list("is_ref_proteome" = ifelse(is_ref_proteome,
                                                              "true",
                                                              "false")))
  )

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "proteomes"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get proteome by proteome/proteins UPID
#'
#' @param upid
#' @param get_proteins
#' @param reviewed
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_proteomes = function(upid,
                                 get_proteins = FALSE,
                                 reviewed = NA,
                                 verbose = TRUE,
                                 progress_bar = FALSE,
                                 diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = upid,
                                         name = "upid",
                                         class = "character"),
                                    list(arg = get_proteins,
                                         name = "get_proteins",
                                         class = "logical"),
                                    list(arg = reviewed,
                                         name = "reviewed",
                                         class = "logical")),
                        cond = list(list(get_proteins == FALSE && !is.na(reviewed),
                                         "'reviewed' argument is ignored because you provided 'get_proteins' as FALSE")),
                        cond_warning = TRUE,
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /proteomes/proteins/{upid} Get proteins by proteome UPID",
            "get /proteomes/{upid} Get proteome by proteome UPID")
  }
  ## make function-specific calls
  if (get_proteins == TRUE) {
    ## build GET API request's query

    additional_pars = list(list(!is.na(reviewed),
                                list("reviewed" = ifelse(reviewed,
                                                         "true",
                                                         "false")))
    )

    call_query = rba_ba_body_add_pars(call_body = list(),
                                      additional_pars = additional_pars)
    path_input = paste0(getOption("rba_pth_uniprot"),
                        "proteomes/proteins/",
                        upid)
  } else {
    call_query = NULL
    path_input = paste0(getOption("rba_pth_uniprot"),
                        "proteomes/",
                        upid)
  }

  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = path_input,
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->list",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#### Genecentric Endpoints ####

#' Search gene centric proteins
#'
#' @param upid
#' @param accession
#' @param gene
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_genecentric_search = function(upid = NA,
                                          accession = NA,
                                          gene = NA,
                                          verbose = TRUE,
                                          progress_bar = FALSE,
                                          diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = upid,
                                         name = "upid",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = accession,
                                         name = "accession",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = gene,
                                         name = "gene",
                                         class = "character",
                                         max_len = 20)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /genecentric Search gene centric proteins")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(upid)),
                              list("upid" = paste0(upid,
                                                   collapse = ","))),
                         list(any(!is.na(accession)),
                              list("accession" = paste0(accession,
                                                        collapse = ","))),
                         list(any(!is.na(gene)),
                              list("gene" = paste0(gene,
                                                   collapse = ",")))
  )

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "genecentric"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get gene centric proteins by Uniprot accession
#'
#' @param accession
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_genecentric = function(accession,
                                   verbose = TRUE,
                                   progress_bar = FALSE,
                                   diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /genecentric/{accession} Get gene centric proteins by Uniprot accession")
  }
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "genecentric/",
                                                  accession),
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = "json->list",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}
