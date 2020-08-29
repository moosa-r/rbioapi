#### Internal functions ####
#' Internal function to alternate uniprot calls between saving files/R objects
#'
#' @param call_func_input
#' @param save_switch
#' @param file_accept
#' @param file_parser
#' @param file_ext
#' @param file_name
#' @param obj_accept
#' @param obj_parser
#' @param verbose
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_ba_uniprot_call = function(call_func_input,
                               save_switch,
                               file_accept,
                               file_parser,
                               file_ext,
                               file_name,
                               obj_accept,
                               obj_parser,
                               verbose = TRUE,
                               diagnostics = FALSE) {
  if (save_switch == TRUE) {
    accept_input = file_accept
    parser_input = file_parser
    # create file_path
    save_to = rba_ba_file_path(file_ext = file_ext,
                               file_name = file_name,
                               dir_name = "rba_uniprot",
                               save_to = save_to,
                               verbose = verbose,
                               diagnostics = diagnostics)
    call_func_input = as.call(append(as.list(call_func_input),
                                     quote(httr::write_disk(save_to,
                                                            overwrite = TRUE))))
  } else {
    accept_input = obj_accept
    parser_input = obj_parser
  }
  call_func_input = as.call(append(as.list(call_func_input),
                                   quote(httr::accept(accept_input))))
}
#### Proteins Endpoints ####
#' Search UniProt entries
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
rba_uniprot_proteins_search = function(accession = NA,
                                       reviewed = NA,
                                       isoform = NA,
                                       go_term = NA,
                                       keyword = NA,
                                       ec = NA,
                                       gene = NA,
                                       exact_gene = NA,
                                       protein = NA,
                                       organism = NA,
                                       taxid = NA,
                                       pubmed = NA,
                                       seq_length = NA,
                                       md5 = NA,
                                       verbose = TRUE,
                                       progress_bar = FALSE,
                                       diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = reviewed,
                                         name = "reviewed",
                                         class = "logical"),
                                    list(arg = isoform,
                                         name = "isoform",
                                         class = "numeric",
                                         val = 0:2),
                                    list(arg = go_term,
                                         name = "go_terms",
                                         class = "character"),
                                    list(arg = keyword,
                                         name = "keyword",
                                         class = "character"),
                                    list(arg = ec,
                                         name = "ec",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = gene,
                                         name = "gene",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = exact_gene,
                                         name = "exact_gene",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = protein,
                                         name = "protein",
                                         class = "character"),
                                    list(arg = organism,
                                         name = "organism",
                                         class = "character"),
                                    list(arg = taxid,
                                         name = "taxid",
                                         class = "numeric",
                                         max_len = 20),
                                    list(arg = pubmed,
                                         name = "pubmed",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = seq_length,
                                         name = "seq_length",
                                         class = "character"),
                                    list(arg = md5,
                                         name = "md5",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /proteins")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(accession)),
                              list("accession" = paste0(accession,
                                                        collapse = ","))),
                         list(!is.na(reviewed),
                              list("reviewed" = ifelse(reviewed,
                                                       "true",
                                                       "false"))),
                         list(!is.na(isoform),
                              list("isoform" = isoform)),
                         list(!is.na(go_term),
                              list("goterms" = go_term)),
                         list(!is.na(keyword),
                              list("keyword" = keyword)),
                         list(any(!is.na(ec)),
                              list("ec" = paste0(ec,
                                                 collapse = ","))),
                         list(any(!is.na(gene)),
                              list("gene" = paste0(gene,
                                                   collapse = ","))),
                         list(any(!is.na(exact_gene)),
                              list("exact_gene" = paste0(exact_gene,
                                                         collapse = ","))),
                         list(!is.na(protein),
                              list("protein" = protein)),
                         list(!is.na(organism),
                              list("organism" = organism)),
                         list(any(!is.na(taxid)),
                              list("taxid" = paste0(taxid,
                                                    collapse = ","))),
                         list(any(!is.na(pubmed)),
                              list("pubmed" = paste0(pubmed,
                                                     collapse = ","))),
                         list(!is.na(seq_length),
                              list("seq_length" = seq_length)),
                         list(!is.na(md5),
                              list("md5" = md5))
  )

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "proteins"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get UniProt entry by accession
#'
#' @param accession
#' @param interaction
#' @param isoforms
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_proteins = function(accession = NA,
                                interaction = FALSE,
                                isoforms = FALSE,
                                verbose = TRUE,
                                progress_bar = FALSE,
                                diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /proteins/{accession} Get UniProt entry by accession",
            "get /proteins/{accession}/isoforms Get UniProt isoform entries from parent entry accession",
            "get /proteins/interaction/{accession} Get UniProt interactions by accession")
  }

  ## make function-specific calls
  path_input = paste0(getOption("rba_pth_uniprot"),
                      "proteins/",
                      accession)
  if (interaction == TRUE) {
    path_input = sub(pattern = "/proteins/",
                     replacement = "/proteins/interaction/",
                     x = path_input)
  } else if (isoforms == TRUE) {
    path_input = paste0(path_input, "/isoforms")
  }
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = path_input,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get UniProt entries by UniProt cross reference and its ID
#'
#' @param db_type
#' @param db_id
#' @param reviewed
#' @param isoform
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_proteins_crossref = function(db_type,
                                         db_id,
                                         reviewed = NA,
                                         isoform = NA,
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = db_type,
                                         name = "db_type",
                                         class = "character"),
                                    list(arg = db_id,
                                         name = "db_id",
                                         class = "character"),
                                    list(arg = reviewed,
                                         name = "reviewed",
                                         class = "logical"),
                                    list(arg = isoform,
                                         name = "isoform",
                                         class = "numeric",
                                         val = c(0,1))),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /proteins/{dbtype}:{dbid} Get UniProt entries by UniProt cross reference and its ID")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(!is.na(reviewed),
                              list("reviewed" = ifelse(reviewed,
                                                       "true",
                                                       "false"))),
                         list(!is.na(isoform),
                              list("isoform" = isoform))
  )

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "proteins/",
                                                  db_type, ":",
                                                  db_id),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#### Features Endpoints ####

#' Search protein sequence features in UniProt
#'
#' @param accession
#' @param gene
#' @param exact_gene
#' @param protein
#' @param reviewed
#' @param organism
#' @param taxid
#' @param categories
#' @param types
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_features_search = function(accession = NA,
                                       gene = NA,
                                       exact_gene = NA,
                                       protein = NA,
                                       reviewed = NA,
                                       organism = NA,
                                       taxid = NA,
                                       categories = NA,
                                       types = NA,
                                       verbose = TRUE,
                                       progress_bar = FALSE,
                                       diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = gene,
                                         name = "gene",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = exact_gene,
                                         name = "exact_gene",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = protein,
                                         name = "protein",
                                         class = "character"),
                                    list(arg = reviewed,
                                         name = "reviewed",
                                         class = "logical"),
                                    list(arg = organism,
                                         name = "organism",
                                         class = "character"),
                                    list(arg = taxid,
                                         name = "taxid",
                                         class = "numeric",
                                         max_len = 20),
                                    list(arg = categories,
                                         name = "categories",
                                         class = "character",
                                         val = c("MOLECULE_PROCESSING",
                                                 "TOPOLOGY",
                                                 "SEQUENCE_INFORMATION",
                                                 "STRUCTURAL",
                                                 "DOMAINS_AND_SITES",
                                                 "PTM",
                                                 "VARIANTS",
                                                 "MUTAGENESIS."),
                                         max_len = 8),
                                    list(arg = types,
                                         name = "types",
                                         class = "character",
                                         max_len = 20,
                                         val = c("INIT_MET",
                                                 "SIGNAL",
                                                 "PROPEP",
                                                 "TRANSIT",
                                                 "CHAIN",
                                                 "PEPTIDE",
                                                 "TOPO_DOM",
                                                 "TRANSMEM",
                                                 "DOMAIN",
                                                 "REPEAT",
                                                 "CA_BIND",
                                                 "ZN_FING",
                                                 "DNA_BIND",
                                                 "NP_BIND",
                                                 "REGION",
                                                 "COILED",
                                                 "MOTIF",
                                                 "COMPBIAS",
                                                 "ACT_SITE",
                                                 "METAL",
                                                 "BINDING",
                                                 "SITE",
                                                 "NON_STD",
                                                 "MOD_RES",
                                                 "LIPID",
                                                 "CARBOHYD",
                                                 "DISULFID",
                                                 "CROSSLNK",
                                                 "VAR_SEQ",
                                                 "VARIANT",
                                                 "MUTAGEN",
                                                 "UNSURE",
                                                 "CONFLICT",
                                                 "NON_CONS",
                                                 "NON_TER",
                                                 "HELIX",
                                                 "TURN",
                                                 "STRAND",
                                                 "INTRAMEM")
                                    )),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /features Search protein sequence features in UniProt")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(accession)),
                              list("accession" = paste0(accession,
                                                        collapse = ","))),
                         list(any(!is.na(gene)),
                              list("gene" = paste0(gene,
                                                   collapse = ","))),
                         list(any(!is.na(exact_gene)),
                              list("exact_gene" = paste0(exact_gene,
                                                         collapse = ","))),
                         list(!is.na(protein),
                              list("protein" = protein)),
                         list(!is.na(reviewed),
                              list("reviewed" = ifelse(reviewed,
                                                       "true",
                                                       "false"))),
                         list(!is.na(organism),
                              list("organism" = organism)),
                         list(any(!is.na(taxid)),
                              list("taxid" = paste0(taxid,
                                                    collapse = ","))),
                         list(any(!is.na(categories)),
                              list("categories" = paste0(categories,
                                                         collapse = ","))),
                         list(any(!is.na(types)),
                              list("types" = paste0(types,
                                                    collapse = ",")))
  )

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "features"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Search protein sequence features of a given type in UniProt
#' Search for term(s) that appear in feature description for your specified
#' feature type. For example, you can search by type=DOMAIN and
#' Term=Kinase. Comma separated values
#'
#' @param terms
#' @param type
#' @param categories
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_features_type = function(terms,
                                     type,
                                     categories = NA,
                                     verbose = TRUE,
                                     progress_bar = FALSE,
                                     diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = terms,
                                         name = "terms",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = type,
                                         name = "type",
                                         class = "character",
                                         len = 1,
                                         val = c("INIT_MET",
                                                 "SIGNAL",
                                                 "PROPEP",
                                                 "TRANSIT",
                                                 "CHAIN",
                                                 "PEPTIDE",
                                                 "TOPO_DOM",
                                                 "TRANSMEM",
                                                 "DOMAIN",
                                                 "REPEAT",
                                                 "CA_BIND",
                                                 "ZN_FING",
                                                 "DNA_BIND",
                                                 "NP_BIND",
                                                 "REGION",
                                                 "COILED",
                                                 "MOTIF",
                                                 "COMPBIAS",
                                                 "ACT_SITE",
                                                 "METAL",
                                                 "BINDING",
                                                 "SITE",
                                                 "NON_STD",
                                                 "MOD_RES",
                                                 "LIPID",
                                                 "CARBOHYD",
                                                 "DISULFID",
                                                 "CROSSLNK",
                                                 "VAR_SEQ",
                                                 "VARIANT",
                                                 "MUTAGEN",
                                                 "UNSURE",
                                                 "CONFLICT",
                                                 "NON_CONS",
                                                 "NON_TER",
                                                 "HELIX",
                                                 "TURN",
                                                 "STRAND",
                                                 "INTRAMEM")),
                                    list(arg = categories,
                                         name = "categories",
                                         class = "character",
                                         val = c("MOLECULE_PROCESSING",
                                                 "TOPOLOGY",
                                                 "SEQUENCE_INFORMATION",
                                                 "STRUCTURAL",
                                                 "DOMAINS_AND_SITES",
                                                 "PTM",
                                                 "VARIANTS",
                                                 "MUTAGENESIS."),
                                         max_len = 8)
  ),
  diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /features/type/{type} Search protein sequence features of a given type in UniProt")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(categories)),
                              list("categories" = paste0(categories,
                                                         collapse = ","))),
                         list(any(!is.na(terms)),
                              list("terms" = paste0(terms,
                                                    collapse = ",")))
  )

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "features/type/",
                                                  type),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get UniProt protein sequence features by accession
#'
#' @param accession
#' @param type
#' @param categories
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_features = function(accession,
                                types = NA,
                                categories = NA,
                                verbose = TRUE,
                                progress_bar = FALSE,
                                diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character"),
                                    list(arg = types,
                                         name = "types",
                                         class = "character",
                                         len = 1,
                                         val = c("INIT_MET",
                                                 "SIGNAL",
                                                 "PROPEP",
                                                 "TRANSIT",
                                                 "CHAIN",
                                                 "PEPTIDE",
                                                 "TOPO_DOM",
                                                 "TRANSMEM",
                                                 "DOMAIN",
                                                 "REPEAT",
                                                 "CA_BIND",
                                                 "ZN_FING",
                                                 "DNA_BIND",
                                                 "NP_BIND",
                                                 "REGION",
                                                 "COILED",
                                                 "MOTIF",
                                                 "COMPBIAS",
                                                 "ACT_SITE",
                                                 "METAL",
                                                 "BINDING",
                                                 "SITE",
                                                 "NON_STD",
                                                 "MOD_RES",
                                                 "LIPID",
                                                 "CARBOHYD",
                                                 "DISULFID",
                                                 "CROSSLNK",
                                                 "VAR_SEQ",
                                                 "VARIANT",
                                                 "MUTAGEN",
                                                 "UNSURE",
                                                 "CONFLICT",
                                                 "NON_CONS",
                                                 "NON_TER",
                                                 "HELIX",
                                                 "TURN",
                                                 "STRAND",
                                                 "INTRAMEM")),
                                    list(arg = categories,
                                         name = "categories",
                                         class = "character",
                                         val = c("MOLECULE_PROCESSING",
                                                 "TOPOLOGY",
                                                 "SEQUENCE_INFORMATION",
                                                 "STRUCTURAL",
                                                 "DOMAINS_AND_SITES",
                                                 "PTM",
                                                 "VARIANTS",
                                                 "MUTAGENESIS."),
                                         max_len = 8)
  ),
  diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /features/{accession} Get UniProt protein sequence features by accession")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(categories)),
                              list("categories" = paste0(categories,
                                                         collapse = ","))),
                         list(any(!is.na(types)),
                              list("types" = paste0(types,
                                                    collapse = ",")))
  )

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "features/",
                                                  accession),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#### Variation Endpoints ####

#' Search natural variants in UniProt
#' Implementation Notes Among the available response content types, PEFF
#' format (PSI Extended FASTA Format from the Human Proteome Organisation -
#' Proteomics Standards Initiative, HUPO-PSI) is provided with only variants
#' reported in the output.
#'
#' @param accession
#' @param source_type
#' @param consequence_type
#' @param wild_type
#' @param alternative_sequence
#' @param location
#' @param disease
#' @param omim
#' @param evidence
#' @param taxid
#' @param db_type
#' @param db_id
#' @param save_peff
#' @param save_to
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_variation_search = function(accession = NA,
                                        source_type = NA,
                                        consequence_type = NA,
                                        wild_type = NA,
                                        alternative_sequence = NA,
                                        location = NA,
                                        disease = NA,
                                        omim = NA,
                                        evidence = NA,
                                        taxid = NA,
                                        db_type = NA,
                                        db_id = NA,
                                        save_peff = FALSE,
                                        save_to = NA,
                                        verbose = TRUE,
                                        progress_bar = FALSE,
                                        diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = source_type,
                                         name = "source_type",
                                         class = "character",
                                         val = c("uniprot",
                                                 "large scale study",
                                                 "mixed"),
                                         max_len = 2),
                                    list(arg = consequence_type,
                                         name = "consequence_type",
                                         class = "character",
                                         val = c("missense",
                                                 "stop gained",
                                                 "stop lost")),
                                    list(arg = wild_type,
                                         name = "wild_type",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = alternative_sequence,
                                         name = "alternative_sequence",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = location,
                                         name = "location",
                                         class = "character"),
                                    list(arg = disease,
                                         name = "disease",
                                         class = "character"),
                                    list(arg = omim,
                                         name = "omim",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = evidence,
                                         name = "evidence",
                                         class = "numeric",
                                         max_len = 20),
                                    list(arg = taxid,
                                         name = "taxid",
                                         class = "numeric",
                                         max_len = 20),
                                    list(arg = db_type,
                                         name = "db_type",
                                         class = "character",
                                         max_len = 2),
                                    list(arg = db_id,
                                         name = "db_id",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = save_peff,
                                         name = "save_peff",
                                         class = "logical"),
                                    list(arg = save_to,
                                         name = "save_to",
                                         class = "character")),
                        cond = list(list(!is.na(save_to) & save_peff == FALSE,
                                         "'save_to' is ignored because you didn't set 'save_peff' to TRUE.")),
                        cond_warning = TRUE,
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /variation Search natural variants in UniProt")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(accession)),
                              list("accession" = paste0(accession,
                                                        collapse = ","))),
                         list(any(!is.na(source_type)),
                              list("sourcetype" = paste0(source_type,
                                                         collapse = ","))),
                         list(any(!is.na(consequence_type)),
                              list("consequencetype" = paste0(consequence_type,
                                                              collapse = ","))),
                         list(any(!is.na(wild_type)),
                              list("wildtype" = paste0(wild_type,
                                                       collapse = ","))),
                         list(any(!is.na(alternative_sequence)),
                              list("alternativesequence" = paste0(alternative_sequence,
                                                                  collapse = ","))),
                         list(!is.na(location),
                              list("location" = location)),
                         list(!is.na(disease),
                              list("disease" = disease)),
                         list(any(!is.na(omim)),
                              list("omim" = paste0(omim,
                                                   collapse = ","))),
                         list(any(!is.na(evidence)),
                              list("evidence" = paste0(evidence,
                                                       collapse = ","))),
                         list(any(!is.na(taxid)),
                              list("taxid" = paste0(taxid,
                                                    collapse = ","))),
                         list(any(!is.na(db_type)),
                              list("dbtype" = paste0(db_type,
                                                     collapse = ","))),
                         list(any(!is.na(db_id)),
                              list("dbid" = paste0(db_type,
                                                   collapse = ","))))

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "variation"),
                                    query = call_query))
  if (save_peff == TRUE) {
    accept_input = "text/x-peff"
    parser_input = "text->chr"
    # create file_path
    save_to = rba_ba_file_path(file_ext = "peff",
                               file_name = format(Sys.time(),
                                                  "%Y_%m_%d_%H_%M_%S"),
                               dir_name = "rba_uniprot",
                               save_to = save_to,
                               verbose = verbose,
                               diagnostics = diagnostics)
    call_func_input = as.call(append(as.list(call_func_input),
                                     quote(httr::write_disk(save_to,
                                                            overwrite = TRUE))))
  } else {
    accept_input = "application/json"
    parser_input = "json->list"
  }
  call_func_input = as.call(append(as.list(call_func_input),
                                   quote(httr::accept(accept_input))))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = parser_input,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get natural variants in UniProt by NIH-NCBI SNP database identifier
#'
#' @param db_id
#' @param source_type
#' @param consequence_type
#' @param wild_type
#' @param alternative_sequence
#' @param location
#' @param save_peff
#' @param save_to
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_variation_dbsnp = function(db_id,
                                       source_type = NA,
                                       consequence_type = NA,
                                       wild_type = NA,
                                       alternative_sequence = NA,
                                       location = NA,
                                       save_peff = FALSE,
                                       save_to = NA,
                                       verbose = TRUE,
                                       progress_bar = FALSE,
                                       diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = db_id,
                                         name = "db_id",
                                         class = "character"),
                                    list(arg = source_type,
                                         name = "source_type",
                                         class = "character",
                                         val = c("uniprot",
                                                 "large scale study",
                                                 "mixed"),
                                         max_len = 2),
                                    list(arg = consequence_type,
                                         name = "consequence_type",
                                         class = "character",
                                         val = c("missense",
                                                 "stop gained",
                                                 "stop lost")),
                                    list(arg = wild_type,
                                         name = "wild_type",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = alternative_sequence,
                                         name = "alternative_sequence",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = location,
                                         name = "location",
                                         class = "character"),
                                    list(arg = save_peff,
                                         name = "save_peff",
                                         class = "logical"),
                                    list(arg = save_to,
                                         name = "save_to",
                                         class = "character")),
                        cond = list(list(!is.na(save_to) & save_peff == FALSE,
                                         "'save_to' is ignored because you didn't set 'save_peff' to TRUE.")),
                        cond_warning = TRUE,
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /variation/dbsnp/{dbid} Get natural variants in UniProt by NIH-NCBI SNP database identifier")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(source_type)),
                              list("sourcetype" = paste0(source_type,
                                                         collapse = ","))),
                         list(any(!is.na(consequence_type)),
                              list("consequencetype" = paste0(consequence_type,
                                                              collapse = ","))),
                         list(any(!is.na(wild_type)),
                              list("wildtype" = paste0(wild_type,
                                                       collapse = ","))),
                         list(any(!is.na(alternative_sequence)),
                              list("alternativesequence" = paste0(alternative_sequence,
                                                                  collapse = ","))),
                         list(!is.na(location),
                              list("location" = location)))

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "variation/dbsnp/",
                                                  db_id),
                                    query = call_query))
  if (save_peff == TRUE) {
    accept_input = "text/x-peff"
    parser_input = "text->chr"
    # create file_path
    save_to = rba_ba_file_path(file_ext = "peff",
                               file_name = format(Sys.time(),
                                                  "%Y_%m_%d_%H_%M_%S"),
                               dir_name = "rba_uniprot",
                               save_to = save_to,
                               verbose = verbose,
                               diagnostics = diagnostics)
    call_func_input = as.call(append(as.list(call_func_input),
                                     quote(httr::write_disk(save_to,
                                                            overwrite = TRUE))))
  } else {
    accept_input = "application/json"
    parser_input = "json->list"
  }
  call_func_input = as.call(append(as.list(call_func_input),
                                   quote(httr::accept(accept_input))))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = parser_input,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get natural variants in UniProt by HGVS expression
#'
#' @param hgvs
#' @param source_type
#' @param consequence_type
#' @param wild_type
#' @param alternative_sequence
#' @param location
#' @param save_peff
#' @param save_to
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_variation_hgvs = function(hgvs,
                                      source_type = NA,
                                      consequence_type = NA,
                                      wild_type = NA,
                                      alternative_sequence = NA,
                                      location = NA,
                                      save_peff = FALSE,
                                      save_to = NA,
                                      verbose = TRUE,
                                      progress_bar = FALSE,
                                      diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = hgvs,
                                         name = "hgvs",
                                         class = "character"),
                                    list(arg = source_type,
                                         name = "source_type",
                                         class = "character",
                                         val = c("uniprot",
                                                 "large scale study",
                                                 "mixed"),
                                         max_len = 2),
                                    list(arg = consequence_type,
                                         name = "consequence_type",
                                         class = "character",
                                         val = c("missense",
                                                 "stop gained",
                                                 "stop lost")),
                                    list(arg = wild_type,
                                         name = "wild_type",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = alternative_sequence,
                                         name = "alternative_sequence",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = location,
                                         name = "location",
                                         class = "character"),
                                    list(arg = save_peff,
                                         name = "save_peff",
                                         class = "logical"),
                                    list(arg = save_to,
                                         name = "save_to",
                                         class = "character")),
                        cond = list(list(!is.na(save_to) & save_peff == FALSE,
                                         "'save_to' is ignored because you didn't set 'save_peff' to TRUE.")),
                        cond_warning = TRUE,
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /variation/hgvs/{hgvs} Get natural variants in UniProt by HGVS expression")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(source_type)),
                              list("sourcetype" = paste0(source_type,
                                                         collapse = ","))),
                         list(any(!is.na(consequence_type)),
                              list("consequencetype" = paste0(consequence_type,
                                                              collapse = ","))),
                         list(any(!is.na(wild_type)),
                              list("wildtype" = paste0(wild_type,
                                                       collapse = ","))),
                         list(any(!is.na(alternative_sequence)),
                              list("alternativesequence" = paste0(alternative_sequence,
                                                                  collapse = ","))),
                         list(!is.na(location),
                              list("location" = location)))

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "variation/hgvs/",
                                                  hgvs),
                                    query = call_query))
  if (save_peff == TRUE) {
    accept_input = "text/x-peff"
    parser_input = "text->chr"
    # create file_path
    save_to = rba_ba_file_path(file_ext = "peff",
                               file_name = format(Sys.time(),
                                                  "%Y_%m_%d_%H_%M_%S"),
                               dir_name = "rba_uniprot",
                               save_to = save_to,
                               verbose = verbose,
                               diagnostics = diagnostics)
    call_func_input = as.call(append(as.list(call_func_input),
                                     quote(httr::write_disk(save_to,
                                                            overwrite = TRUE))))
  } else {
    accept_input = "application/json"
    parser_input = "json->list"
  }
  call_func_input = as.call(append(as.list(call_func_input),
                                   quote(httr::accept(accept_input))))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = parser_input,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get natural variants by UniProt accession
#'
#' @param hgvs
#' @param source_type
#' @param consequence_type
#' @param wild_type
#' @param alternative_sequence
#' @param location
#' @param save_peff
#' @param save_to
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_variation_accession = function(accession,
                                           source_type = NA,
                                           consequence_type = NA,
                                           wild_type = NA,
                                           alternative_sequence = NA,
                                           location = NA,
                                           save_peff = FALSE,
                                           save_to = NA,
                                           verbose = TRUE,
                                           progress_bar = FALSE,
                                           diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character"),
                                    list(arg = source_type,
                                         name = "source_type",
                                         class = "character",
                                         val = c("uniprot",
                                                 "large scale study",
                                                 "mixed"),
                                         max_len = 2),
                                    list(arg = consequence_type,
                                         name = "consequence_type",
                                         class = "character",
                                         val = c("missense",
                                                 "stop gained",
                                                 "stop lost")),
                                    list(arg = wild_type,
                                         name = "wild_type",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = alternative_sequence,
                                         name = "alternative_sequence",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = location,
                                         name = "location",
                                         class = "character"),
                                    list(arg = save_peff,
                                         name = "save_peff",
                                         class = "logical"),
                                    list(arg = save_to,
                                         name = "save_to",
                                         class = "character")),
                        cond = list(list(!is.na(save_to) & save_peff == FALSE,
                                         "'save_to' is ignored because you didn't set 'save_peff' to TRUE.")),
                        cond_warning = TRUE,
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /variation/hgvs/{hgvs} Get natural variants in UniProt by HGVS expression")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(source_type)),
                              list("sourcetype" = paste0(source_type,
                                                         collapse = ","))),
                         list(any(!is.na(consequence_type)),
                              list("consequencetype" = paste0(consequence_type,
                                                              collapse = ","))),
                         list(any(!is.na(wild_type)),
                              list("wildtype" = paste0(wild_type,
                                                       collapse = ","))),
                         list(any(!is.na(alternative_sequence)),
                              list("alternativesequence" = paste0(alternative_sequence,
                                                                  collapse = ","))),
                         list(!is.na(location),
                              list("location" = location)))

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "variation/",
                                                  accession),
                                    query = call_query))
  if (save_peff == TRUE) {
    accept_input = "text/x-peff"
    parser_input = "text->chr"
    # create file_path
    save_to = rba_ba_file_path(file_ext = "peff",
                               file_name = format(Sys.time(),
                                                  "%Y_%m_%d_%H_%M_%S"),
                               dir_name = "rba_uniprot",
                               save_to = save_to,
                               verbose = verbose,
                               diagnostics = diagnostics)
    call_func_input = as.call(append(as.list(call_func_input),
                                     quote(httr::write_disk(save_to,
                                                            overwrite = TRUE))))
  } else {
    accept_input = "application/json"
    parser_input = "json->list"
  }
  call_func_input = as.call(append(as.list(call_func_input),
                                   quote(httr::accept(accept_input))))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = parser_input,
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#### Proteomics Endpoints ####
#' Search proteomics peptides in UniProt
#'
#' @param accession
#' @param taxid
#' @param upid
#' @param data_source
#' @param peptide
#' @param unique
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_proteomics_search = function(accession = NA,
                                         taxid = NA,
                                         upid = NA,
                                         data_source = NA,
                                         peptide = NA,
                                         unique = NA,
                                         verbose = TRUE,
                                         progress_bar = FALSE,
                                         diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = taxid,
                                         name = "taxid",
                                         class = "numeric",
                                         max_len = 20),
                                    list(arg = upid,
                                         name = "upid",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = data_source,
                                         name = "data_source",
                                         class = "character",
                                         max_len = 2,
                                         vals = c("MaxQB",
                                                  "PeptideAtlas",
                                                  "EPD",
                                                  "ProteomicsDB")),
                                    list(arg = peptide,
                                         name = "peptide",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = unique,
                                         name = "unique",
                                         class = "logical")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /proteomics Search proteomics peptides in UniProt")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(accession)),
                              list("accession" = paste0(accession,
                                                        collapse = ","))),
                         list(any(!is.na(taxid)),
                              list("taxid" = paste0(taxid,
                                                    collapse = ","))),
                         list(any(!is.na(upid)),
                              list("upid" = paste0(upid,
                                                   collapse = ","))),
                         list(any(!is.na(data_source)),
                              list("data_source" = paste0(data_source,
                                                          collapse = ","))),
                         list(any(!is.na(peptide)),
                              list("peptide" = paste0(peptide,
                                                      collapse = ","))),
                         list(!is.na(unique),
                              list("unique" = ifelse(unique, "true", "false")))
  )

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "proteomics"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get proteomics peptides mapped to UniProt by accession
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
rba_uniprot_proteomics = function(accession,
                                  verbose = TRUE,
                                  progress_bar = FALSE,
                                  diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /proteomics/{accession} Get proteomics peptides mapped to UniProt by accession")
  }
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "proteomics/",
                                                  accession),
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#### Antigen Endpoints ####
#' Search antigens in UniProt
#'
#' @param accession
#' @param taxid
#' @param upid
#' @param data_source
#' @param peptide
#' @param unique
#' @param verbose
#' @param progress_bar
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_antigen_search = function(accession = NA,
                                      antigen_sequence = NA,
                                      antigen_id = NA,
                                      ensembl_id = NA,
                                      match_score = NA,
                                      verbose = TRUE,
                                      progress_bar = FALSE,
                                      diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character",
                                         max_len = 100),
                                    list(arg = antigen_sequence,
                                         name = "antigen_sequence",
                                         class = "character"),
                                    list(arg = antigen_id,
                                         name = "antigen_id",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = ensembl_id,
                                         name = "ensembl_id",
                                         class = "character",
                                         max_len = 20),
                                    list(arg = match_score,
                                         name = "match_score",
                                         class = "numeric")),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /antigen Search antigens in UniProt")
  }
  ## build GET API request's query
  call_query = list("size" = "-1")

  additional_pars = list(list(any(!is.na(accession)),
                              list("accession" = paste0(accession,
                                                        collapse = ","))),
                         list(!is.na(antigen_sequence),
                              list("antigen_sequence" = antigen_sequence)),
                         list(any(!is.na(antigen_id)),
                              list("antigen_id" = paste0(antigen_id,
                                                         collapse = ","))),
                         list(any(!is.na(ensembl_id)),
                              list("ensembl_id" = paste0(ensembl_id,
                                                         collapse = ","))),
                         list(!is.na(match_score),
                              list("match_score" = match_score)))

  call_query = rba_ba_body_add_pars(call_body = call_query,
                                    additional_pars = additional_pars)
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "antigen"),
                                    query = call_query,
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list_no_simp",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}

#' Get antigen by UniProt accession
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
rba_uniprot_antigen = function(accession,
                                  verbose = TRUE,
                                  progress_bar = FALSE,
                                  diagnostics = FALSE) {
  ## Check input arguments
  invisible(rba_ba_args(cons = list(list(arg = accession,
                                         name = "accession",
                                         class = "character",
                                         len = 1)),
                        diagnostics = diagnostics))

  if (verbose == TRUE){
    message("get /antigen/{accession} Get antigen by UniProt accession")
  }
  ## make function-specific calls
  call_func_input = quote(httr::GET(url = getOption("rba_url_uniprot"),
                                    path = paste0(getOption("rba_pth_uniprot"),
                                                  "antigen/",
                                                  accession),
                                    httr::accept_json()
  ))

  ## call API
  final_output = rba_ba_skeletion(call_function = call_func_input,
                                  response_parser = NULL,
                                  parser_type = "json->list",
                                  user_agent = TRUE,
                                  progress_bar = progress_bar,
                                  verbose = verbose,
                                  diagnostics = diagnostics)

  return(final_output)
}
