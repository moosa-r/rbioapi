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
#' @param ...
#' @param md5
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
                                       ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character",
                               max_len = 100),
                          list(arg = "reviewed",
                               class = "logical"),
                          list(arg = "isoform",
                               class = "numeric",
                               val = 0:2),
                          list(arg = "go_terms",
                               class = "character"),
                          list(arg = "keyword",
                               class = "character"),
                          list(arg = "ec",
                               class = "character",
                               max_len = 20),
                          list(arg = "gene",
                               class = "character",
                               max_len = 20),
                          list(arg = "exact_gene",
                               class = "character",
                               max_len = 20),
                          list(arg = "protein",
                               class = "character"),
                          list(arg = "organism",
                               class = "character"),
                          list(arg = "taxid",
                               class = "numeric",
                               max_len = 20),
                          list(arg = "pubmed",
                               class = "character",
                               max_len = 20),
                          list(arg = "seq_length",
                               class = "character"),
                          list(arg = "md5",
                               class = "character")))

  v_msg("get /proteins")
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("accession",
                                 any(!is.na(accession)),
                                 paste0(accession,
                                        collapse = ",")),
                            list("reviewed",
                                 !is.na(reviewed),
                                 ifelse(reviewed,
                                        "true",
                                        "false")),
                            list("isoform",
                                 !is.na(isoform),
                                 isoform),
                            list("goterms",
                                 !is.na(go_term),
                                 go_term),
                            list("keyword",
                                 !is.na(keyword),
                                 keyword),
                            list("ec",
                                 any(!is.na(ec)),
                                 paste0(ec,
                                        collapse = ",")),
                            list("gene",
                                 any(!is.na(gene)),
                                 paste0(gene,
                                        collapse = ",")),
                            list("exact_gene",
                                 any(!is.na(exact_gene)),
                                 paste0(exact_gene,
                                        collapse = ",")),
                            list("protein",
                                 !is.na(protein),
                                 protein),
                            list("organism",
                                 !is.na(organism),
                                 organism),
                            list("taxid",
                                 any(!is.na(taxid)),
                                 paste0(taxid,
                                        collapse = ",")),
                            list("pubmed",
                                 any(!is.na(pubmed)),
                                 paste0(pubmed,
                                        collapse = ",")),
                            list("seq_length",
                                 !is.na(seq_length),
                                 seq_length),
                            list("md5",
                                 !is.na(md5),
                                 md5))
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "proteins"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get UniProt entry by accession
#'
#' @param accession
#' @param interaction
#' @param ...
#' @param isoforms
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_proteins = function(accession = NA,
                                interaction = FALSE,
                                isoforms = FALSE,
                                ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character")),
              cond = list(quote(sum(interaction, isoforms) == 2),
                          "You can only set only one of interaction or isoform as TRUE in one function call."))

  v_msg("get /proteins/{accession} Get UniProt entry by accession",
        "get /proteins/{accession}/isoforms Get UniProt isoform entries from parent entry accession",
        "get /proteins/interaction/{accession} Get UniProt interactions by accession")

  ## make function-specific calls
  path_input = sprintf("%s%s/%s",
                       rba_ba_stg("uniprot", "pth"),
                       ifelse(interaction == TRUE,
                              yes = "proteins/interaction",
                              no = "proteins"),
                       accession)
  if (isoforms == TRUE) {
    path_input = paste0(path_input, "/isoforms")
  }
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = path_input,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get UniProt entries by UniProt cross reference and its ID
#'
#' @param db_type
#' @param db_id
#' @param reviewed
#' @param ...
#' @param isoform
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_proteins_crossref = function(db_type,
                                         db_id,
                                         reviewed = NA,
                                         isoform = NA,
                                         ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "db_type",
                               class = "character"),
                          list(arg = "db_id",
                               class = "character"),
                          list(arg = "reviewed",
                               class = "logical"),
                          list(arg = "isoform",
                               class = "numeric",
                               val = c(0,1)))
  )

  v_msg("get /proteins/{dbtype}:{dbid} Get UniProt entries by UniProt cross reference and its ID")
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("reviewed",
                                 !is.na(reviewed),
                                 ifelse(reviewed,
                                        "true",
                                        "false")),
                            list("isoform",
                                 !is.na(isoform),
                                 isoform))
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = sprintf("%sproteins/%s:%s",
                                          rba_ba_stg("uniprot", "pth"),
                                          db_type,
                                          db_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
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
#' @param ...
#' @param types
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
                                       ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character",
                               max_len = 100),
                          list(arg = "gene",
                               class = "character",
                               max_len = 20),
                          list(arg = "exact_gene",
                               class = "character",
                               max_len = 20),
                          list(arg = "protein",
                               class = "character"),
                          list(arg = "reviewed",
                               class = "logical"),
                          list(arg = "organism",
                               class = "character"),
                          list(arg = "taxid",
                               class = "numeric",
                               max_len = 20),
                          list(arg = "categories",
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
                          list(arg = "types",
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
                          ))
  )

  v_msg("get /features Search protein sequence features in UniProt")
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("accession",
                                 any(!is.na(accession)),
                                 paste0(accession,
                                        collapse = ",")),
                            list("gene",
                                 any(!is.na(gene)),
                                 paste0(gene,
                                        collapse = ",")),
                            list("exact_gene",
                                 any(!is.na(exact_gene)),
                                 paste0(exact_gene,
                                        collapse = ",")),
                            list("protein",
                                 !is.na(protein),
                                 protein),
                            list("reviewed",
                                 !is.na(reviewed),
                                 ifelse(reviewed,
                                        "true",
                                        "false")),
                            list("organism",
                                 !is.na(organism),
                                 organism),
                            list("taxid",
                                 any(!is.na(taxid)),
                                 paste0(taxid,
                                        collapse = ",")),
                            list("categories",
                                 any(!is.na(categories)),
                                 paste0(categories,
                                        collapse = ",")),
                            list("types",
                                 any(!is.na(types)),
                                 paste0(types,
                                        collapse = ",")))
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "features"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Search protein sequence features of a given type in UniProt
#' Search for term(s) that appear in feature description for your specified
#' feature type. For example, you can search by type=DOMAIN and
#' Term=Kinase. Comma separated values
#'
#' @param terms
#' @param type
#' @param ...
#' @param categories
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_features_type = function(terms,
                                     type,
                                     categories = NA,
                                     ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "terms",
                               class = "character",
                               max_len = 20),
                          list(arg = "type",
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
                          list(arg = "categories",
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
  )
  )

  v_msg("get /features/type/{type} Search protein sequence features of a given type in UniProt")
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("categories",
                                 any(!is.na(categories)),
                                 paste0(categories,
                                        collapse = ",")),
                            list("terms",
                                 any(!is.na(terms)),
                                 paste0(terms,
                                        collapse = ",")))
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "features/type/",
                                         type),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get UniProt protein sequence features by accession
#'
#' @param accession
#' @param types
#' @param ...
#' @param categories
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_features = function(accession,
                                types = NA,
                                categories = NA,
                                ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character"),
                          list(arg = "types",
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
                          list(arg = "categories",
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
  )
  )

  v_msg("get /features/{accession} Get UniProt protein sequence features by accession")
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("categories",
                                 any(!is.na(categories)),
                                 paste0(categories,
                                        collapse = ",")),
                            list("types",
                                 any(!is.na(types)),
                                 paste0(types,
                                        collapse = ",")))
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "features/",
                                         accession),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## call API
  final_output = rba_ba_skeleton(input_call)
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
#' @param ...
#' @param save_peff
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_variation_search = function(accession = NA,
                                        save_peff = FALSE,
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
                                        ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character",
                               max_len = 100),
                          list(arg = "source_type",
                               class = "character",
                               val = c("uniprot",
                                       "large scale study",
                                       "mixed"),
                               max_len = 2),
                          list(arg = "consequence_type",
                               class = "character",
                               val = c("missense",
                                       "stop gained",
                                       "stop lost")),
                          list(arg = "wild_type",
                               class = "character",
                               max_len = 20),
                          list(arg = "alternative_sequence",
                               class = "character",
                               max_len = 20),
                          list(arg = "location",
                               class = "character"),
                          list(arg = "disease",
                               class = "character"),
                          list(arg = "omim",
                               class = "character",
                               max_len = 20),
                          list(arg = "evidence",
                               class = "numeric",
                               max_len = 20),
                          list(arg = "taxid",
                               class = "numeric",
                               max_len = 20),
                          list(arg = "db_type",
                               class = "character",
                               max_len = 2),
                          list(arg = "db_id",
                               class = "character",
                               max_len = 20),
                          list(arg = "save_peff",
                               class = c("logical",
                                         "character"))))

  v_msg("get /variation Search natural variants in UniProt")
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("accession",
                                 any(!is.na(accession)),
                                 paste0(accession,
                                        collapse = ",")),
                            list("sourcetype",
                                 any(!is.na(source_type)),
                                 paste0(source_type,
                                        collapse = ",")),
                            list("consequencetype",
                                 any(!is.na(consequence_type)),
                                 paste0(consequence_type,
                                        collapse = ",")),
                            list("wildtype",
                                 any(!is.na(wild_type)),
                                 paste0(wild_type,
                                        collapse = ",")),
                            list("alternativesequence",
                                 any(!is.na(alternative_sequence)),
                                 paste0(alternative_sequence,
                                        collapse = ",")),
                            list("location",
                                 !is.na(location),
                                 location),
                            list("disease",
                                 !is.na(disease),
                                 disease),
                            list("omim",
                                 any(!is.na(omim)),
                                 paste0(omim,
                                        collapse = ",")),
                            list("evidence",
                                 any(!is.na(evidence)),
                                 paste0(evidence,
                                        collapse = ",")),
                            list("taxid",
                                 any(!is.na(taxid)),
                                 paste0(taxid,
                                        collapse = ",")),
                            list("dbtype",
                                 any(!is.na(db_type)),
                                 paste0(db_type,
                                        collapse = ",")),
                            list("dbid",
                                 any(!is.na(db_id)),
                                 paste0(db_type,
                                        collapse = ",")))
  ## make function-specific calls
  save_to = rba_ba_file(file_ext = "peff",
                        file_name = "uniprot_variation",
                        save_to = save_peff)

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "variation"),
                           query = call_query,
                           save_to = save_to,
                           file_accept = "text/x-peff",
                           file_parser = "text->chr",
                           obj_accept = "application/json",
                           obj_parser = "json->list"
  )
  ## call API
  final_output = rba_ba_skeleton(input_call)
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
#' @param ...
#' @param save_peff
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_variation_dbsnp = function(db_id,
                                       save_peff = FALSE,
                                       source_type = NA,
                                       consequence_type = NA,
                                       wild_type = NA,
                                       alternative_sequence = NA,
                                       location = NA,
                                       ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "db_id",
                               class = "character"),
                          list(arg = "source_type",
                               class = "character",
                               val = c("uniprot",
                                       "large scale study",
                                       "mixed"),
                               max_len = 2),
                          list(arg = "consequence_type",
                               class = "character",
                               val = c("missense",
                                       "stop gained",
                                       "stop lost")),
                          list(arg = "wild_type",
                               class = "character",
                               max_len = 20),
                          list(arg = "alternative_sequence",
                               class = "character",
                               max_len = 20),
                          list(arg = "location",
                               class = "character"),
                          list(arg = "save_peff",
                               class = c("logical",
                                         "character"))))

  v_msg("get /variation/dbsnp/{dbid} Get natural variants in UniProt by NIH-NCBI SNP database identifier")
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("sourcetype",
                                 any(!is.na(source_type)),
                                 paste0(source_type,
                                        collapse = ",")),
                            list("consequencetype",
                                 any(!is.na(consequence_type)),
                                 paste0(consequence_type,
                                        collapse = ",")),
                            list("wildtype",
                                 any(!is.na(wild_type)),
                                 paste0(wild_type,
                                        collapse = ",")),
                            list("alternativesequence",
                                 any(!is.na(alternative_sequence)),
                                 paste0(alternative_sequence,
                                        collapse = ",")),
                            list("location",
                                 !is.na(location),
                                 location))
  ## make function-specific calls
  save_to = rba_ba_file(file_ext = "peff",
                        file_name = "uniprot_variation_dbsnp",
                        save_to = save_peff)
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "variation/dbsnp/",
                                         db_id),
                           query = call_query,
                           save_to = save_to,
                           file_accept = "text/x-peff",
                           file_parser = "text->chr",
                           obj_accept = "application/json",
                           obj_parser = "json->list"
  )

  ## call API
  final_output = rba_ba_skeleton(input_call)
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
#' @param ...
#' @param save_peff
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_variation_hgvs = function(hgvs,
                                      save_peff = FALSE,
                                      source_type = NA,
                                      consequence_type = NA,
                                      wild_type = NA,
                                      alternative_sequence = NA,
                                      location = NA,
                                      ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "hgvs",
                               class = "character"),
                          list(arg = "source_type",
                               class = "character",
                               val = c("uniprot",
                                       "large scale study",
                                       "mixed"),
                               max_len = 2),
                          list(arg = "consequence_type",
                               class = "character",
                               val = c("missense",
                                       "stop gained",
                                       "stop lost")),
                          list(arg = "wild_type",
                               class = "character",
                               max_len = 20),
                          list(arg = "alternative_sequence",
                               class = "character",
                               max_len = 20),
                          list(arg = "location",
                               class = "character"),
                          list(arg = "save_peff",
                               class = c("logical",
                                         "character"))))

  v_msg("get /variation/hgvs/{hgvs} Get natural variants in UniProt by HGVS expression")
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("sourcetype",
                                 any(!is.na(source_type)),
                                 paste0(source_type,
                                        collapse = ",")),
                            list("consequencetype",
                                 any(!is.na(consequence_type)),
                                 paste0(consequence_type,
                                        collapse = ",")),
                            list("wildtype",
                                 any(!is.na(wild_type)),
                                 paste0(wild_type,
                                        collapse = ",")),
                            list("alternativesequence",
                                 any(!is.na(alternative_sequence)),
                                 paste0(alternative_sequence,
                                        collapse = ",")),
                            list("location",
                                 !is.na(location),
                                 location))
  ## make function-specific calls
  save_to = rba_ba_file(file_ext = "peff",
                        file_name = "uniprot_variation_hgvs",
                        save_to = save_peff)
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "variation/hgvs/",
                                         hgvs),
                           query = call_query,
                           save_to = save_to,
                           file_accept = "text/x-peff",
                           file_parser = "text->chr",
                           obj_accept = "application/json",
                           obj_parser = "json->list"
  )

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get natural variants by UniProt accession
#'
#' @param source_type
#' @param consequence_type
#' @param wild_type
#' @param alternative_sequence
#' @param location
#' @param save_peff
#' @param accession
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_variation_accession = function(accession,
                                           save_peff = FALSE,
                                           source_type = NA,
                                           consequence_type = NA,
                                           wild_type = NA,
                                           alternative_sequence = NA,
                                           location = NA,
                                           ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character"),
                          list(arg = "source_type",
                               class = "character",
                               val = c("uniprot",
                                       "large scale study",
                                       "mixed"),
                               max_len = 2),
                          list(arg = "consequence_type",
                               class = "character",
                               val = c("missense",
                                       "stop gained",
                                       "stop lost")),
                          list(arg = "wild_type",
                               class = "character",
                               max_len = 20),
                          list(arg = "alternative_sequence",
                               class = "character",
                               max_len = 20),
                          list(arg = "location",
                               class = "character"),
                          list(arg = "save_peff",
                               class = c("logical",
                                         "character"))))

  v_msg("get /variation/hgvs/{hgvs} Get natural variants in UniProt by HGVS expression")
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("sourcetype",
                                 any(!is.na(source_type)),
                                 paste0(source_type,
                                        collapse = ",")),
                            list("consequencetype",
                                 any(!is.na(consequence_type)),
                                 paste0(consequence_type,
                                        collapse = ",")),
                            list("wildtype",
                                 any(!is.na(wild_type)),
                                 paste0(wild_type,
                                        collapse = ",")),
                            list("alternativesequence",
                                 any(!is.na(alternative_sequence)),
                                 paste0(alternative_sequence,
                                        collapse = ",")),
                            list("location",
                                 !is.na(location),
                                 location))
  ## make function-specific calls
  save_to = rba_ba_file(file_ext = "peff",
                        file_name = "uniprot_variation",
                        save_to = save_peff)
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "variation/",
                                         accession),
                           query = call_query,
                           save_to = save_to,
                           file_accept = "text/x-peff",
                           file_parser = "text->chr",
                           obj_accept = "application/json",
                           obj_parser = "json->list"
  )
  ## call API
  final_output = rba_ba_skeleton(input_call)
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
#' @param ...
#' @param unique
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
                                         ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character",
                               max_len = 100),
                          list(arg = "taxid",
                               class = "numeric",
                               max_len = 20),
                          list(arg = "upid",
                               class = "character",
                               max_len = 100),
                          list(arg = "data_source",
                               class = "character",
                               max_len = 2,
                               vals = c("MaxQB",
                                        "PeptideAtlas",
                                        "EPD",
                                        "ProteomicsDB")),
                          list(arg = "peptide",
                               class = "character",
                               max_len = 20),
                          list(arg = "unique",
                               class = "logical"))
  )

  v_msg("get /proteomics Search proteomics peptides in UniProt")
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("accession",
                                 any(!is.na(accession)),
                                 paste0(accession,
                                        collapse = ",")),
                            list("taxid",
                                 any(!is.na(taxid)),
                                 paste0(taxid,
                                        collapse = ",")),
                            list("upid",
                                 any(!is.na(upid)),
                                 paste0(upid,
                                        collapse = ",")),
                            list("data_source",
                                 any(!is.na(data_source)),
                                 paste0(data_source,
                                        collapse = ",")),
                            list("peptide",
                                 any(!is.na(peptide)),
                                 paste0(peptide,
                                        collapse = ",")),
                            list("unique",
                                 !is.na(unique),
                                 ifelse(unique, "true", "false")))
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "proteomics"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get proteomics peptides mapped to UniProt by accession
#'
#' @param ...
#' @param accession
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_proteomics = function(accession,
                                  ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character",
                               len = 1))
  )

  v_msg("get /proteomics/{accession} Get proteomics peptides mapped to UniProt by accession")
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "proteomics/",
                                         accession),
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#### Antigen Endpoints ####
#' Search antigens in UniProt
#'
#' @param antigen_sequence
#' @param antigen_id
#' @param ensembl_id
#' @param match_score
#' @param ...
#' @param accession
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
                                      ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character",
                               max_len = 100),
                          list(arg = "antigen_sequence",
                               class = "character"),
                          list(arg = "antigen_id",
                               class = "character",
                               max_len = 20),
                          list(arg = "ensembl_id",
                               class = "character",
                               max_len = 20),
                          list(arg = "match_score",
                               class = "numeric"))
  )

  v_msg("get /antigen Search antigens in UniProt")
  ## build GET API request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("accession",
                                 any(!is.na(accession)),
                                 paste0(accession,
                                        collapse = ",")),
                            list("antigen_sequence",
                                 !is.na(antigen_sequence),
                                 antigen_sequence),
                            list("antigen_id",
                                 any(!is.na(antigen_id)),
                                 paste0(antigen_id,
                                        collapse = ",")),
                            list("ensembl_id",
                                 any(!is.na(ensembl_id)),
                                 paste0(ensembl_id,
                                        collapse = ",")),
                            list("match_score",
                                 !is.na(match_score),
                                 match_score))
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "antigen"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list_no_simp")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get antigen by UniProt accession
#'
#' @param ...
#' @param accession
#'
#' @return
#' @export
#'
#' @examples
rba_uniprot_antigen = function(accession,
                               ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character",
                               len = 1))
  )

  v_msg("get /antigen/{accession} Get antigen by UniProt accession")
  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "antigen/",
                                         accession),
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

