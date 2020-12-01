#' Search UniParc Entries
#'
#' Use this function to search
#'   \href{https://www.uniprot.org/help/uniparc}{UniProt Archive (UniParc)}
#'   entries.You may also refine your search with modifiers such as sequence
#'   length, taxon id etc. refer to "Arguments section" for more information.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.\cr\cr
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/uniparc"
#'
#' @param upi unique UniParc Identifier(s). You can provide up to 100 IDs.
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can provide up to 100
#'   accession numbers.
#' @param db_type \href{https://www.uniprot.org/database/}{cross-reference}
#'   (external database) name.
#' @param db_id Protein ID in the cross-reference (external) database.
#'   You can provide up to 100 IDs.
#' @param gene \href{https://www.uniprot.org/help/gene_name}{UniProt gene
#'   name(s)}. You can provide up to 20 gene names.
#' @param protein \href{https://www.uniprot.org/help/protein_names}{UniProt
#'   protein name}.
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can provide up to 20 taxon IDs.
#' @param organism \href{https://www.uniprot.org/taxonomy/}{Organism name}.
#' @param sequence_checksum Sequence CRC64 checksum.
#' @param ipr \href{https://www.ebi.ac.uk/interpro/about/interpro/}{InterPro
#'   identifier(s)}. You can provide up to 20 IDs.
#' @param signature_db InterPro's
#'   \href{https://interpro-documentation.readthedocs.io/en/latest/databases.html}{signature
#'   database}. You can provide up to 13 of the following values:\cr
#'   "CATH", "CDD", "HAMAP", "MobiDB Lite", "Panther", "Pfam", "PIRSF",
#'   "PRINTS", "Prosite", "SFLD", "SMART", "SUPERFAMILY" and/or "TIGRfams"
#' @param signature_id Signature ID in the InterPro's
#'   \href{https://interpro-documentation.readthedocs.io/en/latest/databases.html}{signature
#'   database}. You can provide up to 20 IDs.
#' @param upid \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can provide up to 100 UPIDs.
#' @param seq_length An exact sequence length (e.g. 150) or a range of sequence
#'   lengths (e.g. "130-158").
#' @param rf_dd_type Filter the content of the each UniParc entry by
#'   \href{https://www.uniprot.org/database/}{cross-reference} names. You can
#'   provide multiple values.
#' @param rf_db_id Filter the content of the each UniParc entry by protein
#'   identifiers in any cross-reference database. You can provide multiple
#'   values.
#' @param rf_active (logical ) Filter the content of each UniParc entry based on
#'   active status on source database:\cr\itemize{
#'   \item NA: don't filter contents based on active status.
#'   \item TRUE: only return contents which are still active.
#'   \item FALSE: Only return contents which are not active.}
#' @param rf_tax_id (Numeric) Filter the content of each UniParc entry by
#'   NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}. You can
#'   provide multiple values.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A List where each element corresponds to one UniParc entry returned
#'   by your search query. The element itself is a sub-list containing sequence
#'   information and reference entries.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_uniprot_uniparc_search(upi = "UPI00000000C9")
#' rba_uniprot_uniparc_search(accession = "P30914")
#' rba_uniprot_uniparc_search(accession = "P30914", rf_active = TRUE)
#' rba_uniprot_uniparc_search(taxid = "694009", protein = "Nucleoprotein")
#' @family "UniProt API, UniParc"
#' @export
rba_uniprot_uniparc_search = function(upi = NA,
                                      accession = NA,
                                      db_type = NA,
                                      db_id = NA,
                                      gene = NA,
                                      protein = NA,
                                      taxid = NA,
                                      organism = NA,
                                      sequence_checksum= NA,
                                      ipr = NA,
                                      signature_db = NA,
                                      signature_id = NA,
                                      upid = NA,
                                      seq_length = NA,
                                      rf_dd_type = NA,
                                      rf_db_id = NA,
                                      rf_active = NA,
                                      rf_tax_id = NA,
                                      ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "upi",
                               class = "character",
                               max_len = 100),
                          list(arg = "accession",
                               class = "character",
                               max_len = 100),
                          list(arg = "db_type",
                               class = "character"),
                          list(arg = "db_id",
                               class = "character"),
                          list(arg = "gene",
                               class = "character",
                               max_len = 20),
                          list(arg = "protein",
                               class = "character",
                               len = 1),
                          list(arg = "taxid",
                               class = "character",
                               max_len = 20),
                          list(arg = "organism",
                               class = "character"),
                          list(arg = "sequence_checksum",
                               class = "character"),
                          list(arg = "ipr",
                               class = "character",
                               max_len = 100),
                          list(arg = "signature_db",
                               class = "character",
                               max_len = 13),
                          list(arg = "upid",
                               class = "character",
                               max_len = 100),
                          list(arg = "seq_length",
                               class = "character"),
                          list(arg = "rf_dd_type",
                               class = "character"),
                          list(arg = "rf_db_id",
                               class = "character"),
                          list(arg = "rf_active",
                               class = "logical"),
                          list(arg = "rf_tax_id",
                               class = "character"))
  )

  v_msg("Searching UniParc and retrieving entries that match your provided inputs.")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("upi",
                                 any(!is.na(upi)),
                                 paste0(upi,
                                        collapse = ",")),
                            list("accession",
                                 any(!is.na(accession)),
                                 paste0(accession,
                                        collapse = ",")),
                            list("dbtype",
                                 !is.na(db_type),
                                 db_type),
                            list("dbid",
                                 any(!is.na(db_id)),
                                 paste0(db_id,
                                        collapse = ",")),
                            list("gene",
                                 any(!is.na(gene)),
                                 paste0(gene,
                                        collapse = ",")),
                            list("protein",
                                 !is.na(protein),
                                 protein),
                            list("taxid",
                                 any(!is.na(taxid)),
                                 paste0(taxid,
                                        collapse = ",")),
                            list("organism",
                                 !is.na(organism),
                                 organism),
                            list("sequencechecksum",
                                 !is.na(sequence_checksum),
                                 sequence_checksum),
                            list("ipr",
                                 any(!is.na(ipr)),
                                 paste0(ipr,
                                        collapse = ",")),
                            list("signaturetype",
                                 any(!is.na(signature_db)),
                                 paste0(signature_db,
                                        collapse = ",")),
                            list("signatureid",
                                 any(!is.na(signature_id)),
                                 paste0(signature_id,
                                        collapse = ",")),
                            list("upid",
                                 any(!is.na(upid)),
                                 paste0(upid,
                                        collapse = ",")),
                            list("seqLength",
                                 !is.na(seq_length),
                                 seq_length),
                            list("rfDdtype",
                                 any(!is.na(rf_dd_type)),
                                 paste0(rf_dd_type,
                                        collapse = ",")),
                            list("rfDbid",
                                 any(!is.na(rf_db_id)),
                                 paste0(rf_db_id,
                                        collapse = ",")),
                            list("rfActive",
                                 !is.na(rf_active),
                                 ifelse(rf_active,
                                        "true",
                                        "false")),
                            list("rfTaxId",
                                 any(!is.na(rf_tax_id)),
                                 paste0(rf_tax_id,
                                        collapse = ",")))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "uniparc"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_uniparc_search.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get UniParc entry
#'
#' Use this function to retrieve UniParc entries. You can use either -and only
#'   one of- UniProt accession, Cross-reference database id, UniParc ID or
#'   UniProt Proteome UPID. You can also filter the returned content of
#'   the returned UniParc entry. see "Argument" section for more details.
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/uniparc/accession/{accession} "
#'  "GET https://ebi.ac.uk/proteins/api/uniparc/dbreference/{dbid}"
#'  "GET https://ebi.ac.uk/proteins/api/uniparc/proteome/{upid}"
#'  "GET https://ebi.ac.uk/proteins/api/uniparc/upi/{upi}"
#'
#' @param upi unique UniParc Identifier.
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param db_id Protein ID in the cross-reference (external) database.
#' @param upid \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can provide up to 100 UPIDs.
#' @param rf_dd_type Filter the content of the UniParc entry by
#'   \href{https://www.uniprot.org/database/}{cross-reference} names. You can
#'   provide multiple values.
#' @param rf_db_id Filter the content of the UniParc entry by protein
#'   identifiers in any cross-reference database. You can provide multiple
#'   values.
#' @param rf_active (logical ) Filter the content of UniParc entry based on
#'   active status on source database:\cr\itemize{
#'   \item NA: don't filter contents based on active status.
#'   \item TRUE: only return contents which are still active.
#'   \item FALSE: Only return contents which are not active.}
#' @param rf_tax_id (Numeric) Filter the content of the UniParc entry by
#'   NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}. You can
#'   provide multiple values.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list which correspond to a UniParc entry.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_uniprot_uniparc(upi = "UPI00000000C9")
#' rba_uniprot_uniparc(upi = "UPI00000000C9")
#' rba_uniprot_uniparc(upi = "UPI00000000C9", rf_active = FALSE)
#' @family "UniProt API, UniParc"
#' @export
rba_uniprot_uniparc = function(upi = NA,
                               accession = NA,
                               db_id = NA,
                               upid = NA,
                               rf_dd_type = NA,
                               rf_db_id = NA,
                               rf_active = NA,
                               rf_tax_id = NA,
                               ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character"),
                          list(arg = "db_id",
                               class = "character"),
                          list(arg = "upid",
                               class = "character"),
                          list(arg = "upi",
                               class = "character"),
                          list(arg = "rf_dd_type",
                               class = "character"),
                          list(arg = "rf_db_id",
                               class = "character"),
                          list(arg = "rf_active",
                               class = "logical"),
                          list(arg = "rf_tax_id",
                               class = "character")),
              cond = list(list(quote(sum(!is.na(accession), !is.na(db_id), !is.na(upid), !is.na(upi)) != 1),
                               "Please provide -only- one of the arguments 'accession', 'db_id', 'upid' or 'upi'."))
  )

  v_msg("Retriving UniParc entry with %s.",
        if (!is.na(accession)) {paste0("UniProt accession ", accession)
        } else if (!is.na(db_id)) {
          path_input = paste0("cross-reference database ID ", accession)
        } else if (!is.na(upid)) {
          path_input = paste0("UniProt Proteome ID ", accession)
        } else if (!is.na(upi)) {
          path_input = paste0("UniParc ID ", accession)
        })

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("rfDdtype",
                                 any(!is.na(rf_dd_type)),
                                 paste0(rf_dd_type,
                                        collapse = ",")),
                            list("rfDbid",
                                 any(!is.na(rf_db_id)),
                                 paste0(rf_db_id,
                                        collapse = ",")),
                            list("rfActive",
                                 !is.na(rf_active),
                                 ifelse(rf_active,
                                        "true",
                                        "false")),
                            list("rfTaxId",
                                 any(!is.na(rf_tax_id)),
                                 paste0(rf_tax_id,
                                        collapse = ",")))
  ## Build Function-Specific Call
  if (!is.na(accession)) {
    path_input = paste0(rba_ba_stg("uniprot", "pth"),
                        "uniparc/accession/", accession)
  } else if (!is.na(db_id)) {
    path_input = paste0(rba_ba_stg("uniprot", "pth"),
                        "uniparc/dbreference/", db_id)
  } else if (!is.na(upid)) {
    path_input = paste0(rba_ba_stg("uniprot", "pth"),
                        "uniparc/proteome/", upid)
  } else if (!is.na(upi)) {
    path_input = paste0(rba_ba_stg("uniprot", "pth"),
                        "uniparc/upi/", upi)
  }
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_uniparc.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get UniParc Longest Sequence for Entries
#'
#' This function returns the UniParc Entry with a cross-reference to the
#'   longest active UniProtKB sequence (preferably from Swiss-Prot and if not
#'   then TrEMBL). If it finds more than one longest active UniProtKB sequence
#'   it returns 400 (Bad Request) error response with the list of cross
#'   references found.
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/uniparc/bestguess"
#'
#' @param upid \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can provide up to 100 UPIDs.
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can provide up to 100
#'   accession numbers.
#' @param db_id Protein ID in the cross-reference (external) database.
#'   You can provide up to 100 IDs.
#' @param gene \href{https://www.uniprot.org/help/gene_name}{UniProt gene
#'   name(s)}. You can provide up to 20 gene names.
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can provide up to 20 taxon IDs.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list where each element correspond to a UniParc entry.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_uniprot_uniparc_bestguess("UPI00000000C9")
#' @family "UniProt API, UniParc"
#' @export
rba_uniprot_uniparc_bestguess = function(upi = NA,
                                         accession = NA,
                                         db_id = NA,
                                         gene = NA,
                                         taxid = NA,
                                         ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "upi",
                               class = "character",
                               max_len = 100),
                          list(arg = "accession",
                               class = "character",
                               max_len = 100),
                          list(arg = "db_id",
                               class = "character"),
                          list(arg = "gene",
                               class = "character",
                               max_len = 20),
                          list(arg = "taxid",
                               class = "character",
                               max_len = 20))
  )

  v_msg("Retrieving UniParc longest Sequence.")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("upi",
                                 any(!is.na(upi)),
                                 paste0(upi,
                                        collapse = ",")),
                            list("accession",
                                 any(!is.na(accession)),
                                 paste0(accession,
                                        collapse = ",")),
                            list("dbid",
                                 any(!is.na(db_id)),
                                 paste0(db_id,
                                        collapse = ",")),
                            list("gene",
                                 any(!is.na(gene)),
                                 paste0(gene,
                                        collapse = ",")),
                            list("taxid",
                                 any(!is.na(taxid)),
                                 paste0(taxid,
                                        collapse = ",")))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "uniparc/bestguess"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_uniparc_bestguess.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get UniParc Entries by Sequence
#'
#' Retrieve UniParc Entry by providing an exact sequence. Note that partial
#'   matches will not be accepted. You can also filter the returned content of
#'   the returned UniParc entry. see "Argument" section for more details.
#'
#' @section Corresponding API Resources:
#'  "POST https://ebi.ac.uk/proteins/api/uniparc/sequence"
#'
#' @param sequence Exact UniParc protein sequence. Partial matches will not be
#'   accepted.
#' @param rf_dd_type Filter the content of the UniParc entry by
#'   \href{https://www.uniprot.org/database/}{cross-reference} names. You can
#'   provide multiple values.
#' @param rf_db_id Filter the content of the UniParc entry by protein
#'   identifiers in any cross-reference database. You can provide multiple
#'   values.
#' @param rf_active (logical ) Filter the content of UniParc entry based on
#'   active status on source database:\cr\itemize{
#'   \item NA: don't filter contents based on active status.
#'   \item TRUE: only return contents which are still active.
#'   \item FALSE: Only return contents which are not active.}
#' @param rf_tax_id (Numeric) Filter the content of the UniParc entry by
#'   NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}. You can
#'   provide multiple values.
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list which correspond to a UniParc entry.
#'
#' @references \itemize{
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   }
#'
#' @examples
#' rba_uniprot_uniparc_sequence("GMRSCPRGCSQRGRCENGRCVCNPGYTGEDC")
#' @family "UniProt API, UniParc"
#' @export
rba_uniprot_uniparc_sequence = function(sequence,
                                        rf_dd_type = NA,
                                        rf_db_id = NA,
                                        rf_active = NA,
                                        rf_tax_id = NA,
                                        ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "rf_dd_type",
                               class = "character"),
                          list(arg = "rf_db_id",
                               class = "character"),
                          list(arg = "rf_active",
                               class = "logical"),
                          list(arg = "rf_tax_id",
                               class = "character")))

  v_msg("Retrieving UniParc entry that corresspond to your procided sequence.")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("rfDdtype",
                                 any(!is.na(rf_dd_type)),
                                 paste0(rf_dd_type,
                                        collapse = ",")),
                            list("rfDbid",
                                 any(!is.na(rf_db_id)),
                                 paste0(rf_db_id,
                                        collapse = ",")),
                            list("rfActive",
                                 !is.na(rf_active),
                                 ifelse(rf_active,
                                        "true",
                                        "false")),
                            list("rfTaxId",
                                 any(!is.na(rf_tax_id)),
                                 paste0(rf_tax_id,
                                        collapse = ",")))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "post",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "uniparc/sequence"),
                           query = call_query,
                           body = sequence,
                           accept = "application/json",
                           httr::content_type("text/plain"),
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_uniparc_sequence.json"))
  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
