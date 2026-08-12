#' Search UniParc Entries
#'
#' Use this function to search
#'   \href{https://www.uniprot.org/help/uniparc}{UniProt Archive (UniParc)}
#'   entries. Search by identifier, annotation, organism, sequence properties,
#'   or other supported criteria. The \code{rf_*} arguments filter the
#'   cross-references returned within matching entries; they do not select
#'   entries by themselves.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/uniparc"
#'
#' @param upi Character: (optional) Unique UniParc identifier(s). You can supply
#'   up to 100 IDs.
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param db_type Character: (optional)
#'   \href{https://www.uniprot.org/database/}{Cross-reference database} name.
#' @param db_id Character: (optional) Protein ID in a cross-reference database.
#'   You can supply up to 100 IDs.
#' @param gene Character: (optional)
#'   \href{https://www.uniprot.org/help/gene_name}{UniProt gene
#'   name(s)}. You can supply up to 20 gene names.
#' @param protein Character: (optional)
#'   \href{https://www.uniprot.org/help/protein_names}{UniProt
#'   protein name}.
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param organism Character: (optional)
#'   \href{https://www.uniprot.org/taxonomy/}{Organism name}.
#' @param sequence_checksum Character: (optional) A 16-character hexadecimal
#'   sequence CRC64 checksum.
#' @param ipr Character: (optional)
#'   \href{https://www.ebi.ac.uk/interpro/about/interpro/}{InterPro
#'   identifier(s)}. You can supply up to 20 IDs.
#' @param signature_db Character: (optional) InterPro
#'   \href{https://interpro-documentation.readthedocs.io/en/latest/databases.html}{signature
#'   database}. You can supply up to 20 values.
#' @param signature_id Character: (optional) Signature ID in an InterPro
#'   \href{https://interpro-documentation.readthedocs.io/en/latest/databases.html}{signature
#'   database}. You can supply up to 20 IDs.
#' @param upid Character: (optional)
#'   \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param seq_length Character or Numeric: (optional) An exact sequence length
#'   (e.g. 150) or a range of sequence lengths (e.g. "130-158").
#' @param rf_dd_type Character: (optional) Filter each UniParc entry's content by
#'   \href{https://www.uniprot.org/database/}{cross-reference} names. You can
#'   supply multiple values.
#' @param rf_db_id Character: (optional) Filter each UniParc entry's content by
#'   protein
#'   identifiers in any cross-reference database. You can supply multiple
#'   values.
#' @param rf_active Logical: (optional) Filter each UniParc entry's content by
#'   active status in the source database: \code{TRUE} retains active database
#'   references, \code{FALSE} retains inactive references, and \code{NULL}
#'   applies no active-status filter.
#' @param rf_tax_id Numeric: (optional) Filter each UniParc entry's content by
#'   NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}. You can
#'   supply multiple values.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list named by UniParc accession. Each element contains sequence
#'   information and cross-reference entries for one search hit.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_uniparc_search(upi = "UPI00000000C9")
#' }
#' \donttest{
#' rba_uniprot_uniparc_search(accession = "P30914")
#' }
#' \donttest{
#' rba_uniprot_uniparc_search(accession = "P30914", rf_active = TRUE)
#' }
#' \donttest{
#' rba_uniprot_uniparc_search(taxid = 694009, protein = "Nucleoprotein")
#' }
#'
#' @family "UniProt - UniParc"
#' @export
rba_uniprot_uniparc_search <- function(upi = NULL,
                                       accession = NULL,
                                       db_type = NULL,
                                       db_id = NULL,
                                       gene = NULL,
                                       protein = NULL,
                                       taxid = NULL,
                                       organism = NULL,
                                       sequence_checksum = NULL,
                                       ipr = NULL,
                                       signature_db = NULL,
                                       signature_id = NULL,
                                       upid = NULL,
                                       seq_length = NULL,
                                       rf_dd_type = NULL,
                                       rf_db_id = NULL,
                                       rf_active = NULL,
                                       rf_tax_id = NULL,
                                       ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "upi", class = "character", max_len = 100),
      list(arg = "accession", class = "character", max_len = 100),
      list(arg = "db_type", class = "character", len = 1L),
      list(arg = "db_id", class = "character", max_len = 100),
      list(arg = "gene", class = "character", max_len = 20),
      list(arg = "protein", class = "character", len = 1L),
      list(
        arg = "taxid", class = c("numeric", "integer"),
        max_len = 20, integerish = TRUE, min_val = 1
      ),
      list(arg = "organism", class = "character", len = 1L),
      list(
        arg = "sequence_checksum", class = "character", len = 1L,
        regex = "^[[:xdigit:]]{16}$"
      ),
      list(arg = "ipr", class = "character", max_len = 20),
      list(arg = "signature_db", class = "character", max_len = 20),
      list(arg = "signature_id", class = "character", max_len = 20),
      list(arg = "upid", class = "character", max_len = 100),
      list(
        arg = "seq_length", class = c("character", "numeric", "integer"),
        len = 1L, regex = "^[1-9]\\d*(?:-[1-9]\\d*)?$"
      ),
      list(arg = "rf_dd_type", class = "character"),
      list(arg = "rf_db_id", class = "character"),
      list(arg = "rf_active", class = "logical", len = 1L),
      list(
        arg = "rf_tax_id", class = c("numeric", "integer"),
        integerish = TRUE, min_val = 1
      )
    ),
    cond = list(
      list(
        quote(all(
          is.null(upi), is.null(accession), is.null(db_type), is.null(db_id),
          is.null(gene), is.null(protein), is.null(taxid), is.null(organism),
          is.null(sequence_checksum), is.null(ipr), is.null(signature_db),
          is.null(signature_id), is.null(upid), is.null(seq_length)
        )),
        "Supply at least one UniParc search criterion."
      ),
      list(
        quote(
          is.character(seq_length) &&
            grepl("-", seq_length, fixed = TRUE) &&
            diff(as.numeric(strsplit(seq_length, "-", fixed = TRUE)[[1]])) < 0
        ),
        "The start of a `seq_length` range cannot exceed its end."
      )
    )
  )

  .msg(
    "Searching UniParc and retrieving entries that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("upi", !is.null(upi), paste0(upi, collapse = ",")),
    list("accession",  !is.null(accession), paste0(accession, collapse = ",")),
    list("dbtype", !is.null(db_type), db_type),
    list("dbid", !is.null(db_id), paste0(db_id, collapse = ",")),
    list("gene", !is.null(gene), paste0(gene, collapse = ",")),
    list("protein", !is.null(protein), protein),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ",")),
    list("organism", !is.null(organism), organism),
    list("sequencechecksum", !is.null(sequence_checksum), sequence_checksum),
    list("ipr", !is.null(ipr), paste0(ipr, collapse = ",")),
    list("signaturetype", !is.null(signature_db), paste0(signature_db, collapse = ",")),
    list("signatureid", !is.null(signature_id), paste0(signature_id, collapse = ",")),
    list("upid", !is.null(upid), paste0(upid, collapse = ",")),
    list("seqLength", !is.null(seq_length), seq_length),
    list("rfDdtype", !is.null(rf_dd_type), paste0(rf_dd_type, collapse = ",")),
    list("rfDbid", !is.null(rf_db_id), paste0(rf_db_id, collapse = ",")),
    list("rfActive", !is.null(rf_active), ifelse(rf_active, "true", "false")),
    list("rfTaxId", !is.null(rf_tax_id), paste0(rf_tax_id, collapse = ","))
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "uniparc"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_uniparc_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get UniParc entry
#'
#' Retrieve UniParc entries using exactly one UniProt accession,
#'   cross-reference database ID, UniParc ID, or UniProt Proteome UPID. The
#'   \code{rf_*} arguments filter cross-references within returned entries.
#'   Database-reference and proteome lookups may return multiple UniParc
#'   entries.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/uniparc/accession/\{accession\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/uniparc/dbreference/\{dbid\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/uniparc/proteome/\{upid\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/uniparc/upi/\{upi\}"
#'
#' @param upi Character: (optional) Unique UniParc identifier.
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param db_id Character: (optional) Protein ID in a cross-reference database.
#' @param upid Character: (optional)
#'   \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}.
#' @param rf_dd_type Character: (optional) Filter the UniParc entry's content by
#'   \href{https://www.uniprot.org/database/}{cross-reference} names. You can
#'   supply multiple values.
#' @param rf_db_id Character: (optional) Filter the UniParc entry's content by
#'   protein
#'   identifiers in any cross-reference database. You can supply multiple
#'   values.
#' @param rf_active Logical: (optional) Filter the UniParc entry's content by
#'   active status in the source database: \code{TRUE} retains active database
#'   references, \code{FALSE} retains inactive references, and \code{NULL}
#'   applies no active-status filter.
#' @param rf_tax_id Numeric: (optional) Filter the UniParc entry's content by
#'   NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}. You can
#'   supply multiple values.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A UniParc entry, or a list of entries for a database-reference or
#'   proteome lookup.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_uniparc(accession = "P30914")
#' }
#' \donttest{
#' rba_uniprot_uniparc(upi = "UPI00000000C9")
#' }
#' \donttest{
#' rba_uniprot_uniparc(upi = "UPI00000000C9", rf_active = FALSE)
#' }
#'
#' @family "UniProt - UniParc"
#' @export
rba_uniprot_uniparc <- function(upi = NULL,
                                accession = NULL,
                                db_id = NULL,
                                upid = NULL,
                                rf_dd_type = NULL,
                                rf_db_id = NULL,
                                rf_active = NULL,
                                rf_tax_id = NULL,
                                ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1L),
      list(arg = "db_id", class = "character", len = 1L),
      list(arg = "upid", class = "character", len = 1L),
      list(arg = "upi", class = "character", len = 1L),
      list(arg = "rf_dd_type", class = "character"),
      list(arg = "rf_db_id", class = "character"),
      list(arg = "rf_active", class = "logical", len = 1L),
      list(
        arg = "rf_tax_id", class = c("numeric", "integer"),
        integerish = TRUE, min_val = 1
      )
    ),
    cond = list(
      list(
        quote(sum(!is.null(accession), !is.null(db_id), !is.null(upid), !is.null(upi)) != 1),
        "Supply exactly one of `accession`, `db_id`, `upid`, or `upi`."
      )
    )
  )

  if (!is.null(accession)) {
    id_description <- sprintf("UniProt accession %s", accession)
  } else if (!is.null(db_id)) {
    id_description <- sprintf("cross-reference database ID %s", db_id)
  } else if (!is.null(upid)) {
    id_description <- sprintf("UniProt Proteome ID %s", upid)
  } else {
    id_description <- sprintf("UniParc ID %s", upi)
  }

  .msg(
    "Retrieving UniParc entry with %s.",
    id_description
  )

  ## Build GET API Request's query
  if (!is.null(db_id) || !is.null(upid)) {
    call_query <- list("size" = "-1")
  } else {
    call_query <- list()
  }

  call_query <- .rba_query(
    init = call_query,
    list("rfDdtype", !is.null(rf_dd_type), paste0(rf_dd_type, collapse = ",")),
    list("rfDbid", !is.null(rf_db_id), paste0(rf_db_id, collapse = ",")),
    list("rfActive", !is.null(rf_active), ifelse(rf_active, "true", "false")),
    list("rfTaxId", !is.null(rf_tax_id), paste0(rf_tax_id, collapse = ","))
  )

  ## Build Function-Specific Call
  if (!is.null(accession)) {
    path_input <- paste0(.rba_stg("uniprot", "pth"), "uniparc/accession/", accession)
  } else if (!is.null(db_id)) {
    path_input <- paste0(.rba_stg("uniprot", "pth"), "uniparc/dbreference/", db_id)
  } else if (!is.null(upid)) {
    path_input <- paste0(.rba_stg("uniprot", "pth"),"uniparc/proteome/", upid)
  } else if (!is.null(upi)) {
    path_input <- paste0(.rba_stg("uniprot", "pth"), "uniparc/upi/", upi)
  }

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = path_input,
    query = call_query,
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_uniparc.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
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
#'  "GET https://www.ebi.ac.uk/proteins/api/uniparc/bestguess"
#'
#' @param upi Character: (optional) Unique UniParc identifier(s). You can supply
#'   up to 100 IDs.
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param db_id Character: (optional) Protein ID in a cross-reference database.
#'   You can supply up to 100 IDs.
#' @param gene Character: (optional)
#'   \href{https://www.uniprot.org/help/gene_name}{UniProt gene
#'   name(s)}. You can supply up to 20 gene names.
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID} used to refine the
#'   search.
#'   You can supply up to 20 taxon IDs.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return The best matching UniParc entry.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_uniparc_bestguess("UPI00000000C9")
#' }
#'
#' @family "UniProt - UniParc"
#' @export
rba_uniprot_uniparc_bestguess <- function(upi = NULL,
                                          accession = NULL,
                                          db_id = NULL,
                                          gene = NULL,
                                          taxid = NULL,
                                          ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "upi", class = "character", max_len = 100),
      list(arg = "accession", class = "character", max_len = 100),
      list(arg = "db_id", class = "character", max_len = 100),
      list(arg = "gene", class = "character", max_len = 20),
      list(
        arg = "taxid", class = c("numeric", "integer"),
        max_len = 20, integerish = TRUE, min_val = 1
      )
    ),
    cond = list(
      list(
        quote(all(
          is.null(upi), is.null(accession), is.null(db_id), is.null(gene)
        )),
        "Supply at least one of `upi`, `accession`, `db_id`, or `gene`."
      )
    )
  )

  .msg(
    "Retrieving UniParc longest Sequence."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list(),
    list("upi", !is.null(upi), paste0(upi, collapse = ",")),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("dbid", !is.null(db_id), paste0(db_id, collapse = ",")),
    list("gene", !is.null(gene), paste0(gene, collapse = ",")),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ","))
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "uniparc/bestguess"),
    query = call_query,
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_uniparc_bestguess.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get UniParc Entries by Sequence
#'
#' Retrieve a UniParc entry using an exact protein sequence. Partial matches
#'   are not accepted. The \code{rf_*} arguments filter cross-references within
#'   the returned entry.
#'
#' @section Corresponding API Resources:
#'  "POST https://www.ebi.ac.uk/proteins/api/uniparc/sequence"
#'
#' @param sequence Character: Exact protein sequence. Partial matches are not
#'   accepted.
#' @param rf_dd_type Character: (optional) Filter the UniParc entry's content by
#'   \href{https://www.uniprot.org/database/}{cross-reference} names. You can
#'   supply multiple values.
#' @param rf_db_id Character: (optional) Filter the UniParc entry's content by
#'   protein
#'   identifiers in any cross-reference database. You can supply multiple
#'   values.
#' @param rf_active Logical: (optional) Filter the UniParc entry's content by
#'   active status in the source database: \code{TRUE} retains active database
#'   references, \code{FALSE} retains inactive references, and \code{NULL}
#'   applies no active-status filter.
#' @param rf_tax_id Numeric: (optional) Filter the UniParc entry's content by
#'   NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}. You can
#'   supply multiple values.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return The matching UniParc entry.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_uniparc_sequence("GMRSCPRGCSQRGRCENGRCVCNPGYTGEDC")
#' }
#'
#' @family "UniProt - UniParc"
#' @export
rba_uniprot_uniparc_sequence <- function(sequence,
                                         rf_dd_type = NULL,
                                         rf_db_id = NULL,
                                         rf_active = NULL,
                                         rf_tax_id = NULL,
                                         ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "sequence", class = "character", len = 1L),
      list(arg = "rf_dd_type", class = "character"),
      list(arg = "rf_db_id", class = "character"),
      list(arg = "rf_active", class = "logical", len = 1L),
      list(
        arg = "rf_tax_id", class = c("numeric", "integer"),
        integerish = TRUE, min_val = 1
      )
    )
  )

  .msg(
    "Retrieving the UniParc entry corresponding to the supplied sequence."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list(),
    list("rfDdtype", !is.null(rf_dd_type), paste0(rf_dd_type, collapse = ",")),
    list("rfDbid", !is.null(rf_db_id), paste0(rf_db_id, collapse = ",")),
    list("rfActive", !is.null(rf_active), ifelse(rf_active, "true", "false")),
    list("rfTaxId", !is.null(rf_tax_id), paste0(rf_tax_id, collapse = ","))
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "uniparc/sequence"),
    query = call_query,
    body = sequence,
    accept = "application/json",
    httr::content_type("text/plain"),
    parser = "json->list",
    save_to = .rba_file("uniprot_uniparc_sequence.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
