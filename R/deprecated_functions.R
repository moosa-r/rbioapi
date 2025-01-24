#' Search Proteomics Peptides in UniProt (Deprecated)
#'
#' This function is Deprecated. Please use
#'   \code{\link{rba_uniprot_proteomics_non_ptm_search}} instead.\cr
#'   UniProt maps proteomics peptides from different sources to the proteins'
#'   sequences. Using this function, you can search for  proteomics
#'   peptides that has been map to UniProt proteins. You may also refine your
#'   search with modifiers such as data_source, peptide etc. See
#'   "Arguments section" for more information.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.
#'   \cr see also: \href{https://www.uniprot.org/help/proteomics}{Mass
#'   spectrometry-based proteomics data in UniProtKB}
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param data_source  Proteomics data source. You can choose up to two of:
#'   \itemize{
#'   \item \href{https://www.uniprot.org/database/DB-0186}{"MaxQB"}
#'   \item \href{https://www.uniprot.org/database/DB-0071}{"PeptideAtlas"}
#'   \item \href{https://www.uniprot.org/database/DB-0205}{"EPD"}
#'   \item \href{https://www.uniprot.org/database/DB-0229}{"ProteomicsDB"}
#'   }
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param upid \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param peptide Peptide sequence(s). You can supply up to 20 sequences.
#' @param unique Logical: Should the results be filtered based on the
#'   Peptide's uniqueness (the fact that a peptide maps to only 1 protein). If
#'   TRUE, Only unique peptides will be returned, if FALSE only un-unique
#'   peptides will be returned; If NULL (default) the results will not be
#'   filtered based on this.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list Where each element correspond to a UniProt protein and
#'   proteomics peptides are organized under the "features" sub-list.
#'
#' @references \itemize{
#'   \item The UniProt Consortium, UniProt: the universal protein
#'   knowledgebase in 2021, Nucleic Acids Research, Volume 49, Issue D1,
#'   8 January 2021, Pages D480–D489, https://doi.org/10.1093/nar/gkaa1100
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \dontrun{
#' #Deprecated
#' rba_uniprot_proteomics_search(peptide = "MEDYTKIEK")
#' }
#' \dontrun{
#' #Deprecated
#' rba_uniprot_proteomics_search(peptide = "MEDYTKIEK")
#' }
#' \dontrun{
#' #Deprecated
#' ### this will generate a very large response!
#'   rba_uniprot_proteomics_search(taxid = 9606,
#'   data_source = "PeptideAtlas",
#'   progress = TRUE, timeout = 999999, unique = TRUE)
#' }
#'
#' @family "Deprecated functions"
#' @export
rba_uniprot_proteomics_search <- function(accession = NULL,
                                          data_source = NULL,
                                          taxid = NULL,
                                          upid = NULL,
                                          peptide = NULL,
                                          unique = NULL,
                                          ...) {

  .Deprecated(new = "rba_uniprot_proteomics_non_ptm_search")

  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", max_len = 100),
      list(arg = "taxid", class = "numeric", max_len = 20),
      list(arg = "upid", class = "character", max_len = 100),
      list(
        arg = "data_source", class = "character", max_len = 2,
        vals = c("MaxQB", "PeptideAtlas", "EPD", "ProteomicsDB")
      ),
      list(arg = "peptide", class = "character", max_len = 20),
      list(arg = "unique", class = "logical")
    )
  )

  .msg(
    "Searching UniProt and retrieving proteomics peptides features of proteins that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ",")),
    list("upid", !is.null(upid), paste0(upid, collapse = ",")),
    list("data_source", !is.null(data_source), paste0(data_source, collapse = ",")),
    list("peptide", !is.null(peptide), paste0(peptide, collapse = ",")),
    list("unique", !is.null(unique), ifelse(unique, "true", "false"))
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "proteomics"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_proteomics_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Proteomics Peptides Mapped to UniProt Protein (Deprecated)
#'
#' This function is Deprecated. Please use
#'   \code{\link{rba_uniprot_proteomics_non_ptm}} instead.\cr
#' UniProt maps proteomics peptides from different sources to the proteins'
#'   sequences. Using this function, you can retrieve all the proteomics
#'   peptides features that has been map to a given UniProt protein's sequence.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics/\{accession\}"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the proteomics peptides features of your supplied
#'   UniProt protein's sequence.
#'
#' @references \itemize{
#'   \item The UniProt Consortium, UniProt: the universal protein
#'   knowledgebase in 2021, Nucleic Acids Research, Volume 49, Issue D1,
#'   8 January 2021, Pages D480–D489, https://doi.org/10.1093/nar/gkaa1100
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \dontrun{
#' #Deprecated
#' rba_uniprot_proteomics(accession = "P25942")
#' }
#'
#' @family "Deprecated functions"
#' @export
rba_uniprot_proteomics <- function(accession,
                                   ...) {

  .Deprecated("rba_uniprot_proteomics_non_ptm")

  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1)
    )
  )

  .msg(
    "Retrieving proteomics peptides features mapped to the sequence of protein %s.",
    accession
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "proteomics/", accession),
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_proteomics.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Search Post-Translational Modification in UniProt (Deprecated)
#'
#' This function is Deprecated. Please use
#'   \code{\link{rba_uniprot_proteomics_ptm_search}} instead.\cr
#' UniProt maps proteomics peptides from different sources to the proteins'
#'   sequences. Using this function, you can search for  proteomics
#'   peptides that has been map to UniProt proteins. You may also refine your
#'   search with modifiers such as data_source, peptide etc. See
#'   "Arguments section" for more information.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.
#'   \cr see also:
#'   \href{https://www.uniprot.org/help/ptm_processing_section}{PTM /
#'   Processing section in UniProtKB}
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics-ptm"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param ptm Post-translational modification name
#' @param data_source Proteomics data source. You can choose up to two of:
#'   \itemize{
#'   \item \href{https://www.uniprot.org/database/DB-0186}{"MaxQB"}
#'   \item \href{https://www.uniprot.org/database/DB-0071}{"PeptideAtlas"}
#'   \item \href{https://www.uniprot.org/database/DB-0205}{"EPD"}
#'   \item \href{https://www.uniprot.org/database/DB-0229}{"ProteomicsDB"}
#'   }
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param upid \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param peptide Peptide sequence(s). You can supply up to 20 sequences.
#' @param unique Logical: Should the results be filtered based on the
#'   Peptide's uniqueness (the fact that a peptide maps to only 1 protein). If
#'   TRUE, Only unique peptides will be returned, if FALSE only un-unique
#'   peptides will be returned; If NULL (default) the results will not be
#'   filtered based on this.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list Where each element correspond to a UniProt protein and
#'   post-translational modification are organized under the "features"
#'   sub-list.
#'
#' @references \itemize{
#'   \item The UniProt Consortium, UniProt: the universal protein
#'   knowledgebase in 2021, Nucleic Acids Research, Volume 49, Issue D1,
#'   8 January 2021, Pages D480–D489, https://doi.org/10.1093/nar/gkaa1100
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \dontrun{
#' #Deprecated
#' rba_uniprot_ptm_search(peptide = "NDQVYQPLRDRDDAQYSHLGGNWAR")
#' }
#'
#' @family "Deprecated functions"
#' @export
rba_uniprot_ptm_search <- function(accession = NULL,
                                   ptm = NULL,
                                   data_source = NULL,
                                   taxid = NULL,
                                   upid = NULL,
                                   peptide = NULL,
                                   unique = NULL,
                                   ...) {

  .Deprecated("rba_uniprot_proteomics_ptm_search")

  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", max_len = 100),
      list(arg = "ptm", class = "character", len = 1),
      list(arg = "taxid", class = "numeric", max_len = 20),
      list(arg = "upid", class = "character", max_len = 100),
      list(
        arg = "data_source", class = "character", max_len = 2,
        val = c("MaxQB", "PeptideAtlas", "EPD", "ProteomicsDB")
      ),
      list(arg = "peptide", class = "character", max_len = 20),
      list(arg = "unique", class = "logical")
    )
  )

  .msg(
    "Searching UniProt and retrieving proteomics Post-translational modification features of proteins that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("ptm", !is.null(ptm), ptm),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ",")),
    list("upid", !is.null(upid), paste0(upid, collapse = ",")),
    list("data_source", !is.null(data_source), paste0(data_source, collapse = ",")),
    list("peptide", !is.null(peptide), paste0(peptide, collapse = ",")),
    list("unique", !is.null(unique), ifelse(unique, "true", "false"))
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "proteomics-ptm"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_ptm_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Post-Translational Modification of UniProt Protein (Deprecated)
#'
#' This function is Deprecated. Please use
#'   \code{\link{rba_uniprot_proteomics_ptm}} instead.\cr
#' UniProt maps post-translational modification features from different sources
#'   to the proteins'  sequences. Using this function, you can retrieve all
#'   the post-translational modification features that has been map to a given
#'   UniProt protein's sequence.
#'
#'   see also:
#'   \href{https://www.uniprot.org/help/ptm_processing_section}{PTM /
#'   Processing section in UniProtKB}
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics-ptm/\{accession\}"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the post-translational modification features of
#' your supplied UniProt protein's sequence.
#'
#' @references \itemize{
#'   \item The UniProt Consortium, UniProt: the universal protein
#'   knowledgebase in 2021, Nucleic Acids Research, Volume 49, Issue D1,
#'   8 January 2021, Pages D480–D489, https://doi.org/10.1093/nar/gkaa1100
#'   \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
#'   Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
#'   Turner, Maria Martin, The Proteins API: accessing key integrated protein
#'   and genome information, Nucleic Acids Research, Volume 45, Issue W1,
#'   3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \dontrun{
#' #Deprecated
#' rba_uniprot_ptm(accession = "P04234")
#' }
#'
#' @family "Deprecated functions"
#' @export
rba_uniprot_ptm <- function(accession,
                            ...) {

  .Deprecated("rba_uniprot_proteomics_ptm")

  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(list(arg = "accession", class = "character", len = 1)
    )
  )

  .msg(
    "Retrieving proteomics Post-translational modification features mapped to the sequence of protein %s.",
    accession
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "proteomics-ptm/", accession),
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_ptm.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
