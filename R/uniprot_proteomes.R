#### Proteomes Endpoints ####
#' Search Proteomes in UniProt
#'
#' UniProt collects and annotates proteomes (protein sets expressed in an
#'   organism). Search available proteomes by name, identifier, taxonomy,
#'   keyword, cross-reference, genome accession, or status. See
#'   \href{https://www.uniprot.org/help/proteome}{What are proteomes?} for
#'   more information.
#'
#' At least one search criterion is required.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomes"
#'
#' @param name Character: (optional) A term in the proteome name.
#' @param upid Character: (optional)
#'   \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param keyword Character: (optional) Limit the search to entries containing
#'   the keyword. See \href{https://www.uniprot.org/keywords/}{UniProt Keywords}.
#' @param xref Character: (optional) Proteome cross-references such as genome
#'   assembly ID or
#'   Biosample ID. You can supply up to 20 cross-reference IDs.
#' @param genome_acc Character: (optional) Genome accession associated with the
#'   proteome's components. You can supply up to 20 accessions.
#' @param is_ref_proteome Logical: (optional) If \code{TRUE}, return only
#'   reference proteomes; if \code{FALSE}, return only non-reference proteomes;
#'   if \code{NULL}, do not filter by this criterion. See
#'   \href{https://www.uniprot.org/help/reference_proteome}{'What are reference
#'   proteomes?'} for more information.
#' @param is_redundant Logical: (optional) If \code{TRUE}, return only redundant
#'   proteomes; if \code{FALSE}, return only non-redundant proteomes; if
#'   \code{NULL}, do not filter by redundancy. See
#'   \href{https://www.uniprot.org/help/proteome_redundancy}{'Reducing proteome
#'   redundancy'} for more information.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list named by UPID. Each element contains one matching proteome's
#'   metadata.
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
#' rba_uniprot_proteomes_search(name = "SARS-CoV")
#' }
#' \donttest{
#' rba_uniprot_proteomes_search(genome_acc = "AY274119")
#' }
#'
#' @family "UniProt - Proteomes"
#' @export
rba_uniprot_proteomes_search <- function(name = NULL,
                                         upid = NULL,
                                         taxid = NULL,
                                         keyword = NULL,
                                         xref = NULL,
                                         genome_acc = NULL,
                                         is_ref_proteome = NULL,
                                         is_redundant = NULL,
                                         ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "upid", class = "character", max_len = 100),
      list(arg = "name", class = "character", len = 1L),
      list(
        arg = "taxid", class = c("numeric", "integer"),
        max_len = 20, integerish = TRUE, min_val = 1
      ),
      list(arg = "keyword", class = "character", len = 1L),
      list(arg = "xref", class = "character", max_len = 20),
      list(arg = "genome_acc", class = "character", max_len = 20),
      list(arg = "is_ref_proteome", class = "logical", len = 1L),
      list(arg = "is_redundant", class = "logical", len = 1L)
    ),
    cond = list(
      list(
        quote(all(
          is.null(name), is.null(upid), is.null(taxid), is.null(keyword),
          is.null(xref), is.null(genome_acc), is.null(is_ref_proteome),
          is.null(is_redundant)
        )),
        "Supply at least one proteome search criterion."
      )
    )
  )

  .msg(
    "Searching UniProt and retrieving proteomes that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("name", !is.null(name), name),
    list("upid", !is.null(upid), paste0(upid, collapse = ",")),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ",")),
    list("keyword", !is.null(keyword), keyword),
    list("xref", !is.null(xref), paste0(xref, collapse = ",")),
    list("genome_acc", !is.null(genome_acc), paste0(genome_acc, collapse = ",")),
    list("is_ref_proteome", !is.null(is_ref_proteome), ifelse(is_ref_proteome, "true", "false")),
    list("is_redundant", !is.null(is_redundant), ifelse(is_redundant, "true", "false"))
  )

  ## Build Function-Specific Call
  parser_input <- list(
    "json->list",
    function(x) {
      .rba_uniprot_search_namer(x, field = "upid")
    }
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "proteomes"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_proteomes_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get a Proteome by UPID
#'
#' UniProt collects and annotates proteomes (protein sets expressed in an
#'   organism). Retrieve a proteome's metadata by UPID, optionally including
#'   its proteins. When proteins are requested, they can be filtered by
#'   UniProtKB review status. See
#'   \href{https://www.uniprot.org/help/proteome}{What are proteomes?} for
#'   more information.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomes/proteins/\{upid\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/proteomes/\{upid\}"
#'
#' @param upid Character:
#'   \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}.
#' @param get_proteins Logical: (default = \code{FALSE}) If \code{TRUE}, embed
#'   the proteins belonging to the supplied proteome in its genome components.
#' @param reviewed Logical: (optional) Used only when \code{get_proteins} is
#'   \code{TRUE}. If \code{TRUE}, return only reviewed UniProtKB/Swiss-Prot
#'   proteins; if \code{FALSE}, return only unreviewed UniProtKB/TrEMBL entries;
#'   if \code{NULL}, do not filter by review status.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the requested proteome. With
#'   \code{get_proteins = TRUE}, protein entries are included under each
#'   element of \code{component}.
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
#' rba_uniprot_proteomes(upid = "UP000000354")
#' }
#' \donttest{
#' rba_uniprot_proteomes(upid = "UP000000354", get_proteins = TRUE)
#' }
#'
#' @family "UniProt - Proteomes"
#' @export
rba_uniprot_proteomes <- function(upid,
                                  get_proteins = FALSE,
                                  reviewed = NULL,
                                  ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "upid", class = "character", len = 1L),
      list(
        arg = "get_proteins", class = "logical", len = 1L,
        no_null = TRUE
      ),
      list(arg = "reviewed", class = "logical", len = 1L)
    ),
    cond = list(
      list(
        quote(isFALSE(get_proteins) && !is.null(reviewed)),
        "`reviewed` is ignored because `get_proteins` is FALSE."
      )
    ),
    cond_warning = TRUE
  )

  .msg(
    "Retrieving proteome %s %s.",
    upid,
    if (!isTRUE(get_proteins)) {
      "without proteins"
    } else if (is.null(reviewed)) {
      "with all proteins"
    } else if (isTRUE(reviewed)) {
      "with only reviewed UniProtKB/Swiss-Prot proteins"
    } else {
      "with only unreviewed UniProtKB/TrEMBL proteins"
    }
  )

  ## Build Function-Specific Call
  if (isTRUE(get_proteins)) {

    ## Build GET API Request's query
    call_query <- .rba_query(
      init = list(),
      list("reviewed", !is.null(reviewed), ifelse(reviewed, "true", "false"))
    )

    path_input <- paste0(.rba_stg("uniprot", "pth"), "proteomes/proteins/", upid)

  } else {

    call_query <- NULL
    path_input <- paste0(.rba_stg("uniprot", "pth"), "proteomes/", upid)

  }

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = path_input,
    query = call_query,
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_proteomes.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#### Genecentric Endpoints ####

#' Search Gene-Centric Proteins
#'
#' UniProt gene-centric protein groups organize related protein entries from a
#'   proteome by gene. Search these groups by proteome, accession, or gene
#'   identifier. For more information, see
#'   \href{https://www.uniprot.org/help/proteome}{What are proteomes?} and
#'   \href{https://www.uniprot.org/help/gene_centric_isoform_mapping}{Automatic
#'   gene-centric isoform mapping for eukaryotic reference proteome entries.}
#'
#' At least one search criterion is required.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/genecentric"
#'
#' @param upid Character: (optional)
#'   \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param gene Character: (optional) Unique gene identifier(s) found in MOD,
#'   \href{https://www.ensembl.org/info/genome/genebuild/gene_names.html}{Ensembl},
#'   Ensembl Genomes, \href{https://www.uniprot.org/help/gene_name}{OLN},
#'   \href{https://www.uniprot.org/help/gene_name}{ORF} or
#'   \href{https://www.uniprot.org/help/gene_name}{UniProt Gene Name}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing matching gene-centric protein groups.
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
#' rba_uniprot_genecentric_search(accession = "P59594")
#' }
#' \donttest{
#' rba_uniprot_genecentric_search(gene = "Spike")
#' }
#' \donttest{
#' rba_uniprot_genecentric_search(upid = "UP000000354")
#' }
#'
#' @family "UniProt - Proteomes"
#' @export
rba_uniprot_genecentric_search <- function(upid = NULL,
                                           accession = NULL,
                                           gene = NULL,
                                           ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "upid", class = "character", max_len = 100),
      list(arg = "accession", class = "character", max_len = 100),
      list(arg = "gene", class = "character", max_len = 20)
    ),
    cond = list(
      list(
        quote(all(is.null(upid), is.null(accession), is.null(gene))),
        "Supply at least one search criterion: upid, accession, or gene."
      )
    )
  )

  .msg(
    "Searching UniProt and retrieving Gene-Centric Proteins that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("upid", !is.null(upid), paste0(upid, collapse = ",")),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("gene", !is.null(gene), paste0(gene, collapse = ","))
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "genecentric"),
    query = call_query,
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_genecentric_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Gene-Centric proteins by UniProt Accession
#'
#' Using this function you can retrieve gene-centrics data. For more
#'   information, see \href{https://www.uniprot.org/help/proteome}{What are
#'   proteomes?} and
#'   \href{https://www.uniprot.org/help/gene_centric_isoform_mapping}{Automatic
#'   gene-centric isoform mapping for eukaryotic reference proteome entries.}.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/genecentric/\{accession\}"
#'
#' @param accession Character:
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the requested gene-centric protein group.
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
#' rba_uniprot_genecentric("P29965")
#' }
#'
#' @family "UniProt - Proteomes"
#' @export
rba_uniprot_genecentric <- function(accession,
                                    ...) {
  ## Load Global Options
  .rba_ext_args(...)
  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1L)
    )
  )

  .msg(
    "Retrieving Gene-Centric proteins by UniProt Accession %s.",
    accession
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "genecentric/", accession),
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("uniprot_genecentric.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
