#' Search Genomic Coordinates of UniProt entries
#'
#' Search \href{https://www.uniprot.org/help/genomic_coordinates}{genomic
#'   coordinates} associated with UniProt entries by accession, chromosome,
#'   Ensembl identifier, gene, protein, taxonomy, or genomic range.
#'
#' At least one search criterion is required.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/coordinates"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param chromosome Character or Numeric: (optional) Chromosome name, such as
#'   "X", "Y", 1, or 20. You can supply up to 20 values.
#' @param ensembl_id Character: (optional) Ensembl stable gene ID, transcript
#'   ID, or translation ID. You can supply up to 20 IDs.
#' @param gene Character: (optional)
#'   \href{https://www.uniprot.org/help/gene_name}{UniProt gene
#'   name(s)}. You can supply up to 20 gene names.
#' @param protein Character: (optional)
#'   \href{https://www.uniprot.org/help/protein_names}{UniProt protein name}.
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param location Character: (optional) Genome location range, such as
#'   "58205437-58219305".
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list named by accession. Each element contains one matching
#'   protein's genomic-coordinate information.
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
#'   \item McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin,
#'   M. J., Wu, C., & The UniProt Consortium. (2019). UniProt genomic
#'   mapping for deciphering functional effects of missense variants.
#'   Human Mutation, 40(6), 694–705.
#'   https://doi.org/10.1002/humu.23738
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_coordinates_search(taxid = 9606, chromosome = "y")
#' }
#'
#' @family "UniProt - Coordinates"
#' @export
rba_uniprot_coordinates_search <- function(accession = NULL,
                                           chromosome = NULL,
                                           ensembl_id = NULL,
                                           gene = NULL,
                                           protein = NULL,
                                           taxid = NULL,
                                           location = NULL,
                                           ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", max_len = 100),
      list(
        arg = "chromosome", class = c("character", "numeric", "integer"),
        max_len = 20
      ),
      list(arg = "ensembl_id", class = "character", max_len = 20),
      list(arg = "gene", class = "character", max_len = 20),
      list(arg = "protein", class = "character", len = 1L),
      list(
        arg = "taxid", class = c("numeric", "integer"), max_len = 20,
        min_val = 1
      ),
      list(arg = "location", class = "character", len = 1L)
    ),
    cond = list(
      list(
        quote(
          all(
            is.null(accession), is.null(chromosome), is.null(ensembl_id),
            is.null(gene), is.null(protein), is.null(taxid), is.null(location)
          )
        ),
        "Supply at least one search criterion: accession, chromosome, ensembl_id, gene, protein, taxid, or location."
      ),
      list(
        quote(!is.null(taxid) && any(!is.finite(taxid) | taxid %% 1 != 0)),
        "`taxid` values should be finite, positive whole numbers."
      )
    )
  )

  .msg(
    "Searching UniProt and retrieving Coordinates of proteins that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("chromosome", !is.null(chromosome), paste0(chromosome, collapse = ",")),
    list("ensembl", !is.null(ensembl_id), paste0(ensembl_id, collapse = ",")),
    list("gene", !is.null(gene), paste0(gene, collapse = ",")),
    list("protein", !is.null(protein), protein),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ",")),
    list("location", !is.null(location), location)
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "coordinates"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_coordinates_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Map Protein Sequence Positions to Genomic Coordinates
#'
#' Map an amino-acid position or range in a UniProt protein sequence to its
#'   corresponding
#'   \href{https://www.uniprot.org/help/genomic_coordinates}{genomic
#'   coordinates}. A protein sequence location may have more than one genomic
#'   mapping. Supply \code{p_position} alone, or supply \code{p_start} and
#'   \code{p_end} together.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/coordinates/location/\{accession\}:\{pPosition\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/coordinates/location/\{accession\}:\{pStart\}-\{pEnd\}"
#'
#' @param accession Character:
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param p_position Numeric: (optional) Protein sequence position. Supply this
#'   alone, or supply both \code{p_start} and \code{p_end}.
#' @param p_start Numeric: (optional) Protein sequence range start.
#' @param p_end Numeric: (optional) Protein sequence range end.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list with a \code{locations} element containing the mapped
#'   protein and genomic boundaries. Records can include chromosome, strand,
#'   genome assembly, nucleotide and Ensembl identifiers, amino acids, and
#'   mapped sequence features.
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
#'   \item McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin,
#'   M. J., Wu, C., & The UniProt Consortium. (2019). UniProt genomic
#'   mapping for deciphering functional effects of missense variants.
#'   Human Mutation, 40(6), 694–705.
#'   https://doi.org/10.1002/humu.23738
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_coordinates_location_protein(accession = "P25942", p_position = 1)
#' }
#' \donttest{
#' rba_uniprot_coordinates_location_protein(accession = "P25942",
#'     p_start = 1, p_end = 277)
#' }
#'
#' @family "UniProt - Coordinates"
#' @export
rba_uniprot_coordinates_location_protein <- function(accession,
                                                     p_position = NULL,
                                                     p_start = NULL,
                                                     p_end = NULL,
                                                     ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1L),
      list(
        arg = "p_position", class = c("numeric", "integer"), len = 1L,
        min_val = 1
      ),
      list(
        arg = "p_start", class = c("numeric", "integer"), len = 1L,
        min_val = 1
      ),
      list(
        arg = "p_end", class = c("numeric", "integer"), len = 1L,
        min_val = 1
      )
    ),
    cond = list(
      list(
        quote(
          (!is.null(p_position) &&
            (!is.null(p_start) || !is.null(p_end))) ||
            (is.null(p_position) &&
              (is.null(p_start) || is.null(p_end)))
        ),
        "Supply either `p_position` alone or `p_start` and `p_end` together."
      ),
      list(
        quote(
          any(
            !is.finite(c(p_position, p_start, p_end)) |
              c(p_position, p_start, p_end) %% 1 != 0
          )
        ),
        "Protein positions should be finite, positive whole numbers."
      ),
      list(
        quote(!is.null(p_start) && !is.null(p_end) && p_start > p_end),
        "`p_start` cannot exceed `p_end`."
      )
    )
  )

  .msg(
    "Retrieving genome coordinates of protein %s in sequence position %s.",
    accession,
    ifelse(is.null(p_position), yes = paste(p_start, p_end, sep = " to "), no = p_position)
  )

  ## Build Function-Specific Call
  path_input <- sprintf(
    "%scoordinates/location/%s:%s",
    .rba_stg("uniprot", "pth"),
    accession,
    ifelse(!is.null(p_position), yes = p_position, no = paste0(p_start, "-", p_end))
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = path_input,
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("uniprot_coordinates_location.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Genomic Coordinates of a Protein
#'
#' Retrieve \href{https://www.uniprot.org/help/genomic_coordinates}{genomic
#'   coordinates} for a protein using either its UniProt accession or its ID
#'   in a cross-reference database (Ensembl, CCDS, HGNC, or RefSeq). You
#'   should supply either \code{accession} alone or \code{db_type} and
#'   \code{db_id} together.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/coordinates/\{accession\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/coordinates/\{dbtype\}:\{dbid\}"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param db_type Character: (optional) Cross-reference database name. One of
#'   "Ensembl", "CCDS", "HGNC", or "RefSeq".
#' @param db_id Character: (optional) Protein identifier in the
#'   cross-reference database.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the requested protein's genomic coordinates.
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
#'   \item McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin,
#'   M. J., Wu, C., & The UniProt Consortium. (2019). UniProt genomic
#'   mapping for deciphering functional effects of missense variants.
#'   Human Mutation, 40(6), 694–705.
#'   https://doi.org/10.1002/humu.23738
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_coordinates(accession = "P25942")
#' }
#' \donttest{
#' rba_uniprot_coordinates(db_type = "HGNC", db_id = "CD40")
#' }
#'
#' @family "UniProt - Coordinates"
#' @export
rba_uniprot_coordinates <- function(accession = NULL,
                                    db_type = NULL,
                                    db_id = NULL,
                                    ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1L),
      list(
        arg = "db_type", class = "character", len = 1L,
        val = c("Ensembl", "CCDS", "HGNC", "RefSeq")
      ),
      list(arg = "db_id", class = "character", len = 1L)
    ),
    cond = list(
      list(
        quote(any(sum(!is.null(accession), !is.null(db_type), !is.null(db_id)) == 3,
                  sum(!is.null(accession), !is.null(db_type), !is.null(db_id)) == 0,
                  sum(!is.null(db_type), !is.null(db_id)) == 1)),
        "Supply either `accession` alone or `db_type` and `db_id` together."
      )
    )
  )

  .msg(
    "Retrieving genome coordinates of protein with ID: %s",
    ifelse(
      is.null(accession),
      yes = sprintf("%s in %s database", db_id, db_type),
      no = accession
    )
  )

  ## Build GET API Request's query
  if (is.null(accession)) {
    call_query <- list("size" = "-1")
  } else {
    call_query <- NULL
  }

  ## Build Function-Specific Call
  path_input <- sprintf(
    "%scoordinates/%s",
    .rba_stg("uniprot", "pth"),
    ifelse(!is.null(accession), yes = accession, no = paste0(db_type, ":", db_id))
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = path_input,
    query = call_query,
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_coordinates.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Search UniProt entries by taxonomy and genomic coordinates
#'
#' Retrieve UniProt entries or mapped protein features for a taxon and
#'   supplied \href{https://www.uniprot.org/help/genomic_coordinates}{genomic
#'   coordinates}.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/coordinates/\{taxonomy\}/\{locations\}/feature"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/coordinates/\{taxonomy\}/\{locations\}"
#'
#' @param taxid Numeric: NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#' @param locations Character: Genomic location formatted as
#'   chromosome:start-end.
#'  (e.g. "Y:17100001-19600000"). If you omit chromosome, it will be interpreted
#'  as any chromosome (e.g. "1-10000").
#' @param in_range Logical: (default = \code{TRUE}) If \code{TRUE}, return only
#'   proteins that are fully contained in the supplied range.
#' @param feature Logical: (default = \code{FALSE}) If \code{TRUE}, return
#'   mapped protein features rather than protein coordinate records.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing UniProt proteins that match the supplied genomic
#'   location and taxonomy ID.
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
#'   \item McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin,
#'   M. J., Wu, C., & The UniProt Consortium. (2019). UniProt genomic
#'   mapping for deciphering functional effects of missense variants.
#'   Human Mutation, 40(6), 694–705.
#'   https://doi.org/10.1002/humu.23738
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_uniprot_coordinates_location(taxid = 9606,
#'     locations = "Y:17100001-19600000", in_range = TRUE)
#' }
#' \donttest{
#' rba_uniprot_coordinates_location(taxid = 9606,
#'     locations = "20:39000001", in_range = FALSE)
#' }
#'
#' @family "UniProt - Coordinates"
#' @export
rba_uniprot_coordinates_location <- function(taxid,
                                             locations,
                                             in_range = TRUE,
                                             feature = FALSE,
                                             ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "taxid", class = c("numeric", "integer"), len = 1L,
        min_val = 1
      ),
      list(arg = "locations", class = "character", len = 1L),
      list(
        arg = "in_range", class = "logical", len = 1L, no_null = TRUE
      ),
      list(arg = "feature", class = "logical", len = 1L, no_null = TRUE)
    ),
    cond = list(
      list(
        quote(!is.finite(taxid) || taxid %% 1 != 0),
        "`taxid` should be a finite, positive whole number."
      )
    )
  )

  .msg(
    "Retrieving UniProt entries in location %s of taxon %s.",
    locations, taxid
  )

  ## Build GET API Request's query
  call_query <- list(
    "size" = "-1",
    "in_range" = ifelse(in_range, "true", "false")
  )

  ## Build Function-Specific Call
  path_input <- sprintf(
    "%scoordinates/%s/%s",
    .rba_stg("uniprot", "pth"),
    taxid,
    locations
  )

  if (isTRUE(feature)) {
    path_input <- paste0(path_input, "/feature")
  }

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = path_input,
    query = call_query,
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_coordinates_location_protein.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Map Genomic Coordinates to Protein Sequence Positions
#'
#' Map a genomic position or range within a chromosome and taxon to the
#'   corresponding UniProt protein sequence locations. A genomic location may
#'   match multiple proteins, isoforms, or transcript mappings. Supply
#'   \code{g_position} alone, or supply \code{g_start} and \code{g_end}
#'   together.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/coordinates/glocation/\{taxonomy\}/\{chromosome\}:\{gPosition\}"
#'  \cr "GET https://www.ebi.ac.uk/proteins/api/coordinates/glocation/\{taxonomy\}/\{chromosome\}:\{gStart\}-\{gEnd\}"
#'
#' @param taxid Numeric: NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#' @param chromosome Character or Numeric: Chromosome name, e.g. 1, 20, or X.
#' @param g_position Numeric: (optional) Genomic position. Supply this alone,
#'   or supply both \code{g_start} and \code{g_end}.
#' @param g_start Numeric: (optional) Genomic range start.
#' @param g_end Numeric: (optional) Genomic range end.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list with a \code{locations} element containing the matching
#'   UniProt protein and genomic mappings. Records can include protein
#'   positions, amino acids, transcript and translation identifiers,
#'   chromosome, strand, genome assembly, and mapped sequence features.
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
#'   \item McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin,
#'   M. J., Wu, C., & The UniProt Consortium. (2019). UniProt genomic
#'   mapping for deciphering functional effects of missense variants.
#'   Human Mutation, 40(6), 694–705.
#'   https://doi.org/10.1002/humu.23738
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \donttest{
#'  rba_uniprot_coordinates_location_genome(
#'  taxid = 9606, chromosome = 11, g_position = 36573305)
#' }
#'
#' @family "UniProt - Coordinates"
#' @export
rba_uniprot_coordinates_location_genome <- function(taxid,
                                                    chromosome,
                                                    g_position = NULL,
                                                    g_start = NULL,
                                                    g_end = NULL,
                                                    ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "taxid", class = c("numeric", "integer"), len = 1L,
        min_val = 1
      ),
      list(
        arg = "chromosome", class = c("numeric", "integer", "character"),
        len = 1L
      ),
      list(
        arg = "g_position", class = c("numeric", "integer"), len = 1L,
        min_val = 1
      ),
      list(
        arg = "g_start", class = c("numeric", "integer"), len = 1L,
        min_val = 1
      ),
      list(
        arg = "g_end", class = c("numeric", "integer"), len = 1L,
        min_val = 1
      )
    ),
    cond = list(
      list(
        quote(
          (!is.null(g_position) &&
            (!is.null(g_start) || !is.null(g_end))) ||
            (is.null(g_position) &&
              (is.null(g_start) || is.null(g_end)))
        ),
        "Supply either `g_position` alone or `g_start` and `g_end` together."
      ),
      list(
        quote(
          !is.finite(taxid) || taxid %% 1 != 0 ||
            any(
              !is.finite(c(g_position, g_start, g_end)) |
                c(g_position, g_start, g_end) %% 1 != 0
            )
        ),
        "`taxid` and genomic positions should be finite, positive whole numbers."
      ),
      list(
        quote(!is.null(g_start) && !is.null(g_end) && g_start > g_end),
        "`g_start` cannot exceed `g_end`."
      )
    )
  )

  .msg(
    "Retrieving genome coordinates of proteins in taxon %s, Chromosome %s, Genome location %s.",
    taxid, chromosome,
    ifelse(
      is.null(g_position),
      yes = paste(g_start, g_end, sep = " to "),
      no = g_position
    )
  )

  ## Build Function-Specific Call
  path_input <- sprintf(
    "%scoordinates/glocation/%s/%s:%s",
    .rba_stg("uniprot", "pth"),
    taxid, chromosome,
    ifelse(
      !is.null(g_position),
      yes = g_position,
      no = paste0(g_start, "-", g_end)
    )
  )

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = path_input,
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("uniprot_coordinates_location_genome.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
