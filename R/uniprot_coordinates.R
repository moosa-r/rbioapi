#' Search Genomic Coordinates of UniProt entries
#'
#' Use this function to search genomic coordinates of UniProt entries.
#'   You may also refine your search with modifiers such as chromosome, taxon
#'   id etc. See "Arguments section" for more information.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.
#'   \cr For more information about how UniProt imports and calculates genomic
#'   coordinates data, see:
#'   \cr McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J.,
#'   Wu, C., & UniProt Consortium (2019). UniProt genomic mapping for
#'   deciphering functional effects of missense variants. Human mutation,
#'   40(6), 694–705. https://doi.org/10.1002/humu.23738
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/coordinates"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param chromosome chromosome name, such as "X", "Y", 1, 20, etc. You can
#' supply up to 20 values.
#' @param ensembl_id Ensembl Stable gene ID, transcript ID or translation ID.
#' You can supply up to 20 IDs.
#' @param gene \href{https://www.uniprot.org/help/gene_name}{UniProt gene
#'   name(s)}. You can supply up to 20 gene names.
#' @param protein \href{https://www.uniprot.org/help/protein_names}{UniProt
#'   protein name}
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param location Genome location range such as "58205437-58219305"
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return List where each element corresponds to one UniProt entity returned
#'   by your search query. The element itself is a sub-list containing that
#'   protein's coordinates information.
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
  .rba_args(cons = list(list(arg = "accession",
                             class = "character",
                             max_len = 100),
                        list(arg = "chromosome",
                             class = c("character",
                                       "numeric"),
                             max_len = 20),
                        list(arg = "ensembl_id",
                             class = "character",
                             max_len = 20),
                        list(arg = "gene",
                             class = "character",
                             max_len = 20),
                        list(arg = "protein",
                             class = "character"),
                        list(arg = "taxid",
                             class = "numeric",
                             max_len = 20),
                        list(arg = "location",
                             class = "character")))

  .msg("Searching UniProt and retrieving Coordinates of proteins that match your supplied inputs.")
  ## Build GET API Request's query
  call_query <- .rba_query(init = list("size" = "-1"),
                           list("accession",
                                !is.null(accession),
                                paste0(accession,
                                       collapse = ",")),
                           list("chromosome",
                                !is.null(chromosome),
                                paste0(chromosome,
                                       collapse = ",")),
                           list("ensembl_id",
                                !is.null(ensembl_id),
                                paste0(ensembl_id,
                                       collapse = ",")),
                           list("gene",
                                !is.null(gene),
                                paste0(gene,
                                       collapse = ",")),
                           list("protein",
                                !is.null(protein),
                                protein),
                           list("taxid",
                                !is.null(taxid),
                                paste0(taxid,
                                       collapse = ",")),
                           list("location",
                                !is.null(location),
                                location))
  ## Build Function-Specific Call
  parser_input <- list("json->list",
                       .rba_uniprot_search_namer)

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = paste0(.rba_stg("uniprot", "pth"),
                                        "coordinates"),
                          query = call_query,
                          accept = "application/json",
                          parser = parser_input,
                          save_to = .rba_file("uniprot_coordinates_search.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Genome coordinate by Protein Sequence position
#'
#' Using this function you can retrieve genome coordinates of a given UniProt
#'   protein by providing protein position or position range. You can either
#'   supply 'p_position' alone or supply 'p_start' and 'p_end' together.
#'
#'  For more information about how UniProt imports and calculates genomic
#'   coordinates data, see:
#'   \cr McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J.,
#'   Wu, C., & UniProt Consortium (2019). UniProt genomic mapping for
#'   deciphering functional effects of missense variants. Human mutation,
#'   40(6), 694–705. https://doi.org/10.1002/humu.23738
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/coordinates/location
#'  /\{accession\}:\{pPosition\}"
#'  \cr "GET https://ebi.ac.uk/proteins/api/coordinates/location
#'  /\{accession\}:\{pStart\}-\{pEnd\}"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param p_position (numeric) Protein sequence position
#' @param p_start (numeric) Protein sequence position start
#' @param p_end (numeric) Protein sequence position end
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return Genome coordinates of your supplied proteins.
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
  .rba_args(cons = list(list(arg = "accession",
                             class = "character"),
                        list(arg = "p_position",
                             class = "numeric"),
                        list(arg = "p_start",
                             class = "numeric"),
                        list(arg = "p_end",
                             class = "numeric")),
            cond = list(list(quote(any(sum(!is.null(p_position), !is.null(p_start), !is.null(p_end)) == 3,
                                       sum(!is.null(p_position), !is.null(p_start), !is.null(p_end)) == 0,
                                       sum(!is.null(p_start), !is.null(p_end)) == 1)),
                             "You should supply either 'p_position' alone or 'p_start' and 'p_end' together.")
            ))

  .msg("Retrieving genome coordinates of protein %s in sequence position %s.",
       accession,
       ifelse(is.null(p_position),
              yes = paste(p_start, p_end, sep = " to "), no = p_position))

  ## Build Function-Specific Call
  path_input <- sprintf("%scoordinates/location/%s:%s",
                        .rba_stg("uniprot", "pth"),
                        accession,
                        ifelse(!is.null(p_position),
                               yes = p_position,
                               no = paste0(p_start, "-", p_end)))

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = path_input,
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("uniprot_coordinates_location.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Genomic Coordinates of a Protein
#'
#' Using this function you can retrieve genomic Coordinates of a Protein by
#'   either providing the protein's UniProt accession or it's ID in a
#'   cross-reference database (Ensembl, CCDC, HGNC or RefSeq). You should
#'   supply either 'accession' alone or 'db_type' and 'db_id' together.
#'
#'  For more information about how UniProt imports and calculates genomic
#'   coordinates data, see:
#'   \cr McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J.,
#'   Wu, C., & UniProt Consortium (2019). UniProt genomic mapping for
#'   deciphering functional effects of missense variants. Human mutation,
#'   40(6), 694–705. https://doi.org/10.1002/humu.23738
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/coordinates/\{accession\}"
#'  \cr "GET https://ebi.ac.uk/proteins/api/coordinates/\{dbtype\}:\{dbid\}"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param db_type cross-reference database name, Should be one of:
#'   "Ensembl", "CCDC", "HGNC" or "RefSeq".
#' @param db_id Protein's ID in the cross-reference database
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list with genome coordinates of your supplied protein.
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
  .rba_args(cons = list(list(arg = "accession",
                             class = "character"),
                        list(arg = "db_type",
                             class = "character",
                             val = c("Ensembl",
                                     "CCDC",
                                     "HGNC",
                                     "RefSeq")),
                        list(arg = "db_id",
                             class = "character")),
            cond = list(list(quote(any(sum(!is.null(accession), !is.null(db_type), !is.null(db_id)) == 3,
                                       sum(!is.null(accession), !is.null(db_type), !is.null(db_id)) == 0,
                                       sum(!is.null(db_type), !is.null(db_id)) == 1)),
                             "You should supply either 'accession' alone or 'db_type' and 'db_id' together.")
            ))

  .msg("Retrieving genome coordinates of protein with ID: %s",
       ifelse(is.null(accession),
              yes = sprintf("%s in %s database", db_id, db_type),
              no = accession))
  ## Build GET API Request's query
  call_query <- list("size" = "-1")
  ## Build Function-Specific Call
  path_input <- sprintf("%scoordinates/%s",
                        .rba_stg("uniprot", "pth"),
                        ifelse(!is.null(accession),
                               yes = accession,
                               no = paste0(db_type, ":", db_id)))

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = path_input,
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("uniprot_coordinates.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Search UniProt entries by taxonomy and genomic coordinates
#'
#'  For more information about how UniProt imports and calculates genomic
#'   coordinates data, see:
#'   \cr McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J.,
#'   Wu, C., & UniProt Consortium (2019). UniProt genomic mapping for
#'   deciphering functional effects of missense variants. Human mutation,
#'   40(6), 694–705. https://doi.org/10.1002/humu.23738
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/coordinates
#'  /\{taxonomy\}/\{locations\}/feature"
#'  \cr "GET https://ebi.ac.uk/proteins/api/coordinates
#'  /\{taxonomy\}/\{locations\}"
#'
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#' @param locations genomic location formatted as: chromosome:start-end.
#'  (e.g. "Y:17100001-19600000"). If you omit chromosome, it will be interpreted
#'  as any chromosome (e.g. "1-10000").
#' @param in_range Only return proteins that are in range.
#' @param feature (logical) Get features?
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return a list containing UniProt proteins which match the supplied genomic
#'   location and taxonomy ID.
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
  .rba_args(cons = list(list(arg = "taxid",
                             class = "numeric"),
                        list(arg = "locations",
                             class = "character"),
                        list(arg = "in_range",
                             class = "logical"),
                        list(arg = "feature",
                             class = "logical"))
  )

  .msg("Retrieving UniProt entries in location %s of taxon %s.",
       locations, taxid)
  ## Build GET API Request's query
  call_query <- list("size" = "-1",
                     "in_range" = ifelse(in_range, "true", "false"))

  ## Build Function-Specific Call
  path_input <- sprintf("%scoordinates/%s/%s",
                        .rba_stg("uniprot", "pth"),
                        taxid,
                        locations)
  if (isTRUE(feature)) {
    path_input <- paste0(path_input, "/feature")
  }
  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = path_input,
                          query = call_query,
                          accept = "application/json",
                          parser = "json->list",
                          save_to = .rba_file("rba_uniprot_coordinates_location.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Genome coordinate by Gene Sequence position
#'
#' Using this function you can retrieve genome coordinates of a given UniProt
#'   protein by providing Genome location position or range. You can either
#'   supply 'g_position' alone or supply 'g_start' and 'g_end' together.
#'
#'  For more information about how UniProt imports and calculates genomic
#'   coordinates data, see:
#'   \cr McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J.,
#'   Wu, C., & UniProt Consortium (2019). UniProt genomic mapping for
#'   deciphering functional effects of missense variants. Human mutation,
#'   40(6), 694–705. https://doi.org/10.1002/humu.23738
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/coordinates/glocation
#'  /\{accession\}:\{pPosition\}"
#'  \cr "GET https://ebi.ac.uk/proteins/api/coordinates/glocation
#'  /\{accession\}:\{pStart\}-\{pEnd\}"
#'
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param chromosome (Character or Numeric): Chromosome name, e.g. 1, 20, X.
#' @param g_position (numeric) Genome location position
#' @param g_start (numeric) Genome location position start
#' @param g_end (numeric) Genome location position end
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return Genome coordinates of your supplied proteins.
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
  .rba_args(cons = list(list(arg = "taxid",
                             class = "numeric"),
                        list(arg = "chromosome",
                             class = c("numeric",
                                       "character")),
                        list(arg = "g_position",
                             class = "numeric"),
                        list(arg = "g_start",
                             class = "numeric"),
                        list(arg = "g_end",
                             class = "numeric")),
            cond = list(list(quote(any(sum(!is.null(g_position), !is.null(g_start), !is.null(g_end)) == 3,
                                       sum(!is.null(g_position), !is.null(g_start), !is.null(g_end)) == 0,
                                       sum(!is.null(g_start), !is.null(g_end)) == 1)),
                             "You should supply either 'g_position' alone or 'g_start' and 'g_end' together.")
            ))

  .msg("Retrieving genome coordinates of proteins in taxon %s, Chromosome %s, Genome location %s.",
       taxid, chromosome,
       ifelse(is.null(g_position),
              yes = paste(g_start, g_end, sep = " to "), no = g_position))

  ## Build Function-Specific Call
  path_input <- sprintf("%scoordinates/glocation/%s/%s:%s",
                        .rba_stg("uniprot", "pth"),
                        taxid, chromosome,
                        ifelse(!is.null(g_position),
                               yes = g_position,
                               no = paste0(g_start, "-", g_end)))

  input_call <- .rba_httr(httr = "get",
                          url = .rba_stg("uniprot", "url"),
                          path = path_input,
                          accept = "application/json",
                          parser = "json->list_simp",
                          save_to = .rba_file("uniprot_coordinates_glocation.json"))

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
