#' Search Genomic Coordinates of UniProt entries
#'
#' Use this function to search genomic coordinates of UniProt entries.
#'   You may also refine your search with modifiers such as chromosome, taxon
#'   id etc. refer to "Arguments section" for more information.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.\cr\cr
#'   For more information about how UniProt imports and calculates genomic
#'   coordinates data, see:\cr
#'   McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J.,
#'   Wu, C., & UniProt Consortium (2019). UniProt genomic mapping for
#'   deciphering functional effects of missense variants. Human mutation,
#'   40(6), 694–705. https://doi.org/10.1002/humu.23738
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/coordinates"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can provide up to 100
#'   accession numbers.
#' @param chromosome Chromose name, such as "X", "Y", 1, 20, etc. You can
#' provide up to 20 values.
#' @param ensembl_id Ensembl Stable gene ID, transcript ID or translation ID.
#' You can provide up to 20 IDs.
#' @param gene \href{https://www.uniprot.org/help/gene_name}{UniProt gene
#'   name(s)}. You can provide up to 20 gene names.
#' @param protein \href{https://www.uniprot.org/help/protein_names}{UniProt
#'   protein name}
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can provide up to 20 taxon IDs.
#' @param location Genome location range such as "58205437-58219305"
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return List where each element corresponds to one UniProt entity returned
#'   by your search query. The element itself is a sub-list containing that
#'   protein's coordinates information.
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
#' rba_uniprot_coordinates_search(taxid = 9606, chromosome = "y")
#' @family "UniProt API, Coordinates"
#' @export
rba_uniprot_coordinates_search = function(accession = NA,
                                          chromosome = NA,
                                          ensembl_id = NA,
                                          gene = NA,
                                          protein = NA,
                                          taxid = NA,
                                          location = NA,
                                          ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "accession",
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

  v_msg("Searching UniProt and retrieving Coordinates of proteins that match your provided inputs.")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list("size" = "-1"),
                            list("accession",
                                 any(!is.na(accession)),
                                 paste0(accession,
                                        collapse = ",")),
                            list("chromosome",
                                 any(!is.na(chromosome)),
                                 paste0(chromosome,
                                        collapse = ",")),
                            list("ensembl_id",
                                 any(!is.na(ensembl_id)),
                                 paste0(ensembl_id,
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
                            list("location",
                                 !is.na(location),
                                 location))
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = paste0(rba_ba_stg("uniprot", "pth"),
                                         "coordinates"),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_coordinates_search.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get Genome coordinate by Protein Sequence position
#'
#' Using this function you can retrieve genome coordinates of a given UniProt
#'   protein by providing protein position or position range. You can either
#'   provide 'p_position' alone or provide 'p_start' and 'p_end' together.
#'
#'  For more information about how UniProt imports and calculates genomic
#'   coordinates data, see:\cr
#'   McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J.,
#'   Wu, C., & UniProt Consortium (2019). UniProt genomic mapping for
#'   deciphering functional effects of missense variants. Human mutation,
#'   40(6), 694–705. https://doi.org/10.1002/humu.23738
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/coordinates/location/{accession}:{pPosition}"
#'  "GET https://ebi.ac.uk/proteins/api/coordinates/location/{accession}:{pStart}-{pEnd}"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param p_position (numeric) Protein sequence position
#' @param p_start (numeric) Protein sequence position start
#' @param p_end (numeric) Protein sequence position end
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return Genome coordinates of your provided proteins.
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
#' rba_uniprot_coordinates_sequence(accession = "P25942", p_position = 1)
#' rba_uniprot_coordinates_sequence(accession = "P25942", p_start = 1, p_end = 277)
#' @family "UniProt API, Coordinates"
#' @export
rba_uniprot_coordinates_sequence = function(accession,
                                            p_position = NA,
                                            p_start = NA,
                                            p_end = NA,
                                            ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character"),
                          list(arg = "p_position",
                               class = "numeric"),
                          list(arg = "p_start",
                               class = "numeric"),
                          list(arg = "p_end",
                               class = "numeric")),
              cond = list(list(quote(any(sum(!is.na(p_position), !is.na(p_start), !is.na(p_end)) == 3,
                                         sum(!is.na(p_position), !is.na(p_start), !is.na(p_end)) == 0,
                                         sum(!is.na(p_start), !is.na(p_end)) == 1)),
                               "You should provide either 'p_position' alone or 'p_start' and 'p_end' together.")
                          ))

  v_msg("Retrieving genome coordinates of protein %s in sequence position %s.",
        accession,
        ifelse(is.na(p_position),
               yes = paste(p_start, p_end, sep = " to "), no = p_position))

  ## Build Function-Specific Call
  path_input = sprintf("%scoordinates/location/%s:%s",
                       rba_ba_stg("uniprot", "pth"),
                       accession,
                       ifelse(!is.na(p_position),
                              yes = p_position,
                              no = paste0(p_start, "-", p_end)))

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = path_input,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("uniprot_coordinates_location.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get Genomic Coordinates of a Protein
#'
#' Using this function you can retrieve genomic Coordinates of a Protein by
#'   either providing the protein's UniProt accession or it's ID in a
#'   cross-reference database (Ensembl, CCDC, HGNC or RefSeq). You should
#'   provide either 'accession' alone or 'db_type' and 'db_id' together.
#'
#'  For more information about how UniProt imports and calculates genomic
#'   coordinates data, see:\cr
#'   McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J.,
#'   Wu, C., & UniProt Consortium (2019). UniProt genomic mapping for
#'   deciphering functional effects of missense variants. Human mutation,
#'   40(6), 694–705. https://doi.org/10.1002/humu.23738
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/coordinates/{accession}"
#'  "GET https://ebi.ac.uk/proteins/api/coordinates/{dbtype}:{dbid}"
#'
#' @param accession \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param db_type cross-reference database name, Should be one of:
#'   "Ensembl", "CCDC", "HGNC" or "RefSeq".
#' @param db_id Protein's ID in the cross-reference database
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return A list with genome coordinates of your provided protein.
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
#' rba_uniprot_coordinates(accession = "P25942")
#' rba_uniprot_coordinates(db_type = "HGNC", db_id = "CD40")
#' @family "UniProt API, Coordinates"
#' @export
rba_uniprot_coordinates = function(accession = NA,
                                   db_type = NA,
                                   db_id = NA,
                                   ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character"),
                          list(arg = "db_type",
                               class = "character",
                               val = c("Ensembl",
                                         "CCDC",
                                         "HGNC",
                                         "RefSeq")),
                          list(arg = "db_id",
                               class = "character")),
              cond = list(list(quote(any(sum(!is.na(accession), !is.na(db_type), !is.na(db_id)) == 3,
                                         sum(!is.na(accession), !is.na(db_type), !is.na(db_id)) == 0,
                                         sum(!is.na(db_type), !is.na(db_id)) == 1)),
                               "You should provide either 'accession' alone or 'db_type' and 'db_id' together.")
              ))

  v_msg("Retrieving genome coordinates of protein with ID: %s",
        ifelse(is.na(accession),
               yes = sprintf("%s in %s database", db_id, db_type),
               no = accession))
  ## Build GET API Request's query
  call_query = list("size" = "-1")
  ## Build Function-Specific Call
  path_input = sprintf("%scoordinates/%s",
                       rba_ba_stg("uniprot", "pth"),
                       ifelse(!is.na(accession),
                              yes = accession,
                              no = paste0(db_type, ":", db_id)))

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("uniprot_coordinates.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Search UniProt entries by taxonomy and genomic coordinates
#'
#'  For more information about how UniProt imports and calculates genomic
#'   coordinates data, see:\cr
#'   McGarvey, P. B., Nightingale, A., Luo, J., Huang, H., Martin, M. J.,
#'   Wu, C., & UniProt Consortium (2019). UniProt genomic mapping for
#'   deciphering functional effects of missense variants. Human mutation,
#'   40(6), 694–705. https://doi.org/10.1002/humu.23738
#'
#' @section Corresponding API Resources:
#'  "GET https://ebi.ac.uk/proteins/api/coordinates/{taxonomy}/{locations}/feature"
#'  "GET https://ebi.ac.uk/proteins/api/coordinates/{taxonomy}/{locations}"
#'
#' @param taxid NIH-NCBI \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#' @param locations genomic location formatted as: chromosome:start-end.
#'  (e.g. "Y:17100001-19600000"). If you omit chromosome, it will be interpreted
#'  as any chromosome (e.g. "1-10000").
#' @param in_range Only return proteins that are in range.
#' @param feature (logical) Get features?
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s
#'   arguments documentation for more information on available options.
#'
#' @return a list containing UniProt proteins which match the provided genomic
#'   location and taxonomy ID.
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
#' rba_uniprot_coordinates_location(taxid = 9606,
#'   locations = "Y:17100001-19600000", in_range = TRUE)
#' rba_uniprot_coordinates_location(taxid = 9606,
#'   locations = "20:39000001", in_range = FALSE)
#' @family "UniProt API, Coordinates"
#' @export
rba_uniprot_coordinates_location = function(taxid,
                                            locations,
                                            in_range = TRUE,
                                            feature = FALSE,
                                            ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "taxid",
                               class = "numeric"),
                          list(arg = "locations",
                               class = "character"),
                          list(arg = "in_range",
                               class = "logical"),
                          list(arg = "feature",
                               class = "logical"))
  )

  v_msg("Retrieving UniProt entries in location %s of taxon %s.",
        locations, taxid)
  ## Build GET API Request's query
  call_query = list("size" = "-1",
                    "in_range" = ifelse(in_range, "true", "false"))

  ## Build Function-Specific Call
  path_input = sprintf("%scoordinates/%s/%s",
                       rba_ba_stg("uniprot", "pth"),
                       taxid,
                       locations)
  if (isTRUE(feature)) {
    path_input = paste0(path_input, "/feature")
  }
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("uniprot", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("rba_uniprot_coordinates_location.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
