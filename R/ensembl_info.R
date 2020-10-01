#### information endpoints in Ensembl API

#' List the names of analyses involved in generating Ensembl data
#'
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_analysis = function(species,
                                     ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1)))
  v_msg("List the names of analyses involved in generating Ensembl data")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/analysis/",
                                         species),
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List the currently available assemblies for a species, along with toplevel
#' sequences, chromosomes and cytogenetic bands.
#'
#' @param species
#' @param bands
#' @param ...
#' @param synonyms
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_aassembly = function(species,
                                      bands = FALSE,
                                      synonyms = FALSE,
                                      ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "bands",
                               class = "logical"),
                          list(arg = "synonyms",
                               class = "logical")))
  v_msg("GET info/assembly/:species")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("bands",
                                 bands == TRUE,
                                 "1"),
                            list("synonyms",
                                 synonyms == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/assembly/",
                                         species),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Returns information about the specified toplevel sequence region for the
#' given species.
#'
#' @param species
#' @param region_name
#' @param bands
#' @param ...
#' @param synonyms
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_assembly_region_name = function(species,
                                                 region_name,
                                                 bands = FALSE,
                                                 synonyms = FALSE,
                                                 ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "region_name",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "bands",
                               class = "logical"),
                          list(arg = "synonyms",
                               class = "logical")))
  v_msg("GET info/assembly/:species/:region_name")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("bands",
                                 bands  == TRUE,
                                 "1"),
                            list("synonyms",
                                 synonyms == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/assembly/",
                                         species,
                                         "/",
                                         region_name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#'  List the functional classifications of gene models that Ensembl associates
#'  with a particular species. Useful for restricting the type of
#'  genes/transcripts retrieved by other endpoints.
#'
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_biotypes = function(species,
                                     ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1)))
  v_msg("GET info/biotypes/:species")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/biotypes/",
                                         species),
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Without argument the list of available biotype groups is returned. With
#' :group argument provided, list the properties of biotypes within that group.
#' Object type (gene or transcript) can be provided for filtering.
#'
#' @param group
#' @param ...
#' @param object_type
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_biotypes_groups = function(group = NA,
                                            object_type = NA,
                                            ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "group",
                               class = "character",
                               len = 1),
                          list(arg = "object_type",
                               class = "character",
                               val = c("gene",
                                       "transcript"))),
              cond = list(list(quote(!is.na(object_type) && is.na(group)),
                               c("You can't provide ",
                                 "'object_type' without ",
                                 "providing 'group'."))))
  v_msg("GET info/biotypes/groups/:group/:object_type")

  ## make function-specific calls
  parser_input = "json->list"
  path_input = "/info/biotypes/groups/"
  if (!is.na(group)){
    parser_input = "json->df"
    path_input = paste0(path_input, group)
  }
  if (!is.na(object_type)){
    path_input = paste0(path_input, "/", object_type)
  }

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           accept = "application/json",
                           parser = parser_input)

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List the properties of biotypes with a given name. Object type
#' (gene or transcript) can be provided for filtering.
#'
#' @param name
#' @param ...
#' @param object_type
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_biotypes_names = function(name,
                                           object_type = NA,
                                           ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "name",
                               class = "character",
                               len = 1),
                          list(arg = "object_type",
                               class = "character",
                               val = c("gene",
                                       "transcript"))))
  v_msg("GET info/biotypes/name/:name/:object_type")

  ## make function-specific calls
  path_input = paste0("info/biotypes/name/", name)
  if (!is.na(object_type)){
    path_input = paste0(path_input, "/", object_type)
  }

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List all compara analyses available (an analysis defines the type of
#' comparative data).
#'
#' @param class
#' @param ...
#' @param compara
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_compara_methods = function(class = NA,
                                            compara = "vertebrates",
                                            ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "class",
                               class = "character",
                               len = 1),
                          list(arg = "compara",
                               class = "character",
                               len = 1)))
  v_msg("GET info/compara/methods")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("class",
                                 !is.na(class),
                                 class),
                            list("compara",
                                 compara != "vertebrates",
                                 compara))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "info/compara/methods/",
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List all collections of species analysed with the specified compara method.
#'
#' @param method
#' @param ...
#' @param compara
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_compara_methods = function(method,
                                            compara = "vertebrates",
                                            ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "method",
                               class = "character",
                               len = 1),
                          list(arg = "compara",
                               class = "character",
                               len = 1)))
  v_msg("GET info/compara/species_sets/:method")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("compara",
                                 compara != "vertebrates",
                                 compara))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/compara/species_sets/",
                                         method),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Lists all available comparative genomics databases and their data release.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_comparas = function(...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args()
  v_msg("GET info/comparas")

  ## build GET API request's query
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "info/comparas",
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Shows the data releases available on this REST server. May return more than
#' one release (unfrequent non-standard Ensembl configuration).
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_data = function(...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args()
  v_msg("GET info/data")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "info/data",
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Returns the Ensembl Genomes version of the databases backing this service
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_eg_version = function(...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args()
  v_msg("GET info/eg_version")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "info/eg_version",
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Lists all available external sources for a species.
#'
#' @param species
#' @param filter
#' @param ...
#' @param feature
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_external_dbs = function(species,
                                         filter = NA,
                                         feature = NA,
                                         ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "filter",
                               class = "character",
                               len = 1),
                          list(arg = "feature",
                               class = "character",
                               len = 1,
                               val = c("dna_align_feature",
                                       "protein_align_feature",
                                       "unmapped_object",
                                       "xref",
                                       "seq_region_synonym"))))

  v_msg("GET info/external_dbs/:species")
  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("filter",
                                 !is.na(filter),
                                 filter),
                            list("feature",
                                 !is.na(feature),
                                 feature))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/external_dbs/",
                                         species),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get list of all Ensembl divisions for which information is available
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_divisions = function(...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args()
  v_msg("GET info/divisions")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get", url = rba_ba_stg("ensembl", "url"),
                           path = "info/divisions",
                           accept = "application/json",
                           parser = "json->chr")

  ## call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Find information about a given genome
#'
#' @param genome_name
#' @param ...
#' @param expand
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_name = function(genome_name,
                                         expand = FALSE,
                                         ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "genome_name",
                               class = "character",
                               len = 1),
                          list(arg = "expand",
                               class = "logical",
                               len = 1)))
  v_msg("GET info/genomes/:genome_name")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("expand",
                                 expand == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/genomes/",
                                         genome_name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Find information about genomes containing a specified INSDC accession
#'
#' @param accession
#' @param ...
#' @param expand
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_accession = function(accession,
                                              expand = FALSE,
                                              ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character",
                               len = 1),
                          list(arg = "expand",
                               class = "logical",
                               len = 1)))
  v_msg("GET info/genomes/accession/:accession")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("expand",
                                 expand == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/genomes/accession/",
                                         accession),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Find information about a genome with a specified assembly
#'
#' @param assembly_id
#' @param ...
#' @param expand
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_assembly = function(assembly_id,
                                             expand = FALSE,
                                             ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "assembly_id",
                               class = "character",
                               len = 1),
                          list(arg = "expand",
                               class = "logical",
                               len = 1)))
  v_msg("GET info/genomes/assembly/:assembly_id")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("expand",
                                 expand == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get", url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/genomes/assembly/",
                                         assembly_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Find information about all genomes in a given division.
#'
#' @param division_name
#' @param ...
#' @param expand
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_division = function(division_name,
                                             expand = FALSE,
                                             ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "division_name",
                               class = "character",
                               len = 1),
                          list(arg = "expand",
                               class = "logical",
                               len = 1)))
  v_msg("GET info/genomes/division/:division_name")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("expand",
                                 expand == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get", url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/genomes/division/",
                                         division_name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Find information about all genomes beneath a given node of the taxonomy
#'
#' @param taxon_name
#' @param ...
#' @param expand
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_taxonomy = function(taxon_name,
                                             expand = FALSE,
                                             ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "taxon_name",
                               class = c("character",
                                         "numeric"),
                               len = 1),
                          list(arg = "expand",
                               class = "logical",
                               len = 1)))
  v_msg("GET info/genomes/taxonomy/:taxon_name")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("expand",
                                 expand == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get", url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/genomes/taxonomy/",
                                         taxon_name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#'  Shows the current version of the Ensembl REST API.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_rest = function(...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args()
  v_msg("GET info/rest")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get", url = rba_ba_stg("ensembl", "url"),
                           path = "info/rest",
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Shows the current version of the Ensembl API used by the REST server.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_software = function(...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args()
  v_msg("GET info/software")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get", url = rba_ba_stg("ensembl", "url"),
                           path = "info/software",
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Lists all available species, their aliases, available adaptor groups and data release.
#'
#' @param division
#' @param hide_strain_info
#' @param strain_collection
#' @param ...
#' @param expand
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_species = function(division = "EnsemblVertebrates",
                                    hide_strain_info = FALSE,
                                    strain_collection = NA,
                                    expand = FALSE,
                                    ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "expand",
                               class = "character",
                               len = 1),
                          list(arg = "hide_strain_info",
                               class = "logical"),
                          list(arg = "strain_collection",
                               class = "character",
                               len = 1)))
  v_msg("GET info/species")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("division",
                                 division != "EnsemblVertebrates",
                                 division),
                            list("hide_strain_info",
                                 hide_strain_info == TRUE,
                                 "1"),
                            list("strain_collection",
                                 !is.na(strain_collection),
                                 strain_collection))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get", url = rba_ba_stg("ensembl", "url"),
                           path = "info/species",
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List the variation sources used in Ensembl for a species.
#'
#' @param species
#' @param ...
#' @param filter
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation_species = function(species,
                                              filter = NA,
                                              ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("character",
                                         "numeric"),
                               len = 1),
                          list(arg = "filter",
                               class = "character",
                               len = 1)))
  v_msg("GET info/variation/:species")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("filter",
                                 !is.na(filter),
                                 filter))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get", url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/variation/",
                                         species),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Lists all variant consequence types.
#'
#' @param ...
#' @param rank
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation_consequence_types = function(rank = FALSE,
                                                        ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "rank",
                               class = "logical",
                               len = 1)))
  v_msg("GET info/variation/consequence_types")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("rank",
                                 rank == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get", url = rba_ba_stg("ensembl", "url"),
                           path = "info/variation/consequence_types",
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List all populations for a species
#'
#' @param species
#' @param ...
#' @param filter
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation_populations = function(species,
                                                  filter = NA,
                                                  ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("character",
                                         "numeric"),
                               len = 1),
                          list(arg = "filter",
                               class = "character",
                               len = 1)))
  v_msg("GET info/variation/populations/:species")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("filter",
                                 !is.na(filter),
                                 filter))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get", url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/variation/populations/",
                                         species),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List all individuals for a population from a species
#'
#' @param species
#' @param ...
#' @param population_name
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation_populations_species = function(species,
                                                          population_name,
                                                          ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("character",
                                         "numeric"),
                               len = 1),
                          list(arg = "population_name",
                               class = "character",
                               len = 1)))
  v_msg("GET info/variation/populations/:species:/:population_name")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get", url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/variation/populations/",
                                         species, "/",
                                         population_name),
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' A wrapper around all info/variation/... endpoints
#'
#' @param species
#' @param consequence_types
#' @param consequence_rank
#' @param populations
#' @param species_filter
#' @param ...
#' @param populations_filter
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation = function(species = NA,
                                      consequence_types = FALSE,
                                      consequence_rank = FALSE,
                                      populations = FALSE,
                                      species_filter = NA,
                                      populations_filter = NA,
                                      ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("character",
                                         "numeric"),
                               len = 1),
                          list(arg = "consequence_types",
                               class = "logical",
                               len = 1),
                          list(arg = "consequence_rank",
                               class = "logical",
                               len = 1),
                          list(arg = "populations",
                               class = c("character",
                                         "logical"),
                               len = 1),
                          list(arg = "species_filter",
                               class = "character",
                               len = 1),
                          list(arg = "populations_filter",
                               class = "character",
                               len = 1)))
  ## Decide which function to call
  if (consequence_types == TRUE) {
    if (verbose == TRUE && !is.na(species)) {
      message("Ignoring: 'species' & 'populations' arguments.")}
    final_output = rba_ensembl_info_variation_consequence_types(rank = consequence_rank,
                                                                ...)
  } else {
    if (!is.na(species) & populations == FALSE) {
      final_output = rba_ensembl_info_variation_species(species = species,
                                                        filter = species_filter,
                                                        ...)
    } else if (!is.na(species) & populations == TRUE) {
      final_output = rba_ensembl_info_variation_populations(species = species,
                                                            filter = populations_filter,
                                                            ...)
    } else if (!is.na(species) & class(populations) == "character") {
      final_output = rba_ensembl_info_variation_populations_species(species = species,
                                                                    population_name = populations,
                                                                    ...)
    } else {
      stop("Wrong combination of provided arguments, ",
           "Consult this function's documentations.", call. = diagnostics)
    }
  }
  return(final_output)
}
#### information endpoints in Ensembl API

#' List the names of analyses involved in generating Ensembl data
#'
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_analysis = function(species,
                                     ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1)))
  v_msg("List the names of analyses involved in generating Ensembl data")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/analysis/",
                                         species),
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List the currently available assemblies for a species, along with toplevel
#' sequences, chromosomes and cytogenetic bands.
#'
#' @param species
#' @param bands
#' @param ...
#' @param synonyms
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_aassembly = function(species,
                                      bands = FALSE,
                                      synonyms = FALSE,
                                      ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "bands",
                               class = "logical"),
                          list(arg = "synonyms",
                               class = "logical")))
  v_msg("GET info/assembly/:species")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("bands",
                                 bands == TRUE,
                                 "1"),
                            list("synonyms",
                                 synonyms == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/assembly/",
                                         species),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Returns information about the specified toplevel sequence region for the
#' given species.
#'
#' @param species
#' @param region_name
#' @param bands
#' @param ...
#' @param synonyms
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_assembly_region_name = function(species,
                                                 region_name,
                                                 bands = FALSE,
                                                 synonyms = FALSE,
                                                 ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "region_name",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "bands",
                               class = "logical"),
                          list(arg = "synonyms",
                               class = "logical")))
  v_msg("GET info/assembly/:species/:region_name")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("bands",
                                 bands  == TRUE,
                                 "1"),
                            list("synonyms",
                                 synonyms == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/assembly/",
                                         species,
                                         "/",
                                         region_name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#'  List the functional classifications of gene models that Ensembl associates
#'  with a particular species. Useful for restricting the type of
#'  genes/transcripts retrieved by other endpoints.
#'
#' @param ...
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_biotypes = function(species,
                                     ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1)))
  v_msg("GET info/biotypes/:species")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/biotypes/",
                                         species),
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Without argument the list of available biotype groups is returned. With
#' :group argument provided, list the properties of biotypes within that group.
#' Object type (gene or transcript) can be provided for filtering.
#'
#' @param group
#' @param ...
#' @param object_type
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_biotypes_groups = function(group = NA,
                                            object_type = NA,
                                            ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "group",
                               class = "character",
                               len = 1),
                          list(arg = "object_type",
                               class = "character",
                               val = c("gene",
                                       "transcript"))),
              cond = list(list(quote(!is.na(object_type) && is.na(group)),
                               c("You can't provide ",
                                 "'object_type' without ",
                                 "providing 'group'."))))
  v_msg("GET info/biotypes/groups/:group/:object_type")

  ## make function-specific calls
  parser_input = "json->list"
  path_input = "/info/biotypes/groups/"
  if (!is.na(group)){
    parser_input = "json->df"
    path_input = paste0(path_input, group)
  }
  if (!is.na(object_type)){
    path_input = paste0(path_input, "/", object_type)
  }

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           accept = "application/json",
                           parser = parser_input)

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List the properties of biotypes with a given name. Object type
#' (gene or transcript) can be provided for filtering.
#'
#' @param name
#' @param ...
#' @param object_type
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_biotypes_names = function(name,
                                           object_type = NA,
                                           ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "name",
                               class = "character",
                               len = 1),
                          list(arg = "object_type",
                               class = "character",
                               val = c("gene",
                                       "transcript"))))
  v_msg("GET info/biotypes/name/:name/:object_type")

  ## make function-specific calls
  path_input = paste0("info/biotypes/name/", name)
  if (!is.na(object_type)){
    path_input = paste0(path_input, "/", object_type)
  }

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List all compara analyses available (an analysis defines the type of
#' comparative data).
#'
#' @param class
#' @param ...
#' @param compara
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_compara_methods = function(class = NA,
                                            compara = "vertebrates",
                                            ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "class",
                               class = "character",
                               len = 1),
                          list(arg = "compara",
                               class = "character",
                               len = 1)))
  v_msg("GET info/compara/methods")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("class",
                                 !is.na(class),
                                 class),
                            list("compara",
                                 compara != "vertebrates",
                                 compara))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "info/compara/methods/",
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List all collections of species analysed with the specified compara method.
#'
#' @param method
#' @param ...
#' @param compara
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_compara_methods = function(method,
                                            compara = "vertebrates",
                                            ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "method",
                               class = "character",
                               len = 1),
                          list(arg = "compara",
                               class = "character",
                               len = 1)))
  v_msg("GET info/compara/species_sets/:method")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("compara",
                                 compara != "vertebrates",
                                 compara))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/compara/species_sets/",
                                         method),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Lists all available comparative genomics databases and their data release.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_comparas = function(...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args()
  v_msg("GET info/comparas")

  ## build GET API request's query
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "info/comparas",
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Shows the data releases available on this REST server. May return more than
#' one release (unfrequent non-standard Ensembl configuration).
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_data = function(...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args()
  v_msg("GET info/data")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "info/data",
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Returns the Ensembl Genomes version of the databases backing this service
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_eg_version = function(...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args()
  v_msg("GET info/eg_version")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "info/eg_version",
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Lists all available external sources for a species.
#'
#' @param species
#' @param filter
#' @param ...
#' @param feature
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_external_dbs = function(species,
                                         filter = NA,
                                         feature = NA,
                                         ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "filter",
                               class = "character",
                               len = 1),
                          list(arg = "compara",
                               class = "character",
                               len = 1,
                               val = c("dna_align_feature",
                                       "protein_align_feature",
                                       "unmapped_object",
                                       "xref",
                                       "seq_region_synonym"))))

  v_msg("GET info/external_dbs/:species")
  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("filter",
                                 !is.na(filter),
                                 filter),
                            list("feature",
                                 !is.na(feature),
                                 feature))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/external_dbs/",
                                         species),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get list of all Ensembl divisions for which information is available
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_divisions = function(...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args()
  v_msg("GET info/divisions")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "info/divisions",
                           accept = "application/json",
                           parser = "json->chr")

  ## call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Find information about a given genome
#'
#' @param genome_name
#' @param ...
#' @param expand
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_name = function(genome_name,
                                         expand = FALSE,
                                         ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "genome_name",
                               class = "character",
                               len = 1),
                          list(arg = "expand",
                               class = "logical",
                               len = 1)))
  v_msg("GET info/genomes/:genome_name")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("expand",
                                 expand == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/genomes/",
                                         genome_name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Find information about genomes containing a specified INSDC accession
#'
#' @param accession
#' @param ...
#' @param expand
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_accession = function(accession,
                                              expand = FALSE,
                                              ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "accession",
                               class = "character",
                               len = 1),
                          list(arg = "expand",
                               class = "logical",
                               len = 1)))
  v_msg("GET info/genomes/accession/:accession")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("expand",
                                 expand == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/genomes/accession/",
                                         accession),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Find information about a genome with a specified assembly
#'
#' @param assembly_id
#' @param ...
#' @param expand
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_assembly = function(assembly_id,
                                             expand = FALSE,
                                             ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "assembly_id",
                               class = "character",
                               len = 1),
                          list(arg = "expand",
                               class = "logical",
                               len = 1)))
  v_msg("GET info/genomes/assembly/:assembly_id")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("expand",
                                 expand == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/genomes/assembly/",
                                         assembly_id),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Find information about all genomes in a given division.
#'
#' @param division_name
#' @param ...
#' @param expand
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_division = function(division_name,
                                             expand = FALSE,
                                             ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "division_name",
                               class = "character",
                               len = 1),
                          list(arg = "expand",
                               class = "logical",
                               len = 1)))
  v_msg("GET info/genomes/division/:division_name")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("expand",
                                 expand == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/genomes/division/",
                                         division_name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Find information about all genomes beneath a given node of the taxonomy
#'
#' @param taxon_name
#' @param ...
#' @param expand
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes_taxonomy = function(taxon_name,
                                             expand = FALSE,
                                             ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "taxon_name",
                               class = c("character",
                                         "numeric"),
                               len = 1),
                          list(arg = "expand",
                               class = "logical",
                               len = 1)))
  v_msg("GET info/genomes/taxonomy/:taxon_name")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("expand",
                                 expand == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/genomes/taxonomy/",
                                         taxon_name),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#'  Shows the current version of the Ensembl REST API.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_rest = function(...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args()
  v_msg("GET info/rest")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "info/rest",
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Shows the current version of the Ensembl API used by the REST server.
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_software = function(...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args()
  v_msg("GET info/software")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "info/software",
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Lists all available species, their aliases, available adaptor groups and data release.
#'
#' @param division
#' @param hide_strain_info
#' @param strain_collection
#' @param ...
#' @param expand
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_species = function(division = "EnsemblVertebrates",
                                    hide_strain_info = FALSE,
                                    strain_collection = NA,
                                    expand = FALSE,
                                    ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "expand",
                               class = "character",
                               len = 1),
                          list(arg = "hide_strain_info",
                               class = "logical"),
                          list(arg = "strain_collection",
                               class = "character",
                               len = 1)))
  v_msg("GET info/species")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("division",
                                 division != "EnsemblVertebrates",
                                 division),
                            list("hide_strain_info",
                                 hide_strain_info == TRUE,
                                 "1"),
                            list("strain_collection",
                                 !is.na(strain_collection),
                                 strain_collection))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "info/species",
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List the variation sources used in Ensembl for a species.
#'
#' @param species
#' @param ...
#' @param filter
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation_species = function(species,
                                              filter = NA,
                                              ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("character",
                                         "numeric"),
                               len = 1),
                          list(arg = "filter",
                               class = "character",
                               len = 1)))
  v_msg("GET info/variation/:species")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("filter",
                                 !is.na(filter),
                                 filter))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/variation/",
                                         species),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Lists all variant consequence types.
#'
#' @param ...
#' @param rank
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation_consequence_types = function(rank = FALSE,
                                                        ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "rank",
                               class = "logical",
                               len = 1)))
  v_msg("GET info/variation/consequence_types")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("rank",
                                 rank == TRUE,
                                 "1"))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = "info/variation/consequence_types",
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List all populations for a species
#'
#' @param species
#' @param ...
#' @param filter
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation_populations = function(species,
                                                  filter = NA,
                                                  ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("character",
                                         "numeric"),
                               len = 1),
                          list(arg = "filter",
                               class = "character",
                               len = 1)))
  v_msg("GET info/variation/populations/:species")

  ## build GET API request's query
  call_query = rba_ba_query(init = list(),
                            list("filter",
                                 !is.na(filter),
                                 filter))

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/variation/populations/",
                                         species),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List all individuals for a population from a species
#'
#' @param species
#' @param ...
#' @param population_name
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation_populations_species = function(species,
                                                          population_name,
                                                          ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("character",
                                         "numeric"),
                               len = 1),
                          list(arg = "population_name",
                               class = "character",
                               len = 1)))
  v_msg("GET info/variation/populations/:species:/:population_name")

  ## make function-specific calls
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/variation/populations/",
                                         species, "/",
                                         population_name),
                           accept = "application/json",
                           parser = "json->list")

  ## call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' A wrapper around all info/variation/... endpoints
#'
#' @param species
#' @param consequence_types
#' @param consequence_rank
#' @param populations
#' @param species_filter
#' @param ...
#' @param populations_filter
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation = function(species = NA,
                                      consequence_types = FALSE,
                                      consequence_rank = FALSE,
                                      populations = FALSE,
                                      species_filter = NA,
                                      populations_filter = NA,
                                      ...) {
  ## Load user options
  rba_ba_ext_args(...)
  ## Check input arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("character",
                                         "numeric"),
                               len = 1),
                          list(arg = "consequence_types",
                               class = "logical",
                               len = 1),
                          list(arg = "consequence_rank",
                               class = "logical",
                               len = 1),
                          list(arg = "populations",
                               class = c("character",
                                         "logical"),
                               len = 1),
                          list(arg = "species_filter",
                               class = "character",
                               len = 1),
                          list(arg = "populations_filter",
                               class = "character",
                               len = 1)))
  ## Decide which function to call
  if (consequence_types == TRUE) {
    if (verbose == TRUE && !is.na(species)) {
      message("Ignoring: 'species' & 'populations' arguments.")}
    final_output = rba_ensembl_info_variation_consequence_types(rank = consequence_rank,
                                                                ...)
  } else {
    if (!is.na(species) & populations == FALSE) {
      final_output = rba_ensembl_info_variation_species(species = species,
                                                        filter = species_filter,
                                                        ...)
    } else if (!is.na(species) & populations == TRUE) {
      final_output = rba_ensembl_info_variation_populations(species = species,
                                                            filter = populations_filter,
                                                            ...)
    } else if (!is.na(species) & class(populations) == "character") {
      final_output = rba_ensembl_info_variation_populations_species(species = species,
                                                                    population_name = populations,
                                                                    ...)
    } else {
      stop("Wrong combination of provided arguments, ",
           "Consult this function's documentations.", call. = diagnostics)
    }
  }
  return(final_output)
}
