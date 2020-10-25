#### information endpoints in Ensembl API

#' List the names of analyses involved in generating Ensembl data
#'
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param species
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_analysis = function(species,
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1)))
  v_msg("List the names of analyses involved in generating Ensembl data")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/analysis/",
                                         species),
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("ensembl_info_analysis.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' List the currently available assemblies for a species, along with toplevel
#' sequences, chromosomes and cytogenetic bands.
#'
#' @param species
#' @param bands
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param synonyms
#' @param region_name
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_aassembly = function(species,
                                      region_name = NA,
                                      bands = FALSE,
                                      synonyms = FALSE,
                                      ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "region_name",
                               class = c("numeric",
                                         "character")),
                          list(arg = "bands",
                               class = "logical"),
                          list(arg = "synonyms",
                               class = "logical")))
  v_msg("GET info/assembly/:species")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("bands",
                                 bands,
                                 "1"),
                            list("synonyms",
                                 synonyms,
                                 "1"))

  ## Build Function-Specific Call
  path_input = paste0("info/assembly/",
                      species)
  if (!is.na(region_name)) {
    path_input = paste0(path_input, "/", region_name)
  }
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = "json->list",
                           save_to = rba_ba_file("ensembl_info_aassembly.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#'  List the functional classifications of gene models that Ensembl associates
#'  with a particular species. Useful for restricting the type of
#'  genes/transcripts retrieved by other endpoints.
#'
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param species
#' @param name
#' @param group
#' @param object_type
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_biotypes = function(species = NA,
                                     name = NA,
                                     group = NA,
                                     object_type = NA,
                                     ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character"),
                               len = 1),
                          list(arg = "name",
                               class = "character",
                               len = 1),
                          list(arg = "group",
                               class = c("character",
                                         "logical"),
                               len = 1),
                          list(arg = "object_type",
                               class = "character",
                               val = c("gene",
                                       "transcript"))),
              cond = list(list(quote(all(is.na(species), is.na(name), is.na(group), is.na(object_type))),
                               "You can not leave all the arguments empty, consult function's documentation."),
                          list(quote(!is.na(species) && any(!is.na(name), !is.na(group), !is.na(object_type))),
                               "when providing 'specie', you can not provide 'name', 'group' or 'object_type'."),
                          list(quote(all(!is.na(name), !is.na(group))),
                               "You can not provide 'name' and 'group' at the same time."),
                          list(quote(!is.na(group) && any(!is.character(group), isTRUE(group))),
                               "'group' should be either a 'character string' or 'TRUE'."),
                          list(quote(!is.na(object_type) & all(is.na(group), is.na(name))),
                               "You can not provide 'object_type' without providing either 'name' or 'group'.")
              ))
  v_msg("GET info/biotypes/:species")

  ## Build Function-Specific Call
  if (!is.na(species)) {
    path_input = paste0("info/biotypes/", species)
    parser_input = "json->df"
  } else if (!is.na(group)) {
    path_input = "/info/biotypes/groups/"
    parser_input = "json->chr"
    if (is.character(group)) {
      path_input = paste0(path_input, group)
      parser_input = "json->df"
    }
  } else if (!is.na(name)) {
    parser_input = "json->df"
    path_input = paste0("info/biotypes/name/", name)
  }
  if (!is.na(object_type)) {
    path_input = paste0(path_input, "/", object_type)
  }

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           accept = "application/json",
                           parser = parser_input,
                           save_to = rba_ba_file("ensembl_info_biotypes.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Lists all available comparative genomics databases and their data release.
#'
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param compara
#' @param methods
#' @param species_sets
#' @param methods_class
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_compara = function(compara = NA,
                                    methods = FALSE,
                                    species_sets = FALSE,
                                    methods_class = NA,
                                    ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "compara",
                               class = "character"),
                          list(arg = "methods",
                               class = c("logical",
                                         "character"),
                               len = 1),
                          list(arg = "species_sets",
                               class = "logical"),
                          list(arg = "methods_class",
                               class = "character")),
              cond = list(list(quote(!is.na(compara) & all(isFALSE(methods), isFALSE(species_sets))),
                               "You can not provide 'compara' without using either 'methods' or 'species_sets'."),
                          list(quote(isTRUE(species_sets) & !is.na(methods_class)),
                               "You can not provide 'methods_class' when using 'species_set."),
                          list(quote(isTRUE(species_sets) & !is.character(methods)),
                               "when using 'species_sets', you should provide 'method' as a 'charachter string'."),
                          list(quote(is.character(methods) && isFALSE(species_sets)),
                               "When using 'methods' alone without 'species_sets', it should 'logical' not a 'charachter string'")
              ))
  v_msg("GET info/comparas")

  ## Build GET API Request's query
  if (all(is.na(compara), isFALSE(methods), isFALSE(species_sets))) {
    path_input = "info/comparas"
    parser_input = "json->df"
    call_query = NULL
  } else if (isTRUE(species_sets)) {
    path_input = paste0("info/compara/species_sets/", methods)
    parser_input = "json->list_simp"
    call_query = rba_ba_query(init = list(),
                              list("compara",
                                   !is.na(compara),
                                   compara))
  } else if (isTRUE(methods)) {
    path_input = "info/compara/methods/"
    parser_input = "json->list_simp"
    call_query = rba_ba_query(init = list(),
                              list("methods_class",
                                   !is.na(methods_class),
                                   methods_class),
                              list("compara",
                                   !is.na(compara),
                                   compara))
  } else {
    stop("Wrong combination of arguments, please refer to the function's documentations.",
         call. = get("diagnostics"))
  }
  print(path_input)
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = parser_input,
                           save_to = rba_ba_file("ensembl_info_comparas.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Shows the data releases available on this REST server.
#'
#' @param data
#' @param REST
#' @param software
#' @param eg_version
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_version = function(data = FALSE,
                                    REST = FALSE,
                                    software = FALSE,
                                    eg_version = FALSE,
                                    ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "data",
                               class = "logical"),
                          list(arg = "REST",
                               class = "logical"),
                          list(arg = "software",
                               class = "logical"),
                          list(arg = "eg_version",
                               class = "logical")),
              cond = list(list(quote(sum(data, REST, software, eg_version) != 1),
                               "You should set one of 'data, REST, software or eg_version' as 'TRUE' and leave the others as 'FALSE'")))
  v_msg("GET info/data")
  ## Build Function-Specific Call
  path_input = c("info/data",
                 "info/rest",
                 "info/software",
                 "info/eg_version")[c(data, REST, software, eg_version)]

  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           accept = "application/json",
                           parser = "json->list_simp",
                           save_to = rba_ba_file("ensembl_info_data.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Lists all available external sources for a species.
#'
#' @param species
#' @param filter
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "species",
                               class = c("numeric",
                                         "character")),
                          list(arg = "filter",
                               class = "character"),
                          list(arg = "feature",
                               class = "character",
                               val = c("dna_align_feature",
                                       "protein_align_feature",
                                       "unmapped_object",
                                       "xref",
                                       "seq_region_synonym"))))

  v_msg("GET info/external_dbs/:species")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("filter",
                                 !is.na(filter),
                                 filter),
                            list("feature",
                                 !is.na(feature),
                                 feature))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = paste0("info/external_dbs/",
                                         species),
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_info_external_dbs.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Get list of all Ensembl divisions for which information is available
#'
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_divisions = function(...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args()
  v_msg("GET info/divisions")

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get", url = rba_ba_stg("ensembl", "url"),
                           path = "info/divisions",
                           accept = "application/json",
                           parser = "json->chr",
                           save_to = rba_ba_file("ensembl_info_divisions.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)

  return(final_output)
}

#' Find information about a given genome
#'
#' @param genome_name
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param expand
#' @param INSDC_sequence_accession
#' @param INSDC_assembly_id
#' @param division_name
#' @param taxon_name
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_genomes = function(genome_name = NA,
                                    INSDC_sequence_accession = NA,
                                    INSDC_assembly_id = NA,
                                    division_name = NA,
                                    taxon_name = NA,
                                    expand = FALSE,
                                    ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "genome_name",
                               class = "character",
                               len = 1),
                          list(arg = "INSDC_sequence_accession",
                               class = "character",
                               len = 1),
                          list(arg = "INSDC_assembly_id",
                               class = "character",
                               len = 1),
                          list(arg = "division_name",
                               class = "character",
                               len = 1),
                          list(arg = "taxon_name",
                               class = c("character",
                                         "numeric"),
                               len = 1),
                          list(arg = "expand",
                               class = "logical",
                               len = 1)),
              cond = list(list(quote(sum(!is.na(genome_name),
                                         !is.na(INSDC_sequence_accession),
                                         !is.na(INSDC_assembly_id),
                                         !is.na(division_name),
                                         !is.na(taxon_name)) != 1),
                               "You should provide one of 'genome_name', 'INSDC_sequence_accession', 'INSDC_assembly_id', 'division_name' or 'taxon_name'")))
  v_msg("GET info/genomes/:genome_name")
  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("expand",
                                 expand,
                                 "1"))

  ## Build Function-Specific Call
  parser_input = "json->list_simp"
  if (!is.na(genome_name)) {
    path_input = paste0("info/genomes/", genome_name)
  } else if (!is.na(INSDC_sequence_accession)) {
    path_input = paste0("info/genomes/accession/", INSDC_sequence_accession)
  } else if (!is.na(INSDC_assembly_id)) {
    path_input = paste0("info/genomes/assembly/", INSDC_assembly_id)
  } else if (!is.na(division_name)) {
    path_input = paste0("info/genomes/division/", division_name)
    if (isFALSE(expand)) {parser_input = "json->df"}
  } else if (!is.na(taxon_name)) {
    path_input = paste0("info/genomes/taxonomy/", taxon_name)
  }
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = parser_input,
                           save_to = rba_ba_file("ensembl_info_genomes.json"))
  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' Lists all available species, their aliases, available adaptor groups and data release.
#'
#' @param division
#' @param hide_strain_info
#' @param strain_collection
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
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
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
  rba_ba_args(cons = list(list(arg = "division",
                               class = "character",
                               len = 1),
                          list(arg = "hide_strain_info",
                               class = "logical"),
                          list(arg = "strain_collection",
                               class = "character",
                               len = 1)))
  v_msg("GET info/species")

  ## Build GET API Request's query
  call_query = rba_ba_query(init = list(),
                            list("division",
                                 division != "EnsemblVertebrates",
                                 division),
                            list("hide_strain_info",
                                 hide_strain_info,
                                 "1"),
                            list("strain_collection",
                                 !is.na(strain_collection),
                                 strain_collection))

  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get", url = rba_ba_stg("ensembl", "url"),
                           path = "info/species",
                           query = call_query,
                           accept = "application/json",
                           parser = "json->df",
                           save_to = rba_ba_file("ensembl_info_species.json"))

  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}

#' A wrapper around all info/variation/... endpoints
#'
#' @param species
#' @param consequence_types
#' @param consequence_rank
#' @param populations
#' @param ... rbioapi option(s). Refer to \code{\link{rba_options}}'s arguments documentation for more information on available options.
#' @param populations_filter
#' @param variation_source
#'
#' @return
#' @export
#'
#' @examples
rba_ensembl_info_variation = function(species = NA,
                                      consequence_types = FALSE,
                                      consequence_rank = FALSE,
                                      populations = NA,
                                      variation_source = NA,
                                      populations_filter = NA,
                                      ...) {
  ## Load Global Options
  rba_ba_ext_args(...)
  ## Check User-input Arguments
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
                          list(arg = "variation_source",
                               class = "character",
                               len = 1),
                          list(arg = "populations_filter",
                               class = "character",
                               len = 1)),
              cond = list(list(quote(sum(isTRUE(consequence_types), !is.na(species)) != 1),
                               "You should either set 'consequence_types' to 'TRUE' or provide 'species'."),
                          list(quote(sum(!is.na(species), !is.na(populations)) == 1),
                               "You should either provide both of 'species' and 'population' or none of them."),
                          list(quote(!is.na(populations_filter) & !is.na(populations) & !isTRUE(populations)),
                               "You can not provide 'populations_filter' when 'populations' is not set to 'TRUE'."),
                          list(quote(!is.na(variation_source) & !is.na(populations) & !isFALSE(populations)),
                               "You can not provide 'variation_source' when 'populations' is not set to 'FALSE'."))
  )
  ## Decide which function to call
  parser_input = "json->df"
  if (isTRUE(consequence_types)) {
    call_query = rba_ba_query(init = list(),
                              list("consequence_rank",
                                   consequence_rank,
                                   "1"))
    path_input = "info/variation/consequence_types"
  } else if (!is.na(species)) {
    if (isFALSE(populations)) {
      call_query = rba_ba_query(init = list(),
                                list("filter",
                                     !is.na(variation_source),
                                     variation_source))
      path_input = paste0("info/variation/", species)
    } else if (isTRUE(populations)) {
      call_query = rba_ba_query(init = list(),
                                list("populations_filter",
                                     !is.na(populations_filter),
                                     populations_filter))
      path_input = paste0("info/variation/populations/", species)

    } else if (is.character(populations)) {
      call_query = list()
      path_input = sprintf("info/variation/populations/%s/%s",
                           species, populations)
      parser_input = "json->list_simp"
    }
  }
  ## Build Function-Specific Call
  input_call = rba_ba_httr(httr = "get",
                           url = rba_ba_stg("ensembl", "url"),
                           path = path_input,
                           query = call_query,
                           accept = "application/json",
                           parser = parser_input,
                           save_to = rba_ba_file("ensembl_info_variation.json"))
  ## Call API
  final_output = rba_ba_skeleton(input_call)
  return(final_output)
}
