#' Map a Set of Identifiers to STRING Identifiers
#'
#' This function calls STRING's API to map a set of common gene or protein
#'   identifiers to STRING identifiers. Although STRING services accept a
#'   variety of identifiers, the STRING API documentation recommends mapping
#'   them to STRING identifiers before using other STRING functions.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/tsv/get_string_ids?identifiers=
#'  \{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your common gene/protein identifier(s) to be mapped.
#' @param species Numeric:
#'   \href{https://www.ncbi.nlm.nih.gov/taxonomy/}{
#'   NCBI Taxonomy identifier}; Human Taxonomy ID is 9606.
#'   (Recommended, but optional.)
#' @param echo_query (default = \code{TRUE}) Include your input IDs as a column of the
#'   results.
#' @param limit Deprecated: Retained temporarily for backward compatibility.
#'   STRING v12 returns only the single best match per input ID, so this
#'   argument has no effect.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame with at most one mapped STRING ID per input ID and
#'   other pertinent information. The \code{queryIndex} column contains the
#'   zero-based position of each resolved ID in the input vector. Unresolved
#'   inputs are omitted; if none can be resolved, a zero-row data frame
#'   retaining the response columns is returned.
#'
#' @references \itemize{
#'   \item Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina
#'    Nastou, Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang,
#'    Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian
#'    von Mering, The STRING database in 2023: protein–protein association
#'    networks and functional enrichment analyses for any sequenced genome of
#'    interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January 2023,
#'    Pages D638–D646, https://doi.org/10.1093/nar/gkac1000
#'   \item \href{https://string-db.org/help/api/}{STRING API Documentation}
#'   \item
#'   \href{https://string-db.org/cgi/about?footer_active_subpage=references}{
#'   Citations note on STRING website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_string_map_ids(ids = c("TP53", "TNF", "EGFR"), species = 9606)
#' }
#'
#' @family "STRING"
#' @export
rba_string_map_ids <- function(ids,
                               species = NULL,
                               echo_query = TRUE,
                               limit = NULL,
                               ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "ids", class = c("character", "numeric", "integer"), min_len = 1L),
      list(
        arg = "species", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 1
      ),
      list(arg = "echo_query", class = "logical", len = 1L)
    )
  )

  if (!is.null(limit)) {
    .Deprecated(
      msg = paste0(
        "`limit` is deprecated and has no effect because STRING v12 returns ",
        "only the single best match for each input ID. It will be removed in ",
        "a future rbioapi release."
      )
    )
  }

  .msg(
    "Mapping %s input Identifiers to STRING Identifiers.",
    length(ids)
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(ids, collapse = "%0d"),
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("species", !is.null(species), species),
    list("echo_query", echo_query, "1")
  )

  ## Build Function-Specific Call
  parser_input <- list(
    "text->chr",
    function(parsed_response) {
      parsed_response <- utils::read.delim(
        text = parsed_response,
        header = TRUE,
        quote = "",
        comment.char = "",
        colClasses = "character",
        na.strings = character(),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      if (utils::hasName(parsed_response, "queryIndex")) {
        parsed_response$queryIndex <- as.integer(parsed_response$queryIndex)
      }

      return(parsed_response)
    }
  )

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), "tsv/get_string_ids"),
    body = call_body,
    encode = "form",
    accept = "text/tab-separated-values",
    parser = parser_input,
    save_to = .rba_file("string_map_ids.tsv")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get STRING Network Image
#'
#' This function retrieves a static image of the interaction network among your
#'   input proteins and, where applicable, additional interactors. The available
#'   arguments control the network contents and appearance.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/network?identifiers=
#'  \{your_identifiers\}&\{optional_parameters\}"
#'  \cr "POST https://string-db.org/api/\{output-format\}/network?network_term_id=
#'  \{your_term\}&\{optional_parameters\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#'   \cr Alternatively, you can retrieve the network of proteins annotated with
#'   a STRING functional term by setting \code{ids = NULL} and supplying
#'   \code{network_term_id}.
#' @param network_term_id Character: A functional term identifier (e.g. a Gene
#'   Ontology, KEGG, or Reactome identifier). Instead of using proteins supplied
#'   through \code{ids}, STRING constructs the network from proteins annotated
#'   with the specified term. Set \code{ids = NULL} and supply \code{species}.
#' @param species Numeric:
#'   \href{https://www.ncbi.nlm.nih.gov/taxonomy/}{
#'   NCBI Taxonomy identifier}; Human Taxonomy ID is 9606.
#'   Required when using \code{network_term_id}; otherwise recommended, but
#'   required if your input contains more than 10 unique IDs.
#' @param image_format Character: One of:\itemize{
#'   \item "image": PNG image with normal resolution.
#'   \item "highres_image": High-resolution PNG image.
#'   \item "svg": Scalable Vector Graphics image.}
#' @param save_image Logical or Character:\itemize{
#'   \item TRUE: Save the image to an automatically-generated path.
#'   \item FALSE: Do not save the image, just return it as an R object.
#'   \item Character string: A valid file path to save the image to.}
#' @param add_color_nodes Numeric: The number of colored nodes (queried proteins
#'   and first shell of interactors) to be added.
#' @param add_white_nodes Numeric: The number of white nodes (second shell of
#'   interactors) to be added after colored nodes.
#' @param required_score Numeric (between 0 and 1000): Minimum interaction
#'   score required for an interaction to be included in the image. If omitted,
#'   STRING applies a network-dependent threshold. Common
#'   confidence thresholds are 150 (low), 400 (medium), 700 (high), and 900
#'   (highest).
#' @param network_flavor Character: The network-edge style. One of:\itemize{
#'   \item "evidence": (default) Edge colors indicate the types of evidence
#'   supporting each interaction.
#'   \item "confidence": Edge thickness indicates the interaction confidence
#'   score.
#'   \item "actions": Edge shape indicates the predicted mode of action.}
#' @param network_type Character: One of:\itemize{
#'   \item "functional": (default) Edges indicate both physical and
#'   functional associations.
#'   \item "physical": Edges indicate that two proteins have a physical
#'   interaction or are parts of a complex.}
#' @param hide_node_labels Logical: (default = \code{FALSE}) Hide protein names from
#'   the image.
#' @param use_query_labels Logical: (default = \code{FALSE}) Use the names supplied
#'   in \code{ids} as node labels instead of STRING's default labels.
#' @param hide_disconnected_nodes Logical: (default = \code{FALSE}) Hide proteins
#'   that are not connected to any other protein.
#' @param hide_structure_pics Logical: (default = \code{FALSE}) Hide protein structure
#'   images inside the nodes.
#' @param flat_nodes Logical: (default = \code{FALSE}) Use a flat node design
#'   instead of the default 3D design.
#' @param node_labels_center Logical: (default = \code{FALSE}) Center protein labels
#'   on the nodes.
#' @param node_labels_font_size Numeric (between 5 and 50; default = 12):
#'   Font size of the protein node labels.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A PNG image array or raw SVG content, depending on
#'   \code{image_format}.
#'
#' @references \itemize{
#'   \item Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina
#'    Nastou, Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang,
#'    Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian
#'    von Mering, The STRING database in 2023: protein–protein association
#'    networks and functional enrichment analyses for any sequenced genome of
#'    interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January 2023,
#'    Pages D638–D646, https://doi.org/10.1093/nar/gkac1000
#'   \item \href{https://string-db.org/help/api/}{STRING API Documentation}
#'   \item
#'   \href{https://string-db.org/cgi/about?footer_active_subpage=references}{
#'   Citations note on STRING website}
#'   }
#'
#' @examples
#' \dontrun{
#' rba_string_network_image(ids = c("9606.ENSP00000269305",
#'     "9606.ENSP00000398698",
#'     "9606.ENSP00000275493"),
#'     network_type = "functional",
#'     save_image = FALSE)
#' }
#' \dontrun{
#' rba_string_network_image(ids = c("TP53", "TNF", "EGFR"),
#'     species = 9606,
#'     save_image = TRUE)
#' }
#' \dontrun{
#' rba_string_network_image(ids = "9606.ENSP00000269305",
#'     image_format = "highres_image",
#'     save_image = file.path(getwd(), "TP53_network.png"))
#' }
#' \dontrun{
#' rba_string_network_image(
#'     ids = NULL,
#'     network_term_id = "GO:0050852",
#'     species = 9606,
#'     save_image = FALSE
#' )
#' }
#'
#' @family "STRING"
#' @seealso
#'   \code{
#'   \link{rba_string_map_ids},
#'   \link{rba_string_interactions_network},
#'   \link{rba_string_interaction_partners},
#'   \link{rba_string_enrichment_ppi}
#'   }
#' @export
rba_string_network_image <- function(ids,
                                     image_format = "image",
                                     save_image = TRUE,
                                     species = NULL,
                                     add_color_nodes = NULL,
                                     add_white_nodes = NULL,
                                     required_score = NULL,
                                     network_flavor = "evidence",
                                     network_type = "functional",
                                     hide_node_labels = FALSE,
                                     use_query_labels = FALSE,
                                     hide_disconnected_nodes = FALSE,
                                     hide_structure_pics = FALSE,
                                     flat_nodes = FALSE,
                                     node_labels_center = FALSE,
                                     node_labels_font_size = 12,
                                     network_term_id = NULL,
                                     ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "ids",
        class = c("character", "numeric", "integer"),
        min_len = 1L,
        no_null = FALSE
      ),
      list(arg = "network_term_id", class = "character", len = 1L),
      list(
        arg = "species", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 1
      ),
      list(
        arg = "image_format", class = "character",
        val = c("image", "highres_image", "svg"),
        len = 1L
      ),
      list(arg = "save_image", class = c("character", "logical"), len = 1L),
      list(
        arg = "add_color_nodes", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 0
      ),
      list(
        arg = "add_white_nodes", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 0
      ),
      list(
        arg = "required_score",
        class = c("numeric", "integer"),
        min_val = 0,
        max_val = 1000,
        len = 1L
      ),
      list(
        arg = "network_flavor", class = "character",
        val = c("evidence", "confidence", "actions"),
        len = 1L
      ),
      list(
        arg = "network_type",
        class = "character",
        val = c("functional", "physical"),
        len = 1L
      ),
      list(arg = "hide_node_labels", class = "logical", len = 1L),
      list(arg = "use_query_labels", class = "logical", len = 1L),
      list(arg = "hide_disconnected_nodes", class = "logical", len = 1L),
      list(arg = "hide_structure_pics", class = "logical", len = 1L),
      list(arg = "flat_nodes", class = "logical", len = 1L),
      list(arg = "node_labels_center", class = "logical", len = 1L),
      list(
        arg = "node_labels_font_size",
        class = c("numeric", "integer"),
        min_val = 5,
        max_val = 50,
        len = 1L
      )
    ),
    cond = list(
      list(
        quote(!is.null(ids) && !is.null(network_term_id)),
        paste0(
          "`ids` and `network_term_id` cannot be supplied together. ",
          "Set `ids = NULL` when using `network_term_id`."
        )
      ),
      list(
        quote(is.null(ids) && is.null(network_term_id)),
        "`network_term_id` must be supplied when `ids = NULL`."
      ),
      list(
        quote(
          is.null(ids) &&
            !is.null(network_term_id) &&
            is.null(species)
        ),
        "`species` must be supplied when using `network_term_id`."
      ),
      list(
        quote(
          !is.null(ids) &&
            is.null(network_term_id) &&
            length(unique(ids)) > 10 &&
            is.null(species)
        ),
        sprintf(
          "You supplied %s unique IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(unique(ids))
        )
      )
    )
  )

  if (is.null(ids)) {
    .msg(
      "Retrieving STRING network image for functional term %s.",
      network_term_id
    )
  } else {
    .msg(
      "Retrieving STRING network image of %s unique input Identifiers.",
      length(unique(ids))
    )
  }

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("identifiers", !is.null(ids), paste(unique(ids), collapse = "%0d")),
    list("network_term_id", !is.null(network_term_id), network_term_id),
    list("species", !is.null(species), species),
    list("add_color_nodes", !is.null(add_color_nodes), add_color_nodes),
    list("add_white_nodes", !is.null(add_white_nodes), add_white_nodes),
    list("required_score", !is.null(required_score), required_score),
    list("network_flavor", !is.null(network_flavor), network_flavor),
    list("network_type", !is.null(network_type), network_type),
    list("hide_node_labels", hide_node_labels, "1"),
    list("show_query_node_labels", use_query_labels, "1"),
    list("hide_disconnected_nodes", hide_disconnected_nodes, "1"),
    list("block_structure_pics_in_bubbles", hide_structure_pics, "1"),
    list("flat_node_design", flat_nodes, "1"),
    list("center_node_labels", node_labels_center, "1"),
    list("custom_label_font_size", node_labels_font_size != 12, node_labels_font_size)
  )

  ## make file path
  if (image_format == "svg") {

    ext_input <- "svg"
    accept_input <- "image/svg+xml"
    parser_input <- function(x) { httr::content(x, as = "raw") }

  } else {

    ext_input <- "png"
    accept_input <- "image/png"
    parser_input <- function(x) {
      png::readPNG(httr::content(x, as = "raw"))
    }

  }

  save_image <- .rba_file(
    file = paste0("string_network_image.", ext_input),
    save_to = save_image
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), image_format, "/network"),
    accept = accept_input,
    parser = parser_input,
    body = call_body,
    encode = "form",
    save_to = save_image
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get STRING Network Interactions
#'
#' This function retrieves STRING interaction pairs among the input proteins,
#'   including the combined score and separate scores for each evidence
#'   channel. You can expand the network using the \code{add_nodes} parameter.
#'
#' This function returns interactions among the supplied proteins and any
#'   neighboring proteins added through \code{add_nodes}. To retrieve
#'   interactions between your input proteins and all their STRING interaction
#'   partners, see
#'   \code{\link{rba_string_interaction_partners}}.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/network?identifiers=
#'  \{your_identifiers\}&\{optional_parameters\}"
#'  \cr "POST https://string-db.org/api/\{output-format\}/network?network_term_id=
#'  \{your_term\}&\{optional_parameters\}"
#'
#' @param ids Your protein IDs. It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#'   \cr Alternatively, you can retrieve interactions among proteins annotated
#'   with a STRING functional term by setting \code{ids = NULL} and supplying
#'   \code{network_term_id}.
#' @param network_term_id Character: A functional term identifier (e.g. a Gene
#'   Ontology, KEGG, or Reactome identifier). Instead of using proteins supplied
#'   through \code{ids}, STRING constructs the network from proteins annotated
#'   with the specified term. Set \code{ids = NULL} and supply \code{species}.
#' @param species Numeric:
#'   \href{https://www.ncbi.nlm.nih.gov/taxonomy/}{
#'   NCBI Taxonomy identifier}; Human Taxonomy ID is 9606.
#'   Required when using \code{network_term_id}; otherwise recommended, but
#'   required if your input contains more than 10 unique IDs.
#' @param required_score Numeric (between 0 and 1000): Minimum interaction
#'   score required for an interaction to be included in the returned network.
#'   If omitted, STRING applies a network-dependent threshold. Common
#'   confidence thresholds are 150 (low), 400 (medium), 700 (high), and 900
#'   (highest).
#' @param add_nodes Numeric: Number of neighboring proteins to add to the
#'   network. For identifier-based requests, if omitted, STRING determines the
#'   value from the number of input IDs:\enumerate{
#'   \item One ID: STRING adds 10 proteins to retrieve its interaction
#'   neighborhood.
#'   \item Multiple IDs: STRING adds no proteins, so only interactions among the
#'   input proteins are returned.}
#' @param network_type Character: One of:\itemize{
#'   \item "functional": (default) Edges indicate both physical and
#'   functional associations.
#'   \item "physical": Edges indicate that two proteins have a physical
#'   interaction or are parts of a complex.}
#' @param use_query_labels Logical: (default = \code{FALSE}) Use the names supplied
#'   in \code{ids} as node labels instead of STRING's default labels.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame in which each row is a network interaction and the
#'   columns contain interactor information and interaction scores: \itemize{
#'   \item stringId_A: STRING identifier (protein A)
#'   \item stringId_B: STRING identifier (protein B)
#'   \item preferredName_A: common protein name (protein A)
#'   \item preferredName_B: common protein name (protein B)
#'   \item ncbiTaxonId: NCBI taxon identifier
#'   \item score: combined score
#'   \item nscore: gene neighborhood score
#'   \item fscore: gene fusion score
#'   \item pscore: phylogenetic profile score
#'   \item ascore: co-expression score
#'   \item escore: experimental score
#'   \item dscore: database score
#'   \item tscore: textmining score}
#'
#' @references \itemize{
#'   \item Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina
#'    Nastou, Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang,
#'    Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian
#'    von Mering, The STRING database in 2023: protein–protein association
#'    networks and functional enrichment analyses for any sequenced genome of
#'    interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January 2023,
#'    Pages D638–D646, https://doi.org/10.1093/nar/gkac1000
#'   \item \href{https://string-db.org/help/api/}{STRING API Documentation}
#'   \item
#'   \href{https://string-db.org/cgi/about?footer_active_subpage=references}{
#'   Citations note on STRING website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_string_interactions_network(ids = c("9606.ENSP00000269305",
#'     "9606.ENSP00000398698",
#'     "9606.ENSP00000275493"),
#'     network_type = "functional")
#' }
#' \donttest{
#' rba_string_interactions_network(ids = c("9606.ENSP00000269305",
#'     "9606.ENSP00000398698",
#'     "9606.ENSP00000275493"),
#'     species = 9606,
#'     add_nodes = 10)
#' }
#' \donttest{
#' rba_string_interactions_network(
#'     ids = NULL,
#'     network_term_id = "GO:0050852",
#'     species = 9606,
#'     required_score = 900
#' )
#' }
#'
#' @family "STRING"
#' @seealso
#'   \code{
#'   \link{rba_string_map_ids},
#'   \link{rba_string_interaction_partners},
#'   \link{rba_string_network_image},
#'   \link{rba_string_enrichment_ppi}
#'   }
#' @export
rba_string_interactions_network <- function(ids,
                                            species = NULL,
                                            required_score = NULL,
                                            add_nodes = NULL,
                                            network_type = "functional",
                                            use_query_labels = FALSE,
                                            network_term_id = NULL,
                                            ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(
        arg = "ids",
        class = c("character", "numeric", "integer"),
        min_len = 1L,
        no_null = FALSE
      ),
      list(arg = "network_term_id", class = "character", len = 1L),
      list(
        arg = "species", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 1
      ),
      list(
        arg = "required_score",
        class = c("numeric", "integer"),
        min_val = 0,
        max_val = 1000,
        len = 1L
      ),
      list(
        arg = "add_nodes",
        class = c("numeric", "integer"),
        integerish = TRUE,
        min_val = 0,
        len = 1L
      ),
      list(
        arg = "network_type",
        class = "character",
        val = c("functional", "physical"),
        len = 1L
      ),
      list(arg = "use_query_labels", class = "logical", len = 1L)
    ),
    cond = list(
      list(
        quote(!is.null(ids) && !is.null(network_term_id)),
        paste0(
          "`ids` and `network_term_id` cannot be supplied together. ",
          "Set `ids = NULL` when using `network_term_id`."
        )
      ),
      list(
        quote(is.null(ids) && is.null(network_term_id)),
        "`network_term_id` must be supplied when `ids = NULL`."
      ),
      list(
        quote(
          is.null(ids) &&
            !is.null(network_term_id) &&
            is.null(species)
        ),
        "`species` must be supplied when using `network_term_id`."
      ),
      list(
        quote(
          !is.null(ids) &&
            is.null(network_term_id) &&
            length(unique(ids)) > 10 &&
            is.null(species)
        ),
        sprintf(
          "You supplied %s unique IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(unique(ids))
        )
      )
    )
  )

  if (is.null(ids)) {
    .msg(
      "Retrieving STRING network interactions for functional term %s.",
      network_term_id
    )
  } else {
    .msg(
      "Retrieving STRING Network interaction of %s unique input Identifiers.",
      length(unique(ids))
    )
  }

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("identifiers", !is.null(ids), paste(unique(ids), collapse = "%0d")),
    list("network_term_id", !is.null(network_term_id), network_term_id),
    list("species", !is.null(species), species),
    list("required_score", !is.null(required_score), required_score),
    list("add_nodes", !is.null(add_nodes), add_nodes),
    list("network_type", !is.null(network_type), network_type),
    list("show_query_node_labels", use_query_labels, "1")
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), "json/network"),
    body = call_body,
    encode = "form",
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("rba_string_interactions_network.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get All STRING Interaction Partners
#'
#' This function retrieves STRING interactions involving any of your input
#'   proteins as one party of the interaction, including interactions with
#'   proteins outside the input set.
#'   \cr Given the size of the STRING database, this function can return many
#'   interactions. Use the filtering arguments to limit the results.
#'
#' To retrieve only interactions among the input proteins, see
#'   \code{\link{rba_string_interactions_network}}.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/interaction_partners?
#'  identifiers=\{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#' @param species Numeric:
#'   \href{https://www.ncbi.nlm.nih.gov/taxonomy/}{
#'   NCBI Taxonomy identifier}; Human Taxonomy ID is 9606.
#'   (Recommended, but required if your input contains more than 10 unique IDs.)
#' @param required_score Numeric (between 0 and 1000): Minimum interaction
#'   score required for an interaction to be included in the returned
#'   interactions. If omitted, STRING applies a network-dependent threshold.
#'   Common confidence thresholds are 150 (low), 400 (medium), 700 (high), and
#'   900 (highest).
#' @param network_type Character: One of:\itemize{
#'   \item "functional": (default) Edges indicate both physical and
#'   functional associations.
#'   \item "physical": Edges indicate that two proteins have a physical
#'   interaction or are parts of a complex.}
#' @param limit Numeric: Maximum number of interaction partners returned for
#'   each input protein, ordered by confidence.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame in which each row is a network interaction and the
#'   columns contain interactor information and interaction scores.
#'
#' @references \itemize{
#'   \item Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina
#'    Nastou, Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang,
#'    Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian
#'    von Mering, The STRING database in 2023: protein–protein association
#'    networks and functional enrichment analyses for any sequenced genome of
#'    interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January 2023,
#'    Pages D638–D646, https://doi.org/10.1093/nar/gkac1000
#'   \item \href{https://string-db.org/help/api/}{STRING API Documentation}
#'   \item
#'   \href{https://string-db.org/cgi/about?footer_active_subpage=references}{
#'   Citations note on STRING website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_string_interaction_partners(ids = c("9606.ENSP00000269305",
#'     "9606.ENSP00000398698",
#'     "9606.ENSP00000275493"),
#'     network_type = "functional")
#' }
#' \donttest{
#'     rba_string_interaction_partners(ids = "9606.ENSP00000269305",
#'     species = 9606,
#'     required_score = 700)
#' }
#'
#' @family "STRING"
#' @seealso
#'   \code{
#'   \link{rba_string_map_ids},
#'   \link{rba_string_interactions_network},
#'   \link{rba_string_network_image},
#'   \link{rba_string_enrichment_ppi}
#'   }
#' @export
rba_string_interaction_partners <- function(ids,
                                            species = NULL,
                                            required_score = NULL,
                                            network_type = "functional",
                                            limit = NULL,
                                            ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "ids", class = c("character", "numeric", "integer"), min_len = 1L),
      list(
        arg = "species", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 1
      ),
      list(
        arg = "required_score",
        class = c("numeric", "integer"),
        min_val = 0,
        max_val = 1000,
        len = 1L
      ),
      list(
        arg = "network_type",
        class = "character",
        val = c("functional", "physical"),
        len = 1L
      ),
      list(
        arg = "limit",
        class = c("numeric", "integer"),
        integerish = TRUE,
        min_val = 1,
        len = 1L
      )
    ),
    cond = list(
      list(
        quote(length(unique(ids)) > 10 && is.null(species)),
        sprintf(
          "You supplied %s unique IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(unique(ids))
        )
      )
    )
  )

  .msg(
    "Retrieving Interacting partners of %s unique input Identifiers.",
    length(unique(ids))
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(unique(ids), collapse = "%0d"),
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("species", !is.null(species), species),
    list("limit", !is.null(limit), limit),
    list("required_score", !is.null(required_score), required_score),
    list("network_type", !is.null(network_type), network_type)
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), "json/interaction_partners"),
    body = call_body,
    encode = "form",
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("string_interaction_partners.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Protein Similarity Scores Within a Species
#'
#' Retrieve Smith-Waterman bit scores among proteins from the same species.
#'   STRING uses these sequence-similarity scores as a proxy for protein
#'   homology.
#'
#' To retrieve the best similarity hit for each input protein in other STRING
#'   species, see
#'   \code{\link{rba_string_homology_inter}}.
#'
#' STRING imports the similarity matrix from the
#'   \href{https://doi.org/10.1093/nar/gkt970}{Similarity Matrix of
#'   Proteins (SIMAP)} project.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/homology?identifiers=
#'  \{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#' @param species Numeric:
#'   \href{https://www.ncbi.nlm.nih.gov/taxonomy/}{
#'   NCBI Taxonomy identifier}; Human Taxonomy ID is 9606.
#'   (Recommended, but required if your input contains more than 10 unique IDs.)
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame containing pairwise bit scores and self-hits for the
#'   supplied proteins. STRING returns only one half of the symmetric similarity
#'   matrix to reduce data transfer.
#'
#' @references \itemize{
#'   \item Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina
#'    Nastou, Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang,
#'    Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian
#'    von Mering, The STRING database in 2023: protein–protein association
#'    networks and functional enrichment analyses for any sequenced genome of
#'    interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January 2023,
#'    Pages D638–D646, https://doi.org/10.1093/nar/gkac1000
#'   \item \href{https://string-db.org/help/api/}{STRING API Documentation}
#'   \item
#'   \href{https://string-db.org/cgi/about?footer_active_subpage=references}{
#'   Citations note on STRING website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_string_homology_intra(ids = c("CDK1", "CDK2"), species = 9606)
#' }
#'
#' @family "STRING"
#' @seealso
#'   \code{
#'   \link{rba_string_map_ids},
#'   \link{rba_string_homology_inter}
#'   }
#' @export
rba_string_homology_intra <- function(ids,
                                      species = NULL,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "ids", class = c("character", "numeric", "integer"), min_len = 1L),
      list(
        arg = "species", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 1
      )
    ),
    cond = list(
      list(
        quote(length(unique(ids)) > 10 && is.null(species)),
        sprintf(
          "You supplied %s unique IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(unique(ids))
        )
      )
    )
  )

  .msg(
    "Retrieving similarity scores of %s unique input Identifiers.",
    length(unique(ids))
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(unique(ids), collapse = "%0d"),
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("species", !is.null(species), species)
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), "json/homology"),
    body = call_body,
    encode = "form",
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("string_homology.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Best Protein Similarity Hits Across Species
#'
#' Retrieve the highest Smith-Waterman bit-score hit between each input protein
#'   and proteins in every other STRING species. STRING uses these
#'   sequence-similarity scores as a proxy for protein homology.
#'
#' To retrieve pairwise similarity scores among input proteins within one
#'   species, see \code{\link{rba_string_homology_intra}}.
#'
#' STRING imports the similarity matrix from the
#'   \href{https://doi.org/10.1093/nar/gkt970}{Similarity Matrix of
#'   Proteins (SIMAP)} project.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/homology_best?
#'  identifiers=\{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#' @param species Numeric:
#'   \href{https://www.ncbi.nlm.nih.gov/taxonomy/}{
#'   NCBI Taxonomy identifier} of your input proteins; Human Taxonomy ID is
#'   9606. (Recommended, but required if your input contains more than 10
#'   unique IDs.)
#' @param species_b Numeric: One or more
#'   \href{https://www.ncbi.nlm.nih.gov/taxonomy/}{
#'   NCBI Taxonomy identifiers} used to restrict the search for closest
#'   homologs. The default is \code{NULL}, which searches all STRING species.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame containing each input protein and its closest homolog
#'   in every other STRING species, or in the species selected by
#'   \code{species_b}.
#'
#' @references \itemize{
#'   \item Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina
#'    Nastou, Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang,
#'    Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian
#'    von Mering, The STRING database in 2023: protein–protein association
#'    networks and functional enrichment analyses for any sequenced genome of
#'    interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January 2023,
#'    Pages D638–D646, https://doi.org/10.1093/nar/gkac1000
#'   \item \href{https://string-db.org/help/api/}{STRING API Documentation}
#'   \item
#'   \href{https://string-db.org/cgi/about?footer_active_subpage=references}{
#'   Citations note on STRING website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_string_homology_inter(ids = "p53",
#'     species = 9606,
#'     species_b = 7070)
#' }
#' \donttest{
#' rba_string_homology_inter(ids = "ENSP00000269305", species = 9606)
#' }
#'
#' @family "STRING"
#' @seealso
#'   \code{
#'   \link{rba_string_map_ids},
#'   \link{rba_string_homology_intra}
#'   }
#' @export
rba_string_homology_inter <- function(ids,
                                      species = NULL,
                                      species_b = NULL,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "ids", class = c("character", "numeric", "integer"), min_len = 1L),
      list(
        arg = "species", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 1
      ),
      list(
        arg = "species_b", class = c("numeric", "integer"), min_len = 1L,
        integerish = TRUE, min_val = 1
      )
    ),
    cond = list(
      list(
        quote(length(unique(ids)) > 10 && is.null(species)),
        sprintf(
          "You supplied %s unique IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(unique(ids))
        )
      )
    )
  )

  .msg(
    "Retrieving Best similarity scores hits of %s unique input Identifiers.",
    length(unique(ids))
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(unique(ids), collapse = "%0d"),
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("species", !is.null(species), species),
    list("species_b", !is.null(species_b), paste(unique(species_b), collapse = "%0d"))
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), "json/homology_best"),
    body = call_body,
    encode = "form",
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("string_homology_best.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}


#' Get Functional Enrichment
#'
#' STRING cross-references proteins with several annotation resources. (See
#'   'Details' section). Provide an input protein set and, optionally, a
#'   background protein set to perform an enrichment test and retrieve
#'   enriched terms with their associated statistics.
#'   Use \code{\link{rba_string_enrichment_image}} to retrieve the analysis
#'   results as a plot.
#'
#' STRING currently returns enrichment results from Gene
#'   Ontology (GO), KEGG pathways, UniProt Keywords, PubMed publications, Pfam
#'   domains, InterPro domains, and SMART domains.
#'   \cr STRING returns only terms with a raw p-value below 0.1. To retrieve
#'   annotations without filtering by enrichment p-value, use
#'   \code{\link{rba_string_annotations}}.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/enrichment?identifiers=
#'  \{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#'   Note that if only one ID is supplied, STRING expands the network by 10
#'   proteins.
#' @param species Numeric:
#'   \href{https://www.ncbi.nlm.nih.gov/taxonomy/}{
#'   NCBI Taxonomy identifier}; Human Taxonomy ID is 9606.
#'   (Recommended, but optional.)
#' @param background Character vector: A set of STRING protein IDs
#'   to be used as the statistical background (or universe) when computing
#'   term p-values. Only STRING IDs are accepted. See
#'   \code{\link{rba_string_map_ids}} to map your IDs.
#' @param split_df Logical: (default = \code{TRUE}) Split results into a list
#'   of data frames by \code{category}; otherwise, return one data frame.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame in which each row is an enriched term with a raw p-value
#'   below 0.1 and the columns contain the term category, description, gene
#'   counts, p-value, FDR, and other pertinent information. If
#'   \code{split_df = TRUE}, a list of data frames split by category is returned.
#'
#' @references \itemize{
#'   \item Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina
#'    Nastou, Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang,
#'    Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian
#'    von Mering, The STRING database in 2023: protein–protein association
#'    networks and functional enrichment analyses for any sequenced genome of
#'    interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January 2023,
#'    Pages D638–D646, https://doi.org/10.1093/nar/gkac1000
#'   \item \href{https://string-db.org/help/api/}{STRING API Documentation}
#'   \item
#'   \href{https://string-db.org/cgi/about?footer_active_subpage=references}{
#'   Citations note on STRING website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_string_enrichment(ids = c("TP53", "TNF", "EGFR"), species = 9606)
#' }
#'
#' @family "STRING"
#' @family "Enrichment/Over-representation"
#' @seealso
#'   \code{
#'   \link{rba_string_map_ids},
#'   \link{rba_string_annotations},
#'   \link{rba_string_enrichment_image},
#'   \link{rba_string_functional_terms}
#'   }
#' @export
rba_string_enrichment <- function(ids,
                                  species = NULL,
                                  background = NULL,
                                  split_df = TRUE,
                                  ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "ids", class = c("character", "numeric", "integer"), min_len = 1L),
      list(
        arg = "species", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 1
      ),
      list(arg = "background", class = "character", min_len = 1L),
      list(arg = "split_df", class = "logical", len = 1L)
    )
  )

  .msg(
    "Performing functional enrichment of %s unique input Identifiers.",
    length(unique(ids))
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(unique(ids), collapse = "%0d"),
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("species", !is.null(species), species),
    list("background_string_identifiers", !is.null(background), paste(unique(background), collapse = "%0d"))
  )

  ## Build Function-Specific Call
  if (isTRUE(split_df)) {
    parser_input <- list(
      "json->df",
      function(x) {
        if (utils::hasName(x, "category")) { split(x, x$category) } else { x }
      }
    )
  } else {
    parser_input <- "json->df"
  }

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), "json/enrichment"),
    body = call_body,
    encode = "form",
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("string_enrichment.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Functional Annotations
#'
#' STRING cross-references proteins with several annotation resources. This
#'   function retrieves the complete set of annotations assigned to the input
#'   proteins, together with information about each term.
#'
#' STRING currently retrieves annotations based on Gene Ontology (GO), UniProt
#'   Keywords, PubMed publications, Pfam domains, InterPro domains, and SMART
#'   domains. KEGG annotations are unavailable from this endpoint because of
#'   KEGG licensing restrictions.
#'   \cr This function returns annotations without enrichment filtering. To
#'   perform enrichment and retrieve only enriched terms, use
#'   \code{\link{rba_string_enrichment}}.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/functional_annotation?
#'  identifiers=\{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#' @param species Numeric:
#'   \href{https://www.ncbi.nlm.nih.gov/taxonomy/}{
#'   NCBI Taxonomy identifier}; Human Taxonomy ID is 9606.
#'   (Recommended, but optional.)
#' @param allow_pubmed Logical (default = \code{FALSE}): Include PubMed
#'   annotations. These annotations are excluded by default because many
#'   publications may be assigned to each protein. This argument is ignored
#'   when \code{only_pubmed = TRUE}.
#' @param split_df Logical: (default = \code{TRUE}) Split results into a list
#'   of data frames by \code{category}; otherwise, return one data frame.
#' @param only_pubmed Logical (default = \code{FALSE}): Return only PubMed
#'   annotations. This takes precedence over \code{allow_pubmed}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame in which every row is an assigned term and the columns
#'   contain the term category, description, number of genes, and other
#'   pertinent information. If \code{split_df = TRUE}, a list of data frames
#'   split by category is returned. With \code{only_pubmed = TRUE}, a
#'   one-element list named \code{PMID} is returned when PubMed annotations are
#'   available.
#'
#' @references \itemize{
#'   \item Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina
#'    Nastou, Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang,
#'    Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian
#'    von Mering, The STRING database in 2023: protein–protein association
#'    networks and functional enrichment analyses for any sequenced genome of
#'    interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January 2023,
#'    Pages D638–D646, https://doi.org/10.1093/nar/gkac1000
#'   \item \href{https://string-db.org/help/api/}{STRING API Documentation}
#'   \item
#'   \href{https://string-db.org/cgi/about?footer_active_subpage=references}{
#'   Citations note on STRING website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_string_annotations(ids = "TP53", species = 9606)
#' }
#' \dontrun{
#' rba_string_annotations(
#'     ids = "TP53",
#'     species = 9606,
#'     only_pubmed = TRUE
#' )
#' }
#'
#' @family "STRING"
#' @seealso
#'   \code{
#'   \link{rba_string_map_ids},
#'   \link{rba_string_enrichment},
#'   \link{rba_string_enrichment_image},
#'   \link{rba_string_functional_terms}
#'   }
#' @export
rba_string_annotations <- function(ids,
                                   species = NULL,
                                   allow_pubmed = FALSE,
                                   split_df = TRUE,
                                   only_pubmed = FALSE,
                                   ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "ids", class = c("character", "numeric", "integer"), min_len = 1L),
      list(
        arg = "species", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 1
      ),
      list(arg = "allow_pubmed", class = "logical", len = 1L),
      list(arg = "split_df", class = "logical", len = 1L),
      list(arg = "only_pubmed", class = "logical", len = 1L)
    ),
    cond = list(
      list(
        quote(isTRUE(allow_pubmed) && isTRUE(only_pubmed)),
        paste0(
          "`allow_pubmed` is ignored when `only_pubmed = TRUE`; ",
          "only PubMed annotations will be returned."
        ),
        warn = TRUE
      )
    )
  )

  .msg(
    "Retrieving functional annotations of %s unique input Identifiers.",
    length(unique(ids))
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(unique(ids), collapse = "%0d"),
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("species", !is.null(species), species),
    list("allow_pubmed", allow_pubmed && !only_pubmed, 1),
    list("only_pubmed", only_pubmed, 1)
  )

  ## Build Function-Specific Call
  if (isTRUE(split_df)) {
    parser_input <- list(
      "json->df",
      function(x) { split(x, x$category) }
    )
  } else {
    parser_input <- "json->df"
  }

  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), "json/functional_annotation"),
    body = call_body,
    encode = "form",
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("string_functional_annotation.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Search STRING Functional Terms
#'
#' STRING maps several functional annotation resources onto its proteins. This
#'   function searches for functional terms using an identifier or descriptive
#'   text and retrieves the matching terms and their annotated proteins.
#'
#' This endpoint supports only one species per query. If multiple functional
#'   terms match \code{term_text}, STRING returns them in order of relevance,
#'   with the best match first.
#'   \cr The complete number of annotated proteins is reported in
#'   \code{proteinCount}.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/functional_terms?term_text=
#'  \{your_term\}&\{optional_parameters\}"
#'
#' @param term_text Character: A functional term identifier or descriptive text
#'   used to match one or more functional terms.
#' @param species Numeric:
#'   \href{https://www.ncbi.nlm.nih.gov/taxonomy/}{
#'   NCBI Taxonomy identifier}; Human Taxonomy ID is 9606.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame in which every row is a matching functional term and
#'   the columns contain the term category, identifier, description, number of
#'   annotated proteins, preferred protein names, and STRING protein IDs.
#'   \code{preferredNames} and \code{stringIds} are returned as list-columns.
#'
#' @references \itemize{
#'   \item Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina
#'    Nastou, Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang,
#'    Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian
#'    von Mering, The STRING database in 2023: protein–protein association
#'    networks and functional enrichment analyses for any sequenced genome of
#'    interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January 2023,
#'    Pages D638–D646, https://doi.org/10.1093/nar/gkac1000
#'   \item \href{https://string-db.org/help/api/}{STRING API Documentation}
#'   \item
#'   \href{https://string-db.org/cgi/about?footer_active_subpage=references}{
#'   Citations note on STRING website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_string_functional_terms(
#'     term_text = "T cell receptor signaling pathway",
#'     species = 9606
#' )
#' }
#'
#' @family "STRING"
#' @seealso
#'   \code{
#'   \link{rba_string_annotations},
#'   \link{rba_string_enrichment},
#'   \link{rba_string_enrichment_image}
#'   }
#' @export
rba_string_functional_terms <- function(term_text,
                                        species,
                                        ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "term_text", class = "character", len = 1L),
      list(
        arg = "species", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 1
      )
    )
  )

  .msg(
    "Retrieving STRING functional terms matching '%s'.",
    term_text
  )

  ## Build POST API Request's body
  call_body <- list(
    "term_text" = term_text,
    "species" = species,
    "caller_identity" = getOption("rba_user_agent")
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), "json/functional_terms"),
    body = call_body,
    encode = "form",
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("string_functional_terms.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}


#' Get Protein-Protein Interaction Enrichment
#'
#' STRING compares the interaction pattern of your input proteins with the
#'   proteome-wide background interaction distribution to determine whether
#'   the protein set contains more interactions than expected.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/ppi_enrichment?identifiers=
#'  \{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#' @param species Numeric:
#'   \href{https://www.ncbi.nlm.nih.gov/taxonomy/}{
#'   NCBI Taxonomy identifier}; Human Taxonomy ID is 9606.
#'   (Recommended, but required if your input contains more than 10 unique IDs.)
#' @param required_score Numeric (between 0 and 1000): Minimum interaction
#'   score used when calculating PPI enrichment. If omitted, STRING applies a
#'   network-dependent threshold. Common
#'   confidence thresholds are 150 (low), 400 (medium), 700 (high), and 900
#'   (highest).
#' @param background Character vector: A set of STRING protein IDs
#'   to be used as the background proteome. Only STRING IDs are acceptable.
#'   See \code{\link{rba_string_map_ids}} to map your IDs.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list with protein-protein interaction enrichment results.
#'
#' @references \itemize{
#'   \item Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina
#'    Nastou, Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang,
#'    Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian
#'    von Mering, The STRING database in 2023: protein–protein association
#'    networks and functional enrichment analyses for any sequenced genome of
#'    interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January 2023,
#'    Pages D638–D646, https://doi.org/10.1093/nar/gkac1000
#'   \item \href{https://string-db.org/help/api/}{STRING API Documentation}
#'   \item
#'   \href{https://string-db.org/cgi/about?footer_active_subpage=references}{
#'   Citations note on STRING website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_string_enrichment_ppi(ids = c("p53", "BRCA1", "cdk2", "Q99835",
#'        "CDC42", "CDK1", "KIF23", "PLK1", "RAC2", "RACGAP1"),
#'     species = 9606)
#' }
#'
#' @family "STRING"
#' @seealso
#'   \code{
#'   \link{rba_string_map_ids},
#'   \link{rba_string_interactions_network},
#'   \link{rba_string_interaction_partners},
#'   \link{rba_string_network_image}
#'   }
#' @export
rba_string_enrichment_ppi <- function(ids,
                                      species = NULL,
                                      required_score = NULL,
                                      background = NULL,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "ids", class = c("character", "numeric", "integer"), min_len = 1L),
      list(
        arg = "species", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 1
      ),
      list(
        arg = "required_score",
        class = c("numeric", "integer"),
        min_val = 0,
        max_val = 1000,
        len = 1L
      ),
      list(arg = "background", class = "character", min_len = 1L)
    ),
    cond = list(
      list(
        quote(length(unique(ids)) > 10 && is.null(species)),
        sprintf(
          "You supplied %s unique IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(unique(ids))
        )
      )
    )
  )

  .msg(
    "Performing PPI Enrichment of %s unique input Identifiers.",
    length(unique(ids))
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(unique(ids), collapse = "%0d"),
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("species", !is.null(species), species),
    list("required_score", !is.null(required_score), required_score),
    list("background_string_identifiers", !is.null(background), paste(unique(background), collapse = "%0d"))
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), "json/ppi_enrichment"),
    body = call_body,
    encode = "form",
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("string_ppi_enrichment.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Current STRING Version
#'
#' Get the STRING version and stable address currently used by this package.
#'
#' STRING releases a new version approximately every two years. To support
#'   reproducibility, each release has a stable address that remains available
#'   after newer versions are released.
#'
#' @section Corresponding API Resources:
#'  "GET https://string-db.org/api/\{output-format\}/version"
#'
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list with STRING version and stable address.
#'
#' @references \itemize{
#'   \item Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina
#'    Nastou, Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang,
#'    Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian
#'    von Mering, The STRING database in 2023: protein–protein association
#'    networks and functional enrichment analyses for any sequenced genome of
#'    interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January 2023,
#'    Pages D638–D646, https://doi.org/10.1093/nar/gkac1000
#'   \item \href{https://string-db.org/help/api/}{STRING API Documentation}
#'   \item
#'   \href{https://string-db.org/cgi/about?footer_active_subpage=references}{
#'   Citations note on STRING website}
#'   }
#'
#' @examples
#' \donttest{
#' rba_string_version()
#' }
#'
#' @family "STRING"
#' @export
rba_string_version <- function(...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args()

  .msg(
    "Retrieving the STRING database version and address used by rbioapi."
  )

  ## Build POST API Request's body
  call_query <- list("format" = "text")

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), "json/version"),
    body = call_query,
    encode = "form",
    accept = "application/json",
    parser = "json->list_simp",
    save_to = .rba_file("string_version.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get STRING Enrichment Plot
#'
#' Retrieve a plot that summarizes STRING functional-enrichment results. Use
#'   \code{\link{rba_string_enrichment}} to retrieve the results as a data
#'   frame.
#'
#' Available \code{category} values are listed below. The default is "Process".
#'   \itemize{
#'   \item Process: Biological Process (Gene Ontology)
#'   \item Function: Molecular Function (Gene Ontology)
#'   \item Component: Cellular Component (Gene Ontology)
#'   \item Keyword: Annotated Keywords (UniProt)
#'   \item KEGG: KEGG Pathways
#'   \item RCTM: Reactome Pathways
#'   \item HPO: Human Phenotype (Monarch)
#'   \item MPO: The Mammalian Phenotype Ontology (Monarch)
#'   \item DPO: Drosophila Phenotype (Monarch)
#'   \item WPO: C. elegans Phenotype Ontology (Monarch)
#'   \item ZPO: Zebrafish Phenotype Ontology (Monarch)
#'   \item FYPO: Fission Yeast Phenotype Ontology (Monarch)
#'   \item Pfam: Protein Domains (Pfam)
#'   \item SMART: Protein Domains (SMART)
#'   \item InterPro: Protein Domains and Features (InterPro)
#'   \item PMID: Reference Publications (PubMed)
#'   \item NetworkNeighborAL: Local Network Cluster (STRING)
#'   \item COMPARTMENTS: Subcellular Localization (COMPARTMENTS)
#'   \item TISSUES: Tissue Expression (TISSUES)
#'   \item DISEASES: Disease-gene Associations (DISEASES)
#'   \item WikiPathways: WikiPathways}
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/enrichmentfigure?
#'  identifiers=\{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#'   If only one ID is supplied, STRING expands the network by 10
#'   proteins.
#' @param species Numeric:
#'   \href{https://www.ncbi.nlm.nih.gov/taxonomy/}{
#'   NCBI Taxonomy identifier}; Human Taxonomy ID is 9606.
#'   (Recommended, but required if your input contains more than 10 unique IDs.)
#' @param category Character: The term set to use for enrichment analysis.
#'   Valid values are:
#'   "Process" (default), "Function", "Component", "Keyword", "KEGG", "RCTM",
#'   "HPO", "MPO", "DPO", "WPO", "ZPO", "FYPO", "Pfam", "SMART", "InterPro",
#'   "PMID", "NetworkNeighborAL", "COMPARTMENTS", "TISSUES", "DISEASES", or
#'   "WikiPathways". See Details for descriptions.
#' @param image_format Character: One of:\itemize{
#'   \item "image": PNG image with normal resolution.
#'   \item "highres_image": High-resolution PNG image.
#'   \item "svg": Scalable Vector Graphics image.}
#' @param save_image Logical or Character:\itemize{
#'   \item TRUE: Save the image to an automatically-generated path.
#'   \item FALSE: Do not save the image, just return it as an R object.
#'   \item Character string: A valid file path to save the image to.}
#' @param group_by_similarity Numeric: Jaccard-index threshold used to group
#'   related terms visually. Valid values range from 0.1 to 1 in increments of
#'   0.1. The default is \code{NULL}, which disables grouping.
#' @param color_palette Character: Color palette used to represent FDR values.
#'   Valid values are
#'   "mint_blue" (default), "lime_emerald", "green_blue", "peach_purple",
#'   "straw_navy", or "yellow_pink".
#' @param number_of_term_shown Numeric: (default = 10) Maximum number of terms
#'   to include in the plot.
#' @param x_axis Character: Variable displayed on the x-axis and used to rank
#'   the results. Valid values are "signal" (default), "strength", "FDR", and
#'   "gene_count".
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A PNG image array or raw SVG content, depending on
#'   \code{image_format}.
#'
#' @references \itemize{
#'   \item Damian Szklarczyk, Rebecca Kirsch, Mikaela Koutrouli, Katerina
#'    Nastou, Farrokh Mehryary, Radja Hachilif, Annika L Gable, Tao Fang,
#'    Nadezhda T Doncheva, Sampo Pyysalo, Peer Bork, Lars J Jensen, Christian
#'    von Mering, The STRING database in 2023: protein–protein association
#'    networks and functional enrichment analyses for any sequenced genome of
#'    interest, Nucleic Acids Research, Volume 51, Issue D1, 6 January 2023,
#'    Pages D638–D646, https://doi.org/10.1093/nar/gkac1000
#'   \item \href{https://string-db.org/help/api/}{STRING API Documentation}
#'   \item
#'   \href{https://string-db.org/cgi/about?footer_active_subpage=references}{
#'   Citations note on STRING website}
#'   }
#'
#' @examples
#' \dontrun{
#'   rba_string_enrichment_image(
#'   ids = c("TP53", "TNF", "EGFR"),
#'   species = 9606,
#'   category = "KEGG"
#'   )
#' }
#' \dontrun{
#'   rba_string_enrichment_image(
#'   ids = c("TP53", "TNF", "EGFR"),
#'   species = 9606,
#'   x_axis = "strength",
#'   number_of_term_shown = 20
#'   )
#' }
#' \dontrun{
#'   rba_string_enrichment_image(
#'   ids = c("TP53", "TNF", "EGFR"),
#'   species = 9606,
#'   color_palette = "straw_navy"
#'   )
#' }
#'
#' @family "STRING"
#' @family "Enrichment/Over-representation"
#' @seealso
#'   \code{
#'   \link{rba_string_map_ids},
#'   \link{rba_string_enrichment},
#'   \link{rba_string_annotations},
#'   \link{rba_string_functional_terms}
#'   }
#' @export
rba_string_enrichment_image <- function(ids,
                                        species = NULL,
                                        category = "Process",
                                        image_format = "image",
                                        save_image = TRUE,
                                        group_by_similarity = NULL,
                                        color_palette = "mint_blue",
                                        number_of_term_shown = 10,
                                        x_axis = "signal",
                                        ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "ids", class = c("character", "numeric", "integer"), min_len = 1L),
      list(
        arg = "species", class = c("numeric", "integer"), len = 1L,
        integerish = TRUE, min_val = 1
      ),
      list(
        arg = "category", class = "character",
        val = c("Process", "Function", "Component",
                "Keyword", "KEGG", "RCTM",
                "HPO", "MPO", "DPO", "WPO", "ZPO", "FYPO",
                "Pfam", "SMART", "InterPro",
                "PMID", "NetworkNeighborAL",
                "COMPARTMENTS", "TISSUES", "DISEASES",
                "WikiPathways"),
        len = 1L
      ),
      list(
        arg = "image_format", class = "character",
        val = c("image", "highres_image", "svg"),
        len = 1L
      ),
      list(arg = "save_image", class = c("character", "logical"), len = 1L),
      list(
        arg = "group_by_similarity",
        class = c("numeric", "integer"),
        val = seq(0.1, 1, by = 0.1),
        len = 1L
      ),
      list(
        arg = "color_palette", class = "character",
        val = c("mint_blue", "lime_emerald", "green_blue", "peach_purple", "straw_navy", "yellow_pink"),
        len = 1L
      ),
      list(
        arg = "number_of_term_shown",
        class = c("numeric", "integer"),
        integerish = TRUE,
        min_val = 1,
        len = 1L
      ),
      list(
        arg = "x_axis",
        class = "character",
        val = c("signal", "strength", "FDR", "gene_count"),
        len = 1L
      )
    ),
    cond = list(
      list(
        quote(length(unique(ids)) > 10 && is.null(species)),
        sprintf(
          "You supplied %s unique IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(unique(ids))
        )
      )
    )
  )

  .msg(
    "Retrieving STRING enrichment plot of %s unique input Identifiers.",
    length(unique(ids))
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(unique(ids), collapse = "%0d"),
      "category" = category,
      "color_palette" = color_palette,
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("species", !is.null(species), species),
    list("group_by_similarity", !is.null(group_by_similarity), group_by_similarity),
    list("number_of_term_shown", number_of_term_shown != 10, as.integer(number_of_term_shown)),
    list("x_axis", x_axis != "signal", x_axis)
  )

  ## make file path
  if (image_format == "svg") {

    ext_input <- "svg"
    accept_input <- "image/svg+xml"
    parser_input <- function(x) { httr::content(x, as = "raw") }

  } else {

    ext_input <- "png"
    accept_input <- "image/png"
    parser_input <- function(x) {
      png::readPNG(httr::content(x, as = "raw"))
    }

  }

  save_image <- .rba_file(
    file = paste0("string_enrichment_image.", ext_input),
    save_to = save_image
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), image_format, "/enrichmentfigure"),
    accept = accept_input,
    parser = parser_input,
    body = call_body,
    encode = "form",
    save_to = save_image
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

