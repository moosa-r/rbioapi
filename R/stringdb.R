#' Map a Set of Identifiers to STRING Identifiers
#'
#' This function Calls STRING's API to Convert a set of identifiers
#'   to STRING Identifiers. Although You can call STRING services with a variety
#'   of common identifiers, It is recommended by STRING's documentations that
#'   you first map Your Protein/genes IDs to STRING IDs and then proceed
#'   with other STRING's functions.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/get_string_ids?identifiers=
#'  \{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your Common gene/protein Identifier(s) to be mapped.
#' @param species Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
#'   (Recommended, but optional if your input is less than 100 IDs.)
#' @param echo_query (default = FALSE) Include your input IDs as a column of the
#'   results.
#' @param limit (Numeric, Optional) A limit on the number of matches per input
#'   ID. The output are sorted to have the best matches first.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame with the mapped STRING IDs and other pertinent
#'   information.
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
                               echo_query = FALSE,
                               limit = NULL,
                               ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "ids", class = c("character", "numeric")),
      list(arg = "species", class = "numeric"),
      list(arg = "echo_query", class = "logical"),
      list(arg = "limit", class = "numeric")
    ),
    cond = list(
      list(
        quote(length(ids) > 100 && is.null(species)),
        sprintf(
          "You supplied %s IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(ids)
        )
      )
    )
  )

  .msg(
    "Mapping %s Input Identifiers to STRING Identifiers.",
    length(ids)
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(unique(ids), collapse = "%0d"),
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("species", !is.null(species),species),
    list("echo_query", echo_query, "1"),
    list("limit", !is.null(limit), limit)
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), "json/resolve"),
    body = call_body,
    encode = "form",
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("string_map_ids.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get STRING Network Image
#'
#' Depending on that you supplied a single protein ID or more than one protein
#'   ID, this function will produce a static image of the interaction networks
#'   among your input proteins or/and with other proteins. See the
#'   "Arguments" section to learn more about how you can modify the network
#'   image.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/network?identifiers=
#'  \{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#' @param species Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
#'   (Recommended, but optional if your input is less than 100 IDs.)
#' @param image_format one of:\itemize{
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
#' @param required_score Numeric (Between 0 to 1000): A minimum of interaction
#'   score for an interaction to be included in the image. if not supplied, the
#'   threshold will be applied by STRING Based in the network. (low Confidence
#'    = 150, Medium Confidence = 400, High Confidence = 700, Highest
#'    confidence = 900)
#' @param network_flavor The style of network edges, should be one of:\itemize{
#'   \item "evidence": (default) Line's color is based on the type of evidences that
#'   support the interaction.
#'   \item "confidence": Line's thickness is an indicator of the
#'   interaction's confidence score.
#'   \item "action": Line's Shape is an indicator of the interaction's predicted
#'   mode of actions.}
#' @param network_type should be one of:\itemize{
#'   \item "functional": (default) The edge's indicate both physical and
#'   functional associations.
#'   \item "physical": The edges indicate that two proteins have a physical
#'   interaction or are parts of a complex.}
#' @param hide_node_labels Logical: (Default = FALSE) Hide proteins names from
#'   the image
#' @param use_query_labels Logical: (Default = FALSE) Use the names supplied
#'   with the 'ids' argument as the nodes labels instead of STRING's default
#'   ones.
#' @param hide_disconnected_nodes Logical: (Default = FALSE) Hide proteins that
#'   are not connected to any other proteins from the image
#' @param hide_structure_pics Logical: (Default = FALSE) Hide protein's
#'   structure picture from inside the bubbles
#' @param flat_nodes Logical: (Default = FALSE) Make the nodes design flat
#'   instead of the default 3D design
#' @param node_labels_center Logical: (Default = FALSE) Position the protein
#'   names labels center aligned on the nodes
#' @param node_labels_font_size Numeric (Between 5 to 50, Default = 12) Font
#'   size of the protein nodes labels
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A network images which can be PNG or SVG depending on the inputs.
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
#'
#' @family "STRING"
#' @seealso \code{\link{rba_string_map_ids}}
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
                                     ...) {
  ## Load Global Options
  .rba_ext_args(..., ignore_save = TRUE)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "ids", class = c("character", "numeric")),
      list(arg = "species", class = "numeric"),
      list(
        arg = "image_format", class = "character",
        val = c("image", "highres_image", "svg")
      ),
      list(arg = "save_image", class = c("character", "logical")),
      list(arg = "add_color_nodes", class = "numeric"),
      list(arg = "add_white_nodes", class = "numeric"),
      list(arg = "required_score", class = "numeric", min_val = 0, max_val = 1000),
      list(
        arg = "network_flavor", class = "character",
        val = c("evidence", "confidence", "actions")
      ),
      list(arg = "network_type", class = "character", val = c("functional", "physical")),
      list(arg = "hide_node_labels", class = "logical"),
      list(arg = "use_query_labels", class = "logical"),
      list(arg = "hide_disconnected_nodes", class = "logical"),
      list(arg = "hide_structure_pics", class = "logical"),
      list(arg = "flat_nodes", class = "logical"),
      list(arg = "node_labels_center", class = "logical"),
      list(arg = "node_labels_font_size", class = "numeric", min_val = 5, max_val = 50)
    ),
    cond = list(
      list(
        quote(length(ids) > 100 && is.null(species)),
        sprintf(
          "You supplied %s IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(ids)
        )
      )
    )
  )

  .msg(
    "Retrieving STRING network image of %s Input Identifiers.",
    length(ids)
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(unique(ids), collapse = "%0d"),
      "caller_identity" = getOption("rba_user_agent")
    ),
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
    list("flat_node_design", flat_nodes, "1"),
    list("center_node_labels", node_labels_center, "1"),
    list("custom_label_font_size", node_labels_font_size != 12, node_labels_font_size)
  )

  ## make file path
  if (image_format == "svg") {

    ext_input <- "svg"
    accept_input <- "image/svg+xml"
    parser_input <- function(x) { httr::content(x) }

  } else {

    ext_input <- "png"
    accept_input <- "image/png"
    parser_input <- function(x) { httr::content(x, type = "image/png") }

  }

  save_image <- .rba_file(
    file = paste0("string_network_image.", ext_input),
    save_to = save_image
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "post",
    url = .rba_stg("string", "url"),
    path = paste0(.rba_stg("string", "pth"), "image/network"),
    accept = accept_input,
    parser = parser_input,
    body = call_body,
    save_to = save_image
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get STRING Network Interactions
#'
#' This function will retrieve Sting interaction pairs among your input protein
#'   ids, with the combined score and separate score for each STRING score
#'   channels. You can further expand your network to a defined size by
#'   providing "add_node" parameter.
#'
#' Note that this function will return interactions between your set of
#'   supplied proteins, or at most, expand the interaction network by the
#'   given parameters. TO retrieve a list of all possible interacting proteins
#'   with your given input, see
#'   \code{\link{rba_string_interaction_partners}}.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/network?identifiers=
#'  \{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your protein IDs. It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#' @param species Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
#'   (Recommended, but optional if your input is less than 100 IDs.)
#' @param required_score Numeric: A minimum of interaction score for an
#'   interaction to be included in the image. if not supplied, the threshold
#'   will be applied by STRING Based in the network. (low Confidence = 150,
#'   Medium Confidence = 400, High Confidence = 700, Highest confidence = 900)
#' @param add_nodes Numeric: Number of neighboring proteins to be added to the
#'   network. If none supplied by the user, this argument value will depend
#'   on the number of supplied "ids" argument:\enumerate{
#'   \item Single id: add_node will be set to 10 to retrieve the interaction
#'   neighborhood  of you input protein.
#'   \item Multiple ids: add_node will be set to 0, thus the output will be the
#'   interactions between your input proteins.}
#' @param network_type should be one of:\itemize{
#'   \item "functional": (default) The edge's indicate both physical and
#'   functional associations.
#'   \item "physical": The edges indicate that two proteins have a physical
#'   interaction or are parts of a complex.}
#' @param use_query_labels Logical: (Default = FALSE) Use the names supplied
#'   with the 'ids' argument as the nodes labels instead of STRING's default
#'   ones.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame which each row is a network interaction and the
#'   columns contains interactor information and interaction scores: \itemize{
#'   \item stringId_A: STRING identifier (protein A)
#'   \item stringId_B:STRING identifier (protein B)
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
#'
#' @family "STRING"
#' @seealso
#'   \code{\link{rba_string_map_ids}, \link{rba_string_interaction_partners}}
#' @export
rba_string_interactions_network <- function(ids,
                                            species = NULL,
                                            required_score = NULL,
                                            add_nodes = NULL,
                                            network_type = "functional",
                                            use_query_labels = FALSE,
                                            ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "ids", class = c("character", "numeric")),
      list(arg = "species", class = "numeric"),
      list(arg = "required_score", class = "numeric", min_val = 0, max_val = 1000),
      list(arg = "add_nodes", class = "numeric", min_val = 0),
      list(arg = "network_type", class = "character", val = c("functional", "physical")),
      list(arg = "use_query_labels", class = "logical")),
    cond = list(
      list(
        quote(length(ids) > 100 && is.null(species)),
        sprintf(
          "You supplied %s IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(ids)
        )
      )
    )
  )

  .msg(
    "Retrieving STRING Network interaction of %s Input Identifiers.",
    length(ids)
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(unique(ids), collapse = "%0d"),
      "caller_identity" = getOption("rba_user_agent")
    ),
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
#' This function will retrieve all the STRING interactions which include your
#'   proteins as one party of the interaction. (e.g. interaction between your
#'   proteins and every other STRING proteins.)
#'   \cr Given the size of STRING database, this function could return a very
#'   long results. See "Arguments" section for information on how to filter
#'   the interactions.
#'
#' Note that this function will retrieve the interactions between your input
#'   proteins and every other STRING proteins. To retrieve the interaction
#'   among your input protein-set, see
#'   \code{\link{rba_string_interactions_network}}.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/
#'  interaction_partners?identifiers=\{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#' @param species Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
#'   (Recommended, but optional if your input is less than 100 IDs.)
#' @param required_score Numeric: A minimum of interaction score for an
#'   interaction to be included in the image. if not supplied, the threshold
#'   will be applied by STRING Based in the network. (low Confidence = 150,
#'   Medium Confidence = 400, High Confidence = 700, Highest confidence = 900)
#' @param network_type should be one of:\itemize{
#'   \item "functional": (default) The edge's indicate both physical and
#'   functional associations.}
#' @param limit Limit the number returned interaction partners per each of
#'   your input proteins. (e.g. Number of the most confident interaction partner
#'   to return per each input protein.)
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame which each row is a network interaction and the
#'   columns contains interactor information and interaction scores.
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
#'   \code{\link{rba_string_map_ids}, \link{rba_string_interactions_network}}
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
      list(arg = "ids", class = c("character", "numeric")),
      list(arg = "species", class = "numeric"),
      list(arg = "required_score", class = "numeric", min_val = 0, max_val = 1000),
      list(arg = "network_type", class = "character", val = c("functional", "physical")),
      list(arg = "limit", class = "numeric", min_val = 1)
    ),
    cond = list(
      list(
        quote(length(ids) > 100 && is.null(species)),
        sprintf(
          "You supplied %s IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(ids)
        )
      )
    )
  )

  .msg(
    "Retrieving Interacting partners of %s Input Identifiers.",
    length(ids)
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
    path = paste0(.rba_stg("string", "pth"), "/json/interaction_partners"),
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

#' Get Similarity Scores Hits of Proteins in a Species
#'
#' Using this function, you can retrieve the Smith-Waterman bit scores among
#'   proteins of the same species.
#'   Bit Scores serve as similarity scores between protein sequence;
#'   And, according to STRING documentations, as a proxy for protein homology.
#'
#' Note that this function will retrieve similarity scores of different
#'   proteins "within the same species". To Get a similarity scores of a given
#'   protein and it's closets homologous proteins in other species, see
#'   \code{\link{rba_string_homology_inter}}.
#'   \cr Similarity matrix is imported -by STRING- from:
#'   \href{https://cube.univie.ac.at/resources/simap}{Similarity Matrix of
#'   Proteins (SIMAP)}
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/homology?identifiers=
#'  \{your_identifiers\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#' @param species Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
#'   (Recommended, but optional if your input is less than 100 IDs.)
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame with bit scores between your supplied proteins and
#'   their self-hit. To Reduce the transferred data, STRING returns only one
#'   half of the similarity matrix; This will not pose a problem because
#'   similarity matrix is symmetrical.
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
#'   \code{\link{rba_string_map_ids}, \link{rba_string_homology_inter}}
#' @export
rba_string_homology_intra <- function(ids,
                                      species = NULL,
                                      ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "ids", class = c("character", "numeric")),
      list(arg = "species", class = "numeric")
    ),
    cond = list(
      list(
        quote(length(ids) > 100 && is.null(species)),
        sprintf(
          "You supplied %s IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(ids)
        )
      )
    )
  )

  .msg(
    "Retrieving similarity scores of %s Input Identifiers.",
    length(ids)
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

#' Get Similarity Scores Hits of Proteins in Different Species
#'
#' Using this function, you can retrieve highest Smith-Waterman bit scores
#'   among your input proteins and proteins in every other STRING species
#'   (e.g. the closest homologous protein of your input protein in other
#'   species).
#'   Bit Scores serve as similarity scores between protein sequence;
#'   And, according to STRING documentations, as a proxy for protein homology.
#'
#' Note that this function will return the highest similarity score hits of
#'   your given protein(s) and their closets homologous proteins in other
#'   species. to retrieve similarity scores of different proteins within the
#'   same species see \code{\link{rba_string_homology_intra}}.
#'   \cr Similarity matrix is imported -by STRING- from:
#'   \href{https://cube.univie.ac.at/resources/simap}{Similarity Matrix of
#'   Proteins (SIMAP)}
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output-format\}/homology_best?
#'  identifiers=\{your_identifiers\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#' @param species Numeric: NCBI Taxonomy identifier of your input proteins;
#'   Human Taxonomy ID is 9606. (Recommended, but optional if your input is
#'   less than 100 IDs.)
#' @param species_b (optional) Numeric: one or more NCBI Taxonomy identifiers
#'   of species to limit the closets homologous proteins search.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame with Your input proteins and it's closest homologous
#'   proteins among all other (or a defined) STRING species.
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
#'   \code{\link{rba_string_map_ids}, \link{rba_string_homology_intra}}
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
      list(arg = "ids", class = c("character", "numeric")),
      list(arg = "species", class = "numeric"),
      list(arg = "species_b", class = "numeric")
    ),
    cond = list(
      list(
        quote(length(ids) > 100 && is.null(species)),
        sprintf(
          "You supplied %s IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(ids)
        )
      )
    )
  )

  .msg(
    "Retrieving Best similarity scores hits of %s Input Identifiers.",
    length(ids)
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(unique(ids), collapse = "%0d"),
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("species", !is.null(species), species),
    list("species_b", !is.null(species_b), paste(unique(species_b),collapse = "%0d"))
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


#' Getting Functional Enrichment
#'
#' STRING cross-reference the proteins with several databases (see "Details"
#'   section). By providing your input set o proteins (and optionally
#'   background or universe protein set), you can use this function to
#'   perform enrichment test and retrieve a list of enriched terms in each
#'   database, among with pertinent information for each term.
#'   Use \code{\link{rba_string_enrichment_image}} to retrieve the analysis
#'   results as a plot.
#'
#' STRING currently maps to and retrieve enrichment results based on Gene
#'   Ontology (GO), KEGG pathways, UniProt Keywords, PubMed publications, Pfam
#'   domains, InterPro domains, and SMART domains.
#'   \cr Note that this function will only return the enriched terms pertinent
#'   to your proteins that have a p-value lesser than 0.1. To retrieve a full
#'   list of the terms -unfiltered by enrichment p-values-, use
#'   \code{\link{rba_string_annotations}}.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output_format\}/enrichment?identifiers=
#'  \{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#'   Note that if only one id is supplied, STRING expands the network by 10
#'   proteins.
#' @param species Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
#'   (Recommended, but optional if your input is less than 100 IDs.)
#' @param background character vector: A set of STRING protein IDs
#'   to be used as the statistical background (or universe) when computing
#'   P-value for the terms. Only STRING IDs are acceptable. (See
#'   \code{\link{rba_string_map_ids}} to map your IDs.)
#' @param split_df (logical, default = TRUE), If TRUE, instead of one
#'   data frame, results from different categories will be split into
#'   multiple data frames based on their 'category'.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list of data frames which every row is an enriched terms with p-value
#'   smaller than 0.1 and the columns are the terms category, description,
#'   number of genes, p-value, fdr and other pertinent information.
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
#'   \code{\link{rba_string_map_ids},
#'   \link{rba_string_annotations},
#'   \link{rba_string_enrichment_image}
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
      list(arg = "ids", class = c("character", "numeric")),
      list(arg = "species", class = "numeric"),
      list(arg = "background", class = "character"),
      list(arg = "split_df", class = "logical")
    ),
    cond = list(
      list(
        quote(length(ids) > 100 && is.null(species)),
        sprintf(
          "You supplied %s IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(ids)
        )
      )
    )
  )

  .msg(
    "Performing functional enrichment of %s Input Identifiers.",
    length(ids)
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

#' Retrieving Functional Annotation
#'
#' STRING cross-reference the proteins with several databases (see "Details"
#'   section). By providing your input set o proteins (and optionally
#'   background or universe protein set), you can use this function to
#'   retrieve full set of terms (annotations) pertinent to your input proteins in
#'   each database, among with information for each term.
#'
#' STRING currently maps to and retrieve enrichment results based on Gene
#'   Ontology (GO), KEGG pathways, UniProt Keywords, PubMed publications, Pfam
#'   domains, InterPro domains, and SMART domains.
#'   \cr Note that this function will return a full list of the terms containing
#'   your supplied proteins. To perform enrichment and only retrieve a enriched
#'   subset of the terms, use \code{\link{rba_string_enrichment}}.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output_format\}/functional_annotation?
#'  identifiers=\{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#' @param species Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
#'   (Recommended, but optional if your input is less than 100 IDs.)
#' @param allow_pubmed logical: (default = FALSE) PubMed usually  assigns a
#'   large number of reference publications to each protein. In order to reduce
#'   the output size, PubMed's results will be excluded from the results,
#'   unless stated otherwise by setting this argument to TRUE.
#' @param split_df (logical, default = TRUE), If TRUE, instead of one
#'   data frame, results from different categories will be split into
#'   multiple data frames based on their 'category'.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame which every row is an assigned terms and the columns
#'   are the terms category, description, number of genes, and other pertinent
#'   information.
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
#'
#' @family "STRING"
#' @seealso
#'   \code{\link{rba_string_map_ids},
#'   \link{rba_string_enrichment},
#'   \link{rba_string_enrichment_image}
#'   }
#' @export
rba_string_annotations <- function(ids,
                                   species = NULL,
                                   allow_pubmed = FALSE,
                                   split_df = TRUE,
                                   ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "ids", class = c("character", "numeric")),
      list(arg = "species", class = "numeric"),
      list(arg = "allow_pubmed", class = "logical"),
      list(arg = "split_df", class = "logical")
    ),
    cond = list(
      list(
        quote(length(ids) > 100 && is.null(species)),
        sprintf(
          "You supplied %s IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(ids)
        )
      )
    )
  )

  .msg(
    "Retrieving functional annotations of %s Input Identifiers.",
    length(ids)
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(unique(ids), collapse = "%0d"),
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("species", !is.null(species), species),
    list("allow_pubmed", allow_pubmed, 1)
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


#' Get Protein-Protein Interaction Enrichment
#'
#' Even when there is no annotation for your input proteins, STRING can Compare
#'   your Given proteins interactions pattern with the background proteome-wide
#'   interaction distribution to determine if your given set of proteins are
#'   functionally related.
#'
#' @section Corresponding API Resources:
#'  "POST https://string-db.org/api/\{output_format\}/ppi_enrichment?identifiers=
#'  \{your_identifiers\}&\{optional_parameters\}"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#' @param species Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
#'   (Recommended, but optional if your input is less than 100 IDs.)
#' @param required_score Numeric: A minimum of interaction score for an
#'   interaction to be included in the image. if not supplied, the threshold
#'   will be applied by STRING Based in the network. (low Confidence = 150,
#'   Medium Confidence = 400, High Confidence = 700, Highest confidence = 900)
#' @param background character vector: A set of STRING protein IDs
#'   to be used as the background proteome. Only STRING IDs are acceptable.
#'   (See \code{\link{rba_string_map_ids}} to map your IDs.)
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
#' @seealso \code{\link{rba_string_map_ids}}
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
      list(arg = "ids", class = c("character", "numeric")),
      list(arg = "species", class = "numeric"),
      list(arg = "required_score", class = "numeric", min_val = 0, max_val = 1000),
      list(arg = "background", class = "character")
    ),
    cond = list(
      list(
        quote(length(ids) > 100 && is.null(species)),
        sprintf(
          "You supplied %s IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(ids)
        )
      )
    )
  )

  .msg(
    "Performing PPI Enrichment of %s Input Identifiers.",
    length(ids)
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
#' Get STRING version and stable Address that this package currently uses.
#'
#' Note that STRING releases new version at approximately 2 years cycle.
#'   Nevertheless, to insure reproducibility, STRING dedicates a stable address
#'   for each release. Thus you can always reproduce research and results
#'   obtained via a certain STRING version. If the version that rbioapi returns
#'   is outdated, Kindly contact me.
#'
#' @section Corresponding API Resources:
#'  "GET https://string-db.org/api/\{output_format\}/version"
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
#' In addition to performing enrichment analysis, STRING allows you to also
#'   visualize the analysis results. Use \code{\link{rba_string_enrichment}}
#'   to retrieve the analysis results as a data frame.
#'
#' Available values for category are as follow. Default value is "Process".
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
#'  "POST https://string-db.org/api/\{output_format\}/enrichmentfigure"
#'
#' @param ids Your protein ID(s). It is strongly recommended to supply
#'   STRING IDs. See \code{\link{rba_string_map_ids}} for more information.
#'   Note that if only one id is supplied, STRING expands the network by 10
#'   proteins.
#' @param species Numeric: NCBI Taxonomy identifier; Human Taxonomy ID is 9606.
#' @param category The terms set to use to perform enrichment analysis. valid
#'   values are (See details for more info):
#'   "Process" (default), "Function", "Component", "Keyword", "KEGG", "RCTM",
#'   "HPO", "MPO", "DPO", "WPO", "ZPO", "FYPO", "Pfam", "SMART", "InterPro",
#'   "PMID", "NetworkNeighborAL", "COMPARTMENTS", "TISSUES", "DISEASES", or
#'   "WikiPathways"
#' @param image_format one of:\itemize{
#'   \item "image": PNG image with normal resolution.
#'   \item "highres_image": High-resolution PNG image.
#'   \item "svg": Scalable Vector Graphics image.}
#' @param save_image Logical or Character:\itemize{
#'   \item TRUE: Save the image to an automatically-generated path.
#'   \item FALSE: Do not save the image, just return it as an R object.
#'   \item Character string: A valid file path to save the image to.}
#' @param group_by_similarity Jackard index treshold to visually group the
#'   related terms. Valid values are between 0.1 to 1 with increment of 0.1.
#'   Default value is NULL (i.e. no grouping).
#' @param color_palette Color pallet to code FDR values. Valid values are:
#'   "mint_blue" (default), "lime_emerald", "green_blue", "peach_purple",
#'   "straw_navy", or "yellow_pink"
#' @param number_of_term_shown (default: 10) Maximum number of results to
#'    include in the plot.
#' @param x_axis The variable to show on the x axis and rank the results based
#'   on it. Valid values are: "signal" (default), "strength", "FDR", or
#'   "gene_count"
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A plot summarizing the enrichment results, which can be PNG or
#'   SVG depending on the inputs.
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
#'   \code{\link{rba_string_map_ids},
#'   \link{rba_string_enrichment},
#'   \link{rba_string_annotations}
#'   }
#' @export
rba_string_enrichment_image <- function(ids,
                                        species,
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
      list(arg = "ids", class = c("character", "numeric")),
      list(arg = "species", class = "numeric"),
      list(
        arg = "category", class = "character",
        val = c("Process", "Function", "Component",
                "Keyword", "KEGG", "RCTM",
                "HPO", "MPO", "DPO", "WPO", "ZPO", "FYPO",
                "Pfam", "SMART", "InterPro",
                "PMID", "NetworkNeighborAL",
                "COMPARTMENTS", "TISSUES", "DISEASES",
                "WikiPathways")
      ),
      list(
        arg = "image_format", class = "character",
        val = c("image", "highres_image", "svg")
      ),
      list(arg = "save_image", class = c("character", "logical")),
      list(arg = "group_by_similarity", class = "numeric", val = seq(0.1, 1, by = 0.1)),
      list(
        arg = "color_palette", class = "character",
        val = c("mint_blue", "lime_emerald", "green_blue", "peach_purple", "straw_navy", "yellow_pink")
      ),
      list(arg = "number_of_term_shown", class = "numeric", min_val = 1),
      list(
        arg = "x_axis", class = "character", val = c("signal", "strength", "FDR", "gene_count")
      )
    ),
    cond = list(
      list(
        quote(length(ids) > 100 && is.null(species)),
        sprintf(
          "You supplied %s IDs. Please Specify the species (Homo Sapiens NCBI taxonomy ID is 9606).",
          length(ids)
        )
      )
    )
  )

  .msg(
    "Retrieving STRING enrichment plot of %s input Identifiers.",
    length(ids)
  )

  ## Build POST API Request's body
  call_body <- .rba_query(
    init = list(
      "identifiers" = paste(unique(ids), collapse = "%0d"),
      "species" = species,
      "category" = category,
      "color_palette" = color_palette,
      "caller_identity" = getOption("rba_user_agent")
    ),
    list("group_by_similarity", !is.null(group_by_similarity), group_by_similarity),
    list("number_of_term_shown", number_of_term_shown != 10, as.integer(number_of_term_shown)),
    list("x_axis", x_axis != "signal", x_axis)
  )

  ## make file path
  if (image_format == "svg") {

    ext_input <- "svg"
    accept_input <- "image/svg+xml"
    parser_input <- function(x) { httr::content(x) }

  } else {

    ext_input <- "png"
    accept_input <- "image/png"
    parser_input <- function(x) { httr::content(x, type = "image/png") }

  }

  save_image <- .rba_file(
    file = paste0("string_network_image.", ext_input),
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
    save_to = save_image)

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

