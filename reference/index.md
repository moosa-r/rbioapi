# Package index

## Helper functions

Functions that assist you with the overall experience of rbioapi

- [`rba_options()`](https://rbioapi.moosa-r.com/reference/rba_options.md)
  : Set rbioapi Global Options
- [`rba_connection_test()`](https://rbioapi.moosa-r.com/reference/rba_connection_test.md)
  : Test if the Supported Services Are Responding
- [`rba_pages()`](https://rbioapi.moosa-r.com/reference/rba_pages.md) :
  Retrieve Multiple Pages of a Paginated Resource

## Enrichr (rba_enrichr\_\*)

Functions that connect you to
[Enrichr](https://maayanlab.cloud/Enrichr/) (Gene list enrichment
analysis tool) API resources.

- [`rba_enrichr()`](https://rbioapi.moosa-r.com/reference/rba_enrichr.md)
  : A One-step Wrapper for Gene-list Enrichment Using Enrichr
- [`rba_enrichr_add_background()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_background.md)
  : Upload Background Gene-List to Enrichr
- [`rba_enrichr_add_list()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_add_list.md)
  : Upload Your Gene-List to Enrichr
- [`rba_enrichr_enrich()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_enrich.md)
  : Get Enrichr Enrichment Results
- [`rba_enrichr_gene_map()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_map.md)
  : Find Enrichr Terms That Contain a Given Gene
- [`rba_enrichr_gene_sets()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_gene_sets.md)
  : Retrieve Gene Sets From an Enrichr Library
- [`rba_enrichr_libs()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_libs.md)
  : Retrieve a List of available libraries from Enrichr
- [`rba_enrichr_view_list()`](https://rbioapi.moosa-r.com/reference/rba_enrichr_view_list.md)
  : View an Uploaded Gene List

## JASPAR (rba_jaspar\_\*)

Functions that connect you to [JASPAR](https://jaspar.elixir.no/)
(Database of transcription factor binding profiles) API resources.

- [`rba_jaspar_collections()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_collections.md)
  : List collections available in JASPAR
- [`rba_jaspar_collections_matrices()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_collections_matrices.md)
  : List matrices available in a JASPAR collection
- [`rba_jaspar_matrix()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_matrix.md)
  : Get a position frequency matrix (PFM) with annotations
- [`rba_jaspar_matrix_search()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_matrix_search.md)
  : Search matrix profiles available in JASPAR
- [`rba_jaspar_matrix_versions()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_matrix_versions.md)
  : List matrix profile versions associated with a base ID
- [`rba_jaspar_releases()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_releases.md)
  : Get information about JASPAR database releases
- [`rba_jaspar_sites()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_sites.md)
  : Get binding sites of a matrix profile
- [`rba_jaspar_species()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_species.md)
  : List available species in JASPAR
- [`rba_jaspar_species_matrices()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_species_matrices.md)
  : List matrices available in JASPAR of a species
- [`rba_jaspar_taxons()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_taxons.md)
  : List available taxonomic groups in JASPAR
- [`rba_jaspar_taxons_matrices()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_taxons_matrices.md)
  : List matrices available in JASPAR of a taxonomic group
- [`rba_jaspar_tffm()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_tffm.md)
  : Get information about a TF flexible model (TFFM)
- [`rba_jaspar_tffm_search()`](https://rbioapi.moosa-r.com/reference/rba_jaspar_tffm_search.md)
  : Search TF flexible models (TFFMs) available in JASPAR

## miEAA (rba_mieaa\_\*)

Functions that connect you to
[miEAA](https://ccb-compute2.cs.uni-saarland.de/mieaa/) (miRNA
Enrichment Analysis and Annotation Tool) API resources.

- [`rba_mieaa_cats()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_cats.md)
  : Get Supported Enrichment Categories for a Species and miRNA Type
- [`rba_mieaa_convert_type()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_convert_type.md)
  : Convert Between Mature and Precursor miRNA Identifiers
- [`rba_mieaa_convert_version()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_convert_version.md)
  : Convert miRNA Identifiers Between Different miRBase Versions
- [`rba_mieaa_enrich()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich.md)
  : A One-step Wrapper for miRNA Enrichment Using miEAA
- [`rba_mieaa_enrich_results()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_results.md)
  : Retrieve the Results of a Finished miEAA Enrichment Analysis
- [`rba_mieaa_enrich_status()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_status.md)
  : Check the Status of a Submitted miEAA Enrichment Analysis
- [`rba_mieaa_enrich_submit()`](https://rbioapi.moosa-r.com/reference/rba_mieaa_enrich_submit.md)
  : Submit miEAA miRNA Enrichment Analysis Request

## PANTHER (rba_panther\_\*)

Functions that connect you to [PANTHER](https://www.pantherdb.org/)
(Protein Analysis THrough Evolutionary Relationships) API resources.

- [`rba_panther_enrich()`](https://rbioapi.moosa-r.com/reference/rba_panther_enrich.md)
  : PANTHER Over-Representation or Enrichment Analysis
- [`rba_panther_family()`](https://rbioapi.moosa-r.com/reference/rba_panther_family.md)
  : Get PANTHER Families and Sub-Families
- [`rba_panther_genome()`](https://rbioapi.moosa-r.com/reference/rba_panther_genome.md)
  : Retrieve Genes from a PANTHER Genome
- [`rba_panther_homolog()`](https://rbioapi.moosa-r.com/reference/rba_panther_homolog.md)
  : Search PANTHER for Homologs of Gene(s)
- [`rba_panther_info()`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md)
  : Get PANTHER database Information
- [`rba_panther_mapping()`](https://rbioapi.moosa-r.com/reference/rba_panther_mapping.md)
  : Map A Gene-set to PANTHER Database
- [`rba_panther_ortholog()`](https://rbioapi.moosa-r.com/reference/rba_panther_ortholog.md)
  : Search PANTHER for Orthologs of Gene(s)
- [`rba_panther_tree_grafter()`](https://rbioapi.moosa-r.com/reference/rba_panther_tree_grafter.md)
  : PANTHER Tree Grafter

## Reactome Analysis Services (rba_reactome\_\*)

Functions that connect you to [Reactome](https://reactome.org/) pathway
database’s Analysis services API resources.

- [`rba_reactome_analysis()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md)
  : Reactome Over-Representation or Expression Analysis
- [`rba_reactome_analysis_download()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_download.md)
  : Download Different Reactome Analysis Results
- [`rba_reactome_analysis_import()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_import.md)
  : Import Saved Analysis JSON to Reactome
- [`rba_reactome_analysis_mapping()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_mapping.md)
  : Maps Molecule Identifiers
- [`rba_reactome_analysis_pdf()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_pdf.md)
  : Generate PDF file with Reactome Analysis Results
- [`rba_reactome_analysis_species()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_species.md)
  : Compare Human Pathways with with Other Species
- [`rba_reactome_analysis_token()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_token.md)
  : Return the Results Associated with a Token

## Reactome Contents Services (rba_reactome\_\*)

Functions that connect you to [Reactome](https://reactome.org/) pathway
database’s Contents Services API resources.

### Database Info Queries

- [`rba_reactome_version()`](https://rbioapi.moosa-r.com/reference/rba_reactome_version.md)
  : The version number of current database

### Disease Related Queries

- [`rba_reactome_diseases()`](https://rbioapi.moosa-r.com/reference/rba_reactome_diseases.md)
  : Reactome Diseases

### Physical Entity Queries

- [`rba_reactome_complex_list()`](https://rbioapi.moosa-r.com/reference/rba_reactome_complex_list.md)
  : Get Complexes That Include a Molecule
- [`rba_reactome_complex_subunits()`](https://rbioapi.moosa-r.com/reference/rba_reactome_complex_subunits.md)
  : Get a Complex's Subunits
- [`rba_reactome_participant_of()`](https://rbioapi.moosa-r.com/reference/rba_reactome_participant_of.md)
  : Get Larger Reactome Structures Which Include an Entity
- [`rba_reactome_entity_other_forms()`](https://rbioapi.moosa-r.com/reference/rba_reactome_entity_other_forms.md)
  : Get Other forms of a Reactome Entity

### Queries Related to Events

- [`rba_reactome_event_ancestors()`](https://rbioapi.moosa-r.com/reference/rba_reactome_event_ancestors.md)
  : Get Reactome Events Ancestors
- [`rba_reactome_event_hierarchy()`](https://rbioapi.moosa-r.com/reference/rba_reactome_event_hierarchy.md)
  : Retrieve the Reactome Event Hierarchy of a Species

### Format Exporter

- [`rba_reactome_exporter_diagram()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_diagram.md)
  : Get a Reactome Event Diagram
- [`rba_reactome_exporter_event()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_event.md)
  : Exports A Reactome Event to SBGN or SBML
- [`rba_reactome_exporter_overview()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_overview.md)
  : Get a Reactome Pathway Overview
- [`rba_reactome_exporter_reaction()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_reaction.md)
  : Get a Reactome Reaction Event

### Molecule Interactors

- [`rba_reactome_interactors_psicquic()`](https://rbioapi.moosa-r.com/reference/rba_reactome_interactors_psicquic.md)
  : The interface From Reactome to PSICQUIC
- [`rba_reactome_interactors_static()`](https://rbioapi.moosa-r.com/reference/rba_reactome_interactors_static.md)
  : Get Static (IntAct) Interaction Information of a Protein

### Mapping Related Queries

- [`rba_reactome_mapping()`](https://rbioapi.moosa-r.com/reference/rba_reactome_mapping.md)
  : Map External ID to Reactome Pathways/Reactions

### Orthology Related Queries

- [`rba_reactome_orthology()`](https://rbioapi.moosa-r.com/reference/rba_reactome_orthology.md)
  : Get Orthologous (Computationally Inferred) Events

### Queries Related to Participants

- [`rba_reactome_participants()`](https://rbioapi.moosa-r.com/reference/rba_reactome_participants.md)
  : Get Participants of a Reactome Event

### Pathway Related Queries

- [`rba_reactome_pathways_events()`](https://rbioapi.moosa-r.com/reference/rba_reactome_pathways_events.md)
  : Get Events Contained in an Upstream Event
- [`rba_reactome_pathways_low()`](https://rbioapi.moosa-r.com/reference/rba_reactome_pathways_low.md)
  : Get lower level pathways Containing a 'Physical Entity' or Event
- [`rba_reactome_pathways_top()`](https://rbioapi.moosa-r.com/reference/rba_reactome_pathways_top.md)
  : Get Top Level Pathways in a Species

### Person Queries

- [`rba_reactome_people_id()`](https://rbioapi.moosa-r.com/reference/rba_reactome_people_id.md)
  : Get a Person by Identifier
- [`rba_reactome_people_name()`](https://rbioapi.moosa-r.com/reference/rba_reactome_people_name.md)
  : Get Persons Information by Name

### Common Data Retrieval

- [`rba_reactome_query()`](https://rbioapi.moosa-r.com/reference/rba_reactome_query.md)
  : Retrieve Reactome Knowledgebase Objects
- [`rba_reactome_search()`](https://rbioapi.moosa-r.com/reference/rba_reactome_search.md)
  : Search the Reactome Knowledgebase

### ReferenceEntity Queries

- [`rba_reactome_xref()`](https://rbioapi.moosa-r.com/reference/rba_reactome_xref.md)
  : Map Cross-Reference Identifiers to Reactome

### Species Related Queries

- [`rba_reactome_species()`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  : Get Reactome Species

## STRING (rba_string\_\*)

Functions that connect you to [STRING](https://string-db.org/)
(Functional protein association networks) API resources.

- [`rba_string_annotations()`](https://rbioapi.moosa-r.com/reference/rba_string_annotations.md)
  : Get Functional Annotations
- [`rba_string_enrichment()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment.md)
  : Get Functional Enrichment
- [`rba_string_enrichment_image()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_image.md)
  : Get STRING Enrichment Plot
- [`rba_string_enrichment_ppi()`](https://rbioapi.moosa-r.com/reference/rba_string_enrichment_ppi.md)
  : Get Protein-Protein Interaction Enrichment
- [`rba_string_functional_terms()`](https://rbioapi.moosa-r.com/reference/rba_string_functional_terms.md)
  : Search STRING Functional Terms
- [`rba_string_homology_inter()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_inter.md)
  : Get Best Protein Similarity Hits Across Species
- [`rba_string_homology_intra()`](https://rbioapi.moosa-r.com/reference/rba_string_homology_intra.md)
  : Get Protein Similarity Scores Within a Species
- [`rba_string_interaction_partners()`](https://rbioapi.moosa-r.com/reference/rba_string_interaction_partners.md)
  : Get All STRING Interaction Partners
- [`rba_string_interactions_network()`](https://rbioapi.moosa-r.com/reference/rba_string_interactions_network.md)
  : Get STRING Network Interactions
- [`rba_string_map_ids()`](https://rbioapi.moosa-r.com/reference/rba_string_map_ids.md)
  : Map a Set of Identifiers to STRING Identifiers
- [`rba_string_network_image()`](https://rbioapi.moosa-r.com/reference/rba_string_network_image.md)
  : Get STRING Network Image
- [`rba_string_version()`](https://rbioapi.moosa-r.com/reference/rba_string_version.md)
  : Get Current STRING Version

## UniProt (rba_uniprot\_\*)

Functions that connect you to [UniProt](https://www.uniprot.org/)
(Universal Protein Resource) API resources.

### Proteins - Proteins

- [`rba_uniprot_antigens()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_antigens.md)
  : Get Antigens by UniProt Accession
- [`rba_uniprot_antigens_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_antigens_search.md)
  : Search Antigens in UniProt
- [`rba_uniprot_epitope()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_epitope.md)
  : Retrieve Epitopes by Accession
- [`rba_uniprot_epitope_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_epitope_search.md)
  : Search UniProt Epitopes
- [`rba_uniprot_features()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features.md)
  : Get UniProt protein sequence features by accession
- [`rba_uniprot_features_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features_search.md)
  : Search UniProt protein sequence features
- [`rba_uniprot_features_type()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features_type.md)
  : Search UniProt protein sequence features by description
- [`rba_uniprot_mutagenesis()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_mutagenesis.md)
  : Get Mutagenesis by UniProt Accession
- [`rba_uniprot_mutagenesis_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_mutagenesis_search.md)
  : Search Mutagenesis in UniProt
- [`rba_uniprot_proteins()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins.md)
  : Get UniProt entry by accession
- [`rba_uniprot_proteins_crossref()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins_crossref.md)
  : Get UniProt Entry by UniProt Cross-Reference Database and ID
- [`rba_uniprot_proteins_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins_search.md)
  : Search UniProt entries
- [`rba_uniprot_rna_edit()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_rna_edit.md)
  : Retrieve UniProt RNA-Editing Annotations by Accession
- [`rba_uniprot_rna_edit_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_rna_edit_search.md)
  : Search RNA Editing in UniProt
- [`rba_uniprot_variation()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation.md)
  : Retrieve UniProt Natural Variants by Identifier
- [`rba_uniprot_variation_locations()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation_locations.md)
  : Retrieve UniProt Natural Variants by Sequence Position
- [`rba_uniprot_variation_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation_search.md)
  : Search UniProt Natural Variants

### Proteomics

- [`rba_uniprot_proteomics()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics.md)
  : Get Proteomics Peptides Mapped to UniProt Protein (Deprecated)
- [`rba_uniprot_proteomics_hpp()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_hpp.md)
  : Get HPP Proteomics data in UniProt
- [`rba_uniprot_proteomics_hpp_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_hpp_search.md)
  : Search HPP Proteomics data in UniProt
- [`rba_uniprot_proteomics_non_ptm()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_non_ptm.md)
  : Get Proteomics data in UniProt
- [`rba_uniprot_proteomics_non_ptm_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_non_ptm_search.md)
  : Search Proteomics data in UniProt
- [`rba_uniprot_proteomics_ptm()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_ptm.md)
  : Get Post-Translational Modification of UniProt Protein
- [`rba_uniprot_proteomics_ptm_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_ptm_search.md)
  : Search Post-Translational Modification Proteomics in UniProt
- [`rba_uniprot_proteomics_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_search.md)
  : Search Proteomics Peptides in UniProt (Deprecated)
- [`rba_uniprot_proteomics_species()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_species.md)
  : Get UniProt Proteomics Metadata
- [`rba_uniprot_ptm()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_ptm.md)
  : Get Post-Translational Modification of UniProt Protein (Deprecated)
- [`rba_uniprot_ptm_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_ptm_search.md)
  : Search Post-Translational Modification in UniProt (Deprecated)

### Proteomes

- [`rba_uniprot_proteomes()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomes.md)
  : Get a Proteome by UPID
- [`rba_uniprot_proteomes_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomes_search.md)
  : Search Proteomes in UniProt
- [`rba_uniprot_genecentric()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_genecentric.md)
  : Get Gene-Centric proteins by UniProt Accession
- [`rba_uniprot_genecentric_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_genecentric_search.md)
  : Search Gene-Centric Proteins

### Taxonomy

- [`rba_uniprot_taxonomy()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy.md)
  : Get UniProt Taxonomy Nodes
- [`rba_uniprot_taxonomy_lca()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_lca.md)
  : Get Lowest Common Ancestor (LCA) of Two Taxonomy Nodes
- [`rba_uniprot_taxonomy_lineage()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_lineage.md)
  : Get Taxonomic Lineage
- [`rba_uniprot_taxonomy_name()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_name.md)
  : Search UniProt Taxonomic Names
- [`rba_uniprot_taxonomy_path()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_path.md)
  : Traverse UniProt Taxonomic Tree Path
- [`rba_uniprot_taxonomy_relationship()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_relationship.md)
  : Get Shortest Path Between Two Taxonomy Nodes

### Coordinates

- [`rba_uniprot_coordinates()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates.md)
  : Get Genomic Coordinates of a Protein
- [`rba_uniprot_coordinates_location()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location.md)
  : Search UniProt entries by taxonomy and genomic coordinates
- [`rba_uniprot_coordinates_location_genome()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location_genome.md)
  : Map Genomic Coordinates to Protein Sequence Positions
- [`rba_uniprot_coordinates_location_protein()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location_protein.md)
  : Map Protein Sequence Positions to Genomic Coordinates
- [`rba_uniprot_coordinates_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_search.md)
  : Search Genomic Coordinates of UniProt entries

### Uniparc

- [`rba_uniprot_uniparc()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc.md)
  : Get UniParc entry
- [`rba_uniprot_uniparc_bestguess()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc_bestguess.md)
  : Get UniParc Longest Sequence for Entries
- [`rba_uniprot_uniparc_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc_search.md)
  : Search UniParc Entries
- [`rba_uniprot_uniparc_sequence()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc_sequence.md)
  : Get UniParc Entries by Sequence
