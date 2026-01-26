# Search UniProt protein sequence features

UniProt maintains [sequence annotations
(features)](https://www.uniprot.org/help/sequence_annotation) that
describe regions in the protein sequence. Using this function, you can
search and retrieve UniProt proteins' sequence annotations (features).
you may also refine your search query with variety of modifiers.

## Usage

``` r
rba_uniprot_features_search(
  accession = NULL,
  gene = NULL,
  exact_gene = NULL,
  protein = NULL,
  reviewed = NULL,
  organism = NULL,
  taxid = NULL,
  categories = NULL,
  types = NULL,
  ...
)
```

## Arguments

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- gene:

  [UniProt gene name(s)](https://www.uniprot.org/help/gene_name). You
  can supply up to 20 gene names. e.g. if you supply "CD40", "CD40
  ligand" will also be included.

- exact_gene:

  [UniProt exact gene name(s)](https://www.uniprot.org/help/gene_name).
  You can supply up to 20 exact gene names. e.g. if you supply "CD40",
  "CD40 ligand" will not be included in the results.

- protein:

  [UniProt protein name](https://www.uniprot.org/help/protein_names)

- reviewed:

  Logical: If TRUE, only return "UniProtKB/Swiss-Prot" (reviewed)
  entries; If FALSE, only return TrEMBL (un-reviewed) entries.

- organism:

  [Organism name](https://www.uniprot.org/taxonomy/).

- taxid:

  NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/). You can supply
  up to 20 taxon IDs.

- categories:

  [Sequence annotation
  (Features)](https://www.uniprot.org/help/sequence_annotation)
  categories (subsection). accepted values are: "MOLECULE_PROCESSING",
  "TOPOLOGY", "SEQUENCE_INFORMATION", "STRUCTURAL", "DOMAINS_AND_SITES",
  "PTM", "VARIANTS" and/or "MUTAGENESIS". You can supply up to 8
  categories.

- types:

  [Sequence annotation
  (Features)](https://www.uniprot.org/help/sequence_annotation) types.
  accepted values are: "INIT_MET", "SIGNAL", "PROPEP", "TRANSIT",
  "CHAIN", "PEPTIDE", "TOPO_DOM", "TRANSMEM", "DOMAIN", "REPEAT",
  "CA_BIND", "ZN_FING", "DNA_BIND", "NP_BIND", "REGION", "COILED",
  "MOTIF", "COMPBIAS", "ACT_SITE", "METAL", "BINDING", "SITE",
  "NON_STD", "MOD_RES", "LIPID", "CARBOHYD", "DISULFID", "CROSSLNK",
  "VAR_SEQ", "VARIANT", "MUTAGEN", "UNSURE", "CONFLICT", "NON_CONS",
  "NON_TER", "HELIX", "TURN", "STRAND" and/or "INTRAMEM". You can supply
  up to 20 types.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List where each element corresponds to one UniProt entity returned by
your search query. The element itself is a sub-list containing all
information that UniProt has about that entity.

## Details

Note that this is a search function. Thus, you are not required to fill
every argument; You may use whatever combinations of arguments you see
fit for your query.  
UniProt Entries are grouped in two sections:

1.  Reviewed(Swiss-Prot): Manually annotated records with information
    extracted from literature and curator-evaluated computational
    analysis.

2.  Unreviewed (TrEMBL): Computationally analyzed records that await
    full manual annotation.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/features"

## References

- The UniProt Consortium , UniProt: the Universal Protein Knowledgebase
  in 2025, Nucleic Acids Research, 2024;, gkae1010,
  https://doi.org/10.1093/nar/gkae1010

- Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
  Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
  Turner, Maria Martin, The Proteins API: accessing key integrated
  protein and genome information, Nucleic Acids Research, Volume 45,
  Issue W1, 3 July 2017, Pages W539–W544,
  https://doi.org/10.1093/nar/gkx237

- [Proteins API Documentation](https://www.ebi.ac.uk/proteins/api/doc/)

- [Citations note on UniProt
  website](https://www.uniprot.org/help/publications)

## See also

Other "UniProt - Features":
[`rba_uniprot_features()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features.md)

## Examples

``` r
# \donttest{
rba_uniprot_features_search(accession = "Q99616")
# }
# \donttest{
rba_uniprot_features_search(gene = "cd40")
# }
# \donttest{
rba_uniprot_features_search(gene = "cd40 ligand")
# }
# \donttest{
rba_uniprot_features_search(gene = "cd40",  reviewed = TRUE)
# }
# \donttest{
rba_uniprot_features_search(accession = "Q99616",
    categories = c("MOLECULE_PROCESSING", "TOPOLOGY"))
# }
# \donttest{
rba_uniprot_features_search(accession = "Q99616", types = "DISULFID")
# }
```
