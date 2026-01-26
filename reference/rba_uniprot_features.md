# Get UniProt protein sequence features by accession

Use this function to retrieve [sequence annotations
(features)](https://www.uniprot.org/help/sequence_annotation) of a
protein by it's UniProt accession.

## Usage

``` r
rba_uniprot_features(
  accession,
  types = NULL,
  categories = NULL,
  location = NULL,
  ...
)
```

## Arguments

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers).

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

- categories:

  [Sequence annotation
  (Features)](https://www.uniprot.org/help/sequence_annotation)
  categories (subsection). accepted values are: "MOLECULE_PROCESSING",
  "TOPOLOGY", "SEQUENCE_INFORMATION", "STRUCTURAL", "DOMAINS_AND_SITES",
  "PTM", "VARIANTS" and/or "MUTAGENESIS". You can supply up to 8
  categories.

- location:

  (character) Filter the features by the amino acid position in the
  sequence(s). Provide the range as a character string with the format
  "begin-end", e.g. "35-70"

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list in which you can find all of your given protein's sequence
annotations in a sub-list named "features".

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/features/{accession}"

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
[`rba_uniprot_features_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_features("Q99616")
# }
# \donttest{
rba_uniprot_features(accession = "Q99616", types = "DISULFID")
# }
```
