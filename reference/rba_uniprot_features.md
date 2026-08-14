# Get UniProt protein sequence features by accession

[UniProt sequence
features](https://www.uniprot.org/help/sequence_annotation) describe
biologically relevant sites and regions within a protein sequence.
Retrieve these annotations for one UniProtKB accession, optionally
filtered by annotation type, category, or amino-acid range.

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

  Character: [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers).

- types:

  Character: (optional) [Sequence annotation
  (features)](https://www.uniprot.org/help/sequence_annotation) types.
  Accepted values are: "INIT_MET", "SIGNAL", "PROPEP", "TRANSIT",
  "CHAIN", "PEPTIDE", "TOPO_DOM", "TRANSMEM", "DOMAIN", "REPEAT",
  "ZN_FING", "DNA_BIND", "REGION", "COILED", "MOTIF", "COMPBIAS",
  "ACT_SITE", "BINDING", "SITE", "NON_STD", "MOD_RES", "LIPID",
  "CARBOHYD", "DISULFID", "CROSSLNK", "VAR_SEQ", "VARIANT", "MUTAGEN",
  "UNSURE", "CONFLICT", "NON_CONS", "NON_TER", "HELIX", "TURN", "STRAND"
  and/or "INTRAMEM". You can supply up to 20 types.

- categories:

  Character: (optional) [Sequence annotation
  (features)](https://www.uniprot.org/help/sequence_annotation)
  categories. Accepted values are: "MOLECULE_PROCESSING", "TOPOLOGY",
  "SEQUENCE_INFORMATION", "STRUCTURAL", "DOMAINS_AND_SITES", "PTM",
  "VARIANTS" and/or "MUTAGENESIS". You can supply up to 20 categories.

- location:

  Character: (optional) Amino-acid range in `"begin-end"` format, e.g.
  `"35-70"`.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing the entry metadata, sequence, and matching annotations
in its `features` element.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/features/{accession}"

## References

- The UniProt Consortium. (2025). UniProt: the Universal Protein
  Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
  https://doi.org/10.1093/nar/gkae1010

- Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales, L.,
  Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
  Proteins API: Accessing key integrated protein and genome information.
  Nucleic Acids Research, 45(W1), W539–W544.
  https://doi.org/10.1093/nar/gkx237

- [Proteins API Documentation](https://www.ebi.ac.uk/proteins/api/doc/)

- [Citations note on UniProt
  website](https://www.uniprot.org/help/publications)

## See also

Other "UniProt - Features":
[`rba_uniprot_features_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features_search.md),
[`rba_uniprot_features_type()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features_type.md)

## Examples

``` r
# \donttest{
rba_uniprot_features("Q99616")
# }
# \donttest{
rba_uniprot_features(accession = "Q99616", types = "DISULFID")
# }
```
