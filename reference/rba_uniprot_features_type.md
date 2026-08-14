# Search UniProt protein sequence features by description

Search for terms in the descriptions of one specified [UniProt sequence
annotation (feature)](https://www.uniprot.org/help/sequence_annotation)
type. The function returns protein entries with at least one feature of
that type whose description matches a supplied term.

## Usage

``` r
rba_uniprot_features_type(terms, type, categories = NULL, types = NULL, ...)
```

## Arguments

- terms:

  Character: Terms to find in feature descriptions. You can supply up to
  20 terms.

- type:

  Character: [Sequence annotation
  (feature)](https://www.uniprot.org/help/sequence_annotation) type
  whose descriptions are searched. One of: "INIT_MET", "SIGNAL",
  "PROPEP", "TRANSIT", "CHAIN", "PEPTIDE", "TOPO_DOM", "TRANSMEM",
  "DOMAIN", "REPEAT", "ZN_FING", "DNA_BIND", "REGION", "COILED",
  "MOTIF", "COMPBIAS", "ACT_SITE", "BINDING", "SITE", "NON_STD",
  "MOD_RES", "LIPID", "CARBOHYD", "DISULFID", "CROSSLNK", "VAR_SEQ",
  "VARIANT", "MUTAGEN", "UNSURE", "CONFLICT", "NON_CONS", "NON_TER",
  "HELIX", "TURN", "STRAND", or "INTRAMEM".

- categories:

  Character: (optional) [Sequence annotation
  (feature)](https://www.uniprot.org/help/sequence_annotation)
  categories to include in each returned entry. Accepted values are:
  "MOLECULE_PROCESSING", "TOPOLOGY", "SEQUENCE_INFORMATION",
  "STRUCTURAL", "DOMAINS_AND_SITES", "PTM", "VARIANTS" and/or
  "MUTAGENESIS". You can supply up to 20 categories.

- types:

  Character: (optional) [Sequence annotation
  (feature)](https://www.uniprot.org/help/sequence_annotation) types to
  include in each returned entry. Accepted values are the same as for
  `type`. You can supply up to 20 types.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list named by UniProt accession. Each element contains the entry
metadata, sequence, and the annotations selected by `categories` and
`types` in its `features` element. Without those optional filters, all
annotations of each matching entry are returned.

## Details

The `type` and `terms` arguments determine which protein entries match
the search. The optional `categories` and `types` arguments only select
the annotations included in each returned entry; they do not change
which entries match.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/features/type/{type}"

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
[`rba_uniprot_features()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features.md),
[`rba_uniprot_features_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_features_type(
    terms = "Alzheimer", type = "VARIANT", types = "VARIANT")
# }
```
