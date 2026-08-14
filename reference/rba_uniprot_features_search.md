# Search UniProt protein sequence features

[UniProt sequence
features](https://www.uniprot.org/help/sequence_annotation) describe
biologically relevant sites and regions within protein sequences. Search
and retrieve these annotations using protein, gene, organism, and
annotation criteria.

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

  Character: (optional) [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- gene:

  Character: (optional) [UniProt gene
  name(s)](https://www.uniprot.org/help/gene_name). You can supply up to
  20 gene names. For example, if you supply "CD40", "CD40 ligand" will
  also be included.

- exact_gene:

  Character: (optional) [UniProt exact gene
  name(s)](https://www.uniprot.org/help/gene_name). You can supply up to
  20 exact gene names. For example, if you supply "CD40", "CD40 ligand"
  will not be included in the results.

- protein:

  Character: (optional) [UniProt protein
  name](https://www.uniprot.org/help/protein_names).

- reviewed:

  Logical: (optional) If `TRUE`, return only reviewed Swiss-Prot
  entries. If `FALSE`, return only unreviewed TrEMBL entries.

- organism:

  Character: (optional) Organism name.

- taxid:

  Numeric: (optional) NIH-NCBI [Taxon
  ID](https://www.uniprot.org/taxonomy/). You can supply up to 20 taxon
  IDs.

- categories:

  Character: (optional) [Sequence annotation
  (features)](https://www.uniprot.org/help/sequence_annotation)
  categories. Accepted values are: "MOLECULE_PROCESSING", "TOPOLOGY",
  "SEQUENCE_INFORMATION", "STRUCTURAL", "DOMAINS_AND_SITES", "PTM",
  "VARIANTS" and/or "MUTAGENESIS". You can supply up to 20 categories.

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

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list named by UniProt accession. Each element contains the entry
metadata, sequence, and matching annotations in its `features` element.

## Details

At least one of `accession`, `gene`, `exact_gene`, `protein`,
`organism`, or `taxid` is required. The remaining arguments refine those
primary criteria.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/features"

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
[`rba_uniprot_features_type()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features_type.md)

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
