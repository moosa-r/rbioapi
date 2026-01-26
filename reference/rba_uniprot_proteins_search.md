# Search UniProt entries

Using this function, you can search and retrieve UniProt Knowledge-base
(UniProtKB) protein entries using variety of options. You may also
refine your search with modifiers such as sequence length, review status
etc. See "Arguments" section" for more information.

## Usage

``` r
rba_uniprot_proteins_search(
  accession = NULL,
  reviewed = NULL,
  isoform = NULL,
  go_term = NULL,
  keyword = NULL,
  ec = NULL,
  gene = NULL,
  exact_gene = NULL,
  protein = NULL,
  organism = NULL,
  taxid = NULL,
  pubmed = NULL,
  seq_length = NULL,
  md5 = NULL,
  ...
)
```

## Arguments

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- reviewed:

  Logical: If TRUE, only return "UniProtKB/Swiss-Prot" (reviewed)
  entries; If FALSE, only return TrEMBL (un-reviewed) entries.

- isoform:

  Numeric: you have three options:

  - 0: Exclude isoforms.

  - 1: Return isoforms only.

  - 2: Return both.

  see: [Alternative
  products](https://www.uniprot.org/help/alternative_products)

- go_term:

  Limit the search to entries associated with your supplied GO ([Gene
  Ontology](https://www.uniprot.org/help/gene_ontology)) term. You can
  supply Either GO ID or a character string -partially or fully-
  matching the term. e.g. "GO:0001776" or "leukocyte homeostasis". if
  You supply "leukocyte", any term containing that word will be
  included, e.g "leukocyte chemotaxis", "leukocyte activation".

- keyword:

  Limit the search to entries that contain your supplied keyword. see:
  [UniProt Keywords](https://www.uniprot.org/keywords/)

- ec:

  [EC (Enzyme Commission) number(s)](https://enzyme.expasy.org/). You
  can supply up to 20 EC numbers.

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

- organism:

  [Organism name](https://www.uniprot.org/taxonomy/).

- taxid:

  NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/). You can supply
  up to 20 taxon IDs.

- pubmed:

  Entries which [cite to](https://www.uniprot.org/citations/) the
  article with your supplied PubMed ID.

- seq_length:

  An exact sequence length (e.g. 150) or a range of sequence lengths
  (e.g. "130-158").

- md5:

  Sequence md5 value.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A List where each element corresponds to one UniProt entity returned by
your search query. The element itself is a sub-list containing all
information that UniProt has about that entity.

## Details

Note that this is a search function. Thus, you are not required to fill
every argument; You may use whatever combinations of arguments you see
fit for your query.s  
UniProt Entries are grouped in two sections:

1.  Reviewed(Swiss-Prot): Manually annotated records with information
    extracted from literature and curator-evaluated computational
    analysis.

2.  Unreviewed (TrEMBL): Computationally analyzed records that await
    full manual annotation.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/proteins"

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

Other "UniProt - Proteins":
[`rba_uniprot_proteins()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins.md),
[`rba_uniprot_proteins_crossref()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins_crossref.md)

## Examples

``` r
# \donttest{
rba_uniprot_proteins_search(accession = "Q99616")
# }
# \donttest{
rba_uniprot_proteins_search(gene = "cd40")
# }
# \donttest{
rba_uniprot_proteins_search(gene = "cd40 ligand")
# }
# \donttest{
rba_uniprot_proteins_search(gene = "cd40",  reviewed = TRUE)
# }
# \donttest{
rba_uniprot_proteins_search(gene = "cd40",  reviewed = TRUE, isoform = 1)
# }
# \donttest{
rba_uniprot_proteins_search(keyword = "Inhibition of host chemokines by virus")
# }
# \donttest{
rba_uniprot_proteins_search(keyword = "chemokines")
# }
```
