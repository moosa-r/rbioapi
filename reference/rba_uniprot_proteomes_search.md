# Search Proteomes in UniProt

UniProt collects and annotates proteomes (Protein sets expressed in an
organism). Using this function you can search UniProt for available
proteomes. see [What are
proteomes?](https://www.uniprot.org/help/proteome) for more information.
You may also refine your search with modifiers such as keyword, taxon id
etc. See "Arguments section" for more information.

## Usage

``` r
rba_uniprot_proteomes_search(
  name = NULL,
  upid = NULL,
  taxid = NULL,
  keyword = NULL,
  xref = NULL,
  genome_acc = NULL,
  is_ref_proteome = NULL,
  is_redundant = NULL,
  ...
)
```

## Arguments

- name:

  a keyword in proteome's name

- upid:

  [UniProt Proteome identifier
  (UPID)](https://www.uniprot.org/help/proteome_id). You can supply up
  to 100 UPIDs.

- taxid:

  NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/). You can supply
  up to 20 taxon IDs.

- keyword:

  Limit the search to entries that contain your supplied keyword. see:
  [UniProt Keywords](https://www.uniprot.org/keywords/)

- xref:

  Proteome cross-references such as Genome assembly ID or Biosample ID.
  You can supply up to 20 cross-reference IDs.

- genome_acc:

  Genome accession associated with the proteome's components.

- is_ref_proteome:

  (logical) If TRUE, only return reference proteomes; If FALSE, only
  returns non-reference proteomes; If NULL (default), the results will
  not be filtered by this criteria see ['What are reference
  proteomes?'](https://www.uniprot.org/help/reference_proteome) for more
  information.

- is_redundant:

  (logical) If TRUE, only return redundant proteomes; If FALSE, only
  returns non-redundant proteomes; If NULL (default), the results will
  not be filtered by redundancy. see ['Reducing proteome
  redundancy'](https://www.uniprot.org/help/proteome_redundancy) for
  more information.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list where each element is a list that corresponds to a single
proteome (search hit) and contains informations pertinent to that
proteome.

## Details

Note that this is a search function. Thus, you are not required to fill
every argument; You may use whatever combinations of arguments you see
fit for your query.

## Corresponding API Resources

"GET https://ebi.ac.uk/proteins/api/proteomes"

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

Other "UniProt - Proteomes":
[`rba_uniprot_genecentric()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_genecentric.md),
[`rba_uniprot_genecentric_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_genecentric_search.md),
[`rba_uniprot_proteomes()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomes.md)

## Examples

``` r
# \donttest{
rba_uniprot_proteomes_search(name = "SARS-CoV")
# }
# \donttest{
rba_uniprot_proteomes_search(name = "SARS-CoV", is_ref_proteome = TRUE)
# }
# \donttest{
rba_uniprot_proteomes_search(name = "SARS-CoV", is_ref_proteome = TRUE)
# }
# \donttest{
rba_uniprot_proteomes_search(genome_acc = "AY274119")
# }
```
