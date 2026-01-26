# Search Antigens in UniProt

UniProt maps Antigenic (Antibody-binding) features from different
sources to the proteins' sequences. Using this function, you can search
for Antigenic sequences that has been map to UniProt proteins. You may
also refine your search with modifiers such as score etc. See "Arguments
section" for more information.

## Usage

``` r
rba_uniprot_antigens_search(
  accession = NULL,
  antigen_sequence = NULL,
  antigen_id = NULL,
  ensembl_id = NULL,
  match_score = NULL,
  ...
)
```

## Arguments

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- antigen_sequence:

  Protein sequence in the antigenic site.

- antigen_id:

  Human Protein Atlas (HPA) antigen ID. You can supply up to 20 IDs.

- ensembl_id:

  Ensembl Stable Transcript ID. You can supply up to 20 IDs.

- match_score:

  (Numeric) Minimum alignment score for the antigen sequence and the
  target protein sequence.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list Where each element correspond to a UniProt protein (search hit)
and Antigenic features are organized under the "features" sub-list.

## Details

Note that this is a search function. Thus, you are not required to fill
every argument; You may use whatever combinations of arguments you see
fit for your query.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/antigen"

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

Other "UniProt - Antigen":
[`rba_uniprot_antigens()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_antigens.md)

## Examples

``` r
# \donttest{
rba_uniprot_antigens_search(antigen_id = "HPA001060")
# }
```
