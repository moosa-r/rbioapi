# Get Post-Translational Modification of UniProt Protein (Deprecated)

This function is Deprecated. Please use
[`rba_uniprot_proteomics_ptm`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_ptm.md)
instead.  
UniProt maps post-translational modification features from different
sources to the proteins' sequences. Using this function, you can
retrieve all the post-translational modification features that has been
map to a given UniProt protein's sequence.

## Usage

``` r
rba_uniprot_ptm(accession, ...)
```

## Arguments

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing the post-translational modification features of your
supplied UniProt protein's sequence.

## Details

see also: [PTM / Processing section in
UniProtKB](https://www.uniprot.org/help/ptm_processing_section)

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/proteomics-ptm/{accession}"

## References

- The UniProt Consortium, UniProt: the universal protein knowledgebase
  in 2021, Nucleic Acids Research, Volume 49, Issue D1, 8 January 2021,
  Pages D480–D489, https://doi.org/10.1093/nar/gkaa1100

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

Other "Deprecated functions":
[`rba_uniprot_proteomics()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics.md),
[`rba_uniprot_proteomics_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_search.md),
[`rba_uniprot_ptm_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_ptm_search.md)

## Examples

``` r
if (FALSE) { # \dontrun{
#Deprecated
rba_uniprot_ptm(accession = "P04234")
} # }
```
