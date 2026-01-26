# Get natural variants in UniProt by NIH-NCBI SNP database identifier

Retrieve natural variant annotations of a sequence using UniProt protein
accession, dbSNP or HGVS expression.

## Usage

``` r
rba_uniprot_variation(
  id,
  id_type,
  source_type = NULL,
  consequence_type = NULL,
  wild_type = NULL,
  alternative_sequence = NULL,
  location = NULL,
  save_peff = FALSE,
  ...
)
```

## Arguments

- id:

  An ID which can be either a [UniProt primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers), NIH-NCBI
  dbSNP ID or HGVS expression. [NIH-NCBI dbSNP
  id](https://www.ncbi.nlm.nih.gov/snp/) or [HGVS
  Expression](https://varnomen.hgvs.org/).

- id_type:

  The type of supplied ID argument, one of:
  ["uniprot"](https://www.uniprot.org/help/accession_numbers),
  ["dbsnp"](https://www.ncbi.nlm.nih.gov/snp/) or
  ["hgvs"](https://varnomen.hgvs.org/)

- source_type:

  Variation's source type. You can choose up to two of: "UniProt",
  "large scale study" and/or "mixed".

- consequence_type:

  Variation's consequence type. You can choose up to two of: "missense",
  "stop gained" or "stop lost".

- wild_type:

  Wild type amino acid. Accepted values are IUPAC single-letter amino
  acid (e.g. D for Aspartic acid) and "\*" for stop codon. You can
  supply up to 20 values.

- alternative_sequence:

  Alternative amino acid. Accepted values are IUPAC single-letter amino
  acid (e.g. D for Aspartic acid) and "\*" for stop codon and "-" for
  deletion. You can supply up to 20 values.

- location:

  A valid amino acid range (e.g. 10-25) within the sequence range where
  the variation occurs. You can supply up to 20 values.

- save_peff:

  Logical or Character:

  - FALSE: (default) Do not save PEFF file, just return as a list
    object.

  - TRUE: Save as PEFF file to an automatically-generated path.

  - Character string: A valid file path to save the PEFF file.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list where each element is a list that corresponds to a UniProt
protein entry.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/variation/dbsnp/{dbid}"  
"GET https://www.ebi.ac.uk/proteins/api/variation/hgvs/{hgvs}"  
"GET https://www.ebi.ac.uk/proteins/api/variation/{accession}"

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

Other "UniProt - Variation":
[`rba_uniprot_variation_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_variation(id = "rs121434451", id_type = "dbsnp")
# }
# \donttest{
rba_uniprot_variation(id = "NC_000008.11:g.22119227C>T", id_type = "hgvs")
# }
# \donttest{
rba_uniprot_variation(id = "O43593", id_type = "uniprot")
# }
```
