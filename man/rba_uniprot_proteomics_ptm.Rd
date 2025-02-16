% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uniprot_proteomics.R
\name{rba_uniprot_proteomics_ptm}
\alias{rba_uniprot_proteomics_ptm}
\title{Get Post-Translational Modification of UniProt Protein}
\usage{
rba_uniprot_proteomics_ptm(accession, confidence_score = NULL, ...)
}
\arguments{
\item{accession}{\href{https://www.uniprot.org/help/accession_numbers}{
UniProtKB primary or secondary accession}.}

\item{confidence_score}{(Character) Valid values: "Bronze", "Silver", or
"gold".\cr UniProt classifies modified residues
 into three categories based on its false localization rate (FLR)
 across multiple dataset. See
 \href{https://www.uniprot.org/help/mod_res_large_scale}{Large scale modified residue}
 for more information.}

\item{...}{rbioapi option(s). See \code{\link{rba_options}}'s
arguments manual for more information on available options.}
}
\value{
A list containing the post-translational modification features of
your supplied UniProt protein's sequence.
}
\description{
UniProt maps post-translational modification proteomics data from different
  sources to the proteins' sequences. Using this function, you can retrieve
  all the post-translational modification features that has been map to a
  given UniProt protein's sequence.
}
\details{
see also:
  \href{https://www.uniprot.org/help/ptm_processing_section}{PTM /
  Processing section in UniProtKB}

UniProt categorizes proteomics data sources into three main data categories:
  PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
  Project); each with corresponding API endpoints, and thus, rbioapi
  functions.
}
\section{Corresponding API Resources}{

 "GET https://www.ebi.ac.uk/proteins/api/proteomics/ptm/\{accession\}"
}

\examples{
\donttest{
rba_uniprot_proteomics_ptm(accession = "P04234")
}

}
\references{
\itemize{
  \item The UniProt Consortium , UniProt: the Universal Protein
  Knowledgebase in 2025, Nucleic Acids Research, 2024;, gkae1010,
  https://doi.org/10.1093/nar/gkae1010
  \item Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
  Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
  Turner, Maria Martin, The Proteins API: accessing key integrated protein
  and genome information, Nucleic Acids Research, Volume 45, Issue W1,
  3 July 2017, Pages W539–W544, https://doi.org/10.1093/nar/gkx237
  \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
  Documentation}
  \item \href{https://www.uniprot.org/help/publications}{Citations note
  on UniProt website}
  }
}
\seealso{
Other "UniProt - Proteomics": 
\code{\link{rba_uniprot_proteomics_hpp}()},
\code{\link{rba_uniprot_proteomics_hpp_search}()},
\code{\link{rba_uniprot_proteomics_non_ptm}()},
\code{\link{rba_uniprot_proteomics_non_ptm_search}()},
\code{\link{rba_uniprot_proteomics_ptm_search}()},
\code{\link{rba_uniprot_proteomics_species}()}
}
\concept{"UniProt - Proteomics"}
