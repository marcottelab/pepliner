#' Read a FASTA file into a tidy dataframe. Wrapper around seqinr::read.fasta
#'
#' @param fasta_filename FASTA file from which the sequences will be extracted.
#' @param annot If TRUE, extract sequence ID annotation into its own column
#' @param ... Unused extra arguments
#' @importFrom dplyr %>%
#' @importFrom seqinr read.fasta
#' @importFrom broom fix_data_frame
#' @return A \code{tidy} data.frame with one column for ID and one column for sequence.
#'   \item{ID}{Sequence header from FASTA '>ID' }
#'   \item{Sequence}{Corresponding sequence, any bioalphabet (RNA/DNA/etc)}
#'   \item{Annot}{Full annotated FASTA header '>ID info|info|info'}
#' @examples
#' test.fasta is a FASTA formatted file for ex:
#'    > GENENAME1
#'    atgctgacgta
#'    agctacagtat
#'    cagctactaag
#
#' read_fasta("test.fasta")
#' read_fasta("test.fasta", annot=TRUE)
#' @export

read_fasta <- function(fasta_filename, annot = FALSE){
    fasta <- seqinr::read.fasta(fasta_filename, as.string = TRUE)

    # Convert seqinr SeqFastadna object to data.frame
    fasta_df <- fasta %>%
                   sapply(function(x){x[1:length(x)]}) %>%
                   as.data.frame %>%
                   fix_data_frame(newcol = "ID", newnames = "Sequence")

    if(annot == TRUE){
        annot_df <- getAnnot(fasta) %>%
                         sapply(function(x){x[1:length(x)]}) %>%
                         as.data.frame() %>%
                         fix_data_frame(newnames = "Annot")

        fasta_df <- cbind(fasta_df, annot_df)
    }
    return(fasta_df)
}
