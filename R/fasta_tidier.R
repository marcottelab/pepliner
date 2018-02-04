#' Converts a SeqFastadna object into a tidy dataframe.
#'
#' @param fasta_filename FASTA file from which the sequences will be extracted.
#' @param annot If TRUE, extract sequence ID annotation into its own column
#' @param ... Unused extra arguments
#' @importFrom dplyr %>%
#' @importFrom broom fix_data_frame
#' @return A \code{tidy} data.frame with one column for ID and one column for sequence.
#'   \item{ID}{Sequence header from FASTA '>ID' }
#'   \item{Sequence}{Corresponding sequence, any bioalphabet (RNA/DNA/etc)}
#'   \item{Annot}{Full annotated FASTA header '>ID info|info|info'}
#' @examples
#' seqinr::read.fasta function reads a FASTA file (format below) and returns a SeqFastadna object
#'    > GENENAME1
#'    atgctgacgta
#'    agctacagtat
#'    cagctactaag
#
#' seqinr_fasta_object <- seqinr::read.fasta("test.fasta")
#' fasta_tidier(seqinr_fasta_object)
#' fasta_tider(seqinr_fasta_object, annot=TRUE)
#' @export

fasta_tidier <- function(fasta_seqinr_object, annot = FALSE){

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
