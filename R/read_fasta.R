#' Read a fasta file into a two column dataframe, with one column for sequenceid and one column for sequence
#'
#' @param proteome_file FASTA file from which the sequences will be extracted.
#' @param seqid Colname of column for containint protein IDs.
#' @param seq Colname for column containing sequences. Will be searched against the sequences extracted from the FASTA file.
#' @importFrom purrr as_vector %>%
#' @importFrom seqinr read.fasta
#' @return Two column dataframe with one column for ID and one column for sequence.
#' @examples
#' read_fasta(test.fasta, id="ID", sequenceid = "Sequence")
#' @export
read_fasta <- function(proteome_file, seqid="ID", seq = "Sequence"){
    fasta <- seqinr::read.fasta(proteome_file)
    #create a list of strings with all the sequences, capitalize them
    list_seq <- lapply(fasta,function(lst){lst[1:length(lst)] %>% paste(collapse='')})
    #convert the list into a data frame
    proteome_df <- data.frame(names(list_seq) %>% purrr::as_vector(),unname(list_seq) %>% purrr::as_vector())
    names(proteome_df) <- c(seqid, seq)

    return(proteome_df)
}

