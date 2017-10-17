#' Append columns related to the position of peptide in a protein: Start (start position of a peptide in a protein), End (end position of a peptide in a protein), Protlength (length of the full protein), Appearance (if a peptide appears multiple times in a protein).
#'
#' @param data_table Data frame to which the coverage columns will be appended. Must contain a column of peptide sequences and a column of the parental protein of each peptide.
#' @param elementid Colname of column containing peptide sequences. Will be searched against the sequences extracted from the proteome file.
#' @importFrom dplyr mutate
#' @importFrom stringr str_locate_all
#' @importFrom purrr as_vector map %>%
#' @importFrom stats setNames
#' @return Data frame with the following added columns: "Start", "End", "Sequence", "Appearance".
#' @examples
#' cov_columns(test_data, seqid='ID',elementid='Peptide')
#' @export
cov_columns <- function(df, elementid="Peptide"){

    pepregex <- mapply(gsub, pattern = "[I|J|L]",
                       replacement = "(I|J|L)", df[,elementid]) %>% as.vector()
    df$pepregex <- pepregex

    #create Length vector, to be appended to the original data frame afterwards
    Length <- nchar(df$Sequence)
    df$Length <- Length

    getStartorEnd <- function(peptide_column, sequence_column, s=1){
        #Find start end positions of a substring within a string

        #s=1 for start position, s=2 for end position.
        comparison <- stringr::str_locate_all(sequence_column[1],peptide_column[1])[[1]][,s][1]

        return(comparison)
    }
    df$Start <- mapply(getStartorEnd, df$pepregex, df$Sequence, 1)
    df$End <- mapply(getStartorEnd, df$pepregex, df$Sequence, 2)

    #Not sure about this
    #df <- df %>% filter(!is.na(Start)) %>% filter(!is.na(End))
    #this, ideally, would differentiate between peptides found twice within the protein structure. Still pending.
    df$Appearance <- ''
    #for(i in 1:length(peptide_column)){
    #    Appearance[i] <- length(stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1])
    #}

    df$pepregex <- NULL

    return(df)
}

