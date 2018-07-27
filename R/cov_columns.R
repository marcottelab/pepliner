#' Append columns related to the position of peptide in a protein: Start (start position of a peptide in a protein), End (end position of a peptide in a protein), Protlength (length of the full protein), Appearance (if a peptide appears multiple times in a protein).
#'
#' @param data_table Data frame to which the coverage columns will be appended. Must contain a column of peptide sequences (default: Peptide, and a column of the parental protein of each peptide (called Sequence).
#' @param peptide_column Colname of column containing peptide sequences. Will be searched against the.
#' @importFrom dplyr mutate
#' @importFrom stringr str_locate_all
#' @importFrom purrr as_vector map %>%
#' @importFrom stats setNames
#' @return Data frame with the following added columns: "Start", "End", "Sequence", "Appearance".
#' @examples
#' cov_columns(test_data, seqid='ID',peptide_column='Peptide')
#' @export
cov_columns <- function(df, peptide_column="Peptide", sequence_column="Sequence"){

    df$pepregex <- mapply(gsub, pattern = "[I|J|L]",
                       replacement = "(I|J|L)", df[,peptide_column]) %>% as.vector()

    #create Length vector, to be appended to the original data frame afterwards
    print("in cov_columns")
    print(df$Sequence)

    df <- df %>% mutate(Length = str_length(Sequence))
    #df$Length <- nchar(mapply(as.character(df$Sequence)))
    #df$Length <- Length
  
    #This needs to be made into one function..??
    getStartorEnd <- function(peptide_column, sequence_column, s=1){
        #Find start end positions of a substring within a string
        
        #s=1 for start position, s=2 for end position.
        comparison <- stringr::str_locate_all(sequence_column[1],coll(peptide_column[1], ignore_case = TRUE))[[1]][,s][1]
        return(comparison)
    }
    df$Start <- mapply(getStartorEnd, df$pepregex, df$Sequence, 1)
    df$End <- mapply(getStartorEnd, df$pepregex, df$Sequence, 2)

    #Not sure about this
    #df <- df %>% filter(!is.na(Start)) %>% filter(!is.na(End))
    #this, ideally, would differentiate between peptides found twice within the protein structure. Still pending.
    #df$Appearance <- ''
    #for(i in 1:length(peptide_column)){
    #    Appearance[i] <- length(stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1])
    #}

    df$pepregex <- NULL

    return(df)
}

