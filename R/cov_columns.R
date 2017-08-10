#' Append columns related to the position of peptide in a protein: Start (start position of a peptide in a protein), End (end position of a peptide in a protein), Protlength (length of the full protein), Appearance (if a peptide appears multiple times in a protein).
#'
#' @param data_table Data frame to which the coverage columns will be appended. Must contain a column of peptide sequences and a column of the parental protein of each peptide.
#' @param proteome FASTA file from which the sequences will be extracted. IDs must match protein IDs in the input data_table.
#' @param outname If export = TRUE, output the resulting data frame as a .csv file named after this argument.
#' @param groupid Colname of column (factor, not numeric) containing protein IDs. IDs must match proteome identifiers.
#' @param elementid Colname of column containing peptide sequences. Will be searched against the sequences extracted from the FASTA file.
#' @importFrom dplyr mutate
#' @importFrom stringr str_locate_all
#' @importFrom purrr as_vector map %>%
#' @importFrom seqinr read.fasta
#' @importFrom stats setNames
#' @return Data frame with the following added columns: "Start", "End", "Sequence", "Appearance".
#' @export
#' @examples
#' test_data <- read.csv(paste0(system.file('extdata',package='pepliner'),'/msdata.csv'))
#' sequences <- paste0(system.file('extdata',package='pepliner'),'/proteome.fasta')
#' cov_columns(test_data,sequences,groupid='ID',elementid='Peptide')

cov_columns <- function(data_table,proteome,groupid,elementid,outname=paste0(deparse(substitute(data_table)),'_cov.csv')){
    fasta <- seqinr::read.fasta(proteome)
    #create a list of strings with all the sequences, capitalize them
    list_seq <- lapply(fasta,function(lst){lst[1:length(lst)] %>% paste(collapse='') %>% toupper()})
    #convert the list into a data frame
    list_seq <- data.frame(names(list_seq) %>% purrr::as_vector(),unname(list_seq) %>% purrr::as_vector())
    names(list_seq) <- c(groupid,'Sequence')
    #add the full protein sequence column to the original data set.
    pep <- merge(x = data_table, y = list_seq, by = groupid, all.x = TRUE)

    #replace Is, Js and Ls with (I|J|L) to use for regex lookup, store the data set as "pep"
    pep[,elementid] <- gsub("[I|J|L]", "(I|J|L)", pep[,elementid])

    #elementid_quo <- dplyr::enquo(elementid)
    #message(elementid_quo)
    #print(head(pep))

    #pep <- pep %>% dplyr::rowwise() %>% summarize(x = stringr::str_locate_all("Peptide", "Sequence"))


    #extract the columns with the peptides and the sequences respectively
    peptide_column <- pep[,colnames(pep)==elementid]
    sequence_column <- pep[,colnames(pep)=='Sequence'] %>% as.character()
    #create Length vector, to be appended to the original data frame afterwards
    Length <- nchar(sequence_column)
    pep$Length <- Length

    getStartorEnd <- function(peptide_column, sequence_column, s=1){
        #Find start end positions of a substring within a string

        #s=1 for start position, s=2 for end position.
        comparison <- stringr::str_locate_all(sequence_column[1],peptide_column[1])[[1]][,s]

        #if(length(comparison) == 0){
        #
            #if the peptide sequence appears once in the full protein sequence, fill row$Pos with the number
        #}else if(length(comparison) == 1){
        #    Pos <- comparison
        #    #if it appears more than once, it should return a list with the starting positions. Still pending.
        #}else{
        #    Pos <- comparison %>% list()
        #}
    #to do: flatten the comparison to multiple rows
    return(comparison)
    }

    pep$Start <- mapply(getStartorEnd, pep[,elementid], pep$Sequence, 1)
    pep$End <- mapply(getStartorEnd, pep[,elementid], pep$Sequence, 2)

    pep <- pep %>% filter(!is.na(Start)) %>% filter(!is.na(End))
    #this, ideally, would differentiate between peptides found twice within the protein structure. Still pending.
    pep$Appearance <- ''
    #for(i in 1:length(peptide_column)){
    #    Appearance[i] <- length(stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1])
    #}

    #exclude Sequence column.
    pep$Sequence <- NULL

    return(pep)
}
