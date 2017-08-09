#' Append columns related to the coverage of a protein.
#'
#' @param data_table Data frame to which the coverage columns will be appended.
#' @param proteome FASTA file from which the sequences will be extracted.
#' @param outname If export = TRUE, output the resulting data frame as a .csv file named after this argument.
#' @param groupid Data frame column (factor) corresponding to the protein name to be matched with the IDs extracted from the FASTA file.
#' @param elementid Data frame column (factor) corresponding to the peptide sequence to be searched against the sequences extracted from the FASTA file.
#' @param export If false (default), return a data frame. If true, write a .csv file named after the input file + '_cov.csv'.
#' @importFrom dplyr mutate mutate_
#' @importFrom stringr str_locate_all
#' @importFrom purrr as_vector map %>%
#' @importFrom lazyeval interp
#' @importFrom seqinr read.fasta
#' @importFrom stats setNames
#' @return Data frame with the following added columns: "Start", "End", "Sequence", "Appearance".
#' @export
#' @examples
#' test_data <- read.csv(paste0(system.file('extdata',package='pepliner'),'/msdata.csv'))
#' sequences <- paste0(system.file('extdata',package='pepliner'),'/proteome.fasta')
#' cov_columns(test_data,sequences,groupid='ID',elementid='Peptide')

cov_columns <- function(data_table,proteome,groupid,elementid,outname=paste0(deparse(substitute(data_table)),'_cov.csv'),export=FALSE){
    fasta <- seqinr::read.fasta(proteome)

    #create a list of strings with all the sequences, capitalize them
    list_seq <- lapply(fasta,function(lst){lst[1:length(lst)] %>% paste(collapse='') %>% toupper()})
    #convert the list into a data frame
    list_seq <- data.frame(names(list_seq) %>% purrr::as_vector(),unname(list_seq) %>% purrr::as_vector())
    names(list_seq) <- c(groupid,'Sequence')
    #add the sequence column to the original data set. NOTE: This column is discarded later. If I omit this step the coverage columns get added wrong. Trying to fix this.
    tmp <- merge(x = data_table, y = list_seq, by = groupid, all.x = TRUE)

    #replace Is, Js and Ls with (I|J|L) to use for regex lookup, store the data set as "pep"
    pep <- tmp %>% dplyr::mutate_(.dots = stats::setNames(list(lazyeval::interp(~gsub(pattern='I',replacement='J',x=var),var=as_name(elementid))),elementid))
    pep <- pep %>% dplyr::mutate_(.dots = stats::setNames(list(lazyeval::interp(~gsub(pattern='L',replacement='J',x=var),var=as_name(elementid))),elementid))
    pep <- pep %>%  dplyr::mutate_(.dots = stats::setNames(list(lazyeval::interp(~gsub(pattern='J',replacement='(I|J|L)',x=var),var=as_name(elementid))),elementid))

    #extract the columns with the peptides and the sequences respectively
    peptide_column <- pep[,colnames(pep)==elementid]
    sequence_column <- pep[,colnames(pep)=='Sequence'] %>% as.character()
    #create Length vector, to be appended to the original data frame afterwards
    Length <- nchar(sequence_column)

    #crete empty vector "Start"
    Start <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        #if the peptide sequence is not present in the full protein sequence, fill row$Start with "Not found".
        if(length(stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]) == 0){
            Start[i] <- 'Not found'
        #if the peptide sequence appears once in the full protein sequence, fill row$Start with the number
        }else if(length(stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]) == 1){
            Start[i] <- stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]
        #if it appears more than once, it should return a list with the starting positions. Still pending.
        }else{
            Start[i] <- stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1] %>% list()
        }
    }
    #This would crash if anything but numbers are present in the vector Start. Still pending.
    Start <- as.integer(Start)

    #idem Start
    End <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        if(length(stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2]) == 0){
            End[i] <- 'Not found'
        }else if(length(stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2]) == 1){
            End[i] <- stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2]
        }else{
            End[i] <- stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2] %>% list()
        }}

    End <- as.integer(End)

    #this, ideally, would differentiate between peptides found twice within the protein structure. Still pending.
    Appearance <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        Appearance[i] <- length(stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1])
    }

    Appearance <- as.integer(Appearance)

    #create a data frame with the coverage columns created above
    cov_cols <- data.frame(Length,Start,End,Appearance)

    #bind the data frame above with the union of the original data and the sequence column. NOTE: Joining the original data directly does not work as intended.
    out_table <- cbind(tmp,cov_cols)
    #exclude Sequence column.
    out_table <- out_table[colnames(out_table)!='Sequence'] #todo: optimize step

    #export?
    if(export==FALSE){
        return(out_table)
    }else{
        utils::write.csv(out_table,outname)
        'Your file has been saved as' %>% paste(outname) %>% cat
    }
}
