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
#' @importFrom purrr as_vector map
#' @importFrom lazyeval interp
#' @importFrom seqinr read.fasta
#' @return Data frame with the following added columns: "Start", "End", "Sequence", "Appearance".
#' @export
#' @examples
#' cov_columns(test_data,proteome='sequences.fasta',groupid='ID',elementid='Peptide')

# suppressPackageStartupMessages(library(tidyverse))
# suppressPackageStartupMessages(library(lazyeval))
# suppressPackageStartupMessages(library(seqinr))


cov_columns <- function(data_table,proteome,groupid,elementid,outname=paste0(deparse(substitute(data_table)),'_cov.csv'),export=FALSE){
    fasta <- seqinr::read.fasta(proteome)

    list_seq <- lapply(fasta,function(lst){lst[1:length(lst)] %>% paste(collapse='') %>% toupper()})
    list_seq <- data.frame(names(list_seq) %>% as_vector(),unname(list_seq) %>% as_vector())
    names(list_seq) <- c(groupid,'Sequence')

    tmp <- merge(x = data_table, y = list_seq, by = groupid, all.x = TRUE)


    pep <- tmp %>% dplyr::mutate_(.dots = setNames(list(lazyeval::interp(~gsub(pattern='I',replacement='J',x=var),var=as_name(elementid))),elementid))
    pep <- pep %>% dplyr::mutate_(.dots = setNames(list(lazyeval::interp(~gsub(pattern='L',replacement='J',x=var),var=as_name(elementid))),elementid))
    pep <- pep %>%  dplyr::mutate_(.dots = setNames(list(lazyeval::interp(~gsub(pattern='J',replacement='(I|J|L)',x=var),var=as_name(elementid))),elementid))
    peptide_column <- pep[,colnames(pep)==elementid]
    sequence_column <- pep[,colnames(pep)=='Sequence'] %>% as.character()

    Length <- nchar(sequence_column)

    Start <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        if(length(stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]) == 0){
            Start[i] <- 'Not found'
        }else if(length(stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]) == 1){
            Start[i] <- stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]
        }else{
            Start[i] <- stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1] %>% list()
        }
    }

    Start <- as.integer(Start)

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


    Appearance <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        Appearance[i] <- length(stringr::str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1])
    }

    Appearance <- as.integer(Appearance)

    cov_cols <- data.frame(Length,Start,End,Appearance)

    out_table <- cbind(tmp,cov_cols)
    out_table <- out_table[colnames(out_table)!='Sequence'] #todo: optimize step
    if(export==FALSE){
        return(out_table)
    }else{
        utils::write.csv(out_table,outname)
        'Your file has been saved as' %>% paste(outname) %>% cat
    }
}
