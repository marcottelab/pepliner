<<<<<<< HEAD
#' Append columns related to the coverage of a protein.
#'
#' @param data_table Data frame to which the coverage columns will be appended.
#' @param proteome FASTA file from which the sequences will be extracted.
#' @param outname If export = TRUE, output the resulting .
#' @param groupid Data frame column (factor) corresponding to the protein name to be matched with the IDs extracted from the FASTA file.
#' @param elementid Data frame column (factor) corresponding to the peptide sequence to be searched against the sequences extracted from the FASTA file.
#' @param export If false (default), return a data frame. If true, write a .csv file named after the input file + '_cov.csv'.
#' @import dplyr
#' @import stringr
#' @import purrr
#' @import lazyeval
#' @import seqinr
#' @return Data frame with the following added columns: "Start", "End", "Sequence", "Appearance".
#' @export
#' @examples
#' cov_columns(test_data,proteome='sequences.fasta',groupid='ID',elementid='Peptide')

# suppressPackageStartupMessages(library(tidyverse))
# suppressPackageStartupMessages(library(lazyeval))
# suppressPackageStartupMessages(library(seqinr))


cov_columns <- function(data_table,proteome,groupid,elementid,export=FALSE, outname=paste0(deparse(substitute(data_table)),'_cov.csv')){
    fasta <- read.fasta(proteome, as.string = TRUE)


    print(head(fasta))
    #Why is this in list structure vs. dataframe structure?
    #list_seq <- lapply(fasta,function(lst){lst[1:length(lst)] %>% paste(collapse='') %>% toupper()})
    list_seq <- lapply(fasta,function(lst){lst %>% toupper()})
    list_seq <- data.frame(names(list_seq) %>% as_vector(),unname(list_seq) %>% as_vector())
    print(list_seq)
    names(list_seq) <- c(groupid,'Sequence')
    pep <- merge(x = data_table, y = list_seq, by = groupid, all.x = TRUE)

    pep <- pep %>% mutate_(.dots = setNames(list(interp(~gsub(pattern='I',replacement='J',x=var),var=as_name(elementid))),elementid))
    pep <- pep %>% mutate_(.dots = setNames(list(interp(~gsub(pattern='L',replacement='J',x=var),var=as_name(elementid))),elementid))
    pep <- pep %>%  mutate_(.dots = setNames(list(interp(~gsub(pattern='J',replacement='(I|L)',x=var),var=as_name(elementid))),elementid))
    peptide_column <- pep[,colnames(pep)==elementid]
    sequence_column <- pep[,colnames(pep)=='Sequence'] %>% as.character()

    Length <- nchar(sequence_column)

    Start <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        if(length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]) == 0){
            Start[i] <- 'Not found'
        }else if(length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]) == 1){
            Start[i] <- str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]
        }else{
            Start[i] <- str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1] %>% list()
        }
    }

    Start <- as.integer(Start)

    End <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        if(length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2]) == 0){
            End[i] <- 'Not found'
        }else if(length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2]) == 1){
            End[i] <- str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2]
        }else{
            End[i] <- str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2] %>% list()
        }}

    End <- as.integer(End)

    #what is this column for?
    Appearance <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        Appearance[i] <- length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1])
    }

    Appearance <- as.integer(Appearance)

    cov_cols <- data.frame(Length,Start,End,Appearance)

    out_table <- cbind(data_table,cov_cols)
    if(export==FALSE){
        return(out_table)
    }else{
        write.csv(out_table,outname)
        'Your file has been saved as' %>% paste(outname) %>% cat
    }
}
=======
#' Append columns related to the coverage of a protein.
#'
#' @param data_table Data frame to which the coverage columns will be appended.
#' @param proteome FASTA file from which the sequences will be extracted.
#' @param outname If export = TRUE, output the resulting .
#' @param groupid Data frame column (factor) corresponding to the protein name to be matched with the IDs extracted from the FASTA file.
#' @param elementid Data frame column (factor) corresponding to the peptide sequence to be searched against the sequences extracted from the FASTA file.
#' @param export If false (default), return a data frame. If true, write a .csv file named after the input file + '_cov.csv'.
#' @import dplyr
#' @import stringr
#' @import purrr
#' @import lazyeval
#' @import seqinr
#' @return Data frame with the following added columns: "Start", "End", "Sequence", "Appearance".
#' @export
#' @examples
#' cov_columns(test_data,proteome='sequences.fasta',groupid='ID',elementid='Peptide')

# suppressPackageStartupMessages(library(tidyverse))
# suppressPackageStartupMessages(library(lazyeval))
# suppressPackageStartupMessages(library(seqinr))


cov_columns <- function(data_table,proteome,outname=paste0(deparse(substitute(data_table)),'_cov.csv'),groupid,elementid,export=FALSE){
    fasta <- read.fasta(proteome)

    list_seq <- lapply(fasta,function(lst){lst[1:length(lst)] %>% paste(collapse='') %>% toupper()})
    list_seq <- data.frame(names(list_seq) %>% as_vector(),unname(list_seq) %>% as_vector())
    names(list_seq) <- c(groupid,'Sequence')

    tmp <- merge(x = data_table, y = list_seq, by = groupid, all.x = TRUE)


    pep <- tmp %>% mutate_(.dots = setNames(list(interp(~gsub(pattern='I',replacement='J',x=var),var=as_name(elementid))),elementid))
    pep <- pep %>% mutate_(.dots = setNames(list(interp(~gsub(pattern='L',replacement='J',x=var),var=as_name(elementid))),elementid))
    pep <- pep %>%  mutate_(.dots = setNames(list(interp(~gsub(pattern='J',replacement='(I|L)',x=var),var=as_name(elementid))),elementid))
    peptide_column <- pep[,colnames(pep)==elementid]
    sequence_column <- pep[,colnames(pep)=='Sequence'] %>% as.character()

    Length <- nchar(sequence_column)

    Start <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        if(length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]) == 0){
            Start[i] <- 'Not found'
        }else if(length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]) == 1){
            Start[i] <- str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]
        }else{
            Start[i] <- str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1] %>% list()
        }
    }

    Start <- as.integer(Start)

    End <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        if(length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2]) == 0){
            End[i] <- 'Not found'
        }else if(length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2]) == 1){
            End[i] <- str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2]
        }else{
            End[i] <- str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2] %>% list()
        }}

    End <- as.integer(End)


    Appearance <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        Appearance[i] <- length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1])
    }

    Appearance <- as.integer(Appearance)

    cov_cols <- data.frame(Length,Start,End,Appearance)

    out_table <- cbind(data_table,cov_cols)
    if(export==FALSE){
        return(out_table)
    }else{
        write.csv(out_table,outname)
        'Your file has been saved as' %>% paste(outname) %>% cat
    }
}
>>>>>>> parent of d56b055... Move optional arguments to end of function args
