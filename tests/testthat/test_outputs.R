library(pepliner)
library(testthat)
#setwd('../tests/testhat')
#setwd('~/Dropbox/pepliner/parent_directory/pepliner/tests/testthat/')

#test_data <- read.csv(system.file('data/msdata.csv',package='pepliner'))

test_peptide_tidy_data <- read.csv("data/Hs_CB660_1105_peptide_elution_human_protein_minimal.csv")

test_fasta <- read_fasta("uniprot-proteome_human_reviewed_minimal.fasta")

test_that("Test successful read of FASTA file with read_fasta function", {
    expect_true(ncol(test_fasta) == 2)
    expect_true(nrow(test_fasta) == 11)
    expect_true(names(test_fasta)) == c("ID", "Sequence")
})

cat('Testing peptide position calculation (cov_columns)\n')


test_peptides <- test_peptide_tidy_data %>% select(ID, Peptide) %>% unique

test_seqannot_peptides <- left_join(test_peptides, test_fasta, by = "ID")

test_that("Input to calculating coverage is correct", {

    expect_true(ncol(test_seqannot_peptides) == 3)
    expect_true(names(test_seqannot_peptides) == c("ID", "Peptide", "Sequence"))
    expect_true(nrow(test_peptides) == nrow(test_seqannot_peptides))
})

test_cov <- cov_columns(test_seqannot_peptides, peptide_column="Peptide", sequence_column="Sequence")

test_that("Coverage attributes correct", {

    expect_true(nrow(test_peptides)==nrow(test_cov))
    expect_true('Start' %in% names(test_cov))
    expect_true('End' %in% names(test_cov))
    expect_true('Length' %in% names(test_cov))
    expect_equal(class(test_cov$Start),"integer")
    expect_equal(class(test_cov$End),"integer")
    expect_equal(class(test_cov$Length),"integer")
})

test_that("Coverage Start position correct", {
     expect_true(any(is.na(test_cov$Start)) == FALSE)
})

test_that("Coverage End position correct", {
     expect_true(any(is.na(test_cov$End)) == FALSE)
})

test_that("Coverage Length is correct", {
     expect_true(any(is.na(test_cov$Length)) == FALSE)
})

cat('Testing that completing tidy observations is correct\n')

test_peptide_tidy_complete <- complete_counts(test_peptide_tidy_data, 



#cat('Successful read\n')
#test_cov <- test_data %>% cov_columns(system.file('extdata/proteome.fasta',package='pepliner'),'ID','Peptide')
#cat('Column adding: no errors.\n')
#test_cc <- test_data %>% complete_counts('FractionID','PeptideCount')
#cat('Row adding: no errors.\n')
#test_cov_cc <- test_cov %>% complete_counts('FractionID','PeptideCount')
#cat('Applying cov_columns and complete counts to the same data set: no errors.\n')
#test_sparkplot <- test_cov_cc %>% sparkplot('Peptide','FractionID','PeptideCount','ID','sp|P55036|PSMD4_HUMAN')
#cat('Sparkplotting: no errors.\n')
#test_sparkplot %>% cowplot::ggdraw()
#
#test_covplot <- test_cov_cc %>% covplot(elementid='Peptide',groupid='ID',group_name = 'sp|Q9P258|RCC2_HUMAN')
#cat('Covplotting: no errors.\n')
#test_covplot %>% cowplot::ggdraw() #+ cowplot::draw_label("DRAFT!", size = 20, alpha = .8, x=0.15, y=0.05)
#
#
#
#test_that('Test that the output of cov_columns is correct',{
#    expect_true(nrow(test_data)==nrow(test_cov))
#    expect_true('Start'%in%names(test_cov))
#    expect_true('End'%in%names(test_cov))
#    expect_true('Length'%in%names(test_cov))
#    expect_true('Appearance'%in%names(test_cov))
#    expect_equal(class(test_cov$Start),"integer")
#    expect_equal(class(test_cov$End),"integer")
#    expect_equal(class(test_cov$Length),"integer")
#})
#cat('\nTest1: success\n')
#test_that('Test that the output of complete_counts is correct',{
#    expect_equal(nrow(test_cc),1512)
#    expect_true(ncol(test_data)==ncol(test_cc))
#    expect_true(nrow(test_cc) == nrow(unique(test_cc)))
#})
#cat('\nTest2: success\n')

