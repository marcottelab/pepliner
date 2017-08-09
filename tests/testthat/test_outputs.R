library(pepliner)
library(testthat)
#setwd('../tests/testhat')
#setwd('~/Dropbox/pepliner/parent_directory/pepliner/tests/testthat/')
test_data <- read.csv('../../../pepliner/inst/extdata/msdata.csv')
cat('Successful read\n')
test_cov <- test_data %>% cov_columns('../../../pepliner/inst/extdata/proteome.fasta','ID','Peptide')
cat('Successful column adding\n')
test_cc <- test_data %>% complete_counts('FractionID','PeptideCount')
cat('Successful row adding\n')
test_cov_cc <- test_cov %>% complete_counts('FractionID','PeptideCount')
cat('Successful piping\n')
test_sparkplot <- test_cov_cc %>% sparkplot('Peptide','FractionID','PeptideCount','ID','sp|P55036|PSMD4_HUMAN')
cat('Succesful sparkplotting\n')
test_sparkplot %>% cowplot::ggdraw()


test_covplot <- test_cov_cc %>% covplot(elementid='Peptide',groupid='ID',group_name = 'sp|P55036|PSMD4_HUMAN')
cat('Successful covplotting\n')
test_covplot %>% cowplot::ggdraw()

#
#
# column presence
# nrow
test_that('Test that the output of cov_columns is correct',{
    expect_true(nrow(test_data)==nrow(test_cov))
    expect_true('Start'%in%names(test_cov))
    expect_true('End'%in%names(test_cov))
    expect_true('Length'%in%names(test_cov))
    expect_true('Appearance'%in%names(test_cov))
})
cat('\nTest1: success\n')
test_that('Test that the output of complete_counts is correct',{
    expect_equal(nrow(test_cc),1512)
    expect_true(ncol(test_data)==ncol(test_cc))
    expect_true(nrow(test_cc) == nrow(unique(test_cc)))
})
cat('\nTest2: success\n')

