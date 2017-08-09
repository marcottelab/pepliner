library(pepliner)
library(testthat)
#setwd('../tests/testhat')
test_data <- read.csv('../../data/msdata.csv')
test_cov <- test_data %>% cov_columns('../../data/proteome.fasta','ID','Peptide')
test_cc <- test_data %>% complete_counts('FractionID','PeptideCount')
test_cov_cc <- test_cov %>% cov_columns('R/test_proteome.fasta','ID','Peptide') %>% complete_counts('FractionID','PeptideCount')

test_sparkplot <- test_cov_cc %>% sparkplot('Peptide','FractionID','PeptideCount','ID','sp|P55036|PSMD4_HUMAN')
test_sparkplot %>% cowplot::ggdraw()

test_covplot <- test_cov_cc %>% covplot('Peptide','ID','sp|P55036|PSMD4_HUMAN')
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

test_that('Test that the output of complete_counts is correct',{
    expect_equal(nrow(test_cc),1512)
    expect_true(ncol(test_data)==ncol(test_cc))
    expect_true(nrow(test_cc) == nrow(unique(test_cc)))
})
