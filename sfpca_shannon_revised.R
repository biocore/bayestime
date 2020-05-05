setwd("~/Dropbox/lab/Kids")
library(vegan)
library(tidyr)
library(parallel)
library(rstan)
library(foreach)
library(Matrix)
options(mc.cores = parallel::detectCores())
source('Qiime2/code/sfpca_revised.R')
#only keep kids in yonug group
meta <- read.delim('Qiime2/data/11405_prep_3914_qiime_20190424-152313.txt', sep="\t", stringsAsFactors = F)
meta_young <- meta[meta$age_group == 'young', ]
shannon <-read.csv('Qiime2/data/60671-alpha_shannon/alpha-diversity.tsv', stringsAsFactors=FALSE, sep='\t')
data <- merge(meta_young, shannon, by.x = 'X.SampleID', by.y = 'X')

data$age <- as.numeric(data$age)
age_ix <- which(colnames(data)=='age')
var_names <- colnames(data)[age_ix:ncol(data)]
data_invar_names <- get_invariants(data, var_names, 'host_subject_id', 'age_group_month')

#qqplot
plot_qqplot(data$shannon, 'Qiime2/results/qqPlot_shannon.pdf')
plot_qqplot(data$shannon)

#preprocess data
prepared_data = prepare_data(data=data, unique_subject_id = 'host_subject_id', time_var='age', 
                             response='shannon', transform.y='standardize', scale.time=TRUE)

Nsamples = 1000
Nchains = 3
model_file = "Qiime2/code/sfpca2.stan"
smod = stan_model(model_file)
PC_max = 2 # number of PCs
D_max = 2 # number of knots

sfpca_stan_results <- sfpca_stan(PC_max, D_max, Nsamples = 1000, Nchain = 3, smod, prepared_data)
#save(sfpca_stan_results, file='Qiime2/results/sfpca/stan_shannon_2pc2knots.RData')
#load('Qiime2/results/sfpca/stan_shannon_2pc2knots.RData')
sfpca_model <- get_optimal_model(sfpca_stan_results)

# plot_k_diagnostic(sfpca_model, prepared_data, Nsamples, Nchains,
#                   'Qiime2/results/sfpca/plot_diagnostic_shannon.pdf')
plot_k_diagnostic(sfpca_model, prepared_data, Nsamples, Nchains)

results_basis = basis_setup_sparse(prepared_data=prepared_data, sfpca_model$k, orth=TRUE)

results_rotation <- post_hoc_rotation(sfpca_model, prepared_data, Nchains, Nsamples)

results_list <- output_results(prepared_data, data_invar_names, results_rotation, results_basis)
summary(results_list
        )

# sfpca_rda_results <- sfpca_rda(results_list, sfpca_model, data_invar_names, 'host_subject_id',
#                                path='Qiime2/results/sfpca/rda_shannon.png')
sfpca_rda_results <- sfpca_rda(results_list, sfpca_model, data_invar_names, 'host_subject_id')

#save(sfpca_rda_results, file='Qiime2/results/sfpca/rda_table_shannon.RData')

#plot_residual_analysis(results_list, 'Qiime2/results/sfpca/plot_residual_shannon.pdf')
plot_residual_analysis(results_list)

# plot_mean_curve(results_basis, results_list, prepared_data, 'host_subject_id', 'age', 'shannon',
#                 data_invar_names,-1,3, path='Qiime2/results/sfpca/plot_mean_curve_shannon.pdf')
plot_mean_curve(results_basis, results_list, prepared_data, 'host_subject_id', 'age', 'shannon',
                data_invar_names)

# plot_group(data, 'age', 'shannon', 'host_subject_id', data_invar_names,
#            path='Qiime2/results/sfpca/plot_group_shannon.pdf')
plot_group(data, 'age', 'shannon', 'host_subject_id', data_invar_names[2])


