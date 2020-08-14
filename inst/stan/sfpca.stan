data {
  int <lower=1> N; // number of total unique subjects
  int <lower=1> K; // number of principal components
  int <lower=1> Q; // number of basis for the cubic spline basis 
  int <lower=1> cov_size; // number of elements for the covariance matrix

  int <lower=1> V[N]; // number of total visits for each subject
  int <lower=1> subject_starts[N]; // starting index for each subjects' visit
  int <lower=1> subject_stops[N]; // ending index for each subjects' visit
  int <lower=1> cov_starts[N]; // starting index for each subjects' covariance matrix
  int <lower=1> cov_stops[N]; // ending index for each subjects' covariance matrix

  vector[sum(V)] Y; // record response at each available time point for each subject 
  matrix[sum(V), Q] B; // transpose of sparse spline basis 
}
transformed data{
  vector[K] zero_k;
  vector[Q] zero_q;
  vector[sum(V)] zero_v;
  zero_k = rep_vector(0,K);
  zero_q = rep_vector(0,Q);
  zero_v = rep_vector(0, sum(V)); 
}
parameters { 
  vector[K] alpha[N];
  matrix[Q,K] Theta;
  real<lower=0> sigma_eps;
  vector[Q] theta_mu; 
}
model {
  sigma_eps ~ cauchy(0, 1); 
  theta_mu ~ normal(0, 1);

  for(k in 1:K){
    Theta[, k] ~ normal(0, 1);
  }

  // posterior draws
  for(n in 1:N){
    alpha[n] ~ multi_normal(zero_k, diag_matrix(rep_vector(1,K)));
    Y[subject_starts[n]: subject_stops[n]] ~ multi_normal(B[subject_starts[n]: subject_stops[n]]*theta_mu + 
                      B[subject_starts[n]: subject_stops[n]] * Theta * alpha[n], diag_matrix(rep_vector(sigma_eps^2, V[n])));
  }
} 
generated quantities{
  vector[cov_size] Sigma;
  matrix[Q, Q] W;
  vector[N] log_lik_marg;
  vector[N] log_lik;
  vector[sum(V)] Ynew;

  W = Theta * Theta';
  for (n in 1:N){
    Sigma[cov_starts[n]: cov_stops[n]] = to_vector(B[subject_starts[n]: subject_stops[n]] * Theta * Theta' * 
                                         B[subject_starts[n]: subject_stops[n]]' + diag_matrix(rep_vector(sigma_eps^2,V[n])));
    log_lik_marg[n] = multi_normal_lpdf(Y[subject_starts[n]: subject_stops[n]] | zero_v[subject_starts[n]: subject_stops[n]], 
                                        to_matrix(Sigma[cov_starts[n]: cov_stops[n]], V[n], V[n]));
    log_lik[n] = multi_normal_lpdf(Y[subject_starts[n]: subject_stops[n]] | B[subject_starts[n]: subject_stops[n]]*theta_mu + 
                          B[subject_starts[n]: subject_stops[n]] * Theta * alpha[n], diag_matrix(rep_vector(sigma_eps^2,V[n])));
    Ynew[subject_starts[n]: subject_stops[n]] = multi_normal_rng(B[subject_starts[n]: subject_stops[n]]*theta_mu + 
                      B[subject_starts[n]: subject_stops[n]] * Theta * alpha[n], diag_matrix(rep_vector(sigma_eps^2, V[n])));
  }
}
