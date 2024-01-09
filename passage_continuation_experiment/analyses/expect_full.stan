data {
  //for fitting
  int N_free; //obs in the free prompt data
  array[N_free] int NP1_free; // referent == 1 in free prompt data
  int N_subj; // N subj in free prompt data
  int N_item; // total number of subject
  vector[N_item] rtypej1; // Narration==1, Contrast==0, Result== -1 for each item
  vector[N_item] rtypej2; // Narration==0, Contrast==1, Result==-1 for each item 
  array[N_free] int<lower=1, upper=N_subj> subj_free; // indexes subjects
  array[N_free] int<lower=1, upper=N_item> item_free; // indexes items
  //for prediction
  int N_pron; //obs in the pronoun prompt data
  array[N_pron] int NP1_pron; // referent == 1 in pronoun prompt data
  array[N_pron] int<lower=1, upper=N_item> item_pron; // indexes items in the pronoun prompt data
  array[N_pron] int<lower=1, upper=N_subj> subj_pron; // indexes subjects
}
parameters {
  real<lower=0, upper=1> P_ref_NP1; // P(referent==1)
  real beta_rtype1; // difference between narration and mean in logodds
  real beta_rtype2; // difference between contrast and mean in logodds
  // Random effects:
  vector<lower=0>[3] tau_u; // by subject variance component
  real<lower=0> tau_w; // by items variance component
  matrix[3, N_subj] z_u;
  vector[N_item] z_w;
  cholesky_factor_corr[3] L_u;
}
transformed parameters {
  matrix[N_subj, 3] u; // adjustments for subjects
  vector[N_item] w; // adjustment for item
  real alpha_NP1 = logit(P_ref_NP1); //log odds of referent == 1
  vector[N_free] lo_ref; //log odds of referent == 1
  u = (diag_pre_multiply(tau_u, L_u) * z_u)';
  w = tau_w * z_w;
  lo_ref = (alpha_NP1 + u[subj_free, 1] + w[item_free])
           + rtypej1[item_free] .* (beta_rtype1 + u[subj_free, 2])
           + rtypej2[item_free] .* (beta_rtype2 + u[subj_free, 3]);
}
model {
  target += bernoulli_logit_lpmf(NP1_free | lo_ref);
  // Priors for f.e
  target += beta_lpdf(P_ref_NP1 | 1, 1);
  target += normal_lpdf(beta_rtype1 | 0, 2);
  target += normal_lpdf(beta_rtype2 | 0, 2);
  // Priors for r.e.
  target += normal_lpdf(tau_u | 0, 2) - 2 * normal_lccdf(0 | 0, 2);
  target += normal_lpdf(tau_w | 0, 2) - normal_lccdf(0 | 0, 2);
  target += lkj_corr_cholesky_lpdf(L_u | 2);
  target += std_normal_lpdf(to_vector(z_u));
  target += std_normal_lpdf(z_w);
}
generated quantities {
  array[N_pron] int NP1_pred;
  corr_matrix[3] rho_u = L_u * L_u';
  array[N_pron] real loglik;
  real P_expect_narration = inv_logit(alpha_NP1 + beta_rtype1); //narration coded as 1, 0
  real P_expect_contrast = inv_logit(alpha_NP1 + beta_rtype2); //contrast coded as 0,1
  real P_expect_result = inv_logit(alpha_NP1 - beta_rtype1 - beta_rtype2); // results coded as -1, -1
  vector[N_pron] lo_expect_NP1 = alpha_NP1 + u[subj_pron, 1] + w[item_pron]
                                 + rtypej1[item_pron]
                                   .* (beta_rtype1 + u[subj_pron, 2])
				 + rtypej2[item_pron]
				   .* (beta_rtype2 + u[subj_pron, 3]);
  
  for (n in 1 : N_pron) {
    NP1_pred[n] = bernoulli_logit_rng(lo_expect_NP1[n]);
    loglik[n] = bernoulli_logit_lpmf(NP1_pron[n] | lo_expect_NP1[n]);
  }
}
