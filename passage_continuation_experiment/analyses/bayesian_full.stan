data {
  //for fitting
  int N_free; //obs in the free prompt data
  array[N_free] int NP1_free; // referent == 1 in free prompt data
  array[N_free] int pron_free; // 1 is pronoun, 0 is non-pronoun
  int N_subj; // N subj in free prompt data
  int N_item; // total number of subject
  vector[N_item] rtypej1; // Narration==1, Contrast==0, Result== -1 for each item
  vector[N_item] rtypej2; // Narration==0, Contrast==1, Result==-1 for each item 
  vector[N_free] ref_free; // either NP1 ==1, or NP2 ==-1 for each obs
  array[N_free] int<lower=1, upper=N_subj> subj_free; // indexes subjects
  array[N_free] int<lower=1, upper=N_item> item_free; // indexes items
  //for prediction
  int N_pron; //obs in the pronoun prompt data
  array[N_pron] int NP1_pron; // referent == 1 in pronoun prompt data
  array[N_pron] int<lower=1, upper=N_item> item_pron; // indexes items in the pronoun prompt data
  array[N_pron] int<lower=1, upper=N_subj> subj_pron; // indexes subjects
  vector[N_pron] ref_pron; // either NP1 ==1, or NP2 ==-1 for each obs
  // array[N_pron] int pp_pron;
}
parameters {
  real<lower=0, upper=1> P_ref_NP1;
  //fixed effects for all the different log-odds
  real<lower=0, upper=1> alpha;
  array[2] real beta_rtype1;
  array[2] real beta_rtype2;
  real beta_ref;
  real beta_int1;
  real beta_int2;
  // Random effects:
  vector<lower=0>[9] tau_u; // by-participant
  vector<lower=0>[5] tau_w; // by-item
  matrix[9, N_subj] z_u;
  matrix[5, N_item] z_w;
  cholesky_factor_corr[9] L_u;
  cholesky_factor_corr[5] L_w;
}
transformed parameters {
  matrix[N_subj, 9] u = (diag_pre_multiply(tau_u, L_u) * z_u)';
  matrix[N_item, 5] w = (diag_pre_multiply(tau_w, L_w) * z_w)';
  vector[N_free] lo_pp;
  vector[N_free] lo_ref;
  real alpha_NP1 = logit(P_ref_NP1);
  
  //log odds of 'pronoun' vs 'non-pronoun'
  lo_pp = (alpha + u[subj_free, 1] + w[item_free, 1])
          + rtypej1[item_free] .* (beta_rtype1[1] + u[subj_free, 2])
          + rtypej2[item_free] .* (beta_rtype2[1] + u[subj_free, 3])
          + ref_free .* (beta_ref + u[subj_free, 4] + w[item_free, 2])
          + rtypej1[item_free] .* ref_free .* (beta_int1 + u[subj_free, 5] + w[item_free, 3])
	  + rtypej2[item_free] .* ref_free .* (beta_int2 + u[subj_free, 6] + w[item_free, 4]);
  
  //log(theta_ref/(1-theta_ref))
  lo_ref = (alpha_NP1 + u[subj_free, 7] + w[item_free, 5])
           + rtypej1[item_free] .* (beta_rtype1[2] + u[subj_free, 8])
	   + rtypej2[item_free] .* (beta_rtype2[2] + u[subj_free, 9]);
}
model {
  target += bernoulli_logit_lpmf(NP1_free | lo_ref);
  for (i in 1 : N_free) {
    // Print the values of lo_pp[i] and pron_free[i]
    // print("Iteration ", i);
    // print("lo_pp[i] = ", lo_pp[i]);
    // print("pron_free[i] = ", pron_free[i]); 
    target +=  bernoulli_logit_lpmf(pron_free[i] | lo_pp[i]);
  }
  // Priors for f.e
  target += beta_lpdf(alpha | 1, 1);
  //target += normal_lpdf(alpha | 0, 2);
  target += beta_lpdf(P_ref_NP1 | 1, 1);
  target += normal_lpdf(beta_rtype1 | 0, 2);
  target += normal_lpdf(beta_rtype2 | 0, 2);
  target += normal_lpdf(beta_ref | 0, 2);
  target += normal_lpdf(beta_int1 | 0, 2);
  target += normal_lpdf(beta_int2 | 0, 2);
  // Priors for r.e.
  target += normal_lpdf(tau_u | 0, 2) - 9 * normal_lccdf(0 | 0, 2);
  target += normal_lpdf(tau_w | 0, 2) - 5 * normal_lccdf(0 | 0, 2);
  target += lkj_corr_cholesky_lpdf(L_u | 2);
  target += lkj_corr_cholesky_lpdf(L_w | 2);
  target += std_normal_lpdf(to_vector(z_u));
  target += std_normal_lpdf(to_vector(z_w));
}
generated quantities {
  corr_matrix[9] rho_u = L_u * L_u';
  corr_matrix[5] rho_w = L_w * L_w';
  array[N_pron] int NP1_pred;
  array[N_pron] real loglik;
  real P_ref_narration = inv_logit(alpha_NP1 + beta_rtype1[1]); //narra coded as 1,0
  real P_ref_contrast = inv_logit(alpha_NP1 + beta_rtype2[1]); //contrast codes as 0,1
  real P_ref_result = inv_logit(alpha_NP1 + -beta_rtype1[1] + -beta_rtype2[1]); //result codes as -1,-1
  // narra =1, ref np1 = 1
  real pron_ref1_narration = inv_logit(alpha + beta_rtype1[1] + beta_ref
                                     + beta_int1);
  
  real pron_ref2_narration = inv_logit(alpha + beta_rtype1[1] + -beta_ref
                                     + -beta_int1);

  real P_bayes_pp_narration = pron_ref1_narration * P_ref_narration
                        / (pron_ref1_narration * P_ref_narration
                           + pron_ref2_narration * (1 - P_ref_narration));

  real pron_ref1_contrast = inv_logit(alpha + beta_rtype2[1] + beta_ref
                                     + beta_int2);
  
  real pron_ref2_contrast = inv_logit(alpha + beta_rtype2[1] + -beta_ref
                                     + -beta_int2);
  
  real P_bayes_pp_contrast = pron_ref1_contrast * P_ref_contrast
                        / (pron_ref1_contrast * P_ref_contrast
                           + pron_ref2_contrast * (1 - P_ref_contrast));


  real pron_ref1_result = inv_logit(alpha + -beta_rtype1[1] + -beta_rtype2[1] 
				    + beta_ref + -beta_int1 + -beta_int2);
  
  real pron_ref2_result = inv_logit(alpha + -beta_rtype1[1] + -beta_rtype2[1] 
				    + -beta_ref + -beta_int1 + -beta_int2);
  
  real P_bayes_pp_result = pron_ref1_result * P_ref_result
                        / (pron_ref1_result * P_ref_result
                           + pron_ref2_result * (1 - P_ref_result));
  
  vector[N_pron] P_ref1 = inv_logit(alpha_NP1 + u[subj_pron, 7]
                                    + w[item_pron, 5]
                                    + rtypej1[item_pron] .* (beta_rtype1[2] + u[subj_pron, 8])
				    + rtypej2[item_pron] .* (beta_rtype2[2] + u[subj_pron, 9]));
  



  for (n in 1 : N_pron) {
    real pron_ref1 = inv_logit(alpha + u[subj_pron[n], 1]
                                   + w[item_pron[n], 1]
                                   + rtypej1[item_pron[n]]
                                     .* (beta_rtype1[1] + u[subj_pron[n], 2])
                                   + rtypej2[item_pron[n]]
                                     .* (beta_rtype2[1] + u[subj_pron[n], 3])
                                   + (beta_ref + u[subj_pron[n], 4]
                                      + w[item_pron[n], 2])
                                   + rtypej1[item_pron[n]]
                                     .* (beta_int1 + u[subj_pron[n], 5] + w[item_pron[n], 3])
                                   + rtypej2[item_pron[n]]
                                     .* (beta_int2 + u[subj_pron[n], 6] + w[item_pron[n], 4]));
    
    real pron_ref2 = inv_logit(alpha + u[subj_pron[n], 1]
                                   + w[item_pron[n], 1]
                                   + rtypej1[item_pron[n]]
                                     .* (beta_rtype1[1] + u[subj_pron[n], 2])
                                   + rtypej2[item_pron[n]]
                                     .* (beta_rtype2[1] + u[subj_pron[n], 3])
                                   + -(beta_ref + u[subj_pron[n], 4]
                                       + w[item_pron[n], 2])
                                   + -rtypej1[item_pron[n]]
                                     .* (beta_int1 + u[subj_pron[n], 5] + w[item_pron[n], 3])
                                   + -rtypej2[item_pron[n]]
                                     .* (beta_int2 + u[subj_pron[n], 6] + w[item_pron[n], 4]));
    
    // "likelihood for personal pronoun"
    real P_pp = pron_ref1 * P_ref1[n]
                / (pron_ref1 * P_ref1[n] + pron_ref2 * (1 - P_ref1[n]));

    
    
    loglik[n] = bernoulli_lpmf(NP1_pron[n] | P_pp);
    NP1_pred[n] = bernoulli_rng(P_pp);
    
  }
}
