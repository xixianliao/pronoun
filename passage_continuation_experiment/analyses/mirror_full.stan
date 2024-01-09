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
  // array[N_pron] int<lower=0, upper=1> pp_pron;
}
parameters {
  real<lower=0, upper=1> alpha;
  real beta_rtype1;
  real beta_rtype2;
  real beta_ref;
  real beta_int1; // interaction between rtype1 and ref
  real beta_int2; // interaction between rtype2 and ref
  // Random effects:
  vector<lower=0>[6] tau_u;
  vector<lower=0>[4] tau_w;
  matrix[6, N_subj] z_u;
  matrix[4, N_item] z_w;
  cholesky_factor_corr[6] L_u;
  cholesky_factor_corr[4] L_w;
}
transformed parameters {
  matrix[N_subj, 6] u = (diag_pre_multiply(tau_u, L_u) * z_u)'; // adjustments for subjects
  matrix[N_item, 4] w = (diag_pre_multiply(tau_w, L_w) * z_w)'; // adjustment for item
  vector[N_free] lo_pp;
  //vector[N_free] lo_other;
  
  //log odss of pronoun| referent , item, subject, relation type
  
  //log odds of 'pronoun' vs 'non-pronoun'
  lo_pp = (alpha + u[subj_free, 1] + w[item_free, 1])
          + rtypej1[item_free] .* (beta_rtype1 + u[subj_free, 2])
	  + rtypej2[item_free] .* (beta_rtype2 + u[subj_free, 3]) 
          + ref_free .* (beta_ref + u[subj_free, 4] + w[item_free, 2])
          + rtypej1[item_free] .* ref_free .* (beta_int1 + u[subj_free, 5] + w[item_free, 3])
          + rtypej2[item_free] .* ref_free .* (beta_int2 + u[subj_free, 6] + w[item_free, 4]);
  
}
model {
  //for (i in 1 : N_free) 
  target += bernoulli_logit_lpmf(pron_free | lo_pp);
  
  // Priors for f.e
  target += beta_lpdf(alpha | 1, 1); // change from normal (0,2) priors for categorical regression to Bernoulli
  target += normal_lpdf(beta_rtype1 | 0, 2);
  target += normal_lpdf(beta_rtype2 | 0, 2);
  target += normal_lpdf(beta_ref | 0, 2);
  target += normal_lpdf(beta_int1 | 0, 2);
  target += normal_lpdf(beta_int2 | 0, 2);
  // Priors for r.e.
  target += normal_lpdf(tau_u | 0, 2) - 6 * normal_lccdf(0 | 0, 2);
  target += normal_lpdf(tau_w | 0, 2) - 4 * normal_lccdf(0 | 0, 2);
  target += lkj_corr_cholesky_lpdf(L_u | 2);
  target += lkj_corr_cholesky_lpdf(L_w | 2);
  target += std_normal_lpdf(to_vector(z_u));
  target += std_normal_lpdf(to_vector(z_w));
}
generated quantities {
  corr_matrix[6] rho_u = L_u * L_u';
  corr_matrix[4] rho_w = L_w * L_w';
  array[N_pron] int NP1_pred;
  array[N_pron] real loglik;
  
  // beta_rtype1(narration) =1, ref np1 = 1
  // pronoun, non-pronoun
  real pron_ref1_narration = inv_logit(alpha + beta_rtype1 + beta_ref + beta_int1);
  
  real pron_ref2_narration = inv_logit(alpha + beta_rtype1 + -beta_ref + -beta_int1);

  real P_mirror_narration = pron_ref1_narration
                         / (pron_ref1_narration + pron_ref2_narration);
 
  real pron_ref1_contrast = inv_logit(alpha + beta_rtype2 + beta_ref + beta_int2);

  real pron_ref2_contrast = inv_logit(alpha + beta_rtype2 + -beta_ref + -beta_int2);
 
  real P_mirror_contrast = pron_ref1_contrast
                         / (pron_ref1_contrast + pron_ref2_contrast);

  real pron_ref1_result = inv_logit(alpha + -beta_rtype1 + -beta_rtype2 + beta_ref
                                     + -beta_int1 + -beta_int2);
  
  real pron_ref2_result = inv_logit(alpha + -beta_rtype1 + -beta_rtype2 + -beta_ref
                                     + -beta_int1 + -beta_int2);
  
  real P_mirror_result = pron_ref1_result
                         / (pron_ref1_result + pron_ref2_result);

  
  // "likelihood for personal pronoun"
  
  for (n in 1 : N_pron) {
    real pron_ref1 = inv_logit(alpha + u[subj_pron[n], 1]
                                   + w[item_pron[n], 1]
                                   + rtypej1[item_pron[n]]
                                     .* (beta_rtype1 + u[subj_pron[n], 2])
                                   + rtypej2[item_pron[n]]
                                     .* (beta_rtype2 + u[subj_pron[n], 3])
                                   + (beta_ref + u[subj_pron[n], 4]
                                      + w[item_pron[n], 2])
                                   + rtypej1[item_pron[n]]
                                     .* (beta_int1 + u[subj_pron[n], 5] + w[item_pron[n], 3])
                                   + rtypej2[item_pron[n]]
                                     .* (beta_int2 + u[subj_pron[n], 6] + w[item_pron[n], 4]));

    real pron_ref2 = inv_logit(alpha + u[subj_pron[n], 1]
                                   + w[item_pron[n], 1]
                                   + rtypej1[item_pron[n]]
                                     .* (beta_rtype1 + u[subj_pron[n], 2])
                                   + rtypej2[item_pron[n]]
                                     .* (beta_rtype2 + u[subj_pron[n], 3])				   
                                   + -(beta_ref + u[subj_pron[n], 4]
                                       + w[item_pron[n], 2])
                                   + -rtypej1[item_pron[n]]
                                     .* (beta_int1 + u[subj_pron[n], 5] + w[item_pron[n], 3])
                                   + -rtypej2[item_pron[n]]
                                     .* (beta_int2 + u[subj_pron[n], 6] + w[item_pron[n], 4])
);
    real P_pp = pron_ref1 / (pron_ref1 + pron_ref2);
    
    
    loglik[n] = bernoulli_lpmf(NP1_pron[n] | P_pp);
    NP1_pred[n] = bernoulli_rng(P_pp);
    
  }
}

