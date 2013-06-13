#require("distributions");
using Distributions;
#using Gaston;

function draw_from_discrete_mixture_prior(N_samp::Integer)
N_comp = 3;

sigma_obs_h = 0.001;
sigma_obs_k = 0.001;
sigma_e_dist = Uniform(0.0,1.0);
f_dist = Dirichlet(N_comp);

elist = Array(Float64, N_samp);

for i in 1:N_samp
  e = 2.0;
  while (e>=1.0)
  sigma_e_vector = [ rand(sigma_e_dist)  rand(sigma_e_dist)  rand(sigma_e_dist) ];
  f = rand(f_dist);
  c_dist = Categorical(f);
  c = rand(c_dist);
  sigmac = sigma_e_vector[c];
  h_dist = Normal(0.0,sigma_e_vector[c]);
  k_dist = Normal(0.0,sigma_e_vector[c]);
  h = rand(h_dist);
  k = rand(k_dist);
  
  #h_hat_dist = Normal(h,sigma_obs_h);
  #k_hat_dist = Normal(k,sigma_obs_k);
  #h_hat = rand(h_hat_dist);
  #k_hat = rand(k_hat_dist);  
  e = sqrt(h*h+k*k);
  end
  elist[i] = e;
end
return elist;
end

function draw_from_continuous_mixture_prior(N_samp::Integer, alpha::Real)
sigma_obs_h = 0.001;
sigma_obs_k = 0.001;
sigma_e_dist = Uniform(0.0,1.0);
#alpha = 2.0;

elist = Array(Float64, N_samp);

for i in 1:N_samp
  e = 2.0
  while (e>=1.0)

  sigma_e = rand(sigma_e_dist);
  lambda_dist = Gamma(alpha*0.5,alpha*0.5);
  lambda = rand(lambda_dist);

  h_dist = Normal(0.0,sigma_e/sqrt(lambda));
  k_dist = Normal(0.0,sigma_e/sqrt(lambda));
  h = rand(h_dist);
  k = rand(k_dist);

  #h_hat_dist = Normal(h,sigma_obs_h);
  #k_hat_dist = Normal(k,sigma_obs_k);
  #h_hat = rand(h_hat_dist);
  #k_hat = rand(k_hat_dist);
  e = sqrt(h*h+k*k);
  end
  elist[i] = e;
end
return elist;
end

function write_histo_discrete_mixture_prior(N_samp::Integer)
  samples = Array(Float64, N_samp);
  tic();
  samples = draw_from_discrete_mixture_prior(N_samp);
  toc();
  N_bins = 50;
  dx_bin = 1.0/N_bins
  bin_boundarys = [0:dx_bin:1];
  counts = hist(samples,bin_boundarys) ./ (N_samp*dx_bin);
  bin_centers = bin_boundarys + 0.5*dx_bin;
  writedlm("discrete_mixture_prior.dat",[bin_centers counts],' ');
end

function write_histo_continuous_mixture_prior(N_samp::Integer, alpha::Real)
  samples = Array(Float64, N_samp);
  tic();
  samples = draw_from_continuous_mixture_prior(N_samp,alpha);
  toc();
  N_bins = 50;
  dx_bin = 1.0/N_bins
  bin_boundarys = [0:dx_bin:1];
  counts = hist(samples,bin_boundarys) ./ (N_samp*dx_bin);
  bin_centers = bin_boundarys + 0.5*dx_bin;
  writedlm("continuous_mixture_prior.dat",[bin_centers counts],' ');
end


write_histo_discrete_mixture_prior(1000000);
write_histo_continuous_mixture_prior(1000000,1.0)
