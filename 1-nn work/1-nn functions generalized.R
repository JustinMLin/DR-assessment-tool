library(spsh)

## gamma(p/2 + 1)
gamma_p2_plus_1 = function(p) {
  ifelse(p %% 2 == 0, factorial(p/2), sqrt(pi) * factorial(p) / (2^p*factorial((p-1)/2))) 
}

Vp = function(p, r) {
  pi^(p/2)/gamma_p2_plus_1(p) * r^p
}

Vp_partial = function(p, r) {
  pi^(p/2)/gamma_p2_plus_1(p) * p*r^(p-1)
}

## Beta((p-1)/2, 1/2)
beta_p_minus = function(p) {
  if (p == 2) pi
  else {
    if (p %% 2 == 0) {
      pi * factorial(p-3) / (2^(p-3) * factorial(p/2-2) * factorial(p/2-1))
    } else {
      2^(p-1) * factorial((p-3)/2) * factorial((p-1)/2) / factorial(p-1)
    }
  }
}

## Beta((p+1)/2, 1/2)
beta_p_plus = function(p) {
  if (p %% 2 == 0) {
    pi * factorial(p-1) / (2^(p-1) * factorial(p/2-1) * factorial(p/2))
  } else {
    2^p * factorial((p-1)/2)^2 / factorial(p)
  }
}

## I(z, (p-1)/2, 1/2)
I_minus = function(z, p) {
  Ibeta(z, (p-1)/2, 1/2)/beta_p_minus(p)
}

## I(z, (p+1)/2, 1/2)
I_plus = function(z, p) {
  Ibeta(z, (p+1)/2, 1/2)/beta_p_plus(p)
}

crescent_vol = function(p, r) {
  Vp(p, r)*(1 - I_plus(3/4, p))
}

f = function(p, r) {
  Vp_partial(p, r) * exp(-Vp(p, r))
}

SA_ratio = function(p, r, d) {
  1/2 * I_minus(1-(d/r)^2, p)
}

inner_phi_p = function(p, d) {
  integrate(function(r) {SA_ratio(p, r, d) * f(p, r)}, d, Inf)$value
}

phi_p = function(p, t) {
  integrate(Vectorize(function(d) {inner_phi_p(p, d)}), 0, t/2)$value * 2/t
}

inner_theta_p = function(p, d) {
  integrate(function(r) {exp(-crescent_vol(p, r)) * SA_ratio(p, r, d) * f(p, r)}, d, Inf)$value
}

theta_p = function(p, t) {
  integrate(Vectorize(function(d) {inner_theta_p(p, d)}), 0, t/2)$value * 2/t
}

prob_p = function(p, t) {
  phi_p(p, t) - theta_p(p, t)/2
}

# approximate E[number of crossings in [-t/2,t/2]]
E_crossings_p = function(p, t) {
  if (p > 1) t^p*prob_p(p, t)
  else (t/8 - 1/4)*exp(-t) + 1/4
}