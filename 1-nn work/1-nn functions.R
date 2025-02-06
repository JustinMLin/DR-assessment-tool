# P(a point d away from x=0 has its nn on the other side | nearest pt is r away)
f = function(d, r) {
  acos(d/r) * 2*r*exp(-pi*r^2)
}

# P(a point d away from x=0 has its nn on the other side)
p = function(d) {
  pnorm(-d*sqrt(2*pi))
}

# P(a point in [-t/2,t/2]^2 has its nn on the other side)
phi = function(t) {
  2/t * integrate(Vectorize(function(d) p(d)), 0, t/2)$value
}
phi_inner_closed = function(d) {
  pnorm(-d*sqrt(2*pi))
}

phi_closed = function(t) {
  pnorm(-t*sqrt(pi/2)) + 1/(pi*t)*(1-exp(-pi*t^2/4))
}

# P(a pt is its nn's nn)
psi = integrate(function(r) {exp(-r^2 * (pi/3 + sqrt(3)/2)) * 2*pi*r*(exp(-pi*r^2))}, 0, Inf)$value

# P(a pt is opp and dbl)
theta_inner = function(d) {
  integrate(function(r) {exp(-r^2 * (pi/3 + sqrt(3)/2)) * acos(d/r) * 2*r*exp(-pi*r^2)}, d, Inf)$value
}

theta = function(t) {
  integrate(Vectorize(theta_inner), 0, t/2)$value * 2/t
}

theta_closed = function(t) {
  a = 4/3*pi + sqrt(3)/2
  
  pi/a * (pnorm(-t*sqrt(a/2)) + 1/(t*sqrt(a*pi))*(1-exp(-a*t^2/4)))
}

prob = function(t) {
  phi(t) - theta(t)/2
}

prob_closed = function(t) {
  phi_closed(t) - theta_closed(t)/2
}

# approximate E[number of crossings in [-t/2,t/2]]
E_crossings = function(t) {
  t^2*prob(t)
}

E_crossings_closed = function(t) {
  t^2*prob_closed(t)
}