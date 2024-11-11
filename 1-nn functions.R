# P(a point d away from x=0 has its nn on the other side | nearest pt is r away)
f = function(d, r) {
  acos(d/r) * 2*r*exp(-pi*r^2)
}

# P(a point d away from x=0 has its nn on the other side)
p = function(d) {
  integrate(function(r) {f(d,r)}, d, Inf)$value
}

# P(a point in [-t/2,t/2]^2 has its nn on the other side)
phi = function(t) {
  2/t * integrate(Vectorize(function(d) p(d)), 0, t/2)$value
}

# P(a pt is its nn's nn)
psi = integrate(function(r) {exp(-r^2 * (pi/3 + sqrt(3)/2)) * 2*pi*r*(exp(-pi*r^2))}, 0, Inf)$value

# P(a pt is opp and dbl)
theta_inner = function(d) {
  integrate(function(r) {exp(-r^2 * (pi/3 + sqrt(3)/2)) * 2*pi*r*(exp(-pi*r^2)) * p(d)}, d, Inf)$value
}

theta = function(t) {
  integrate(Vectorize(theta_inner), 0, t/2)$value * 2/t
}

prob = function(t) {
  phi(t) - theta(t)/2
}

# approximate E[number of crossings in [-t/2,t/2]]
E = function(t) {
  t^2*prob(t)
}