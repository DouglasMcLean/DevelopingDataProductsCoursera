This R-shiny app details the construction of the theoretical density functions for the Poisson, Gamma, Compound Poisson and conditional event distribution given a realised value from the compound Poisson distribution. The compound Poisson variable sums up a random number of continuous iid Gamma random variables. The number of Gamma variables is sampled independently from a Poisson distribution.

Furthermore to the theory, one may simulate and examine the numerical summaries from these theoretical distrubtions by clickling on the tabs. These may be compared back to the theoretical densities.

There are several parameters corresponding to each of the distributions. The discrete Poisson distribution's rate parameter constrains the incidence of events: the higher it is, the higher the frequency of events. It is positive. The continuous Gamma distribution models the size of the events and is parameterised by a scale parameter (determinining how diffuse the Gamma distribution is) and a shape parameter (which changes its curvature). The compound Poisson variable depends on all three parameters.

Several hyper-parameters are required.

1. Series Truncation: the theoretical compound Poisson distribution needs to be truncated after this many terms for practical implementation. 
2. Grid Size/#Simulations: this is either the number of points used to display the theoretical distributions or the number of random replications from each distribution.
3. Range: for the continuous distributions, this is the extent represented on the x-axis.
4. Event Range: this is the range of events displayed for the discrete distributions.
5. Sample y Value: this is the value upon which the Conditional Event distribution is conditioned.

One may play with the Gamma shape and scale parameters, along with the Poisson rate parameter to assess quickly how the (relatively intractible) compound Poisson distribution behaves dependent on these.

