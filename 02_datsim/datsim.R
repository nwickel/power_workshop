#' ---
#' title: "Data simulation in R"
#' author: ""
#' date: "Last modified: 2025-05-01"
#' bibliography: ../lit.bib
#' ---

#'
#' In this session, we will use R to simulate data sets. Simulating data can be
#' considered as the key idea behind hypothesis testing in the frequentist
#' framework, where we assume that we can repeat the data collection under
#' identical conditions. In this course, I will (hopefully) show you how to
#' use it to draw meaningful conclusions from your data.
#' 


#' # Overview
#'
#' 1. Random numbers generation (`rnorm()`, `runif()`, `rpois()`, `rbinom()`,
#'    etc.)
#' 2. Generate categorical variables (`factor()`, `rep()`)
#' 3. Create data frames containing quantitative and categorical variables
#'    (`data.frame()`, `expand.grid()`, `reshape()`)
#' 4. Drawing many data sets (`replicate()`, for loops)
#'


#' # Random numbers generation
#'
#' For drawing random numbers from a statistical distribution, the distribution
#' name is prefixed by “r” (random deviate). See `?Distributions` for a list of
#' distributions.
#'

rnorm(10)                # draw from standard normal distribution
runif(10)                # draw from uniform distribution
rpois(10, lambda = 1)    # draw from Poisson distribution

# Sampling with or without replacement from a vector
sample(1:5, size = 10, replace = TRUE)

#'
#' The random numbers generator in R is seeded: Upon restart of R, new random
#' numbers are generated. To replicate the results of a simulation, the seed
#' (starting value) can be set explicitly:

set.seed(1223)  # set seed, so on each run random numbers will be identical
runif(3)

#' ## Normal distribution
#' 
#' The normal distribution with parameters $\mu$ and $\sigma$: $X \sim N(\mu,
#' \sigma^2)$.

rnorm(100, mean = 100, sd = 15) |> hist(breaks = 30)

#' ## Multivariate normal distribution
#'
#' 

MASS::mvrnorm(100,
              mu = c(0, 0),
              Sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)) |>
  plot(xlab = "x1", ylab = "x2")

#' ## Poisson distribution (for count data)
#'
#' The Poisson distribution is a discrete probability distribution that is used
#' to model the probability of a given number of events occurring in a fixed
#' interval of time if these events occur with a known constant mean rate
#' ($\lambda$) and independently of the time since the last event.

rpois(100, lambda = 2.5) |> hist()

#' ## Binomial distribution
#'
#' The binomial distribution with parameters $n$ and $\pi$ is the discrete
#' probability distribution of the number of successes in a sequence of n
#' independent (Bernoulli) experiments: $X \sim Binom(n, \pi)$.
#'
#' Generate data from a binomial model using the function `rbinom()` in R; try
#' out different values of
#'
#' – $n$ (10, 500, 2000)  
#' – the parameter $\pi$ (0.5, 0.8, 0.44, 0.515)  
#'
#' and see how this affects the output.

n <- 10   # 500, 2000
p <- 0.5  # 0.8, 0.44, 0.515

rbinom(100, size = n, prob = p) |> table() |> plot()

#'
#' * With these data, test different null hypotheses using binom.test();
#'   these may or may not coincide with the values of $\pi$ used for data
#'   generation.

x <- rbinom(1, size = 10, prob = 0.3)
binom.test(x, n = 10, p = 0.5)

#'
#' * If you repeat data generation and testing, can you usually reject H0?
#'

pval <- replicate(500, {
  x <- rbinom(1, size = 50, prob = 0.3)
  binom.test(x, n = 50, p = 0.5)$p.value
  }
)

mean(pval < 0.05)


#' # Creating factors
#'
#' It is usually good practice to create categorical variables explicitly as
#' factors. Everything that is not a numeric variable should be a factor (e.g.,
#' id variables).

sex <- factor(rep(c("male", "female"), c(15, 20)),
              levels = c("male", "female", "diverse"))
sex
condition <- factor(rep(1:2, 20), levels = 1:2, labels = c("real", "VR"))
condition
group <- factor(rep(c("ctr", "trt1", "trt2"), each = 5))
group

#'
#' The `levels` argument sets explicitly the ordering of the factor levels. In
#' dummy coding (default in R) the first factor level is taken as the reference
#' category.

model.matrix( ~ group)
contrasts(group)


#' # Data frames
#'
#' When simulating data for a certain experimental design, this is reflected in
#' the structure of your data frame. For the `group` variable from above, the
#' design looks like this

model.matrix( ~ group) |> unique()

#'
#' A linear model will make use of this design and estimate three parameters:
#' \begin{align*}
#'   \beta_0 & = \text{mean of control group} \\
#'   \beta_1 & = \text{effect of treatment group 1} \\
#'   \beta_2 & = \text{effect of treatment group 2} \\
#' \end{align*}

#'
#' For a repeated-measures (within-subjects) design, the data frame will
#' usually be in a long format.

n <- 20

# A and B are recycled to match the length of id
datsim <- data.frame(id = factor(rep(1:n, each = 4)),
                     A = factor(rep(c("a1", "a2"), each = 2)),
                     B = factor(rep(c("b1", "b2"), times = 2)))

xtabs( ~ A + B, datsim)
xtabs( ~ id + A + B, datsim) |> ftable()

# OR

id <- factor(1:n)
A <- factor(c("a1", "a2"))
B <- factor(c("b1", "b2"))

datsim2 <- expand.grid(id = id, A = A, B = B) |> sort_by(~ id + A)

#'
#' We can transform the data frame between long and wide data format with
#' `reshape()`.

datl <- data.frame(id = factor(rep(1:n, each = 7)),
                   time = rep(0:6, times = n),
                   resp = rnorm(n * 7,
                                mean = seq(1, 5, length.out = 7), # seven means
                                sd = 1))

aggregate(resp ~ time, datl, mean)

datw <- reshape(datl, direction = "wide",
                idvar = "id",
                timevar = "time")

colMeans(datw[, -1])

cor(datw[, -1])


#' # Simulate repeatedly
#'
#' * Reading material on functional programming in R [@Wickham2019first]
#'   - http://adv-r.had.co.nz/Functions.html
#'   - http://adv-r.had.co.nz/Functional-programming.html
#'   - http://adv-r.had.co.nz/Functionals.html

set.seed(1133)
replicate(3, rnorm(10), simplify = FALSE)

set.seed(1133)
replicate(3, rnorm(10))

# Using a for loop

## Create an empty list
l1 <- vector(mode = "list", length = 3)

## Loop to add elements to the list
for(i in 1:3) {
  l1[[i]] <- rnorm(10)
}
l1

## Grow a matrix
mat <- NULL

for(i in 1:3) {
  mat <- cbind(mat, rnorm(10))
}
mat

#' ## Simulate many data sets

# Create a list of data sets

data_sets <- replicate(20, {
  data.frame(id = factor(1:15),
             group = rep(c("ctr", "trt1", "trt2"), times = 5),
             resp = rnorm(5 * 3,
                          mean = c(1, 3, 5), # three means
                          sd = 1))
                },
  simplify = FALSE
)

data_sets[[1]]

# Use apply functions to fit model to each data set

fit_model <- function(data) {
  lm(resp ~ group, data = data)
}

models <- lapply(data_sets, fit_model)  # create list of fitted models

summary(models[[1]])

pars <- sapply(models, coef)

rowMeans(pars)
boxplot(t(pars))

#' # Example: Type I error

lm0 <- lm(dist ~ 1, cars)       # H0
lm1 <- lm(dist ~ speed, cars)   # H1

nsim <- 1000
pval <- numeric(nsim)

for (i in 1:nsim) {
  sim <- simulate(lm0)$sim_1
  fit <- lm(sim ~ speed, cars)
  pval[i] <- summary(fit)$coef["speed", "Pr(>|t|)"]
}

# Type I error
mean(pval < 0.05)

#' ## Reference

