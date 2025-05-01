#' ---
#' title: "Two-by-two ANOVA and t-tests"
#' author: ""
#' date: "Last modified: 2025-05-01"
#' ---

#' # Univariate t-test

#' ## Application context

#'
#' Listening experiment
#'
#' - Task of each participant is to repeatedly adjust the frequency of a
#'   comparison tone to sound equal in pitch to a 1000-Hz standard tone
#' - Mean adjustment estimates the point of subjective equality $\mu$
#' - Two participants will take part in the experiment providing adjustments
#'   $X$ and $Y$
#' - Goal is to detect a difference between their points of subjective
#'   equality $\mu_x$ and $\mu_y$ of 4 Hz
#'

#' ## Model

#'
#' Assumptions
#' 
#' - $X_1, \ldots, X_n \sim N(\mu_x, \sigma_x^2)$ i.i.d.
#' - $Y_1, \ldots, Y_m \sim N(\mu_y, \sigma_y^2)$ i.i.d.
#' - both samples independent
#' - $\sigma_x^2 = \sigma_y^2$ but unknown
#' 
#' Hypothesis
#' 
#' - H$_0\colon~ \mu_x - \mu_y = \delta = 0$
#'

#' ## Power simulation

#+ cache = TRUE
n <- 110; m <- 110
pval <- replicate(2000, {
  x <- rnorm(n, mean = 1000 + 4, sd = 10)         # Participant 1 responses
  y <- rnorm(m, mean = 1000,     sd = 10)         # Participant 2 responses
  t.test(x, y, mu = 0, var.equal = TRUE)$p.value
})
mean(pval < 0.05)

#' ## Power curves

#'
#' Turn into a function of n and effect size

pwrFun <- function(n = 30, d = 4, sd = 10,
                   nrep = 50) {
  n <- n; m <- n
  pval <- replicate(nrep, {
    x <- rnorm(n, mean = 1000 + d, sd = sd)
    y <- rnorm(m, mean = 1000,     sd = sd)
    t.test(x, y, mu = 0, var.equal = TRUE)$p.value
  })
  mean(pval < 0.05)
}

#'
#' Set up conditions and call power function

#+ cache = TRUE
cond <- expand.grid(d = 0:5,
                    n = c(50, 75, 100, 125))
system.time(
  cond$pwr <- mapply(pwrFun, n = cond$n, d = cond$d,
                     MoreArgs = list(nrep = 500))
)

## Plot results
lattice::xyplot(pwr ~ d, cond, groups = n, type = c("g", "b"),
                auto.key = list(corner = c(0, 1)))

#' # ANOVA

#' ## Application context

#'
#' Effect of fertilizers
#'
#' In an experiment, two fertilizers (A and B, each either low or high dose)
#' will be combined and the yield of peas (Y) in kg be observed. Goal is to
#' detect an increase of the Fertilizer-A effect by an additional 12 kg when
#' combined with a high dose of Fertilizer B (interaction effect).
#'

#+ echo = FALSE
dat <- data.frame(
  A = rep(1:2, each = 2),
  B = rep(1:2, times = 2),
  y = c(30, 30 + 5, 30 + 30, 30 + 30 + 5 + 12)
)
par(mai = c(.6, .6, .1, .1), mgp = c(2, .7, 0))
plot(y ~ A, dat, type = "n", xlim = c(0.8, 2.2), ylim = c(20, 80),
     xlab = "Fertilizer A", ylab = "Yield (kg)", xaxt = "n")
lines(y ~ A, dat[dat$B == 1, ], col = "darkblue")
lines(y ~ A, dat[dat$B == 2, ], col = "darkblue")
lines(1:2, c(30 + 5, 30 + 30 + 5), lty = 2, col = "darkblue")
axis(1, 1:2, c("low", "high"))
arrows(2, 30 + 30 + 6, 2, 30 + 30 + 5 + 11, code = 3, length = 0.1,
       col = "darkgray")
text(c(1, 2, 1, 2, 2.07), c(27, 55, 40, 80, 65 + 6),
     c(expression(mu), expression(mu + alpha[2]),
       expression(mu + beta[2]),
       expression(mu + alpha[2] + beta[2] + (alpha * beta)[22]),
       "12 kg")
)
text(1.5, 27 + 30/2, "Fertilizer B: low", srt = 21, col = "darkgray")
text(1.5, 38 + (30 + 12)/2, "Fertilizer B: high", srt = 32, col = "darkgray")

#' ## Model

#'
#' Assumptions
#' 
#' - $Y_{ijk} = \mu + \alpha_i + \beta_j + (\alpha\beta)_{ij} +
#'              \varepsilon_{ijk}$
#' - $\varepsilon_{ijk} \sim N(0, \sigma^2) \text{ i.i.d.}$
#' - $i = 1, \dots, I$; $j = 1, \dots, J$; $k = 1, \dots, K$
#' - $\alpha_1 = \beta_1 := 0$
#' 
#' Hypothesis
#' 
#' - H$_0^{AB}\colon~ (\alpha\beta)_{ij} = 0 \text{ for all } i,j$
#' 

#' ## Setup

set.seed(1704)
n <- 96
dat <- data.frame(
  A = factor(rep(1:2, each = n/2), labels = c("low", "high")),
  B = factor(rep(rep(1:2, each = n/4), times = 2), labels = c("low", "high"))
)
X <- model.matrix(~ A * B, dat)
unique(X)
beta <- c(mu = 30, a2 = 30, b2 = 5, ab22 = 12)
means <- X %*% beta

lattice::xyplot(I(means + rnorm(n, sd = 10)) ~ A, dat, groups = B,
                type = c("g", "p", "a"), auto.key = TRUE, ylab = "Yield (kg)")

#' ## Parameter recovery

#+ cache = TRUE
out <- replicate(2000, {
  y <- means + rnorm(n, sd = 10)   # y = mu + a + b + ab + e
  m <- aov(y ~ A * B, dat)
  c(coef(m), sigma = sigma(m))
})
boxplot(t(out))

#' ## Power simulation

#+ cache = TRUE
pval <- replicate(2000, {
  y <- means + rnorm(n, sd = 10)
  m <- aov(y ~ A * B, dat)
  summary(m)[[1]]$"Pr(>F)"[3]      # test of interaction
})
mean(pval < 0.05)

