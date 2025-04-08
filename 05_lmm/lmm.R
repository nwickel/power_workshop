#' ---
#' title: "Power for mixed-effects models"
#' author: ""
#' date: "Last modified: 2025-03-03"
#' bibliography: ../lit.bib
#' ---

library(lattice)
library(lme4)

#' # Reanalysis

#' ## Application context: Depression and type of diagnosis
#'
#' - @ReisbyGram77 studied the effect of Imipramin on 66 inpatients
#'   treated for depression
#' - Depression was measured with the Hamilton depression rating scale
#' - Patients were classified into endogenous and non-endogenous depressed
#' - Depression was measured weekly for 6 time points
#'
#' Data: [reisby.txt](../data/reisby.txt)

dat      <- read.table("../data/reisby.txt", header = TRUE)
dat$id   <- factor(dat$id)
dat$diag <- factor(dat$diag, levels = c("nonen", "endog"))
dat      <- na.omit(dat)     # drop missing values
head(dat, n = 13)

xyplot(hamd ~ week | id, data = dat, type=c("g", "r", "p"),
  pch = 16, layout = c(11, 6), ylab = "HDRS score", xlab = "Time (week)")


#' ## Random-intercept model
#'
#' $$
#' \begin{aligned}
#' Y_{ij} &= \beta_0 + \beta_1 \, \mathtt{week}_{ij}
#'         + \upsilon_{0i}
#'         + \varepsilon_{ij} \\
#' \upsilon_{0i} &\sim N(0, \sigma^2_{\upsilon_0}) \text{ i.i.d.} \\
#' \mathbf{\varepsilon}_i &\sim N(0, \, \sigma^2)  \text{ i.i.d.} \\
#' i &= 1, \ldots, I, \quad j = 1, \ldots n_i
#' \end{aligned}
#' $$

m1 <- lmer(hamd ~ week + (1 | id), data = dat, REML = FALSE)
summary(m1)

#' ## Random-slope model
#'
#' $$
#' \begin{aligned}
#' Y_{ij} &= \beta_0 + \beta_1 \, \mathtt{week}_{ij}
#'         + \upsilon_{0i} + \upsilon_{1i}\, \mathtt{week}_{ij}
#'         + \varepsilon_{ij} \\
#' \begin{pmatrix} \upsilon_{0i}\\ \upsilon_{1i} \end{pmatrix} &\sim
#'    N \left(\begin{pmatrix} 0\\ 0 \end{pmatrix}, \,
#'            \mathbf{\Sigma}_\upsilon =
#' \begin{pmatrix}
#'           \sigma^2_{\upsilon_0} & \sigma_{\upsilon_0 \upsilon_1} \\
#'           \sigma_{\upsilon_0 \upsilon_1} & \sigma^2_{\upsilon_1} \\
#' \end{pmatrix} \right)
#'       \text{ i.i.d.} \\
#' \mathbf{\varepsilon}_i &\sim N(\mathbf{0}, \, \sigma^2 \mathbf{I}_{n_i})
#'       \text{ i.i.d.} \\
#' i &= 1, \ldots, I, \quad j = 1, \ldots n_i
#' \end{aligned}
#' $$

m2 <- lmer(hamd ~ week + (week | id), data = dat, REML = FALSE)
summary(m2)

#' ## Partial pooling

#+ fig.height = 10, fig.width = 6.5, fig.aling = "center"
indiv <- unlist(
  sapply(unique(dat$id),
         function(i) predict(lm(hamd ~ week, dat[dat$id == i, ])))
)

xyplot(hamd + predict(m2, re.form = ~ 0) + predict(m2) + indiv ~ week | id,
  data = dat, type = c("p", "l", "l", "l"), pch = 16, grid = TRUE,
  distribute.type = TRUE, layout = c(11, 6), ylab = "HDRS score",
  xlab = "Time (week)",
  # customize colors
  col = c("#434F4F", "#3CB4DC", "#FF6900", "#78004B"),
  # add legend
  key = list(space = "top", columns = 3,
             text = list(c("Population", "Mixed model", "Within-subject")),
             lines = list(col = c("#3CB4DC", "#FF6900", "#78004B")))
  )

#' ## By-group random-slope model

m3 <- lmer(hamd ~ week + diag + (week | id), data = dat, REML = FALSE)
m4 <- lmer(hamd ~ week * diag + (week | id), data = dat, REML = FALSE)
anova(m3, m4)

#' ## Means and predicted HDRS score by group

dat2 <- aggregate(hamd ~ week + diag, dat, mean)
dat2$m4 <- predict(m4, newdata = dat2, re.form = ~ 0)

plot(m4 ~ week, dat2[dat2$diag == "endog", ], type = "l",
     ylim=c(0, 28), xlab="Week", ylab = "HDRS score")
lines(m4 ~ week, dat2[dat2$diag == "nonen", ], lty = 2)
points(hamd ~ week, dat2[dat2$diag == "endog", ], pch = 16)
points(hamd ~ week, dat2[dat2$diag == "nonen", ], pch = 21, bg = "white")
legend("topright", c("Endogenous", "Non endogenous"),
       lty = 1:2, pch = c(16, 21), pt.bg = "white", bty = "n")

#' ## Gory details

fixef(m4)
getME(m4, "theta")
t(chol(VarCorr(m4)$id))[lower.tri(diag(2), diag = TRUE)] / sigma(m4)

#' # Power simulation

#' ## Setup

## Study design and sample sizes
n_week <- 6
n_subj <- 80
n <- n_week * n_subj
dat <- data.frame(
     id = rep(seq_len(n_subj), each = n_week),
   week = rep(0:(n_week - 1), times = n_subj),
  treat = rep(0:1, each = n/2)
)

## Fixed effects and variance components
beta <- c("(Intercept)" = 23, week = -0.5, treat = 0, "week:treat" = -1)
s <- 3.5                # residual sd
r <- -0.3
t(chol(VarCorr(m4)$id))[lower.tri(diag(2), diag = TRUE)] / sigma(m3)
# su <- c(3.5, 1.5)
# Su <- r * su %o% su
# diag(Su) <- su^2        # covariance matrix of random effects

su1 <- 3.5
su2 <- 1.5
Su <- matrix(c(su1^2, r * su1 * su2, r * su1 * su2, su2^2), nrow = 2, ncol = 2)

Lt <- chol(Su) / s
pars <- list(theta = t(Lt)[lower.tri(Lt, TRUE)],
             beta = beta, sigma = s)
names(pars$theta) <- c("id.(Intercept)", "id.week.(Intercept)", "id.week")

#' ## Power

#+ cache = TRUE, warning = FALSE
pval <- replicate(200, {
  y <- simulate(~ week * treat + (week | id),
                newparams = pars, newdata = dat)$sim_1
  m1 <- lmer(y ~ week + treat + (week | id), data = dat, REML = FALSE)
  m2 <- lmer(y ~ week * treat + (week | id), data = dat, REML = FALSE)
  anova(m1, m2)$"Pr(>Chisq)"[2]
})
mean(pval < 0.05)

#' ## Parameter recovery

y <- simulate(~ week * treat + (week | id),
              newparams = pars, newdata = dat)$sim_1
m <- lmer(y ~ week * treat + (week | id), data = dat, REML = FALSE)
fixef(m)
VarCorr(m)

#' ### References

