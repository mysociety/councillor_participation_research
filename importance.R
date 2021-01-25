#checking importance of balanced against a linear regression of previous exercise, party, and council status

library(scales)
library(sjPlot)
library(sjmisc)
library(sjlabelled)


source("core.R")
source("ggplot_mysoc_theme//mysoc_theme.R")
source("alt_plot_coefs.R")


m <- lm(balanced ~ dexercise + party_reduced + phasmajority + pmajority, weights = df$weighted, data=df)
summary(m)
r2 <- round(summary(m)$adj.r.squared, 2)


coefs <- c(
  "Previous exercise" = "dexercise",
  "Council has majority" = "phasmajority",
  "Part of majority" = "pmajority",
  "Green" = "party_reducedGRN",
  "Independent" = "party_reducedIND",
  "Labour" = "party_reducedLAB",
  "Liberal Democrats" = "party_reducedLD",
  "Plaid Cymru" = "party_reducedPC",
  "Scottish National Party" = "party_reducedSNP"
)

groups <- list(
  Experience = c(
    "Previous exercise",
    "Council has majority",
    "Part of majority"
  ),
  Party = c(
    "Green",
    "Independent",
    "Labour",
    "Liberal Democrats",
    "Plaid Cymru",
    "Scottish National Party"
  ))


colors <- c("#cd5a62", mysoc_dark_grey, "#007db3")
g <- plot_summs(m, colors = colors, coefs = coefs, facet.label.pos = "left", exp=FALSE, ci_level=0.95) +
  ggtitle("Are demographically balanced exercises important?") +
  ylab("Linear regression (predicted change in importance score)") +
  xlab("Estimate and 95% confidence range") +
  publicsquare_theme() +
labs(caption = paste0("Adjusted R-squared: ", r2))

save_and_show(g,  "outputs//demo_regression_plot.png", no_logo=TRUE)
