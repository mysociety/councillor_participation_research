#checking education against party

library(scales)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

current <- dirname(rstudioapi::getActiveDocumentContext()$path)
target <- paste(current)
setwd(target)

source("core.R")

source("ggplot_mysoc_theme//mysoc_theme.R")
source("alt_plot_coefs.R")

options = c("Education",                        
            "Children.Social.Care",          
            "Adult.Social.Care",                                   
            "Public.Health",
            "Housing.Services",                                    
            "Cultural.Programmes",                                 
            "Environment",                                         
            "Planning",                                            
            "Transport")

col_names =  c("Type","Average score if no exercise or unsure","Average score if previous exercise","Difference","p value")

df$combo_app_score <- 0



for (v in options) {
  
  v1 = paste(v,".Which.areas.do.you.consider.citizen.participation.appropriate.for.Â.",sep="")
  
  df[[v1]] <- as.character(df[[v1]])
  

  df$v1_as_scale <- df[[v1]]
  df$v1_as_scale[df$v1_as_scale=="Appropriate"] <- 1
  df$v1_as_scale[df$v1_as_scale=="No opinion"] <- 0
  df$v1_as_scale[df$v1_as_scale=="Not appropriate"] <- -1
  
  df$v1_as_scale <- as.numeric(df$v1_as_scale)
  df[[v1]] <- df$v1_as_scale
  df$combo_app_score <- df$combo_app_score + df[[v1]] 
}

df$combo_app_score <- df$combo_app_score /9 

m <- lm(combo_app_score ~ dexercise + party_reduced,weights=weighted, data=df)

summary(m)

r2 <- round(summary(m)$adj.r.squared, 2)


coefs <- c(
  "Previous exercise" = "dexercise",
  "Green" = "party_reducedGRN",
  "Independent" = "party_reducedIND",
  "Labour" = "party_reducedLAB",
  "Liberal Democrats" = "party_reducedLD",
  "Plaid Cymru" = "party_reducedPC",
  "Scottish National Party" = "party_reducedSNP"
)

groups <- list(
  Experience = c(
    "Previous exercise"
  ),
  Party = c(
    "Green",
    "Independent",
    "Labour",
    "Liberal Democrats",
    "Plaid Cymru",
    "Scottish National Party"
))


colors <- c(mysoc_dark_grey, "#007db3")
g <- plot_summs(m, colors = colors, groups = groups, coefs = coefs, facet.label.pos = "left", exp=FALSE, ci_level=0.95) +
  ggtitle("Are participatory exercises appropriate?") +
  ylab("Linear regression (predicted change in recognition score") +
  xlab("Estimate and 95% confidence range") +
  publicsquare_theme() +
  labs(caption = paste0("Adjusted R-squared: ", r2))



save_and_show(g,  "outputs//approp_regression_plot.png", no_logo=TRUE)
