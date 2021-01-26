# graph of the difference according to t-test if previous exercise held

current <- dirname(rstudioapi::getActiveDocumentContext()$path)
target <- paste(current)
setwd(target)

source("core.R")
source("chi_graph.R")

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


wtd.t.test.with.paired.bounds <- function(x, y, weight, conf.level = 0.95) {
  # Upper and lower bounds on x and y are paired, meaning
  # a 0.05 p-value on the difference will result in bars that just
  # touch
  # The value of X or Y *can* be larger inside the given conf.level
  # but not while the other is *also* inside the given conf.level.
  new_x <- x[y == 0]
  new_y <- x[y == 1]
  weightx <- weight[y == 0]
  weighty <- weight[y == 1]
  test_results <- wtd.t.test(new_x, new_y, weight = weightx, weighty = weighty, bootse = TRUE)
  
  a <- 1 - (1 - conf.level)/2
  t_value <- qt(a, df=test_results$coefficients["df"])
  
  test_results$additional["diff"] <- test_results$additional["Difference"]
  diff <- test_results$additional["Difference"]
  margin <- t_value * test_results$additional["Std. Err"]
  test_results$additional["diff.lower"] <- diff - margin
  test_results$additional["diff.upper"] <- diff + margin
  
  split.margin <- margin/2
  test_results$additional["Mean.x.lower"] <- test_results$additional["Mean.x"] - split.margin
  test_results$additional["Mean.x.upper"] <- test_results$additional["Mean.x"] + split.margin
  test_results$additional["Mean.y.lower"] <- test_results$additional["Mean.y"] - split.margin
  test_results$additional["Mean.y.upper"] <- test_results$additional["Mean.y"] + split.margin  
  
  test_results
}

# convert results into a dataframe
test.to.df <- function(test_results, set_name, x_name, y_name, conf.level=0.95) {
  
  p_value = unname(test_results$coefficients["p.value"])
  passed_sig_test <- p_value < (1-conf.level)
  
  series <- c(set_name, set_name)
  name <- c(x_name, y_name)
  lower <- c(test_results$additional["Mean.x.lower"], test_results$additional["Mean.y.lower"])
  upper <- c(test_results$additional["Mean.x.upper"], test_results$additional["Mean.y.upper"])
  estimate <- c(test_results$additional["Mean.x"], test_results$additional["Mean.y"])
  origin <- c("x", "y")
  p_value <- c(p_value, p_value)
  sig_test <- c(passed_sig_test, passed_sig_test)
  df <- data.frame(series, name, lower, upper, estimate, origin, p_value, sig_test)
  rownames(df) <- NULL
  df
}

final <- data.frame()

for (v in options) {
  
  v1 = paste0(v,".Which.areas.do.you.consider.citizen.participation.appropriate.for.Â.")
  
  df[[v1]] <- as.character(df[[v1]])

  df$v1_as_scale <- df[[v1]]
  df$v1_as_scale[df$v1_as_scale=="Appropriate"] <- 1
  df$v1_as_scale[df$v1_as_scale=="No opinion"] <- 0
  df$v1_as_scale[df$v1_as_scale=="Not appropriate"] <- -1
  
  df$v1_as_scale <- as.numeric(df$v1_as_scale)
  
  v2 = "dexercise"
  
  test_results <- wtd.t.test.with.paired.bounds(df$v1_as_scale, df$dexercise, df$weighted, conf.level=0.95)
  
  test_df <- test.to.df(test_results,v, "No exercise", "Previous exercise", conf.level=0.95)
  
  final <- rbind(final, test_df)
}

df <- final

df$offset <- 0.5
df$nudge_x <- 0
df$nudge_y <- 0.25
df$nudge_y[df$origin == "y"] <- -0.25

df$color <- "#007db3"
df$color[df$lower < 0] <- mysoc_dark_grey

df$series <- gsub(".", " ", df$series, fixed = TRUE)

df <- df[(df$sig_test==TRUE),]

df$offset <- 0.5
df$nudge_x <- 0
df$nudge_y <- 0.4
df$nudge_y[df$origin == "y"] <- -0.4

g <- ggplot(df, aes(
  y = reorder(series, estimate), x = estimate, xmin = lower,
  xmax = upper, label = name, color = origin, group = NULL
)) +
  ggstance::geom_pointrangeh(
    fill = "white", fatten = 0.8, size = 0.8
  ) +
  geom_text(aes(hjust = offset), nudge_y = df$nudge_y, nudge_x = df$nudge_x, size = standard_pt - 4) +
  xlab("") +
  xlim(-0.4, 1.2) +
  ylab("Appropriateness scores of policy areas with difference (95% paired interval)") +
  ggtitle("Previous exercise and perceived appropriateness") +
  mysoc_theme(legend.position = "none") + 
  scale_colour_manual(values = c("#cd5a62", "#007db3"))

save_and_show(g, height=6, "outputs//appropriate_ttest.png")
