# graph of the difference according to t-test if previous exercise held

current <- dirname(rstudioapi::getActiveDocumentContext()$path)
target <- paste(current)
setwd(target)

source("core.R")
source("chi_graph.R")

options = c("Local.forums",
            "Citizens.assemblies",
            "Co.production.of.services",
            "Participatory.budgeting",
            "Civic.crowdfunding",
            "Citizen.juries")

col_names =  c("Type","Average score if no exercise or unsure","Average score if previous exercise","Difference","p value")

#convert to independent t test
ind.wtd.t.test <- function(x, y, weight) {
  new_x <- x[y == 1]
  new_y <- x[y == 0]
  weightx <- weight[y == 1]
  weighty <- weight[y == 0]
  wtd.t.test(new_x, new_y, weight = weightx, weighty = weighty)
}

significance_level = 1.96

#add confidence bounds to results
bound.wtd.t.test <- function(...) {
  test_results <- ind.wtd.t.test(...)
  test_results$additional["Mean.x.lower"] <- test_results$additional["Mean.x"] - significance_level * test_results$additional["Std. Err"]
  test_results$additional["Mean.x.upper"] <- test_results$additional["Mean.x"] + significance_level * test_results$additional["Std. Err"]
  test_results$additional["Mean.y.lower"] <- test_results$additional["Mean.y"] - significance_level * test_results$additional["Std. Err"]
  test_results$additional["Mean.y.upper"] <- test_results$additional["Mean.y"] + significance_level * test_results$additional["Std. Err"]
  test_results$additional["diff.lower"] <- test_results$additional["Difference"] - significance_level * test_results$additional["Std. Err"]
  test_results$additional["diff.upper"] <- test_results$additional["Difference"] + significance_level * test_results$additional["Std. Err"]
    test_results
}


# convert results into a dataframe
test.to.df <- function(test_results, set_name, x_name, y_name) {
  series <- c(set_name, set_name)
  name <- c(x_name, y_name)
  lower <- c(test_results$additional["Mean.x.lower"], test_results$additional["Mean.y.lower"])
  upper <- c(test_results$additional["Mean.x.upper"], test_results$additional["Mean.y.upper"])
  estimate <- c(test_results$additional["Mean.x"], test_results$additional["Mean.y"])
  origin <- c("x", "y")
  df <- data.frame(series, name, lower, upper, estimate, origin)
  rownames(df) <- NULL
  df
}

final <- data.frame()

for (v in options) {
  
  v1 = paste(v,".How.familiar.are.you.with.the.following.kinds.of.activities.",sep="")
  
  df[[v1]] <- as.character(df[[v1]])
  
  df[[v1]][df[[v1]]=="I know nothing about these kinds of activities"] <- "Know nothing"
  df[[v1]][df[[v1]]=="I know a little about these kinds of activities"] <- "Know a little"
  df[[v1]][df[[v1]]=="I know a lot about these kinds of activities"] <- "Know a lot"
  df$v1_as_scale <- df[[v1]]
  df$v1_as_scale[df$v1_as_scale=="Know nothing"] <- 0
  df$v1_as_scale[df$v1_as_scale=="Know a little"] <- 1
  df$v1_as_scale[df$v1_as_scale=="Know a lot"] <- 2
  
  df$v1_as_scale <- as.numeric(df$v1_as_scale)
  
  v2 = "dexercise"
  
  test_results <- bound.wtd.t.test(df$v1_as_scale, df$dexercise, df$weighted)
  
  test_df <- data.frame(series=v, estimate=test_results$additional["Difference"], lower = test_results$additional["diff.lower"], upper = test_results$additional["diff.upper"] )
  
  final <- rbind(final, test_df)
}

df <- final

df$color <- "#007db3"
df$color[df$lower <= 0.002] <- mysoc_dark_grey

df$series <- gsub(".", " ", df$series, fixed = TRUE)

g <- ggplot(df, aes(
  y = reorder(series, estimate), x = estimate, xmin = lower, color= color,
  xmax = upper, group = NULL
)) +
  ggstance::geom_pointrangeh(
    fill = "white", fatten = 0.8, size = 0.8
  ) +
  xlab("Change in awareness score") +
  ylab("Difference in awareness scores (95% confidence intervals)") +
  ggtitle("Previous exercise and perceived awareness") +
  #scale_x_continuous(label = percent, limits = 0:1) +
  publicsquare_theme(legend.position = "none") +
  scale_colour_manual(values = c("#007db3", mysoc_dark_grey))

save_and_show(g, height=6, "outputs//awareness_ttest.png", no_logo=TRUE)