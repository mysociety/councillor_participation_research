#checking education against party

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

#convert to independent t test
ind.wtd.t.test <- function(x, y, weight) {
  new_x <- x[y == 1]
  new_y <- x[y == 0]
  weightx <- weight[y == 1]
  weighty <- weight[y == 0]
  wtd.t.test(new_x, new_y, weight = weightx, weighty = weighty)
}

significance_level = 2.58

#add confidence bounds to results
bound.wtd.t.test <- function(...) {
  test_results <- ind.wtd.t.test(...)
  test_results$additional["Mean.x.lower"] <- test_results$additional["Mean.x"] - significance_level * test_results$additional["Std. Err"]
  test_results$additional["Mean.x.upper"] <- test_results$additional["Mean.x"] + significance_level * test_results$additional["Std. Err"]
  test_results$additional["Mean.y.lower"] <- test_results$additional["Mean.y"] - significance_level * test_results$additional["Std. Err"]
  test_results$additional["Mean.y.upper"] <- test_results$additional["Mean.y"] + significance_level * test_results$additional["Std. Err"]
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
  
  v1 = paste(v,".Which.areas.do.you.consider.citizen.participation.appropriate.for.Â.",sep="")
  
  df[[v1]] <- as.character(df[[v1]])

  df$v1_as_scale <- df[[v1]]
  df$v1_as_scale[df$v1_as_scale=="Appropriate"] <- 1
  df$v1_as_scale[df$v1_as_scale=="No opinion"] <- 0
  df$v1_as_scale[df$v1_as_scale=="Not appropriate"] <- -1
  
  df$v1_as_scale <- as.numeric(df$v1_as_scale)
  
  v2 = "dexercise"
  
  w <- ind.wtd.t.test(df$v1_as_scale,df$dexercise, df$weighted)
  
  p_value <- w$coefficients["p.value"]
  mean.x <- w$additional["Mean.x"]
  mean.y <- w$additional["Mean.y"]
  diff <- mean.y - mean.x
  names(diff) <- c("Diff")
  
  d <- data.frame(v,mean.x,mean.y,diff, p_value)
  rownames(d) <- NULL
  names(d) <- col_names
  
  final <- rbind(final, d)

}

final$`Average score if no exercise or unsure` = round(final$`Average score if no exercise or unsure`,2)
final$`Average score if previous exercise` = round(final$`Average score if previous exercise`,2)
final$`Difference` = round(final$`Difference`,2)
final$`p value` = round(final$`p value`,4)
final
write.csv(final, file = "tables//appropriate_with_score_previous_ex.csv", row.names = FALSE)

