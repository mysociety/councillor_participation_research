#Does awareness change with previous exercise

source("core.R")
source("chi_graph.R")

options = c("Local.forums",
            "Citizens.assemblies",
            "Co.production.of.services",
            "Participatory.budgeting",
            "Civic.crowdfunding",
            "Citizen.juries")

col_names =  c("Type","Average score if no exercise or unsure","Average score if previous exercise","Difference","p value")

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
  
  w <- weighted_t_test(df$v1_as_scale,df$dexercise, df$weighted)
  
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
write.csv(final, file = "tables//awareness_with_score_previous_ex.csv", row.names = FALSE)

