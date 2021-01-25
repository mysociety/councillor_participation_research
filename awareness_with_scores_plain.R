#Does awareness change with previous exercise

source("core.R")
source("chi_graph.R")

options = c("Local.forums",
            "Citizens.assemblies",
            "Co.production.of.services",
            "Participatory.budgeting",
            "Civic.crowdfunding",
            "Citizen.juries")

col_names =  c("Type","Know nothing","know a little", "Know a lot", "Average score")

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

  total = nrow(df)
  
  know_nothing <- sum(df[(df[[v1]]=="Know nothing"),]$weighted) /total
  know_little <- sum(df[(df[[v1]]=="Know a little"),]$weighted) /total
  know_lot <- sum(df[(df[[v1]]=="Know a lot"),]$weighted) /total
  
  know_nothing = round(know_nothing * 100,2)
  know_little = round(know_little * 100,2)
  know_lot = round(know_lot * 100,2)
  
  average_score <- weighted.mean(df$v1_as_scale, df$weighted)
  
  
  d <- data.frame(v,know_nothing, know_little, know_lot, average_score)
  rownames(d) <- NULL
  names(d) <- col_names
  
  final <- rbind(final, d)

}

final$`Average score` = round(final$`Average score`,2)
write.csv(final, file = "tables//awareness_with_score.csv", row.names = FALSE)


final$score <- final[["Average score"]]

final$Type <- as.character(final$Type)
final$Type <- gsub("[.]"," ", final$Type)

g <- ggplot(data = final, aes(x = score, y = reorder(Type, score), label = round(score, 1))) +
  ggtitle("Awareness of types of participation") +
  geom_col(fill = "#007db3") +
  geom_text(size = 20, nudge_x = -0.1, color = "white") +
  xlab("Average score (0 to 2)") +
  ylab("What kind of exercises were on average more known?") +
  xlim(0, 2) +
  publicsquare_theme()

save_and_show(g, "outputs//awareness_score.png", no_logo=TRUE)

