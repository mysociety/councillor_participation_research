# Does awareness change with previous exercise

source("core.R")
source("chi_graph.R")

options <- c(
  "quality",
  "transparency",
  "balanced",
  "numbers",
  "independent",
  "length"
)

col_names <- c("Type", "1", "2", "3", "4", "5,", "Average score")

final <- data.frame()

for (v in options) {
  v1 <- v

  df[[v1]] <- as.character(df[[v1]])

  df$v1_as_scale <- df[[v1]]
  df$v1_as_scale <- as.numeric(df$v1_as_scale)

  total <- nrow(df)

  one <- sum(df[(df[[v1]] == "1"), ]$weighted) / total
  two <- sum(df[(df[[v1]] == "2"), ]$weighted) / total
  three <- sum(df[(df[[v1]] == "3"), ]$weighted) / total
  four <- sum(df[(df[[v1]] == "4"), ]$weighted) / total
  five <- sum(df[(df[[v1]] == "5"), ]$weighted) / total

  one <- round(one * 100, 2)
  two <- round(two * 100, 2)
  three <- round(three * 100, 2)
  four <- round(four * 100, 2)
  five <- round(five * 100, 2)

  average_score <- weighted.mean(df$v1_as_scale, df$weighted)


  d <- data.frame(v, one, two, three, four, five, average_score)
  rownames(d) <- NULL
  names(d) <- col_names

  final <- rbind(final, d)
}

final$`Average score` <- round(final$`Average score`, 2)

final <- final[order(-final$`Average score`), ]

write.csv(final, file = "tables//importance_with_score.csv", row.names = FALSE)
final

final$score <- final[["Average score"]]

final$Type <- as.character(final$Type)

final$Type[final$Type == "quality"] <- "Discussion quality"
final$Type[final$Type == "transparency"] <- "Process transparency"
final$Type[final$Type == "length"] <- "Length of exercise"
final$Type[final$Type == "independent"] <- "Independent conveners"
final$Type[final$Type == "balanced"] <- "Demographically balanced"
final$Type[final$Type == "numbers"] <- "Number of participants"

g <- ggplot(data = final, aes(x = score, y = reorder(Type, score), label = round(score, 1))) +
  ggtitle("Elements of an exercise") +
  geom_col(fill = "#007db3") +
  geom_text(size = 20, nudge_x = -0.3, color = "white") +
  xlab("Average score (1-5)") +
  ylab("Which elements are more important?") +
  xlim(0, 5) +
  publicsquare_theme()

save_and_show(g, "outputs//importance.png", no_logo=TRUE)
