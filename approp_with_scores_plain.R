#Does awareness change with previous exercise

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

col_names =  c("Type","Not appropriate (-1)","No opinion (0)", "Appropriate (1)", "Average score")

final <- data.frame()

for (v in options) {
  
  v1 = paste(v,".Which.areas.do.you.consider.citizen.participation.appropriate.for.Â.",sep="")
  
  df[[v1]] <- as.character(df[[v1]])
  
  df$v1_as_scale <- df[[v1]]
  df$v1_as_scale[df$v1_as_scale=="Appropriate"] <- 1
  df$v1_as_scale[df$v1_as_scale=="No opinion"] <- 0
  df$v1_as_scale[df$v1_as_scale=="Not appropriate"] <- -1
  
  df$v1_as_scale <- as.numeric(df$v1_as_scale)

  total = nrow(df)
  
  know_nothing <- sum(df[(df[[v1]]=="Not appropriate"),]$weighted) /total
  know_little <- sum(df[(df[[v1]]=="No opinion"),]$weighted) /total
  know_lot <- sum(df[(df[[v1]]=="Appropriate"),]$weighted) /total
  
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

final <- final[order(-final$`Average score`),]

write.csv(final, file = "tables//appropriate_with_score.csv", row.names = FALSE)
final

final$score <- final[["Average score"]]

final$Type <- as.character(final$Type)

final$Type[final$Type == "Adult.Social.Care"] <- "Adult Social Care"
final$Type[final$Type == "Children.Social.Care"] <- "Children Social Care"
final$Type[final$Type == "Cultural.Programmes"] <- "Cultural Programmes"
final$Type[final$Type == "Housing.Services"] <- "Housing Services"
final$Type[final$Type == "Public.Health"] <- "Public Health"

g <- ggplot(data = final, aes(x = score, y = reorder(Type, score), label = round(score, 1))) +
  ggtitle("Appropriateness of an exercise") +
  geom_col(fill = "#007db3") +
  geom_text(size = 20, nudge_x = -0.05, color = "white") +
  xlab("Average score (-1 to 1)") +
  ylab("What kind of exercises are on average more appropriate?") +
  xlim(0, 1) +
  publicsquare_theme()

save_and_show(g, "outputs//appropriate.png", no_logo=TRUE)
