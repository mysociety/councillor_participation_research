#checking majority against majority

source("core.R")
source("chi_graph.R")
v1 = "conflict_perfer_own"
v2 = "council_has_majority"   

df$soverlap[df$soverlap==1] <- "Yes"
df$soverlap[df$soverlap==0] <- "No"
df$cleadership[df$cleadership==1] <- "Yes"
df$cleadership[df$cleadership==0] <- "No"

df$part_of_majority[df$part_of_majority==TRUE] <- "Governing"
df$part_of_majority[df$part_of_majority==FALSE] <- "Opposition"
df$council_has_majority[df$council_has_majority==TRUE] <- "One party majority"
df$council_has_majority[df$council_has_majority==FALSE] <- "No overall control"

df$council_type <- as.character(df$council_type)
df$council_type[df$council_type=="SCT"] <- "Scottish Authority"
df$council_type[df$council_type=="LBO"] <- "London Borough"

items = "Party"
graph_title = "In the event of a conflict, which has more weight?"
col_order <- c("Own views",
               "Exercise results")
order_by = "Own views"

results <- chi_graph(df, v1,v2,items,graph_title,col_order,order_by, colors=c("#e8a91b", "#007db3"))

save_graph(results,"ownviews_majority", height=200)


