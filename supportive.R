#checking party against authoriative

source("core.R")
source("chi_graph.R")

v1 = "cleadership"                                                                                                           
v2 = "part_of_majority"                                                                                                         


df$cleadership[df$cleadership==1] <- "Yes"
df$cleadership[df$cleadership==0] <- "No"

df$part_of_majority[df$part_of_majority==TRUE] <- "Governing"
df$part_of_majority[df$part_of_majority==FALSE] <- "Opposition"


items = "Party"
graph_title = "Support participatory process by council leadership"
col_order <- c("Yes",
               "No")
order_by = "Yes"

results <- chi_graph(df, v1,v2,items,graph_title,col_order,order_by, colors=c("#e8a91b", "#cd5a62"))


save_graph(results,"cleadership_majority")


