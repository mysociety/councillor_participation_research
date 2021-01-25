#checking party against being supportive of leader

current = dirname(rstudioapi::getActiveDocumentContext()$path)
target = paste(current)
setwd(target)

source("core.R")
source("chi_graph.R")

v1 = "cleadership"
v2 = "party_reduced"
items = "Party"
graph_title = "Support process if run by current leader of council"
col_order <- c("No",
               "Yes")
order_by = "Yes"
results <- chi_graph(df,v1,v2,items,graph_title,col_order,order_by)

results$graph

#checking if part of majority against supportive of leader

df$pmajority[df$pmajority==0] <- "No"
df$pmajority[df$pmajority==1] <- "Yes"

v1 = "cleadership"
v2 = "pmajority"
items = "Party"
graph_title = "Support process if run by current leader of council"
col_order <- c("No",
               "Yes")
order_by = "Yes"
results <- chi_graph(df, v1,v2,items,graph_title,col_order,order_by)

