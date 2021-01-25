#checking party against authoriative

source("core.R")
source("chi_graph.R")

v1 = "consult_scale"
v2 = "party_reduced"


items = "Party"
graph_title = "Participatory exercises should be..."
col_order <- c("Authoritative",
               "Authoritative leaning",
               "Consultative leaning",
               "Consultative")
order_by = "Consultative"

results <- chi_graph(df, v1,v2,items,graph_title,col_order,order_by)

save_graph(results,"auth_party", height=300)

