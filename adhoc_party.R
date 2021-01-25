#checking party against temporary/permanent view

source("core.R")
source("chi_graph.R")

v1 = "adhoc_scale"
v2 = "party_reduced"


items = "Party"
graph_title = "Participatory exercises should be..."
col_order <- c("Permanent",
               "Permanent leaning",
               "Ad hoc leaning",
               "Ad hoc")
order_by = "Ad hoc"

results <- chi_graph(df, v1,v2,items,graph_title,col_order,order_by)

save_graph(results,"adhoc_party", height=300)


