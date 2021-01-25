library(scales)

source("ggplot_mysoc_theme\\mysoc_theme.R")


# adapted to public square colours
ps_colors <- c("#cd5a62", "#e8a91b", "#007db3", "#17b12b")
# yellow was too bold, toned down a bit for this

chi_graph <- function(df, v1, v2, items, graph_title, col_order, order_by, colors = NULL) {
  if (is.null(colors)) {
    colors <- ps_colors
  }

  
  #create contrasting text colors, turned off for now
  text_colors = c()
  for (n in 1:length(colors)){
    if (colors[n] == "#e8a91b"){ # stupid yellow
      text_colors = c(text_colors, "#ffffff")
      #text_colors = c(text_colors, "#000000")
    } else {
      text_colors = c(text_colors, "#ffffff")
    }
  }
  
  
  # calculate the global picture for comparison
  average_row <- xtabs(df$weighted ~ df[[v1]])
  average_row <- (average_row / nrow(df)) * 100
  average_row <- as.data.frame(average_row)
  names(average_row) <- c("answer", "freq")
  average_row[[items]] <- "All"
  average_row$labeled_item <- paste("All Responses (", nrow(df), ")", sep = "")
  average_row$alpha <- TRUE
  average_row$chi <- 0
  average_row$text <- ""
  average_row$total_count <- 1000
  average_row$answer <- as.character(average_row$answer)
  average_row$overall <- TRUE

  # gather the table split by the described variable
  t <- xtabs(df$weighted ~ df[[v2]] + df[[v1]])
  pt <- as.data.frame(prop.table(t, 1))
  names(pt) <- c(items, "answer", "freq")
  pt$freq <- pt$freq * 100
  pt$answer <- as.character(pt$answer)

  # remap answers to descriptions given (assumes 0 = first)
  x <- 0
  for (c in col_order) {
    pt$answer[pt$answer == x] <- c
    average_row$answer[average_row$answer == x] <- c
    x <- x + 1
  }


  # adjust row labels to include counts
  options <- levels(pt[[items]])
  pt$labeled_item <- 0
  for (l in options) {
    count <- nrow(df[(df[[v2]] == l), ])
    pt$labeled_item[pt[[items]] == l] <- paste(l, " (", count, ")", sep = "")
    pt$total_count[pt[[items]] == l] <- count
  }

  # reorder factors by impossible score
  ppt <- pt[(pt$answer == order_by), ]
  ppt <- ppt[order(ppt$freq, decreasing = TRUE), ]
  order <- as.character(ppt$labeled_item)
  pt$labeled_item <- factor(pt$labeled_item, levels = unique(order))


  # run chisquare test
  chi <- chisq.test(t)
  chi_lookup <- as.data.frame(chi$stdres)

  allowed <- TRUE
  if (chi$p.value > 0.05) {
    allowed <- FALSE
    warning(chi$p.value, "is greater than 0.05 - overall not significant")
  }

  pt$chi <- chi_lookup$Freq
  if (allowed == FALSE) {
    pt$chi <- 0
  }
  pt$chi[pt$total_count < 3] <- 0 # artifical cut off of 3
  pt$alpha <- FALSE
  pt$alpha[pt$chi >= 2] <- TRUE
  pt$alpha[pt$chi <= -2] <- TRUE
  pt$text <- ""
  pt$text[pt$chi >= 2] <- "+"
  pt$text[pt$chi <= -2] <- "-"
  pt$text[pt$freq == 0] <- ""
  pt$overall <- FALSE
  pt$answer <- factor(pt$answer)
  pt$answer <- factor(pt$answer, levels = col_order)

  is_sig <- unique(pt[[items]][pt$alpha == TRUE])
  pt <- pt[(pt[[items]] %in% is_sig), ]


  pt <- rbind(pt, average_row)

  p_value <- round(chi$p.value, 3)
  if (p_value == 0) {
    p_value <- "<0.001"
  }

  pt$label_number <- paste0(round(pt$freq, 0), "%")
  pt$label_number[pt$text == "" & pt$overall == FALSE] <- ""
  pt$label_number[pt$text != ""] <- paste0(pt$label_number, "(", pt$text, ")")[pt$text != ""]
  pt$label_number[pt$freq < 11] <- pt$text[pt$freq < 11]



  lazy_percent <- function(x) {
    paste0(x, "%")
  }

  p <- ggplot(data = pt, aes(x = labeled_item, y = freq, fill = answer, label = label_number, color = NULL)) +
    geom_col(aes(color = NULL), position = position_stack(reverse = TRUE), width = 0.9) +
    scale_color_manual(values = colors, guide = "none") +
    coord_flip() +
    geom_text(aes(group = answer, color=answer),
      position = position_stack(vjust = .5, reverse = TRUE), size = 18
    ) +
    scale_colour_manual(values=text_colors, guide = "none") +
    scale_fill_manual(values = colors, guide = "none") +
    ggtitle(graph_title) +
    labs(y = "% of Respondents", x = "") +
    scale_y_continuous(labels = lazy_percent, limits = c(0, 101)) +
    labs(caption = paste0("Chi-square p-value: ", p_value)) +
    labs(fill = "") +
    theme_minimal() +
    publicsquare_theme() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.y = element_text(face = pt$format)
    ) +
    guides(fill = guide_legend(override.aes = list(size = 5))) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.margin = margin(c(0, 0, 0, 0)),
      legend.spacing.x = unit(0.03, "cm"),
      legend.text = element_text(size = standard_pt * ratio),
      legend.box.margin = margin(-5, -5, -10, -5)
    )

  # create table
  t <- pt[, (colnames(pt) %in% c(items, "answer", "freq"))]
  tt <- xtabs(t$freq ~ t[[items]] + t$answer)
  tt <- as.data.frame.matrix(tt)
  tt[[items]] <- row.names(tt)
  rownames(tt) <- c()
  names(tt) <- c(col_order, items)
  tt <- tt[c(items, col_order)]
  tt <- tt[(tt[[items]] %in% c(as.character(is_sig), "All")), ]

  # sort same as graph
  tt$sort <- tt[[order_by]]
  tt$sort[tt[[items]] == "All"] <- -10
  tt <- tt[order(tt$sort, decreasing = FALSE), ]
  tt <- subset(tt, select = -c(sort))

  # round and add in sign of difference
  for (o in col_order) {
    tt[[o]] <- round(tt[[o]], 2)
  }

  for (row in 1:nrow(pt)) {
    row_v <- as.character(pt[row, 1])
    col_v <- as.character(pt[row, 2])
    if (pt[row, "text"] != "") {
      sign <- pt[row, "text"]
      current <- tt[[col_v]][tt[[items]] == row_v]
      current <- paste(current, " (", sign, ")", sep = "")
      tt[[col_v]][tt[[items]] == row_v] <- current
    }
  }

  results <- list("table" = tt, "graph" = p, graph_data = pt, chi = chi)
  return(results)
}

save_graph <- function(results, name, height = 500) {
  ratio <- height / 700
  new_height <- 9.34 * ratio
  png_path <- paste("tables//", name, ".png", sep = "")
  table_path <- paste("tables//", name, ".csv", sep = "")
  save_and_show(results$graph, png_path, no_logo = TRUE)
  write.csv(results$table, file = table_path, row.names = FALSE)
}
