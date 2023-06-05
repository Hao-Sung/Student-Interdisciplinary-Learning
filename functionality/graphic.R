library(tidyverse)
library(htmlwidgets)

GRAPH_EXPORT_PATH = paste(c(".", "figs"), collapse = sep)

graph_export = function(graph, width, height, filename) {
  ggsave(filename,
         path = GRAPH_EXPORT_PATH,
         width = width,
         height = height)
}

plot_freq_dist = function(interdiscip, group_by_college = FALSE) {
  freq = interdiscip$freq
  if (group_by_college) {
    group_freq = freq %>%
      group_by(院名稱, 個人跨域次數) %>%
      count(name = "人數") %>%
      group_by(院名稱) %>%
      mutate(比例 = round(人數 / sum(人數), 3)) %>%
      filter(個人跨域次數 <= 20) %>%
      ungroup()
    p =  group_freq %>%
      ggplot() +
      geom_bar(aes(x = 個人跨域次數, y = 比例, fill = 比例), stat = "identity") +
      scale_fill_viridis_c(direction = -1) +
      facet_wrap(vars(院名稱), nrow = 3) +
      labs(title = "各學院 跨領域學習意願 比例分布", x = "學生跨域次數(四年內)", y = "比例") +
      theme_grey() +
      theme(strip.background = element_rect(colour = "white")) +
      theme(plot.title = element_text(size = 14, face = "bold"))
    p %>% graph_export(5, 5, "EDA_bar_bygroup.png")
    
  } else {
    freq = freq %>%
      group_by(個人跨域次數) %>%
      count(name = "人數") %>%
      ungroup() %>%
      mutate(比例 = round(人數 / sum(人數), 3)) %>%
      filter(個人跨域次數 <= 20)
    p = freq %>%
      ggplot() +
      geom_bar(aes(x = 個人跨域次數, y = 比例, fill = 比例), stat = "identity") +
      scale_fill_viridis_c(direction = -1) +
      labs(title = "跨領域學習意願 比例分布", x = "學生跨域次數(四年內)", y = "比例") +
      theme_grey() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    p %>% graph_export(4, 3, "EDA_bar.png")
  }
  return(p)
}


plot_inter_ratio = function(interdiscip, group_by_college = FALSE) {
  blank_theme = theme_grey() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 14, face = "bold")
    )
  
  freq = interdiscip$freq
  color = c("#B22222", "#6A5ACD")
  if (group_by_college) {
    group_freq = freq %>%
      group_by(院名稱, 是否跨域) %>%
      count(name = "人數") %>%
      group_by(院名稱) %>%
      arrange(人數) %>%
      mutate(比例 = round(人數 / sum(人數), 3)) %>%
      mutate(y_coors = 比例 / 2 + c(0, cumsum(比例)[-length(比例)])) %>%
      ungroup()
    p = group_freq %>%
      ggplot(aes(x = "", y = 比例, fill = 是否跨域)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      facet_wrap(vars(院名稱), nrow = 3) +
      blank_theme +
      scale_fill_manual(values = color) +
      geom_text(aes(
        y = 1 - y_coors,
        label = (比例 * 100) %>% paste0("%")
      ), size = 3) +
      ggtitle("各學院 跨領域學習意願 比例呈現")
    p %>% graph_export(5, 5, "EDA_pie_bygroup.png")
    
  } else {
    freq = freq %>%
      group_by(是否跨域) %>%
      count(name = "人數") %>%
      ungroup() %>%
      arrange(desc(人數)) %>%
      mutate(比例 = round(人數 / sum(人數), 3))
    p = freq  %>%
      ggplot(aes(x = "", y = 比例, fill = 是否跨域)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      blank_theme +
      geom_text(aes(
        y = 比例 / 2 + c(0, cumsum(比例)[-length(比例)]),
        label = (比例 * 100) %>% paste0("%")
      ), size = 5) +
      scale_fill_manual(values = color) +
      ggtitle("跨領域學習意願 比例呈現")
    p %>% graph_export(4, 3, "EDA_pie.png")
  }
  return(p)
}

plot_sankey = function(interdiscip) {
  data_long = interdiscip %>% flow_long_table() %>% as.data.frame()
  colnames(data_long) = c("source", "target", "value")
  data_long$target = paste(data_long$target, " ", sep = "")
  
  nodes =
    data.frame(name = c(
      as.character(data_long$source),
      as.character(data_long$target)
    ) %>% unique())
  
  data_long$IDsource = match(data_long$source, nodes$name) - 1
  data_long$IDtarget = match(data_long$target, nodes$name) - 1
  
  ColourScal = 'd3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
  sankey = sankeyNetwork(
    Links = data_long,
    Nodes = nodes,
    Source = "IDsource",
    Target = "IDtarget",
    Value = "value",
    NodeID = "name",
    units = "次",
    sinksRight = FALSE,
    colourScale = ColourScal,
    nodeWidth = 40,
    fontSize = 18,
    nodePadding = 20
  )
  saveWidget(sankey,
             file = paste(c(GRAPH_EXPORT_PATH, "sankey.html"), collapse = sep),
             title = "各學院 學生跨領域學習流向")
  
  return(sankey)
}


time_based_heatmap = function(time_interdiscip) {
  p = ggplot(time_interdiscip, aes(跨域程度, 院名稱, fill = 比例)) +
    geom_tile(color = "white", linewidth = 0.1) +
    scale_fill_viridis(name = "比例", option = "C") +
    facet_wrap(vars(修課學年), nrow = 1) +
    theme_minimal() +
    labs(title = "跨領域學習意向", x = "跨域程度", y = "學院名稱")
  
  p = p + theme(plot.title = element_text(size = 15)) +
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )) +
    theme(axis.text.y = element_text(size = 10)) +
    theme(plot.background = element_rect(fill = "white")) +
    theme(strip.background = element_rect(colour = "white")) +
    theme(plot.title = element_text(hjust = 0)) +
    theme(axis.ticks = element_blank()) +
    theme(axis.text = element_text(size = 7)) +
    theme(legend.title = element_text(size = 8)) +
    theme(legend.text = element_text(size = 8)) +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    removeGrid()
  p %>% graph_export(6, 4, "time_series_heatmap.png")
  
  return(p)
}
