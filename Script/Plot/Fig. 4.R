library(data.table)
library(ggplot2)
library(igraph)
library(ggraph)
library(patchwork)
library(ggpubr)

base_dir <- "Insert-base-dir/path/Data/" # Insert path to "Data" folder

edge_tests <- fread(paste0(base_dir,"edge_tests.csv"))

graph_plot <- function(dt, title="Platform"){
  
  lev <- c("Anxiety", "Bipolar", "Depressive", "Dissociative", "Feed-Eat", "Neurodev", 
           "Obs-Comp", "Personality", "Schiz-Psych", "Sexual", "Sleep", "Subs-Add", "Trauma-Stress")
  
  labels <- c("ANX", "BIP", "DEP", "DIS", "FED", "NDV", "OCD", "PER", 
              "SCZ","SEX", "SLP", "SUB", "TSS")
  
  dt$Category <- factor(dt$Category, levels = lev, labels=labels)
  dt$next_Category <- factor(dt$next_Category, levels = lev, labels=labels)
  
  edges <- dt[significant==T, .(Category, next_Category)]
  names(edges)[1:2] <- c("from", "to")
  
  g <- graph_from_data_frame(edges, directed = T)
  
  indeg <- degree(g, mode = "in")
  indeg <- indeg/(length(indeg)-1)
  V(g)$indeg <- indeg
  
  gg <- ggraph(g, layout = "stress") +
    
  geom_edge_arc(
    arrow = arrow(
      length = unit(2.8, "mm"),
      type   = "closed",
      angle  = 28
    ),
    end_cap   = circle(5, "mm"),
    start_cap = circle(5, "mm"),
    colour = "#555555",
    width  = 0.35,
    alpha  = 0.75,
    strength = 0.12
  ) +
    
  geom_node_point(
    aes(fill = indeg),
    shape  = 21,
    size   = 14,       
    stroke = 0.8,
    colour = "black"
  ) +
    
  geom_node_text(
    aes(label = name),
    repel    = F,
    size     = 4,
    family   = "sans",
    fontface="bold"
  ) +
    
  scale_fill_gradientn(
    colors = c("#4575b4", "#fdae61", "#d73027"), 
    limits =c(0,1),
    name   = "Normalized \nIn-degree"
  ) +
    ggtitle(title)+
    theme_void() +
    theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold", margin = margin(b = 10)),
      legend.position = "right",
      legend.title = element_text(size = 14, face="bold", margin = margin(b = 14)),
      legend.text  = element_text(size = 12),
      text = element_text(family = "sans", color = "black"),
      plot.margin = margin(5, 5, 5, 5)
    )
}

g_FB <- graph_plot(edge_tests[Platform=="Facebook"], title="Facebook")
g_IG <- graph_plot(edge_tests[Platform=="Instagram"], title="Instagram (2021)")
g_reddit <- graph_plot(edge_tests[Platform=="Reddit"], title="Reddit")
g_YT <- graph_plot(edge_tests[Platform=="YouTube"], title="YouTube")
g_tumblr <- graph_plot(edge_tests[Platform=="Tumblr"], title="Tumblr")


lev <- unique(edge_tests[order(Category), Category])
labels <- c("ANX", "BIP", "DEP", "DIS", "FED", "NDV", "OCD", "PER", 
            "SCZ","SEX", "SLP", "SUB", "TSS")


l1 <- ggplot() + 
  annotate("text", x = -1, y = length(labels[1:7]):1, label = paste(labels[1:7], lev[1:7], sep=" - "), 
           hjust = 0, size = 4.5) + 
  theme_void()+  
  theme(plot.margin = margin(0, -100 , 100, -20)) 

l2 <- ggplot() + 
  annotate("text", x = 1, y = length(labels[1:7]):1, label = c(paste(labels[8:13], lev[8:13], sep=" - "), " "), 
           hjust = 0, size = 4.5) + 
  theme_void()+ 
  theme(plot.margin = margin(0, -50 , 100, -50))

leg <- cowplot::plot_grid(l1, l2, ncol = 2, rel_widths = c(1, 2.5))
leg <- annotate_figure(leg,  top=text_grob(" ", face="bold", hjust=0.5, size=16))
leg <- annotate_figure(leg,  top=text_grob("Categories", face="bold", hjust=0.3, size=16))
leg <- annotate_figure(leg,  top=text_grob(" ", face="bold", hjust=0.5, size=16))
leg <- annotate_figure(leg,  right=text_grob(" ", face="bold", hjust=0.5, size=16))

fill_leg <- cowplot::get_legend(g_YT + theme(legend.position = "right", 
                                             legend.title = element_text(size=16),
                                             legend.text = element_text(size=15),
                                             legend.margin = margin(0, 0 , 210, 140)))
fill_leg <- annotate_figure(fill_leg,  right=text_grob(" ", face="bold", hjust=0.5, size=16))
fill_leg <- annotate_figure(fill_leg,  right=text_grob(" ", face="bold", hjust=0.5, size=16))

side <- cowplot::plot_grid(fill_leg, leg, ncol = 2, rel_widths = c(1, 5))
side <- annotate_figure(side,  top=text_grob(" ", face="bold", hjust=0.5, size=16))


final_plot <- cowplot::plot_grid(g_FB + theme(legend.position = "none"),
                                 g_IG + theme(legend.position = "none"),
                                 side,
                                 g_reddit + theme(legend.position = "none"),
                                 g_tumblr + theme(legend.position = "none"),
                                 g_YT + theme(legend.position = "none"),
                                 ncol = 3,
                                 nrow=2,
                                 rel_widths = c(5, 5, 5, 5))

final_plot

ggsave("Path/Transition_graphs_2024.pdf", 
       final_plot, 
       device = "pdf", 
       width=43, 
       height =27,
       units = "cm")



