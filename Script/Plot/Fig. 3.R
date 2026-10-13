library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggpubr)
library(arrow)
library(igraph)
library(coop)
library(caTools)
library(viridis)
library(Kendall)
library(trend)

color_scale <- c("#1877f2", # Facebook
                 "#ff00dd", # Instagram
                 "#FF8600", # Reddit
                 "#2a445f", # Tumblr
                 "#DD0000") # YouTube

base_dir <- "Insert-base-dir/path/Similarity/" # Insert path to "Similarity" folder

year_similarity_lag <- fread(paste0(base_dir,  "year_similarity_lag.csv"))

platform_similarity_meta <- fread(paste0(base_dir,  "platform_similarity_meta.csv"))
platform_similarity_full <- fread(paste0(base_dir,  "platform_similarity_full.csv"))
platform_similarity <- rbind(platform_similarity_meta, platform_similarity_full)

platform_similarity <- platform_similarity[, comb := NULL]
platform_similarity <- rbind(platform_similarity, platform_similarity[, .(year, cos_sim, Platform_1 = Platform_2, Platform_2 = Platform_1)])
platform_similarity <- unique(platform_similarity)


year_similarity_lag$type <- "Intra-Platform"
names(platform_similarity)[3:4] <- c("type", "Platform")

df_similarity_lag <- rbind(platform_similarity, 
                           year_similarity_lag[Platform=="Instagram"&year_2==2021|year_2==2024][, .(year, cos_sim, type, Platform)])


df_similarity_lag$type <- factor(df_similarity_lag$type, 
                                 levels = c("Intra-Platform", "Facebook", "Instagram",
                                            "Reddit", "Tumblr", "YouTube"))

g <- df_similarity_lag %>% 
  ggplot(aes(x=year, y=cos_sim, group = Platform))+
  geom_line(aes(color = Platform), lwd=1.5, alpha = 0.6) +
  geom_point(aes(fill = Platform), size = 3, shape = 21, stroke = .8, alpha = .8) +
  facet_wrap(~type, nrow=2, scales = "fixed")+
  scale_x_continuous(breaks = seq(2012, 2024, 1), labels = c(rbind(seq(2012, 2024, by = 2), ""))[1:13])+
  scale_y_continuous(limits = c(0, 1))+
  ylab("Transition Similarity")+
  xlab("Year")+
  labs(color="Comparison", fill="Comparison")+
  scale_color_manual(values = color_scale)+
  scale_fill_manual(values = color_scale)+
  theme_bw()+
  theme(
    text = element_text(face = "bold", size = 15),
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 17, hjust = 0.5, face = "italic"),
    legend.title = element_text(size = 17, face = "bold"),
    legend.text = element_text(size = 16, face = "plain"),
    legend.background = element_rect(color = "black"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold", vjust=2),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12, color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    aspect.ratio = 1,
    legend.position = "bottom")

g


ggsave("Path/Platforms_Similarity.pdf", 
       g, 
       device = "pdf", 
       width=34, 
       height =24,
       units = "cm")

  
##### SI PLOT ####

year_similarity_meta <- fread(paste0(base_dir,  "year_similarity_meta.csv"))
year_similarity_full <- fread(paste0(base_dir,  "year_similarity_full.csv"))
year_similarity <- rbind(year_similarity_meta, year_similarity_full)

g1 <- year_similarity %>% 
  ggplot(aes(x=year, y=cos_sim, group = Platform))+
  geom_line(aes(color = Platform), lwd=1.5, alpha = 0.6) +
  geom_point(aes(fill = Platform), size = 3, shape = 21, stroke = .8, alpha = .8) +
  scale_x_continuous(breaks = seq(2012, 2023, 1), labels = c(rbind(seq(2012, 2022, by = 2), "")))+
  scale_y_continuous(limits = c(0, 1))+
  ylab("Year on year \nIntra-platform Similarity")+
  xlab("Year")+
  labs(color="Platform", fill="Platform")+
  scale_color_manual(values = color_scale)+
  scale_fill_manual(values = color_scale)+
  theme_bw()+
  theme(
    text = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 17, hjust = 0.5, face = "italic"),
    legend.title = element_text(size = 16, face = "bold", hjust=0.5),
    legend.text = element_text(size = 16, face = "plain"),
    legend.background = element_rect(color = "black"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12, color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    axis.ticks.length = unit(0.2, "cm"), 
    aspect.ratio = 1,
    legend.position = c(0.8, 0.25))


g2 <- ggplot(year_similarity_lag, aes(x=year, y=year_2, fill=cos_sim))+
  geom_tile()+
  facet_wrap(~Platform, nrow=2)+
  scale_fill_gradient2(low="#0B004A", mid = "#F2F2F5", high="#FF0000", 
                       midpoint = 0.6, limits=c(0.2,1))+
  scale_y_continuous(breaks = seq(2013, 2024, 1))+
  scale_x_continuous(breaks = seq(2012, 2023, 1), labels = c(rbind(paste0("'", seq(12, 22, by = 2)), "")))+
  labs(fill="Intra-platform \nSimilarity", x="Year", y="Year")+
  theme_bw()+
  theme(
    text = element_text(face = "bold", size = 16),
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 17, hjust = 0.5, face = "italic"),
    legend.title = element_text(size = 16, face = "bold", vjust=2),
    legend.text = element_text(size = 14, face = "plain"),
    legend.background = element_rect(color = "black"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12, color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    axis.ticks.length = unit(0.2, "cm"), 
    aspect.ratio = 1,
    legend.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
    legend.position = c(0.85, 0.225))

g2

g <- ggarrange(g1, g2, ncol = 2, labels ="auto", 
               font.label = list(size = 17, color = "black", face = "plain"),
               widths = c(1, 1.2))

g


ggsave("Path/Intra_Platform_SI.pdf", 
       g, 
       device = "pdf", 
       width=38, 
       height =16,
       units = "cm")
