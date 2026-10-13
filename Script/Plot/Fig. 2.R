library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggpubr)
library(arrow)
library(igraph)
library(coop)
library(fastmatrix)
library(ggraph)
library(grid)
library(showtext)
library(patchwork)
library(cowplot)

color_scale <- c("#1877f2", # Facebook
                 "#ff00dd", # Instagram
                 "#FF8600", # Reddit
                 "#2a445f", # Tumblr
                 "#DD0000") # YouTube

base_dir <- "Insert-base-dir/path/Data/" # Insert path to "Data" folder

ari <- fread(paste0(base_dir,  "ari_multiple_seeds.csv"))
ari <- ari[year>=2012]

# Panel a
freq_FB <- fread(paste0(base_dir,  "Comment_Freq/FB_comm_freq.csv"))
freq_IG <- fread(paste0(base_dir,  "Comment_Freq/IG_comm_freq.csv"))
freq_reddit <- fread(paste0(base_dir,  "Comment_Freq/reddit_comm_freq.csv"))
freq_YT <- fread(paste0(base_dir,  "Comment_Freq/YT_comm_freq.csv"))
freq_tumblr <- fread(paste0(base_dir,  "Comment_Freq/Tumblr_comm_freq.csv"))

freq_FB$platform <- "Facebook"
freq_IG$platform <- "Instagram"
freq_reddit$platform <- "Reddit"
freq_YT$platform <- "YouTube"
freq_tumblr$platform <- "Tumblr"

freq <- rbind(freq_FB, freq_IG, freq_reddit, freq_YT, freq_tumblr)
setDT(freq)

freq_binned <- freq[Categories>9, .(sum(Users), sum(p)), by=platform]
freq_binned$Categories <- 10
names(freq_binned)[2:3] <- c("Users", "p")
freq_binned <- freq_binned[, .(Categories, Users, p, platform)]

freq_binned <- rbind(freq[Categories<10], freq_binned)

g1 <- freq_binned %>% filter(Categories>1) %>%
  ggplot(aes(x = Categories, 
             y = p, 
             fill=platform,
             group=Categories)) +
  geom_bar(stat = "identity", 
           position = "dodge2",
           color="black",
           alpha = 1) +           
  labs(x = "Categories per user",
       y = "Fraction", 
       fill="Platform") +
  scale_fill_manual(values = color_scale)+
  scale_y_continuous()+
  scale_x_continuous(breaks=seq(2, 10, 1), 
                     labels = c(seq(2, 9, 1), "10+"))+
  theme_bw() +
  theme(text = element_text(face = "bold", size = 16),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 17, hjust = 0.5, face = "italic"),
        axis.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        legend.title = element_text(size = 17, face = "bold"),
        legend.text = element_text(size = 16, face = "plain"),
        legend.background = element_rect(color = "black"),
        axis.text = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 12, color = "black"),
        strip.background = element_rect(fill = "white", color = "black"),
        axis.ticks.length = unit(0.2, "cm"), 
        legend.position = c(0.8, 0.8))

g1     


# Panel b

# Pick every second year
year_breaks <- sort(unique(ari$year))
year_breaks <- year_breaks[seq(1, length(year_breaks), by = 2)]

g2 <- ggplot(ari, aes(x = year, y = ARI_mean,, group = platform))+
  geom_line(aes(color = platform), lwd=1.5, alpha = 0.6) +
  geom_ribbon(aes(ymin = ARI_mean-ARI_sd,
                  ymax = ARI_mean+ARI_sd, fill = platform),
              alpha = 0.3, show.legend = F) +
  geom_point(aes(fill = platform), size = 3, shape = 21, stroke = .8, alpha = .8) +
  scale_color_manual(values = color_scale[3:5])+
  scale_fill_manual(values = color_scale[3:5])+
  ylim(0, 1) +
  scale_x_continuous(breaks = year_breaks) +  
  labs(y = "Adjusted Rand Index (ARI)", x = "Year", color = "Platform", fill = "Platform") +
  guides(fill = "none",
         color = guide_legend(override.aes = list(alpha = 1))) +
  theme_bw()+
  theme(text = element_text(face = "bold", size = 16),
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 17, hjust = 0.5, face = "italic"),
        axis.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        legend.title = element_text(size = 17, face = "bold"),
        legend.text = element_text(size = 16, face = "plain"),
        legend.background = element_rect(color = "black"),
        axis.text = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 12, color = "black"),
        strip.background = element_rect(fill = "white", color = "black"),
        axis.ticks.length = unit(0.2, "cm"), 
        legend.position = "none")

g2


g <- ggarrange(g1, g2, nrow=2, common.legend = F, labels = c("(a)", "(b)"), 
               hjust = -0.2, vjust = 1.2)

g



ggsave("Path/Fraction_ARI.pdf", 
       g, 
       device = "pdf", 
       width=24, 
       height =30,
       units = "cm")

freq_binned[Categories==1, .(round(p,2), platform)]
  