library(data.table)
library(lubridate)
library(viridis)
library(ggplot2)
library(dplyr)
library(arrow)

#########

base_dir <- "Insert-base-dir/path/Data/" # Insert path to "Data" folder

comment_volume <- fread(paste0(base_dir, "comment_volume.csv"))

meta_categories <- unique(comment_volume[Platform=="Instagram", category])

all_platforms <- comment_volume


expand_df <- expand.grid(all_platforms[, unique(category)], #meta_categories, 
                         seq(1, 12, 1),
                         seq(2012, 2024, 1), 
                         c("Reddit", "Tumblr", "YouTube", "Facebook", "Instagram"), 
                         stringsAsFactors = F)
names(expand_df) <- c("category", "month", "year", "Platform")
setDT(expand_df)

expand_df <- expand_df[Platform=="Instagram"&year<2022|Platform!="Instagram"]
expand_df <- expand_df[Platform=="Instagram"&year>2014|Platform!="Instagram"]

expand_df <- expand_df[Platform=="Instagram"&month<2|Platform!="Instagram"]
expand_df <- expand_df[Platform=="Facebook"&month<2|Platform!="Facebook"]
expand_df <- expand_df[category%in%meta_categories&Platform%in%c("Facebook", "Instagram")|Platform%in%c("Facebook", "Instagram")==F]
expand_df <- expand_df[, time := make_date(month = month, year=year)][, month:=NULL]

all_platforms <- merge(all_platforms[, time:=as.Date(time)], expand_df, 
                       by=c("category", "time", "Platform"), all.x=F, all.y = T)
all_platforms[is.na(N), N:=0]


setorder(all_platforms, Platform, time, category)

all_platforms_2 <- all_platforms[, sum(N), by=.(category, year, Platform)]
all_platforms_2 <- all_platforms_2[, p:=V1/sum(V1), by=.(year, Platform)]

all_platforms_2$category <- factor(all_platforms_2$category)
all_platforms_2[category%in%meta_categories, category:= paste0(category, " *")]

levels <- c("Anxiety *", "Bipolar *", "Depressive *", "Feed-Eat *", "Obs-Comp *", "Personality *", "Trauma-Stress *",   
            "Dissociative", "Neurodev", "Schiz-Psych", "Sexual", "Sleep", "Subs-Add")

labels <- c("ANX *", "BIP *", "DEP *", "FED *",  "OCD *", "PER *", "TSS *", 
            "DIS", "NDV", "SCZ","SEX", "SLP", "SUB")

all_platforms_2$category <- factor(all_platforms_2$category, levels = levels, 
                                   labels = labels)

all_platforms_2[Platform%in%c("Facebook", "Instagram"), Platform := paste0(Platform, " *")]
all_platforms_2$Platform <- factor(all_platforms_2$Platform, 
                                   levels=c("Facebook *", "Instagram *", "", 
                                            "Reddit", "Tumblr","YouTube"))

g1 <- all_platforms_2 %>% filter(year<2025&year>2013) %>% 
  ggplot(aes(x=year, y=p, fill = category))+
  geom_area(alpha= 0.8, size=0.8, colour="black")+
  facet_wrap(~Platform, nrow=2, scales = "free_x", drop = FALSE)+
  scale_x_continuous(breaks = seq(2012, 2024, 1), labels = c(rbind(paste0("'",seq(12, 24, by = 2)), ""))[1:13])+
  scale_fill_manual(values = c(viridis(7, option = "D"), 
                               viridis(8, option = "F")[c(1, 3:7)]))+
  
  ylab("Comment volume percentage")+
  xlab("Year")+
  labs(fill="Category")+
  theme_linedraw()+
  theme(
    text = element_text(face = "bold", size = 15),
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 17, hjust = 0.5, face = "italic"),
    legend.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 15, face = "plain"),
    legend.background = element_rect(color = "black"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold", vjust=2),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 14, color = "black"),
    strip.background = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    aspect.ratio = 1,
    legend.position = c(0.84, 0.76))

g1

ggsave("Path/Cat_Freqs.pdf", 
       g1, 
       device = "pdf", 
       width=40, 
       height =25,
       units = "cm")
