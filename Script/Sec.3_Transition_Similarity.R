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

############### FULL PLATFORMS (REDDIT, YOUTUBE, TUMBLR) #####################

base_dir <- "Insert-base-dir/path/Transition_Probs/" # Insert path to "Transition_Probs" folder

probs_YT_year <- fread(paste0(base_dir,  "transitions_probs_year_YT.csv"))
probs_reddit_year <- fread(paste0(base_dir,  "transitions_probs_year_reddit.csv"))
probs_tumblr_year <- fread(paste0(base_dir,  "transitions_probs_year_tumblr.csv"))

probs_reddit_year <- probs_reddit_year[, p := N/sum(N), by=.(Category, year)]
probs_YT_year <- probs_YT_year[, p := N/sum(N), by=.(Category, year)]
probs_tumblr_year <- probs_tumblr_year[, p := N/sum(N), by=.(Category, year)]


probs_reddit_year$Platform <- "Reddit"
probs_YT_year$Platform <- "YouTube"
probs_tumblr_year$Platform <- "Tumblr"

probs_year <- rbind(probs_reddit_year, probs_YT_year, probs_tumblr_year)
probs_year <- probs_year[is.na(year)==F]

expand_df <- expand.grid(unique(probs_year$Category), unique(probs_year$Category), 
                         unique(probs_year$year), unique(probs_year$Platform))
names(expand_df) <- c("Category", "next_Category", "year", "Platform")
setDT(expand_df)
expand_df <- expand_df[Category!=next_Category]

probs_year <- merge(probs_year, expand_df, by=c("Category", "next_Category", "year","Platform"), all=T)

probs_year$Category <- as.character(probs_year$Category)
probs_year$next_Category <- as.character(probs_year$next_Category)
probs_year <- probs_year[year>2011]

setorder(probs_year, Platform, year, Category, next_Category)
probs_year <- probs_year[is.na(N), N := 0]
probs_year <- probs_year[, p := N/sum(N), by=.(Category, year, Platform)]
probs_year <- probs_year[is.nan(p), p := 0]


### Intra- Platform ####

matrix_similarity <- function(df, y, p1){
  
  matrix_1 <- dcast(
    df[year==y&Platform==p1],
    Category ~ next_Category,
    value.var = "p",
    fill = 0)
  
  matrix_1 <- matrix_1[,-1]
  
  matrix_2 <- dcast(
    df[year==y+1&Platform==p1],
    Category ~ next_Category,
    value.var = "p",
    fill = 0)
  
  matrix_2 <- matrix_2[,-1]
  
  if(ncol(matrix_1)==ncol(matrix_2)&nrow(matrix_1)==nrow(matrix_2)){
    v <- data.frame("year" = y, cos_sim = sum(matrix_1 * matrix_2) / (sqrt(sum(matrix_1^2)) * sqrt(sum(matrix_2^2))), 
                    "Platform"=p1)
    return(v)
  }else{
    return(NA)}
}

platforms <- as.character(unique(probs_year$Platform))

year_similarity <- data.table()

for(y in 2012:2023){
  for(j in 1:length(platforms)){
    v <- matrix_similarity(probs_year, y, platforms[j])
    if(is.data.frame(v)){year_similarity <- rbind(year_similarity, v)}}
  print(y)
}

#fwrite(year_similarity, paste0("Insert/Path/",  "year_similarity_full.csv"))


#### Inter-Platform ####

matrix_similarity <- function(df, y, p1, p2){
  
  matrix_1 <- dcast(
    df[year==y&Platform==p1],
    Category ~ next_Category,
    value.var = "p",
    fill = 0)
  
  matrix_1 <- matrix_1[,-1]
  
  matrix_2 <- dcast(
    df[year==y&Platform==p2],
    Category ~ next_Category,
    value.var = "p",
    fill = 0)
  
  matrix_2 <- matrix_2[,-1]
  
  if(ncol(matrix_1)==ncol(matrix_2)&nrow(matrix_1)==nrow(matrix_2)){
    v <- data.frame("year" = y, cos_sim = sum(matrix_1 * matrix_2) / (sqrt(sum(matrix_1^2)) * sqrt(sum(matrix_2^2))), 
                    "Platform_1"=p1, "Platform_2"=p2)
    return(v)
  }else{
    return(NA)}
}

platforms <- as.data.frame(combs(unique(probs_year$Platform), 2))
names(platforms) <- c("p1", "p2")

platform_similarity <- data.table()

for(y in 2012:2024){
  for(j in 1:nrow(platforms)){
    v <- matrix_similarity(probs_year, y, platforms$p1[j], platforms$p2[j])
    if(is.data.frame(v)){platform_similarity <- rbind(platform_similarity, v)}}
  print(y)
}

platform_similarity <- platform_similarity[, comb := paste0(Platform_1, "-", Platform_2)]

#fwrite(platform_similarity, paste0("Insert/Path/,",  "platform_similarity_full.csv"))


############## META PLATFORMS #############


setwd(base_dir)

platforms <- data.frame("file" = c("FB", "instagram", "YT", "tumblr", "reddit"),
                        "Platform" = c("Facebook", "Instagram", "YouTube", "Tumblr", "Reddit"))

categories <- c("Depressive", "Trauma-Stress", "Anxiety", "Personality", "Obs-Comp", "Feed-Eat", "Bipolar")

probs_year <- data.table()

for(i in 1:5){
  file <- list.files(pattern = paste0("transitions_probs_year_", platforms$file[i]))
  df <- fread(file)
  df <- df[Category%in%categories&next_Category%in%categories]
  df <- df[, p := N/sum(N), by=.(Category, year)]
  df <- df[is.na(year)==F]
  
  df$Platform <-  platforms$Platform[i]
  
  probs_year <- rbind(probs_year, df)
}


expand_df <- expand.grid(unique(probs_year$Category), unique(probs_year$Category), 
                         unique(probs_year$year), unique(probs_year$Platform))
names(expand_df) <- c("Category", "next_Category", "year", "Platform")
setDT(expand_df)
expand_df <- expand_df[Category!=next_Category]

probs_year <- merge(probs_year, expand_df, by=c("Category", "next_Category", "year","Platform"), all=T)

probs_year$Category <- as.character(probs_year$Category)
probs_year$next_Category <- as.character(probs_year$next_Category)


#probs_year <- probs_year[year>2013&year<2025]
probs_year <- probs_year[year<2025]
probs_year <- probs_year[Platform=="Instagram"&year<2022|Platform!="Instagram"]

setorder(probs_year, Platform, year, Category, next_Category)
probs_year <- probs_year[is.na(N), N := 0]
probs_year <- probs_year[, p := N/sum(N), by=.(Category, year, Platform)]
probs_year <- probs_year[is.nan(p), p := 0]

### Intra- Platform ####

matrix_similarity <- function(df, y, p1){
  
  matrix_1 <- dcast(
    df[year==y&Platform==p1],
    Category ~ next_Category,
    value.var = "p",
    fill = 0)
  
  matrix_1 <- matrix_1[,-1]
  
  matrix_2 <- dcast(
    df[year==y+1&Platform==p1],
    Category ~ next_Category,
    value.var = "p",
    fill = 0)
  
  matrix_2 <- matrix_2[,-1]
  
  if(ncol(matrix_1)==ncol(matrix_2)&nrow(matrix_1)==nrow(matrix_2)){
    v <- data.frame("year" = y, cos_sim = sum(matrix_1 * matrix_2) / (sqrt(sum(matrix_1^2)) * sqrt(sum(matrix_2^2))), 
                    "Platform"=p1)
    return(v)
  }else{
    return(NA)}
}

platforms <- c("Facebook", "Instagram")

year_similarity <- data.table()

for(y in 2012:2023){
  for(j in 1:length(platforms)){
    v <- matrix_similarity(probs_year, y, platforms[j])
    if(is.data.frame(v)){year_similarity <- rbind(year_similarity, v)}}
  print(y)
}

#fwrite(year_similarity, paste0("Insert/Path/,",  "year_similarity_meta.csv"))


matrix_similarity <- function(df, y, p1, p2){
  
  matrix_1 <- dcast(
    df[year==y&Platform==p1],
    Category ~ next_Category,
    value.var = "p",
    fill = 0)
  
  matrix_1 <- matrix_1[,-1]
  
  matrix_2 <- dcast(
    df[year==y&Platform==p2],
    Category ~ next_Category,
    value.var = "p",
    fill = 0)
  
  matrix_2 <- matrix_2[,-1]
  
  if(ncol(matrix_1)==ncol(matrix_2)&nrow(matrix_1)==nrow(matrix_2)){
    v <- data.frame("year" = y, cos_sim = sum(matrix_1 * matrix_2) / (sqrt(sum(matrix_1^2)) * sqrt(sum(matrix_2^2))), 
                    "Platform_1"=p1, "Platform_2"=p2)
    return(v)
  }else{
    return(NA)}
}

platforms <- as.data.table(combs(unique(as.character(probs_year$Platform)), 2))
names(platforms) <- c("p1", "p2")
platforms <- platforms[p1%in%c("Facebook", "Instagram")|p2%in%c("Facebook", "Instagram")]

platform_similarity <- data.table()

for(y in 2012:2024){
  for(j in 1:nrow(platforms)){
    v <- matrix_similarity(probs_year, y, platforms$p1[j], platforms$p2[j])
    if(is.data.frame(v)){platform_similarity <- rbind(platform_similarity, v)}}
  print(y)
}

platform_similarity <- platform_similarity[, comb := paste0(Platform_1, "-", Platform_2)]

#fwrite(platform_similarity, paste0("Insert/Path/,",  "platform_similarity_meta.csv"))

######### LAG ####

setwd(base_dir)

platforms <- data.frame("file" = c("FB", "instagram", "YT", "tumblr", "reddit"),
                        "Platform" = c("Facebook", "Instagram", "YouTube", "Tumblr", "Reddit"))

categories <- c("Depressive", "Trauma-Stress", "Anxiety", "Personality", "Obs-Comp", "Feed-Eat", "Bipolar")

probs_year <- data.table()

for(i in 1:5){
  file <- list.files(pattern = paste0("transitions_probs_year_", platforms$file[i]))
  df <- fread(file)
  
  expand_df <- expand.grid(unique(df$Category), unique(df$Category), 
                           unique(df$year))
  names(expand_df) <- c("Category", "next_Category", "year")
  setDT(expand_df)
  expand_df <- expand_df[Category!=next_Category]
  
  df <- merge(df, expand_df, by=c("Category", "next_Category", "year"), all=T)
  
  df <- df[is.na(N), N := 0]
  df <- df[, p := N/sum(N), by=.(Category, year)]
  df <- df[is.nan(p), p := 0]
  
  df <- df[is.na(year)==F]
  df$Platform <-  platforms$Platform[i]
  df$Category <- as.character(df$Category)
  df$next_Category <- as.character(df$next_Category)
  
  probs_year <- rbind(probs_year, df)
}

### Intra- Platform ####
matrix_similarity <- function(df, y1, y2, p1){
  
  matrix_1 <- dcast(
    df[year==y1&Platform==p1],
    Category ~ next_Category,
    value.var = "p",
    fill = 0)
  
  matrix_1 <- matrix_1[,-1]
  
  matrix_2 <- dcast(
    df[year==y2&Platform==p1],
    Category ~ next_Category,
    value.var = "p",
    fill = 0)
  
  matrix_2 <- matrix_2[,-1]
  
  if(ncol(matrix_1)==ncol(matrix_2)&nrow(matrix_1)==nrow(matrix_2)){
    v <- data.frame("year" = y1, "year_2" = y2, cos_sim = sum(matrix_1 * matrix_2) / (sqrt(sum(matrix_1^2)) * sqrt(sum(matrix_2^2))), 
                    "Platform"=p1)
    return(v)
  }else{
    return(NA)}
}

platforms <- as.character(unique(probs_year$Platform))

year_similarity_lag <- data.table()

for(j in 1:length(platforms)){
  years <- probs_year[Platform==platforms[j]&year<2025, unique(year)]
  years <- as.data.table(combs(years, 2))
  
  for(y in 1:nrow(years)){
    v <- matrix_similarity(probs_year, years$V1[y], years$V2[y], platforms[j])
    if(is.data.frame(v)){year_similarity_lag <- rbind(year_similarity_lag, v)}}
  print(j)
}

year_similarity_lag <- year_similarity_lag[Platform=="Instagram"&year<2022&year_2<2022|Platform!="Instagram"]

#fwrite(year_similarity_lag, paste0("Insert/Path/,",  "year_similarity_lag.csv"))
