# Script for visualising HDB town LST mean max min std 

meanlst2018 <- rast("lst2018final1.tif")
hdb <- read_sf("finalhdbtowns.shp")
vals <- terra::extract(meanlst2018, vect(hdb), ID=TRUE)
vals_clean <- vals %>% filter(!is.na(LST))
ids <- unique(vals_clean$ID)

summary_list <- lapply(ids, function(i) {
  subset_vals <- vals_clean %>% filter(ID == i)
  vals_vec <- subset_vals$LST
  if (all(is.na(vals_vec))) {
    return(NULL)  
  }
  data.frame(
    ID = i,
    min = min(vals_vec, na.rm = TRUE),
    q1 = quantile(vals_vec, 0.25, na.rm = TRUE),
    median = median(vals_vec, na.rm = TRUE),
    q3 = quantile(vals_vec, 0.75, na.rm = TRUE),
    max = max(vals_vec, na.rm = TRUE),
    mean = mean(vals_vec, na.rm = TRUE),
    sd = sd(vals_vec, na.rm = TRUE)  
  )
})

summary_stats <- bind_rows(summary_list)
hdb$ID <- seq_len(nrow(hdb))
summary_with_name1 <- summary_stats %>%
  left_join(hdb %>% select(ID, name), by = "ID")

library(ggplot2)

summary_with_name1 <- summary_with_name1 %>%
  mutate(name = case_when(
    name == "BP" ~ "Bukit Panjang",
    name == "BB" ~ "Bukit Batok",
    name == "BukitM" ~ "Bukit Merah",
    name == "CCK" ~ "Choa Chu Kang",
    name == "TebanGar" ~ "Teban Gardens",
    name == "TPY" ~ "Toa Payoh",
    name == "MarineP" ~ "Marine Parade",
    TRUE ~ name
  ))

summary_with_name1 <- summary_with_name1 %>%
  filter(!name %in% c("FarrerPark", "EvertonPak", "BukitT","JooSeng","Bedok2","PasirRis2"))

ggplot(summary_with_name1, aes(x = reorder(name, median))) +
  geom_linerange(aes(ymin = min, ymax = max), color = "black") +
  geom_linerange(aes(ymin = q1, ymax = q3), color = "grey", size = 3) +
  geom_point(aes(y = median), color = "red", size = 2) +
  scale_y_continuous(limits = c(25, 55)) +
  theme_minimal() +
  labs(x = "HDB Residential Town", y = "LST Value (°C)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


hdb <- read_sf("finalhdbtowns.shp")
hdb <- hdb %>%
  mutate(name = case_when(
    name == "BP" ~ "Bukit Panjang",
    name == "BB" ~ "Bukit Batok",
    name == "BukitM" ~ "Bukit Merah",
    name == "CCK" ~ "Choa Chu Kang",
    name == "TebanGar" ~ "Teban Gardens",
    name == "TPY" ~ "Toa Payoh",
    name == "MarineP" ~ "Marine Parade",
    TRUE ~ name
  ))
hdblst <- hdb %>%
  left_join(summary_with_name1 %>% select(mean, name), by = "name")
hdblst <- hdblst %>% filter(!is.na(mean))
sg <- read_sf("singaporesubzonesfinal1.shp")

ggplot() +
  geom_sf(data = hdblst, aes(fill = mean)) +
  scale_fill_gradientn(colours = c("green", "yellow", "red")) +
  geom_sf(data = sg2, fill = NA, color = "black", size = 0.4) +  # overlay outlines
  theme_minimal() +
  labs(
    #title = "Median Values by HDB Town",
    fill = "Median"
  )


#FOR 2023 Oct
meanlst2020 <- rast("lstfinal20231013.tif")
hdb <- read_sf("finalhdbtowns.shp")
vals <- terra::extract(meanlst2020, vect(hdb), ID=TRUE)
vals_clean <- vals %>% filter(!is.na(LST))
ids <- unique(vals_clean$ID)

summary_list <- lapply(ids, function(i) {
  subset_vals <- vals_clean %>% filter(ID == i)
  vals_vec <- subset_vals$LST
  if (all(is.na(vals_vec))) {
    return(NULL)  # skip if all NA
  }
  data.frame(
    ID = i,
    min = min(vals_vec, na.rm = TRUE),
    q1 = quantile(vals_vec, 0.25, na.rm = TRUE),
    median = median(vals_vec, na.rm = TRUE),
    q3 = quantile(vals_vec, 0.75, na.rm = TRUE),
    max = max(vals_vec, na.rm = TRUE),
    mean = mean(vals_vec, na.rm = TRUE)
  )
})

summary_stats <- bind_rows(summary_list)
hdb$ID <- seq_len(nrow(hdb))
summary_with_name2 <- summary_stats %>%
  left_join(hdb %>% select(ID, name), by = "ID")

library(ggplot2)

summary_with_name2 <- summary_with_name2 %>%
  mutate(name = case_when(
    name == "BP" ~ "Bukit Panjang",
    name == "BB" ~ "Bukit Batok",
    name == "BukitM" ~ "Bukit Merah",
    name == "CCK" ~ "Choa Chu Kang",
    name == "TebanGar" ~ "Teban Gardens",
    name == "TPY" ~ "Toa Payoh",
    name == "MarineP" ~ "Marine Parade",
    TRUE ~ name
  ))

summary_with_name2 <- summary_with_name2 %>%
  filter(!name %in% c("FarrerPark", "EvertonPak", "BukitT","JooSeng","Bedok2","PasirRis2"))


ggplot(summary_with_name2, aes(x = reorder(name, median))) +
  geom_linerange(aes(ymin = min, ymax = max), color = "black") +
  geom_linerange(aes(ymin = q1, ymax = q3), color = "grey", size = 3) +
  geom_point(aes(y = median), color = "red", size = 2) +
  scale_y_continuous(limits = c(25, 55)) +
  theme_minimal() +
  labs(x = "HDB Residential Town", y = "LST Value (°C)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hdb <- read_sf("finalhdbtowns.shp")
hdb <- hdb %>%
  mutate(name = case_when(
    name == "BP" ~ "Bukit Panjang",
    name == "BB" ~ "Bukit Batok",
    name == "BukitM" ~ "Bukit Merah",
    name == "CCK" ~ "Choa Chu Kang",
    name == "TebanGar" ~ "Teban Gardens",
    name == "TPY" ~ "Toa Payoh",
    name == "MarineP" ~ "Marine Parade",
    TRUE ~ name
  ))
hdblst <- hdb %>%
  left_join(summary_with_name %>% select(mean, name), by = "name")
hdblst <- hdblst %>% filter(!is.na(mean))
sg <- read_sf("singaporesubzonesfinal1.shp")

ggplot() +
  geom_sf(data = hdblst, aes(fill = mean)) +
  scale_fill_gradientn(colours = c("green", "yellow", "red")) +
  geom_sf(data = sg2, fill = NA, color = "black", size = 0.4) +  # overlay outlines
  theme_minimal() +
  labs(
    #title = "Median Values by HDB Town",
    fill = "Median"
  )

#FOR 2023 May
meanlst2023 <- rast("20230522lstfinal.tif")
hdb <- read_sf("finalhdbtowns.shp")
vals <- terra::extract(meanlst2023, vect(hdb), ID=TRUE)
vals_clean <- vals %>% filter(!is.na(LST))
ids <- unique(vals_clean$ID)

summary_list <- lapply(ids, function(i) {
  subset_vals <- vals_clean %>% filter(ID == i)
  vals_vec <- subset_vals$LST
  if (all(is.na(vals_vec))) {
    return(NULL)  # skip if all NA
  }
  data.frame(
    ID = i,
    min = min(vals_vec, na.rm = TRUE),
    q1 = quantile(vals_vec, 0.25, na.rm = TRUE),
    median = median(vals_vec, na.rm = TRUE),
    q3 = quantile(vals_vec, 0.75, na.rm = TRUE),
    max = max(vals_vec, na.rm = TRUE),
    mean = mean(vals_vec, na.rm = TRUE)
  )
})

summary_stats <- bind_rows(summary_list)
hdb$ID <- seq_len(nrow(hdb))
summary_with_name3 <- summary_stats %>%
  left_join(hdb %>% select(ID, name), by = "ID")

library(ggplot2)

summary_with_name3 <- summary_with_name3 %>%
  mutate(name = case_when(
    name == "BP" ~ "Bukit Panjang",
    name == "BB" ~ "Bukit Batok",
    name == "BukitM" ~ "Bukit Merah",
    name == "CCK" ~ "Choa Chu Kang",
    name == "TebanGar" ~ "Teban Gardens",
    name == "TPY" ~ "Toa Payoh",
    name == "MarineP" ~ "Marine Parade",
    TRUE ~ name
  ))

summary_with_name3 <- summary_with_name3 %>%
  filter(!name %in% c("FarrerPark", "EvertonPak", "BukitT","JooSeng","Bedok2","PasirRis2"))


ggplot(summary_with_name3, aes(x = reorder(name, median))) +
  geom_linerange(aes(ymin = min, ymax = max), color = "black") +
  geom_linerange(aes(ymin = q1, ymax = q3), color = "grey", size = 3) +
  geom_point(aes(y = median), color = "red", size = 2) +
  scale_y_continuous(limits = c(25, 55)) +
  theme_minimal() +
  labs(x = "HDB Residential Town", y = "LST Value (°C)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hdb <- read_sf("finalhdbtowns.shp")
hdb <- hdb %>%
  mutate(name = case_when(
    name == "BP" ~ "Bukit Panjang",
    name == "BB" ~ "Bukit Batok",
    name == "BukitM" ~ "Bukit Merah",
    name == "CCK" ~ "Choa Chu Kang",
    name == "TebanGar" ~ "Teban Gardens",
    name == "TPY" ~ "Toa Payoh",
    name == "MarineP" ~ "Marine Parade",
    TRUE ~ name
  ))
hdblst <- hdb %>%
  left_join(summary_with_name %>% select(mean, name), by = "name")
hdblst <- hdblst %>% filter(!is.na(mean))
sg <- read_sf("singaporesubzonesfinal1.shp")

ggplot() +
  geom_sf(data = hdblst, aes(fill = mean)) +
  scale_fill_gradientn(colours = c("green", "yellow", "red")) +
  geom_sf(data = sg2, fill = NA, color = "black", size = 0.4) +  # overlay outlines
  theme_minimal() +
  labs(
    #title = "Median Values by HDB Town",
    fill = "Median"
  )

#2025 boxplot
meanlst2023 <- rast("20250103lstfinal.tif")
hdb <- read_sf("finalhdbtowns.shp")
vals <- terra::extract(meanlst2023, vect(hdb), ID=TRUE)
vals_clean <- vals %>% filter(!is.na(LST))
ids <- unique(vals_clean$ID)

summary_list <- lapply(ids, function(i) {
  subset_vals <- vals_clean %>% filter(ID == i)
  vals_vec <- subset_vals$LST
  if (all(is.na(vals_vec))) {
    return(NULL)  # skip if all NA
  }
  data.frame(
    ID = i,
    min = min(vals_vec, na.rm = TRUE),
    q1 = quantile(vals_vec, 0.25, na.rm = TRUE),
    median = median(vals_vec, na.rm = TRUE),
    q3 = quantile(vals_vec, 0.75, na.rm = TRUE),
    max = max(vals_vec, na.rm = TRUE),
    mean = mean(vals_vec, na.rm = TRUE)
  )
})

summary_stats <- bind_rows(summary_list)
hdb$ID <- seq_len(nrow(hdb))
summary_with_name4 <- summary_stats %>%
  left_join(hdb %>% select(ID, name), by = "ID")

library(ggplot2)

summary_with_name4 <- summary_with_name4 %>%
  mutate(name = case_when(
    name == "BP" ~ "Bukit Panjang",
    name == "BB" ~ "Bukit Batok",
    name == "BukitM" ~ "Bukit Merah",
    name == "CCK" ~ "Choa Chu Kang",
    name == "TebanGar" ~ "Teban Gardens",
    name == "TPY" ~ "Toa Payoh",
    name == "MarineP" ~ "Marine Parade",
    TRUE ~ name
  ))

summary_with_name4 <- summary_with_name4 %>%
  filter(!name %in% c("FarrerPark", "EvertonPak", "BukitT","JooSeng","Bedok2","PasirRis2"))


ggplot(summary_with_name4, aes(x = reorder(name, median))) +
  geom_linerange(aes(ymin = min, ymax = max), color = "black") +
  geom_linerange(aes(ymin = q1, ymax = q3), color = "grey", size = 3) +
  geom_point(aes(y = median), color = "red", size = 2) +
  scale_y_continuous(limits = c(25, 55)) +
  theme_minimal() +
  labs(x = "HDB Residential Town", y = "LST Value (°C)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#visualised map of median values 
hdb <- read_sf("finalhdbtowns.shp")
hdb <- hdb %>%
  mutate(name = case_when(
    name == "BP" ~ "Bukit Panjang",
    name == "BB" ~ "Bukit Batok",
    name == "BukitM" ~ "Bukit Merah",
    name == "CCK" ~ "Choa Chu Kang",
    name == "TebanGar" ~ "Teban Gardens",
    name == "TPY" ~ "Toa Payoh",
    name == "MarineP" ~ "Marine Parade",
    TRUE ~ name
  ))
hdblst <- hdb %>%
  left_join(summary_with_name %>% select(mean, name), by = "name")
hdblst <- hdblst %>% filter(!is.na(mean))
sg <- read_sf("singaporesubzonesfinal1.shp")

ggplot() +
  geom_sf(data = hdblst, aes(fill = mean)) +
  scale_fill_gradientn(colours = c("green", "yellow", "red")) +
  geom_sf(data = sg2, fill = NA, color = "black", size = 0.4) +  # overlay outlines
  theme_minimal() +
  labs(
    #title = "Median Values by HDB Town",
    fill = "Median"
  )


# Combine all years 
combined1 <- summary_with_name1 %>%
  left_join(summary_with_name2 %>% select(mean, name), by = "name")

combined1 <- combined1 %>%
  left_join(summary_with_name3 %>% select(mean, name), by = "name")

combined1 <- combined1 %>%
  left_join(summary_with_name4 %>% select(mean, name), by = "name")

colnames(combined1)[colnames(combined1) %in% c("mean.x", "mean.y", "mean.x.x","mean.y.y")] <- c("2018", "202310","202305","2025")

combined1$meanlst <- rowMeans(combined1[, c("2018", "202310", "202305", "2025")])
