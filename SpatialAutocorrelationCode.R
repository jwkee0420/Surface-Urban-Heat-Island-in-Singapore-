# Autocorrelation script 
library(terra)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(dplyr)
library(car)
library(broom)
library(janitor)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(RSAGA)
library(spdep)
library(sfdep)

lst2018 <-read_sf("2018LSTsubzone.shp")
lst2023 <-read_sf("2023LSTsubzone.shp")

# remove small islands that is not the main city 
values_to_remove <- c("kml_16", "kml_28", "kml_29","kml_30","kml_318","kml_309")

# Remove rows where column_name matches any of those values
lst20182 <- lst2018[!(lst2018$Name %in% values_to_remove), ]
lst20232 <- lst2023[!(lst2023$Name %in% values_to_remove), ]

lst20182 <- clean_names(lst20182)
lst20232 <- clean_names(lst20232)

coordsW <- lst20182%>%
  st_centroid()%>%
  st_geometry()

LWard_nb <- lst20182 %>%
  poly2nb(., queen=T)

plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(lst20182$geometry, add=T)

Lward.lw <- LWard_nb %>%
  nb2mat(., style="B")

Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

I_LWard_Global_Density <- lst20182 %>%
  pull(mean) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Density


getis <- lst20182 |> 
  mutate(
    nb = st_contiguity(geometry),       
    wt = st_weights(nb),                 
    tes_lag = st_lag(lst20182$mean, nb, wt)   
  ) 


getis2 <- getis |> 
  mutate(
    Gi = local_g_perm(lst20182$mean, nb, wt, nsim = 999)
    # nsim = number of Monte Carlo simulations (999 is default)
  ) |> 
  unnest(Gi)

getis2 |> 
  ggplot((aes(fill = gi))) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_gradient2() # makes the value 0 (random) be the middle


# Define levels
classification_levels <- c(
  "Very hot", "Hot", "Somewhat hot",
  "Insignificant",
  "Somewhat cold", "Cold", "Very cold"
)

hotspot_colors <- c(
  "Very hot" = "#67001f",
  "Hot" = "#b2182b",
  "Somewhat hot" = "#d6604d",
  "Insignificant" = "#f7f7f7",
  "Somewhat cold" = "#92c5de",
  "Cold" = "#4393c3",
  "Very cold" = "#2166ac"
)

getis2 |>
  mutate(
    classification = case_when(
      gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
      gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
      gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
      gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
      gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
      gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
      TRUE ~ "Insignificant"
    ),
    classification = factor(classification, levels = classification_levels)
  ) |>
  ggplot(aes(fill = classification)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_manual(
    values = hotspot_colors,
    breaks = classification_levels
  ) +
  theme_void() +
  labs(
    fill = "Hot Spot Classification",
    title = "LST Hot Spots in Singapore (05/24/2018)"
  )

#2023 Data 
coordsW <- lst20232%>%
  st_centroid()%>%
  st_geometry()

LWard_nb <- lst20232 %>%
  poly2nb(., queen=T)

plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(lst20232$geometry, add=T)

Lward.lw <- LWard_nb %>%
  nb2mat(., style="B")

Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

I_LWard_Global_Density <- lst20232 %>%
  pull(mean) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Density




getis3 <- lst20232 |> 
  mutate(
    nb = st_contiguity(geometry),        # neighbors share border/vertex
    wt = st_weights(nb),                 # row-standardized weights
    tes_lag = st_lag(lst20182$mean, nb, wt)    # calculate spatial lag of TreEqty
  ) 


getis4 <- getis3 |> 
  mutate(
    Gi = local_g_perm(lst20232$mean, nb, wt, nsim = 999)
    # nsim = number of Monte Carlo simulations (999 is default)
  ) |> 
  # The new 'Gi' column itself contains a dataframe 
  # We can't work with that, so we need to 'unnest' it
  unnest(Gi)

getis4 |> 
  ggplot((aes(fill = gi))) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_gradient2() # makes the value 0 (random) be the middle


# Define levels and palette in the desired order
classification_levels <- c(
  "Very hot", "Hot", "Somewhat hot",
  "Insignificant",
  "Somewhat cold", "Cold", "Very cold"
)

hotspot_colors <- c(
  "Very hot" = "#67001f",
  "Hot" = "#b2182b",
  "Somewhat hot" = "#d6604d",
  "Insignificant" = "#f7f7f7",
  "Somewhat cold" = "#92c5de",
  "Cold" = "#4393c3",
  "Very cold" = "#2166ac"
)

getis4 |>
  mutate(
    classification = case_when(
      gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
      gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
      gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
      gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
      gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
      gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
      TRUE ~ "Insignificant"
    ),
    classification = factor(classification, levels = classification_levels)
  ) |>
  ggplot(aes(fill = classification)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_manual(
    values = hotspot_colors,
    breaks = classification_levels  # <- This controls legend order!
  ) +
  theme_void() +
  labs(
    fill = "Hot Spot Classification",
    title = "LST Hot Spots in Singapore (05/24/2018)"
  )


# COMBINE and combine both differences 

classify_getis <- function(df) {
  df |> 
    mutate(
      classification = case_when(
        gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
        gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
        gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
        gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
        gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
        gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
        TRUE ~ "Insignificant"
      ),
      classification = factor(classification, levels = c(
        "Very hot", "Hot", "Somewhat hot",
        "Insignificant",
        "Somewhat cold", "Cold", "Very cold"
      ))
    )
}

getis_t1 <- classify_getis(getis2)
getis_t2 <- classify_getis(getis4)


is_LL <- function(x) x %in% c("Very cold", "Cold")
is_HH <- function(x) x %in% c("Very hot", "Hot")

getis_t2_no_geom <- getis_t2 |> 
  st_drop_geometry() |> 
  select(name, classification)

# Join
comparison <- getis_t1 |>
  left_join(getis_t2_no_geom, by = "name")

#change categories 
comparison <- comparison |> 
  mutate(
    change_category = case_when(
      is_LL(classification.x) & is_LL(classification.y) ~ "Stayed LL",
      is_HH(classification.x) & is_HH(classification.y) ~ "Stayed HH",
      is_LL(classification.x) & is_HH(classification.y) ~ "Change LL → HH",
      is_HH(classification.x) & is_LL(classification.y) ~ "Change HH → LL",
      TRUE ~ "No change"
    ),
    change_category = factor(change_category, levels = c(
      "Stayed LL", "Stayed HH", "Change HH → LL", "Change LL → HH", "No change"
    ))
  )

ggplot(comparison, aes(fill = change_category)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_manual(
    values = c(
      "Stayed LL" = "#2166ac",
      "Stayed HH" = "#b2182b",
      "Change HH → LL" = "#6e016b",
      "Change LL → HH" = "#1a9850",
      "No change" = "#cccccc"
    )
  ) +
  theme_void() +
  labs(
    fill = "Hot/Cold Change Category",
    title = "Changes in LST Hot/Cold Spots Over Time"
  )


# Local Moran's I PLEASE FOLLOW THIS part 
# get mean values of LST in shp file for 2018 
hdb <- read_sf("finalhdbtowns.shp")
hdb <- st_as_sf(hdb)
hdb <- st_transform(hdb,crs = crs_3414)
sg <- read_sf("singaporesubzonesfinal1.shp")
sg2 <- st_transform(sg, crs = crs_3414) 

values_to_remove <- c("kml_16", "kml_28", "kml_29","kml_30","kml_318","kml_309")

# Remove rows where column_name matches kml shp numbers 
sg2 <- sg2[!(sg2$Name %in% values_to_remove), ]

meanlst2018 <- rast("lst2018final1.tif")
vals <- terra::extract(meanlst2018, vect(sg2), ID=TRUE)
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
    mean = mean(vals_vec, na.rm = TRUE)
  )
})

summary_stats <- bind_rows(summary_list)
sg2$ID <- seq_len(nrow(sg2))
summary_with_name <- summary_stats %>%
  left_join(sg2 %>% select(ID, Name), by = "ID")

lst20182 <- summary_with_name
lst20182 <- st_as_sf(lst20182)
lst20182 <- st_transform(lst20182, crs = 3414)

names(lst20182)[names(lst20182) == 'mean'] <- 'LST'
nb <- poly2nb(lst20182)  

lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
moran_results <- localmoran(lst20182$LST, lw, zero.policy = TRUE)

lst20182 <- lst20182 |> 
  mutate(
    Ii = moran_results[, "Ii"],
    p_value = moran_results[, 5],
    lag_LST = lag.listw(lw, LST), 
    lisa_category = case_when(
      LST > mean(LST, na.rm = TRUE) & lag_LST > mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "HH",
      LST < mean(LST, na.rm = TRUE) & lag_LST < mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "LL",
      LST > mean(LST, na.rm = TRUE) & lag_LST < mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "HL",
      LST < mean(LST, na.rm = TRUE) & lag_LST > mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "LH",
      TRUE ~ "NS"
    ),
    lisa_category = factor(lisa_category, levels = c("HH", "LL", "HL", "LH", "NS"))
  )

#plot 

ggplot(lst20182) +
  geom_sf(aes(fill = lisa_category), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c(
      "HH" = "red",
      "LL" = "blue",
      "HL" = "#FFCCCB",
      "LH" = "skyblue",
      "NS" = "grey80"
    ),
    name = "LISA Cluster", 
    drop = FALSE
  ) +
  theme_minimal() +
  labs(
    #title = "Local Moran's I (LISA) Clusters of LST",
    #subtitle = "Based on 2018/05/24 LST values",
    fill = "Cluster Type"
  ) +
  theme(
    panel.grid = element_blank(),       
    axis.text = element_blank(),       
    axis.ticks = element_blank(),       
    panel.border = element_blank(),     
    #legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
    
  )

global_moran <- moran.test(lst20182$LST, lw, zero.policy = TRUE)
global_moran

#2023 May
sg <- read_sf("singaporesubzonesfinal1.shp")
sg2 <- st_transform(sg, crs = crs_3414) 

values_to_remove <- c("kml_16", "kml_28", "kml_29","kml_30","kml_318","kml_309")

# Remove rows where column_name matches kml shp numbers 
sg2 <- sg2[!(sg2$Name %in% values_to_remove), ]

meanlst2018 <- rast("20230522lstfinal.tif")
vals <- terra::extract(meanlst2018, vect(sg2), ID=TRUE)
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
    mean = mean(vals_vec, na.rm = TRUE)
  )
})

summary_stats <- bind_rows(summary_list)
sg2$ID <- seq_len(nrow(sg2))
summary_with_name <- summary_stats %>%
  left_join(sg2 %>% select(ID, Name), by = "ID")

lst20202 <- summary_with_name
lst20202 <- st_as_sf(lst20202)
lst20202 <- st_transform(lst20202, crs = 3414)

names(lst20202)[names(lst20202) == 'mean'] <- 'LST'
nb <- poly2nb(lst20202)  

# Convert to listw object
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
moran_results <- localmoran(lst20202$LST, lw, zero.policy = TRUE)

lst20202 <- lst20202 |> 
  mutate(
    Ii = moran_results[, "Ii"],
    p_value = moran_results[, 5],
    lag_LST = lag.listw(lw, LST),  
    lisa_category = case_when(
      LST > mean(LST, na.rm = TRUE) & lag_LST > mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "HH",
      LST < mean(LST, na.rm = TRUE) & lag_LST < mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "LL",
      LST > mean(LST, na.rm = TRUE) & lag_LST < mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "HL",
      LST < mean(LST, na.rm = TRUE) & lag_LST > mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "LH",
      TRUE ~ "NS"
    ),
    lisa_category = factor(lisa_category, levels = c("HH", "LL", "HL", "LH", "NS"))
  )

#plot 

ggplot(lst20202) +
  geom_sf(aes(fill = lisa_category), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c(
      "HH" = "red",
      "LL" = "blue",
      "HL" = "#FFCCCB",
      "LH" = "skyblue",
      "NS" = "grey80"
    ),
    name = "LISA Cluster", 
    drop = FALSE
  ) +
  theme_minimal() +
  labs(
    #title = "Local Moran's I (LISA) Clusters of LST",
    #subtitle = "Based on 2020/07/16 LST values",
    fill = "Cluster Type"
  ) +
  theme(
    panel.grid = element_blank(),       
    axis.text = element_blank(),       
    axis.ticks = element_blank(),       
    panel.border = element_blank(),     
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )

global_moran2 <- moran.test(lst20202$LST, lw, zero.policy = TRUE)
global_moran2

#2023 october 
sg <- read_sf("singaporesubzonesfinal1.shp")
sg2 <- st_transform(sg, crs = crs_3414) 

values_to_remove <- c("kml_16", "kml_28", "kml_29","kml_30","kml_318","kml_309")

# Remove rows where column_name matches kml shp numbers 
sg2 <- sg2[!(sg2$Name %in% values_to_remove), ]

meanlst2018 <- rast("lstfinal20231013.tif")
vals <- terra::extract(meanlst2018, vect(sg2), ID=TRUE)
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
    mean = mean(vals_vec, na.rm = TRUE)
  )
})

summary_stats <- bind_rows(summary_list)
sg2$ID <- seq_len(nrow(sg2))
summary_with_name <- summary_stats %>%
  left_join(sg2 %>% select(ID, Name), by = "ID")

lst20232 <- summary_with_name
lst20232 <- st_as_sf(lst20232)
lst20232 <- st_transform(lst20232, crs = 3414)

names(lst20232)[names(lst20232) == 'mean'] <- 'LST'
nb <- poly2nb(lst20232)  

# Convert to listw object
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
moran_results <- localmoran(lst20232$LST, lw, zero.policy = TRUE)

global_moran <- moran.test(lst20232$LST, lw, zero.policy = TRUE)

lst20232 <- lst20232 |> 
  mutate(
    Ii = moran_results[, "Ii"],
    p_value = moran_results[, 5],
    lag_LST = lag.listw(lw, LST), 
    lisa_category = case_when(
      LST > mean(LST, na.rm = TRUE) & lag_LST > mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "HH",
      LST < mean(LST, na.rm = TRUE) & lag_LST < mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "LL",
      LST > mean(LST, na.rm = TRUE) & lag_LST < mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "HL",
      LST < mean(LST, na.rm = TRUE) & lag_LST > mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "LH",
      TRUE ~ "NS"
    ),
    lisa_category = factor(lisa_category, levels = c("HH", "LL", "HL", "LH", "NS"))
  )

#plot 

ggplot(lst20232) +
  geom_sf(aes(fill = lisa_category), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c(
      "HH" = "red",
      "LL" = "blue",
      "HL" = "#FFCCCB",
      "LH" = "skyblue",
      "NS" = "grey80"
    ),
    name = "LISA Cluster", 
    drop = FALSE
  ) +
  theme_minimal() +
  labs(
    #title = "Local Moran's I (LISA) Clusters of LST",
    #subtitle = "Based on 2020/07/16 LST values",
    fill = "Cluster Type"
  ) +
  theme(
    panel.grid = element_blank(),       
    axis.text = element_blank(),       
    axis.ticks = element_blank(),       
    panel.border = element_blank(),     
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )

global_moran3 <- moran.test(lst20232$LST, lw, zero.policy = TRUE)
global_moran3

## 2025 
sg <- read_sf("singaporesubzonesfinal1.shp")
sg2 <- st_transform(sg, crs = crs_3414) 

values_to_remove <- c("kml_16", "kml_28", "kml_29","kml_30","kml_318","kml_309")

# Remove rows where column_name matches kml shp numbers 
sg2 <- sg2[!(sg2$Name %in% values_to_remove), ]

meanlst2018 <- rast("20250103lstfinal.tif")
vals <- terra::extract(meanlst2018, vect(sg2), ID=TRUE)
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
    mean = mean(vals_vec, na.rm = TRUE)
  )
})

summary_stats <- bind_rows(summary_list)
sg2$ID <- seq_len(nrow(sg2))
summary_with_name <- summary_stats %>%
  left_join(sg2 %>% select(ID, Name), by = "ID")

lst20252 <- summary_with_name
lst20252 <- st_as_sf(lst20252)
lst20252 <- st_transform(lst20252, crs = 3414)

names(lst20252)[names(lst20252) == 'mean'] <- 'LST'
nb <- poly2nb(lst20252) 

# Convert to listw object
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
moran_results <- localmoran(lst20252$LST, lw, zero.policy = TRUE)

lst20252 <- lst20252 |> 
  mutate(
    Ii = moran_results[, "Ii"],
    p_value = moran_results[, 5],
    lag_LST = lag.listw(lw, LST),
    lisa_category = case_when(
      LST > mean(LST, na.rm = TRUE) & lag_LST > mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "HH",
      LST < mean(LST, na.rm = TRUE) & lag_LST < mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "LL",
      LST > mean(LST, na.rm = TRUE) & lag_LST < mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "HL",
      LST < mean(LST, na.rm = TRUE) & lag_LST > mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "LH",
      TRUE ~ "NS"
    ),
    lisa_category = factor(lisa_category, levels = c("HH", "LL", "HL", "LH", "NS"))
  )

#plot 

ggplot(lst20252) +
  geom_sf(aes(fill = lisa_category), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c(
      "HH" = "red",
      "LL" = "blue",
      "HL" = "#FFCCCB",
      "LH" = "skyblue",
      "NS" = "grey80"
    ),
    name = "LISA Cluster", 
    drop = FALSE
  ) +
  theme_minimal() +
  labs(
    #title = "Local Moran's I (LISA) Clusters of LST",
    #subtitle = "Based on 2020/07/16 LST values",
    fill = "Cluster Type"
  ) +
  theme(
    panel.grid = element_blank(),       
    axis.text = element_blank(),       
    axis.ticks = element_blank(),       
    panel.border = element_blank(),     
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )+
  geom_sf(data = hdb, fill = NA, color = "black", size = 0.8)  # overlay outlines

global_moran4 <- moran.test(lst20252$LST, lw, zero.policy = TRUE)
global_moran4






# JOIN 
lst20182 <- lst20182 |> 
  dplyr::rename(lisa_t1 = lisa_category) |> 
  select(Name, geometry, lisa_t1)

# Time 2
lst20202 <- lst20202 |> 
  dplyr::rename(lisa_t2 = lisa_category) |> 
  st_drop_geometry() |> 
  select(Name, lisa_t2)

lst20232 <- lst20232 |> 
  dplyr::rename(lisa_t3 = lisa_category) |> 
  st_drop_geometry() |> 
  select(Name, lisa_t3)

lst20252 <- lst20252 |> 
  dplyr::rename(lisa_t4 = lisa_category) |> 
  st_drop_geometry() |> 
  select(Name, lisa_t4)



combined <- lst20182 |>
  left_join(lst20202, by = "Name")

combined <- combined |>
  left_join(lst20232, by = "Name")

combined <- combined |>
  left_join(lst20252, by = "Name")

combined2 <- combined |> 
  mutate(
    change_category1 = case_when(
      lisa_t1 == "LL" & lisa_t2 == "LL" & lisa_t3 == "LL" & lisa_t4 == "LL"~ "Stayed LL",
      lisa_t1 == "HH" & lisa_t2 == "HH" & lisa_t3 == "HH" & lisa_t4 == "HH"~ "Stayed HH",
      lisa_t1 == "NS" & lisa_t2 == "NS" & lisa_t3 == "NS" & lisa_t4 == "NS"~ "Stayed NS",
      TRUE ~ "Other changes"
    ),
    change_category1 = factor(
      change_category1,
      levels = c("Stayed LL", "Stayed HH", "Stayed NS", "Other changes")
    )
  )

library(ggplot2)

ggplot(combined2, aes(fill = change_category1)) +
  geom_sf(color = "white", lwd = 0.1) +
  scale_fill_manual(
    values = c(
      "Stayed LL" = "blue",
      "Stayed HH" = "red",
      "Other changes" = "#FFDBBB",
      "Stayed NS" = "#cccccc"
    )
  ) +
  theme_void() +
  labs(
    fill = "Change in LISA",
    #title = "Change in LISA Clusters in 2018 to 2020"
  )+
  geom_sf(data = hdb, fill = NA, color = "black", size = 2) 


## compare 2018 may and 2023 may 
combined2 <- combined2 |> 
  mutate(
    change_category2 = case_when(
      lisa_t1 == "LL" & lisa_t2 == "LL" ~ "Stayed LL",
      lisa_t1 == "HH" & lisa_t2 == "HH" ~ "Stayed HH",
      lisa_t1 == "NS" & lisa_t2 == "NS" ~ "Stayed NS",
      lisa_t1 == "HH" & lisa_t2 == "LL" ~ "Change HH → LL",
      lisa_t1 == "LL" & lisa_t2 == "HH" ~ "Change LL → HH",
      TRUE ~ "Other changes"
    ),
    change_category2 = factor(
      change_category2,
      levels = c("Stayed LL", "Stayed HH", "Stayed NS", "Other changes", "Change HH → LL", "Change LL → HH")
    )
  )

library(ggplot2)

ggplot(combined2, aes(fill = change_category2)) +
  geom_sf(color = "white", lwd = 0.1) +
  scale_fill_manual(
    values = c(
      "Stayed LL" = "blue",
      "Stayed HH" = "red",
      "Change HH → LL" = "#762a83",
      "Change LL → HH" = "#1b7837",
      "Other changes" = "#FFDBBB",
      "Stayed NS" = "#cccccc"
    )
  ) +
  theme_void() +
  labs(
    fill = "Change in LISA",
    #title = "Change in LISA Clusters in 2018 to 2020"
  )+
  geom_sf(data = hdb, fill = NA, color = "black", size = 2) 


## compare 2023 may and 2023 oct 
combined2 <- combined2 |> 
  mutate(
    change_category3 = case_when(
      lisa_t2 == "LL" & lisa_t3 == "LL" ~ "Stayed LL",
      lisa_t2 == "HH" & lisa_t3 == "HH" ~ "Stayed HH",
      lisa_t2 == "NS" & lisa_t3 == "NS" ~ "Stayed NS",
      lisa_t2 == "HH" & lisa_t3 == "LL" ~ "Change HH → LL",
      lisa_t2 == "LL" & lisa_t3 == "HH" ~ "Change LL → HH",
      TRUE ~ "Other changes"
    ),
    change_category3 = factor(
      change_category3,
      levels = c("Stayed LL", "Stayed HH", "Stayed NS", "Other changes", "Change HH → LL", "Change LL → HH")
    )
  )

library(ggplot2)

ggplot(combined2, aes(fill = change_category3)) +
  geom_sf(color = "white", lwd = 0.1) +
  scale_fill_manual(
    values = c(
      "Stayed LL" = "blue",
      "Stayed HH" = "red",
      "Change HH → LL" = "#762a83",
      "Change LL → HH" = "#1b7837",
      "Other changes" = "#FFDBBB",
      "Stayed NS" = "#cccccc"
    )
  ) +
  theme_void() +
  labs(
    fill = "Change in LISA",
    #title = "Change in LISA Clusters in 2018 to 2020"
  )+
  geom_sf(data = hdb, fill = NA, color = "black", size = 2) 

## compare 2023 may and 2025 jan 
combined2 <- combined2 |> 
  mutate(
    change_category4 = case_when(
      lisa_t2 == "LL" & lisa_t4 == "LL" ~ "Stayed LL",
      lisa_t2 == "HH" & lisa_t4 == "HH" ~ "Stayed HH",
      lisa_t2 == "NS" & lisa_t4 == "NS" ~ "Stayed NS",
      lisa_t2 == "HH" & lisa_t4 == "LL" ~ "Change HH → LL",
      lisa_t2 == "LL" & lisa_t4 == "HH" ~ "Change LL → HH",
      TRUE ~ "Other changes"
    ),
    change_category4 = factor(
      change_category4,
      levels = c("Stayed LL", "Stayed HH", "Stayed NS", "Other changes", "Change HH → LL", "Change LL → HH")
    )
  )

library(ggplot2)

ggplot(combined2, aes(fill = change_category4)) +
  geom_sf(color = "white", lwd = 0.1) +
  scale_fill_manual(
    values = c(
      "Stayed LL" = "blue",
      "Stayed HH" = "red",
      "Change HH → LL" = "#762a83",
      "Change LL → HH" = "#1b7837",
      "Other changes" = "#FFDBBB",
      "Stayed NS" = "#cccccc"
    )
  ) +
  theme_void() +
  labs(
    fill = "Change in LISA",
    #title = "Change in LISA Clusters in 2018 to 2020"
  )+
  geom_sf(data = hdb, fill = NA, color = "black", size = 2) 



### CALCULATING ACCURACY 
library(caret)
library(irr)
library(readr)


ref1 <- read_csv("reference20251.csv")
ground_truth <- as.factor(ref1$ref_class) 
classified <- as.factor(ref1$SAMPLE_1) 

cm <- confusionMatrix(classified, ground_truth)
print(cm)                   
print(cm$table)             
print(cm$byClass)  

kappa <- kappa2(data.frame(classified, ground_truth))
print(kappa)

ref1 <- read_csv("reference2023.csv")
ground_truth <- as.factor(ref1$ref_class) 
classified <- as.factor(ref1$DN) 

cm <- confusionMatrix(classified, ground_truth)
print(cm)                  
print(cm$table)            
print(cm$byClass)  

kappa <- kappa2(data.frame(classified, ground_truth))
print(kappa)

##NOT IMPT 
# JOIN2
lst20202 <- lst20202 |> 
  dplyr::rename(lisa_t1 = lisa_category) |> 
  select(Name, geometry, lisa_t1)

# Time 2
lst20232 <- lst20232 |> 
  dplyr::rename(lisa_t2 = lisa_category) |> 
  st_drop_geometry() |> 
  select(Name, lisa_t2)

combined <- lst20202 |>
  left_join(lst20232, by = "Name")

combined3 <- combined |> 
  mutate(
    change_category = case_when(
      lisa_t1 == "LL" & lisa_t2 == "LL" ~ "Stayed LL",
      lisa_t1 == "HH" & lisa_t2 == "HH" ~ "Stayed HH",
      lisa_t1 == "HH" & lisa_t2 == "LL" ~ "Change HH → LL",
      lisa_t1 == "LL" & lisa_t2 == "HH" ~ "Change LL → HH",
      TRUE ~ "No change"
    ),
    change_category = factor(
      change_category,
      levels = c("Stayed LL", "Stayed HH", "Change HH → LL", "Change LL → HH", "No change")
    )
  )

library(ggplot2)

ggplot(combined3, aes(fill = change_category)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_manual(
    values = c(
      "Stayed LL" = "blue",
      "Stayed HH" = "red",
      "Change HH → LL" = "#762a83",
      "Change LL → HH" = "#1b7837",
      "No change" = "#cccccc"
    )
  ) +
  theme_void() +
  labs(
    fill = "Change in LISA",
    title = "Change in LISA Clusters in 2020 to 2023"
  )


##### 2023 plots 

sg <- read_sf("singaporesubzonesfinal1.shp")
sg2 <- st_transform(sg, crs = crs_3414) 

values_to_remove <- c("kml_16", "kml_28", "kml_29","kml_30","kml_318","kml_309")

# Remove rows where column_name matches kml shp numbers 
sg2 <- sg2[!(sg2$Name %in% values_to_remove), ]

meanlst2018 <- rast("Mean_LST20231013.tif")
vals <- terra::extract(meanlst2018, vect(sg2), ID=TRUE)
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
    mean = mean(vals_vec, na.rm = TRUE)
  )
})

summary_stats <- bind_rows(summary_list)
sg2$ID <- seq_len(nrow(sg2))
summary_with_name <- summary_stats %>%
  left_join(sg2 %>% select(ID, Name), by = "ID")

lst20232 <- summary_with_name
lst20232 <- st_as_sf(lst20232)
lst20232 <- st_transform(lst20232, crs = 3414)

names(lst20232)[names(lst20232) == 'mean'] <- 'LST'
nb <- poly2nb(lst20232)  

# Convert to listw object
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
moran_results <- localmoran(lst20232$LST, lw, zero.policy = TRUE)

lst20232 <- lst20232 |> 
  mutate(
    Ii = moran_results[, "Ii"],
    p_value = moran_results[, 5],
    lag_LST = lag.listw(lw, LST),  
    lisa_category = case_when(
      LST > mean(LST, na.rm = TRUE) & lag_LST > mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "HH",
      LST < mean(LST, na.rm = TRUE) & lag_LST < mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "LL",
      LST > mean(LST, na.rm = TRUE) & lag_LST < mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "HL",
      LST < mean(LST, na.rm = TRUE) & lag_LST > mean(LST, na.rm = TRUE) & p_value <= 0.05 ~ "LH",
      TRUE ~ "NS"
    ),
    lisa_category = factor(lisa_category, levels = c("HH", "LL", "HL", "LH", "NS"))
  )

#plot 

ggplot(lst20232) +
  geom_sf(aes(fill = lisa_category), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c(
      "HH" = "red",
      "LL" = "blue",
      "HL" = "#FFCCCB",
      "LH" = "skyblue",
      "NS" = "grey80"
    ),
    name = "LISA Cluster", 
    drop = FALSE
  ) +
  theme_minimal() +
  labs(
    title = "Local Moran's I (LISA) Clusters of LST",
    subtitle = "Based on 2023/10/13 LST values",
    fill = "Cluster Type"
  ) +
  theme(
    panel.grid = element_blank(),       
    axis.text = element_blank(),       
    axis.ticks = element_blank(),       
    panel.border = element_blank(),     
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )



