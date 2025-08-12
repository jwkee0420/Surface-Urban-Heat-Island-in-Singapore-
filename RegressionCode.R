library(terra)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(dplyr)
library(car)
library(broom)
library(spdep)
library(janitor)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(RSAGA)
library(spdep)
library(sfdep)
library(exactextractr) 

# for 2025 data planning models 

sg <- read_sf("singaporesubzonesfinal1.shp")
sg2 <- st_transform(sg, crs = crs_3414) 

# remove small islands that is not the main city 
values_to_remove <- c("kml_16", "kml_28", "kml_29","kml_30","kml_318","kml_309")

# Remove rows where column_name matches kml shp numbers 
sg2 <- sg2[!(sg2$Name %in% values_to_remove), ]

# sky-view factor 
svftest <- raster("output_svf2.tif")
crs(svftest) <- "EPSG:3414"
sg2 <- st_transform(sg2, crs(svftest))
sg2$svf_weighted_mean <- exact_extract(svftest, sg2, fun = 'mean')

#building volume, height, fraction and area 
bvol <- rast("WSF3D_V02_BuildingVolume.tif")
bheight <- rast("WSF3D_V02_BuildingHeight.tif")
bfrac <- rast("WSF3D_V02_BuildingFraction.tif")
barea <- rast("WSF3D_V02_BuildingArea.tif")

sg2_v <- vect(sg2)
sg2_v_proj <- project(sg2_v, crs(bvol)) 

bvol_crop <- crop(bvol, sg2_v_proj)
bheight_crop <- crop(bheight, sg2_v_proj)
bfrac_crop <- crop(bfrac, sg2_v_proj)
barea_crop <- crop(barea, sg2_v_proj)
sg2$bvol_weighted_mean <- exact_extract(bvol_crop, sg2, fun = 'mean')
sg2$bheight_weighted_mean <- exact_extract(bheight_crop, sg2, fun = 'mean')
sg2$bfrac_weighted_mean <- exact_extract(bfrac_crop, sg2, fun = 'mean')
sg2$barea_weighted_mean <- exact_extract(barea_crop, sg2, fun = 'mean')

# Housing Density 
resi <- read_sf("fixedlanduse2019.shp")
resi <- resi %>% filter(LU_DESC == 1)
resi <- subset(resi, select = -Name)
resi <- st_make_valid(resi)
resi <- st_transform(resi, 3414)
resiinhdb <- st_intersection(resi, sg2)
resiinhdb <- resiinhdb %>%
  mutate(resi_area = st_area(.))

resiinhdb2 <- clean_names(resiinhdb)
names(resiinhdb2)
geom <- st_geometry(resiinhdb2)
# Rename columns using base R (since sf might mess up rename)
names(resiinhdb2)[names(resiinhdb2) == "name"] <- "SUBZONE_N"
# Reset geometry to the sf object
resiinhdb2 <- st_set_geometry(resiinhdb2, geom)

df <- resiinhdb2 %>% st_drop_geometry()

table(df$subzone_n)
df %>%
  dplyr::group_by(subzone_n) %>%
  dplyr::summarise(count = dplyr::n())

density_df <- df %>%
  dplyr::group_by(subzone_n) %>%    # or whatever your district identifier column is called
  dplyr::summarise(total_resi_area = sum(resi_area, na.rm = TRUE))

sg2$area1 <- st_area(sg2)

sg2 <- sg2 %>%
  left_join(density_df, by = c("SUBZONE_N" = "subzone_n")) %>%
  mutate(resi_density = total_resi_area / area1)

sg2$resiprop <- as.numeric(sg2$resi_density)

#impervious proportion
library(exactextractr)
imperv <- rast("2023classification.tif")
r_bin <- ifel(imperv == 3, 1, 0)
plot(r_bin, col = c("lightgreen", "gray20"), legend = TRUE,
     main = "Impervious Surface (1 = Impervious)")
sg2$impervious_prop <- exact_extract(r_bin, sg2, 'mean')

#road density 
road <- read_sf("2024roads.shp")
road <- st_transform(road, 3414)
roadinhdb <- st_intersection(road, sg2)

df <- roadinhdb %>% st_drop_geometry()

df %>%
  dplyr::group_by(SUBZONE_N) %>%
  dplyr::summarise(count = dplyr::n())

density_df <- df %>%
  dplyr::group_by(SUBZONE_N) %>%    # or whatever your district identifier column is called
  dplyr::summarise(total_length = sum(length, na.rm = TRUE))

sg2 <- sg2 %>%
  left_join(density_df, by = c("SUBZONE_N" = "SUBZONE_N")) %>%
  mutate(
    area_m2 = as.numeric(st_area(geometry)),        # area in m²
    road_density = total_length / area_m2           # length per m²
  )

#gross plot ratio 
gpr <- read_sf("fixedlanduse2019.shp")
gpr$gpr_num <- suppressWarnings(as.numeric(gpr$GPR))
landuse_clean <- gpr[!is.na(gpr$gpr_num), ]
landuse_clean <- st_transform(landuse_clean, crs(sg2))
landuse_vect <- vect(landuse_clean)
imperv <- project(imperv, crs(landuse_vect))
template_raster <- imperv
gpr_raster <- rasterize(landuse_vect, template_raster, field = "gpr_num")
hdb_vect <- vect(sg2)
hdb_vect <- project(vect(sg2), crs(gpr_raster))
mean_gpr <- terra::extract(gpr_raster, hdb_vect, fun = mean, na.rm = TRUE)
sg2$mean_gpr <- mean_gpr$gpr_num 

#NDVI 
ndvi <- rast("NDVI_sg2023.tif")
target_crs <- "EPSG:3414"
ndvi_proj <- project(ndvi, target_crs)
ndvi[ndvi < -1 | ndvi > 1] <- NA
prop_ndvi <- terra::extract(ndvi, sg2, fun=mean, na.rm = TRUE)[,2]
sg2$ndvi <- prop_ndvi

#NDBI 
ndbi <- rast("NDBI_sg2023.tif")
ndbi <- project(ndbi, target_crs)
mean_ndbi <- terra::extract(ndbi, sg2, fun = mean, na.rm = TRUE)[,2]
sg2$ndbi <- mean_ndbi

sg3 <- sg2 %>% select(-Name, -total_resi_area,-resi_density,-total_length,-area1, -area_m2,-barea_weighted_mean)

#LST 
meanlst2018 <- rast("20230522lstfinal.tif")
vals <- terra::extract(meanlst2018, vect(sg3), ID=TRUE)
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
sg3$ID <- seq_len(nrow(sg3))
sg8 <- sg3 %>%
  left_join(summary_stats %>% select(mean, ID), by = "ID")

attrs <- st_drop_geometry(sg4)
# Filter complete cases
sg5 <- sg4[complete.cases(attrs) & apply(attrs, 1, function(x) all(!is.nan(x))), ]

sg6 <- st_drop_geometry(sg5)

#Pearson Correlation 
cor.test(sg6$mean, sg6$svf_weighted_mean, method = "pearson")
cor.test(sg6$mean, sg6$bvol_weighted_mean, method = "pearson")
cor.test(sg6$mean, sg6$bheight_weighted_mean, method = "pearson")
cor.test(sg6$mean, sg6$bfrac_weighted_mean, method = "pearson")
cor.test(sg6$mean, sg6$resiprop, method = "pearson")
cor.test(sg6$mean, sg6$impervious_prop, method = "pearson")
cor.test(sg6$mean, sg6$road_density, method = "pearson")
cor.test(sg6$mean, sg6$mean_gpr, method = "pearson")
cor.test(sg6$mean, sg6$ndvi, method = "pearson")
cor.test(sg6$mean, sg6$ndbi, method = "pearson")


#Prepare for OLS and MGWR by using dummary variables for GPR and zero for resiprop - sg4 
sg8 <- sg8 %>%
  mutate(
    GPR_missing = ifelse(is.na(mean_gpr), 1, 0),
    GPR = ifelse(is.na(mean_gpr), 0, mean_gpr) 
  )
sg8 <- sg8 %>%
  mutate(resiprop = ifelse(is.na(resiprop), 0, resiprop))
sg8 <- clean_names(sg8)

sg8 <- sg8 %>% filter(!is.na(svf_weighted_mean))
sg8 <- sg8 %>% filter(!is.na(bfrac_weighted_mean))

#linear model  
linearmodel <- lm(
  formula = mean ~ svf_weighted_mean + bfrac_weighted_mean + resiprop+ impervious_prop+road_density+GPR+ gpr_missing + ndvi +ndbi,
  data = sg8
)


summary(linearmodel)
vif(linearmodel)
bptest(linearmodel)
shapiro.test(residuals(linearmodel))
plot(linearmodel, which = 1)

###### TESTING FOR CORRELATION BETWEEN VARIABLES - no signficant results; keep all variables 
vars <- sg8[, c("svf_weighted_mean","bfrac_weighted_mean","resiprop","impervious_prop",
               "road_density","GPR","gpr_missing","ndvi","ndbi")]
vars <- st_drop_geometry(vars)   
# Compute correlation matrix
cor_matrix <- cor(vars, use = "pairwise.complete.obs", method = "pearson")
round(cor_matrix, 2)
######

#Season model 
##COMBINED DATA ACCOUNTED WITH SEASON
combined_data20235 <- bind_rows(
  sg8 %>% mutate(Season = "2023"),
  sg4 %>% mutate(Season = "2025")
)
linearmodel3 <- lm(
  formula = mean ~ Season * svf_weighted_mean + Season * bfrac_weighted_mean + Season * resiprop+ Season * impervious_prop+ Season * road_density+ Season *GPR+ Season *gpr_missing + Season *ndvi +Season *ndbi,
  data = combined_data20235
)

#standardised coeffs 
standardised_model <- lm.beta(linearmodel3)
summary(standardised_model)
aic_value <- AIC(linearmodel3)
print(aic_value)

# to calculate mean min max of season variables 
combined_data20235 <- combined_data20235 %>%
  mutate(
    Season2025 = ifelse(Season == "2025", 1, 0),
    Season2025_svf = Season2025 * svf_weighted_mean,
    Season2025_bfrac = Season2025 * bfrac_weighted_mean, 
    Season2025_resi = Season2025 * resiprop,
    Season2025_imperv = Season2025 * impervious_prop,
    Season2025_road = Season2025 * road_density,
    Season2025_gpr = Season2025 * GPR,
    Season2025_gprmiss = Season2025 * gpr_missing,
    Season2025_ndvi = Season2025 * ndvi,
    Season2025_ndbi = Season2025 * ndbi
  ) 

vars2 <- combined_data20235[, c("Season2025","Season2025_svf","Season2025_bfrac","Season2025_resi",
                "Season2025_imperv","Season2025_road","Season2025_gpr","Season2025_gprmiss","Season2025_ndvi","Season2025_ndbi")]
vars2 <- st_drop_geometry(vars2) 
summary_stats <- data.frame(
  Variable = names(vars2),
  Min = sapply(vars2, function(x) min(x, na.rm = TRUE)),
  Max = sapply(vars2, function(x) max(x, na.rm = TRUE)),
  Mean = sapply(vars2, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(vars2, function(x) sd(x, na.rm = TRUE))
)
summary_stats <- summary_stats %>% mutate(across(where(is.numeric), ~round(.x, 3)))
summary_stats

###
summary_stats <- data.frame(
  Variable = names(vars),
  Min = sapply(vars, function(x) min(x, na.rm = TRUE)),
  Max = sapply(vars, function(x) max(x, na.rm = TRUE)),
  Mean = sapply(vars, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(vars, function(x) sd(x, na.rm = TRUE))
)

summary_stats <- summary_stats %>% mutate(across(where(is.numeric), ~round(.x, 3)))
summary_stats

library(car)
vif_values <- vif(linearmodel)
barplot(vif_values, col = "skyblue", main = "Variance Inflation Factor (VIF)")
bp <- barplot(vif_values, col = "skyblue", main = "Variance Inflation Factor (VIF)", 
              xaxt = "n")  

# Add labels at 45 degrees
text(x = bp, y = par("usr")[3] - 0.1, labels = names(vif_values), 
     srt = 45, adj = 1, xpd = TRUE)

sgclean <- st_drop_geometry(sg4)
cor_matrix <- cor(sgclean[c('svf_weighted_mean','bvol_weighted_mean','bheight_weighted_mean','bfrac_weighted_mean','resiprop','impervious_prop','road_density','GPR','gpr_missing','ndvi','ndbi')])
image(cor_matrix, main = "Correlation Matrix", col = colorRampPalette(c("blue", "white", "red"))(20))


# due to high VIF of building vol and building height, they are removed 
# check distribution 
library(ggplot2)
library(reshape2)
sg4_df <- st_drop_geometry(sg4)

# Select only the columns you need
vars <- c("mean","svf_weighted_mean","bfrac_weighted_mean","resiprop","impervious_prop",
          "road_density","GPR","gpr_missing","ndvi","ndbi")
sg4_df <- sg4_df %>% select(all_of(vars))

# Melt to long format
df_long <- melt(sg4_df)

# Plot
ggplot(df_long, aes(x = value)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "black", bins = 30, alpha = 0.6) +
  geom_density(color = "red", size = 0.5) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Distribution of Variables with Density Trend", x = "Value", y = "Density")

linearmodel1 <- lm(
  formula = mean ~ svf_weighted_mean + bfrac_weighted_mean + resiprop+ impervious_prop+road_density+GPR+ gpr_missing + ndvi +ndbi,
  data = sg4
)

standardised_model <- lm.beta(linearmodel1)
aic_value <- AIC(standardised_model)
print(aic_value)

aic_value <- AIC(linearmodel1)
print(aic_value)
summary(linearmodel1)

vif_values <- vif(linearmodel1)
barplot(vif_values, col = "skyblue", main = "Variance Inflation Factor (VIF)")
bp <- barplot(vif_values, col = "skyblue", main = "Variance Inflation Factor (VIF)", 
              xaxt = "n")  # suppress x axis labels

# Add labels at 45 degrees
text(x = bp, y = par("usr")[3] - 0.1, labels = names(vif_values), 
     srt = 45, adj = 1, xpd = TRUE)


#Mixed Geographically Weighted Regression
library(GWmodel)
coordsW <- sg4%>%
  st_centroid()%>%
  st_geometry()
coordsW2 <- st_coordinates(coordsW)
clean5 <- cbind(sg4,coordsW2)

y_var <- "mean"
x_vars <- c('svf_weighted_mean','bfrac_weighted_mean','resiprop','impervious_prop','road_density','GPR','gpr_missing','ndvi','ndbi')

formula_all_local <- as.formula(paste(y_var, "~", paste(x_vars, collapse = " + ")))
bw_all_local <- bw.gwr(formula_all_local, data = clean5, approach = "AICc")
gwr_all_local <- gwr.basic(formula_all_local, data = clean5, bw = bw_all_local)
aicc_all_local <- gwr_all_local$GW.diagnostic$AICc
cat("All-local AICc:", aicc_all_local, "\n")

y_var <- "mean"
x_vars <- c("svf_weighted_mean", "bfrac_weighted_mean", "resiprop",
            "impervious_prop", "road_density", "gpr_missing",
            "ndvi", "ndbi", "GPR")
# Variables (all except GPR are global)
global_vars <- setdiff(x_vars, "svf_weighted_mean")

library(GWmodel)

#z-score first 
clean7 <- st_drop_geometry(clean5)
numeric_cols <- names(clean7)[sapply(clean7, is.numeric)]
numeric_cols <- setdiff(numeric_cols, c("X", "Y","ID","gpr_missing"))

clean7[numeric_cols] <- lapply(clean7[numeric_cols], function(x) as.numeric(scale(x)))
clean7 <- st_as_sf(bind_cols(clean7, geometry = st_geometry(clean5)))

formula_mixed <- mean ~ svf_weighted_mean + bfrac_weighted_mean + resiprop + impervious_prop + road_density + gpr_missing + ndvi + ndbi + GPR
fixed_vars <- c("ndbi")

bw <- bw.gwr(formula = mean ~ svf_weighted_mean + bfrac_weighted_mean + resiprop + impervious_prop + road_density + gpr_missing + ndvi +GPR + ndbi, 
             data = clean7, approach = "AICc")

# Run BASIC gwr for reference AICc
mixed_model <- gwr.basic(formula = formula_mixed,
                         data = clean7,
                         bw = bw,
                         #fixed.vars = fixed_vars)
)
mixed_model


#FINAL MGWR MODEL after choosing global and local variables 
#I did not write a loop here because I was lazy but the code below is edited to change fixed_vars to test AICc for each variable
formula_mixed <- mean ~ svf_weighted_mean + bfrac_weighted_mean + resiprop + impervious_prop + road_density + gpr_missing + ndvi + ndbi + GPR
fixed_vars <- c("GPR","svf_weighted_mean","road_density","ndvi")

bw <- bw.gwr(formula = mean ~ bfrac_weighted_mean + resiprop + impervious_prop + gpr_missing + ndbi, 
             data = clean7, approach = "AICc")

# Run mixed GWR - no approach argument here
mixed_model <- gwr.mixed(formula = formula_mixed,
                         data = clean7,
                         bw = bw,
                         fixed.vars = fixed_vars)
mixed_model

coef_matrix <- as.data.frame(mixed_model$SDF)

# Check column names
names(coef_matrix)

# Summarize local coefficients (for the spatially varying variables)
library(dplyr)
coef_summary <- coef_matrix %>%
  select(bfrac_weighted_mean_L, resiprop_L, impervious_prop_L, gpr_missing_L, ndbi_L, Intercept_L) %>%
  summarise_all(list(
    mean = ~mean(.),
    sd = ~sd(.),
    min = ~min(.),
    max = ~max(.)
  ))

print(coef_summary)

# Because MGWR function does not compute r2 and adj.r2 I have to do it manually
coefs <- sf::st_drop_geometry(mixed_model$SDF)
coef_cols <- c("Intercept_L", "svf_weighted_mean_F", "bfrac_weighted_mean_L", "resiprop_L",
               "impervious_prop_L", "road_density_F", "GPR_F", "gpr_missing_L", "ndvi_F", "ndbi_L")
coef_matrix <- as.matrix(coefs[, coef_cols])

new_names <- c("Intercept", "svf_weighted_mean", "bfrac_weighted_mean", "resiprop",
               "impervious_prop", "road_density", "GPR", "gpr_missing", "ndvi", "ndbi")

colnames(coef_matrix) <- new_names

X <- model.matrix(~ svf_weighted_mean + bfrac_weighted_mean + resiprop + impervious_prop + 
                    road_density + GPR + gpr_missing + ndvi + ndbi, data = clean5)
fitted_vals <- rowSums(X * coef_matrix)

# Actual LST values 
y <- clean5$mean

# Residuals
residuals <- y - fitted_vals

# R squared
ss_res <- sum(residuals^2)
ss_tot <- sum((y - mean(y))^2)
r2 <- 1 - (ss_res / ss_tot)

# Adjusted R squared
n <- length(y)
p <- ncol(X) - 1  
adj_r2 <- 1 - (1 - r2) * (n - 1) / (n - p - 1)

cat("R squared:", r2, "\n")
cat("Adjusted R squared:", adj_r2, "\n")

#visualise coefs
coef_df <- as.data.frame(mixed_model$SDF)
coef_sf <- cbind(clean5, coef_df)

ggplot(coef_sf) +
  geom_sf(aes(fill = bfrac_weighted_mean_L), color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(title = "Coefficient: Building Fraction",
       fill = "Coefficient")

ggplot(coef_sf) +
  geom_sf(aes(fill = resiprop_L), color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(title = "Coefficient: Residential Proportion",
       fill = "Coefficient")

ggplot(coef_sf) +
  geom_sf(aes(fill = impervious_prop_L), color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(title = "Coefficient: imperv",
       fill = "Coefficient")

ggplot(coef_sf) +
  geom_sf(aes(fill = gpr_missing_L), color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(title = "Coefficient: gpr missing",
       fill = "Coefficient")

ggplot(coef_sf) +
  geom_sf(aes(fill = ndbi_L), color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(title = "Coefficient: ndbi",
       fill = "Coefficient")
