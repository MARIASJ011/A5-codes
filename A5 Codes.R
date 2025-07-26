# -------------------------------------------------------------
#  R Script: PCA, Factor Analysis, Clustering, MDS, Conjoint
# -------------------------------------------------------------

# ========================
# Setup and Libraries
# ========================


# Install required packages (if not installed)
packages <- c("fitdistrplus", "ggplot2", "dplyr", "ggrepel", "readxl", "AER", "psych", "FactoMineR", 
              "factoextra", "GPArotation", "corrplot", "cluster", "NbClust", "readr", "stats")
installed <- rownames(installed.packages())
lapply(setdiff(packages, installed), install.packages)

# Load libraries
lapply(packages, library, character.only = TRUE)

# ========================
# PCA & Factor Analysis
# ========================
survey_data <- read.csv("D:\\Data\\Survey.csv")
numeric_data <- na.omit(survey_data[sapply(survey_data, is.numeric)])
scaled_data <- scale(numeric_data)

# PCA
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
summary(pca_result)
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_biplot(pca_result, repel = TRUE)
fviz_pca_var(pca_result, col.var = "contrib", gradient.cols = c("blue", "orange", "red"), repel = TRUE)

# Factor Analysis
fa.parallel(scaled_data, fa = "both", n.iter = 100)
fa_result <- fa(scaled_data, nfactors = 3, rotate = "varimax")
print(fa_result)
fa.diagram(fa_result)
corrplot(cor(scaled_data), method = "circle")

# ========================
# Cluster Analysis
# ========================
background_vars <- survey_data %>% select(Sex, Age, Occupation, City, Income)
background_vars <- na.omit(background_vars)
background_encoded <- model.matrix(~ . -1, data = background_vars)
background_scaled <- scale(background_encoded)
fviz_nbclust(background_scaled, kmeans, method = "wss") + ggtitle("Elbow Method for Optimal Clusters")
set.seed(123)
kmeans_result <- kmeans(background_scaled, centers = 3, nstart = 25)
survey_data$Cluster <- as.factor(kmeans_result$cluster)
table(survey_data$Cluster)
aggregate(background_encoded, by = list(Cluster = survey_data$Cluster), mean)
fviz_cluster(kmeans_result, data = background_scaled, geom = "point", ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal())

# ========================
# Multidimensional Scaling (MDS)
# ========================
icecream <- read.csv("D:\\Data\\icecream.csv")
rownames(icecream) <- icecream$Brand
icecream <- icecream[, -1]
icecream <- icecream %>% mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
diss_matrix <- dist(icecream)
mds_result <- cmdscale(diss_matrix, k = 2, eig = TRUE)
mds_coords <- as.data.frame(mds_result$points)
colnames(mds_coords) <- c("Dim1", "Dim2")
mds_coords$Brand <- rownames(mds_coords)

ggplot(mds_coords, aes(x = Dim1, y = Dim2, label = Brand)) +
  geom_point(size = 4, color = "blue") +
  geom_text(vjust = -0.5, hjust = 0.5) +
  theme_minimal() +
  ggtitle("MDS Plot of Ice Cream Brands")

# ========================
# Conjoint Analysis
# ========================
pizza_data <- read.csv("D:\\Data\\pizza_data.csv", stringsAsFactors = TRUE)
pizza_data$rating <- max(pizza_data$ranking) + 1 - pizza_data$ranking
cols <- c("price", "weight", "crust", "cheese", "size", "toppings", "spicy")
pizza_data[cols] <- lapply(pizza_data[cols], as.factor)
design_matrix <- model.matrix(~ price + weight + crust + cheese + size + toppings + spicy - 1, data = pizza_data)
model <- lm(pizza_data$rating ~ design_matrix)
utilities <- coef(model)[-1]
print("Part-worth Utilities:")
print(utilities)

attribute_prefixes <- c("design_matrixprice", "design_matrixweight", "design_matrixcrust", "design_matrixcheese", "design_matrixsize", "design_matrixtoppings", "design_matrixspicy")

get_importance <- function(utilities, prefix) {
  levels <- grep(paste0("^", prefix), names(utilities), value = TRUE)
  if (length(levels) > 0) {
    range <- max(utilities[levels], na.rm = TRUE) - min(utilities[levels], na.rm = TRUE)
  } else {
    range <- 0
  }
  return(range)
}

importance_raw <- sapply(attribute_prefixes, function(attr) get_importance(utilities, attr))
importance_percent <- round(100 * importance_raw / sum(importance_raw), 2)
names(importance_percent) <- c("price", "weight", "crust", "cheese", "size", "toppings", "spicy")
print("Attribute Importance (%):")
print(importance_percent)

# Plot utilities
utility_df <- data.frame(AttributeLevel = names(utilities), Utility = as.numeric(utilities))
ggplot(utility_df, aes(x = reorder(AttributeLevel, Utility), y = Utility)) +
  geom_col(fill = "#4682B4") +
  coord_flip() +
  labs(title = "Part-Worth Utilities", x = "Attribute Level", y = "Utility Score")

# Plot importance
importance_df <- data.frame(Attribute = names(importance_percent), Importance = as.numeric(importance_percent))
ggplot(importance_df, aes(x = reorder(Attribute, Importance), y = Importance)) +
  geom_col(fill = "#FFA07A") +
  coord_flip() +
  labs(title = "Attribute Importance (%)", x = "Attribute", y = "Importance")
