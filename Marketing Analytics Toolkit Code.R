# --- INSTALL & LOAD PACKAGES -----------------------------------------
# Uncomment the install.packages() call below (run it only once)
install.packages(c("readr", "dplyr", "ggplot2", "cluster", "factoextra",
                   "openxlsx", "reshape2", "mice", "fmsb", "glue"))

library(readr)      # For reading CSV files
library(dplyr)      # Data manipulation
library(ggplot2)    # Data visualization
library(cluster)    # Clustering algorithms
library(factoextra) # Clustering evaluation and visualization
library(openxlsx)   # Exporting results to Excel
library(reshape2)   # Heatmap visualization
library(mice)       # Missing value imputation
library(fmsb)       # Radar charts
library(glue)       # String interpolation
# ---------------------------------------------------------------------


# --- DEFINE COLOR PALETTE FOR MARKETING VISUALS -----------------------
color_palette <- list(
  cluster = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),  # Colors for clustering groups
  fast    = "#1f77b4",      # Blue for plan choice or “fast” response
  moderate= "#ff7f0e",      # Orange for alternative categories
  speed   = "#2ca02c",      # Green used in conjoint analysis plots
  points  = "#d62728",      # Red for scatterplot points
  line    = "#9467bd"       # Purple for regression line
)
# ---------------------------------------------------------------------


# --- DATA LOADING & INSPECTION ---------------------------------------
# Select your "Marketing file.csv" interactively or specify its path.
df <- readr::read_csv(file.choose(), show_col_types = FALSE)

# Inspect the column names
cat("Available columns:\n")
print(names(df))
# Expected columns:
# "MemberID", "UsagePattern", "GymVisitsPerMonth", "PlanChosen", "PlanPrice",
# "FeatureScore", "PreferredPrice", "PreferredClassTime", "PreferredPerk",
# "CampaignType", "SignUpLikelihood", "SatisfactionScore"
# ---------------------------------------------------------------------


# --- UPDATE COLUMN TYPES ---------------------------------------------
# Convert the columns to appropriate types.
# Numerical variables:
#   GymVisitsPerMonth, PlanPrice, FeatureScore, PreferredPrice, 
#   SignUpLikelihood, SatisfactionScore
# Categorical variables:
#   UsagePattern, PlanChosen, PreferredClassTime, PreferredPerk, CampaignType
df <- df %>%
  mutate(across(c(GymVisitsPerMonth, PlanPrice, FeatureScore, PreferredPrice, 
                  SignUpLikelihood, SatisfactionScore), as.numeric)) %>%
  mutate(across(c(UsagePattern, PlanChosen, PreferredClassTime, PreferredPerk, CampaignType), as.factor))
# ---------------------------------------------------------------------


# --- CLUSTER ANALYSIS -------------------------------------------------
# We'll cluster members based on usage and preference attributes.
# Define clustering variables (MemberID omitted)
cluster_vars <- c("GymVisitsPerMonth", "UsagePattern", "PlanChosen", 
                  "PreferredClassTime", "PreferredPerk", "CampaignType")

# Create a dataset for clustering (convert factor variables to numeric codes)
df_selected <- df %>% 
  select(all_of(cluster_vars)) %>% 
  mutate(across(-GymVisitsPerMonth, ~ as.numeric(factor(.))))

# Standardize the data
df_scaled <- scale(df_selected)

# Set seed for reproducibility
set.seed(123)
cat("\n=== CLUSTER DETERMINATION ===\n")

# Determine the optimal number of clusters (Elbow & Silhouette methods)
elbow_plot <- fviz_nbclust(df_scaled, hcut, method = "wss", hc_method = "ward.D2") + 
  ggtitle("Elbow Method") +
  theme(plot.title = element_text(hjust = 0.5))

silhouette_plot <- fviz_nbclust(df_scaled, hcut, method = "silhouette", hc_method = "ward.D2") + 
  ggtitle("Silhouette Method") +
  theme(plot.title = element_text(hjust = 0.5))

# (If adding the two plots with patchwork gives an error, you can display them separately.)
print(elbow_plot)
print(silhouette_plot)

# Perform Hierarchical Clustering
distance <- dist(df_scaled, method = 'euclidean')
hc <- hclust(distance, method = 'ward.D2')

# Instead of a dendrogram, create an alternative cluster plot based on principal components.
cluster_plot <- fviz_cluster(list(data = df_scaled, cluster = cutree(hc, k = 4)),
                             geom = "point", ellipse.type = "convex",
                             palette = color_palette$cluster, ggtheme = theme_minimal(),
                             main = "Cluster Visualization")
print(cluster_plot)

# Create clusters: cut the tree at 4 clusters
optimal_k <- 4  
df$Cluster_Hierarchical <- as.factor(cutree(hc, k = optimal_k))
# ---------------------------------------------------------------------


# --- CLUSTER PROFILING -----------------------------------------------
cluster_profile <- df %>% 
  group_by(Cluster_Hierarchical) %>% 
  summarise(
    Size = n(),
    Proportion = n() / nrow(df) * 100,
    Avg_GymVisits = mean(GymVisitsPerMonth, na.rm = TRUE),
    Top_UsagePattern = names(sort(table(UsagePattern), decreasing = TRUE))[1],
    Top_PlanChosen = names(sort(table(PlanChosen), decreasing = TRUE))[1],
    Top_ClassTime = names(sort(table(PreferredClassTime), decreasing = TRUE))[1],
    Top_Perk = names(sort(table(PreferredPerk), decreasing = TRUE))[1],
    Top_CampaignType = names(sort(table(CampaignType), decreasing = TRUE))[1],
    .groups = 'drop'
  )
# ---------------------------------------------------------------------


# --- CHOICE MODEL ANALYSIS -------------------------------------------
# Analyze plan choice probabilities if PlanChosen and PlanPrice exist.
if (all(c("PlanChosen", "PlanPrice") %in% names(df))) {
  choice_plot <- df %>%
    count(PlanChosen, PlanPrice) %>%
    mutate(Probability = n / sum(n) * 100) %>%
    # Use glue::glue to create labels instead of str_glue
    ggplot(aes(x = glue::glue("{PlanChosen}, ${PlanPrice}"), 
               y = Probability, 
               fill = PlanChosen)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.1f%%", Probability)), vjust = -0.5) +
    labs(title = "Plan Choice Probabilities", 
         x = "Plan Option", y = "Probability (%)",
         fill = "Plan Chosen") +
    coord_flip() +
    theme_minimal()
  print(choice_plot)
} else {
  message("\nSkipping Choice Model: Required columns not found")
}
# ---------------------------------------------------------------------


# --- CONJOINT ANALYSIS -----------------------------------------------
# Use FeatureScore and PreferredPrice to mimic a conjoint analysis.
if (all(c("FeatureScore", "PreferredPrice") %in% names(df))) {
  conjoint_utility <- df %>% 
    # Convert numeric scores to factor for grouping, if desired
    mutate(FeatureScore = factor(FeatureScore),
           PreferredPrice = factor(PreferredPrice)) %>%
    group_by(FeatureScore, PreferredPrice) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    mutate(Utility = Count / sum(Count) * 100)
  
  conjoint_plot <- ggplot(conjoint_utility, 
                          aes(x = interaction(FeatureScore, PreferredPrice), 
                              y = Utility,
                              fill = FeatureScore)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.1f%%", Utility)), vjust = -0.5) +
    labs(title = "Conjoint Analysis: Feature Score vs Preferred Price",
         x = "Feature Score & Preferred Price Combination", 
         y = "Utility (%)",
         fill = "Feature Score") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(conjoint_plot)
  
  # Compute a rough importance measure by comparing ranges of utility values
  utility_ranges <- conjoint_utility %>% 
    group_by(FeatureScore) %>% 
    summarise(range_util = max(Utility) - min(Utility), .groups = 'drop')
  
  total_range <- sum(abs(utility_ranges$range_util))
  
  importance_scores <- tibble(
    Attribute = "FeatureScore", 
    Importance = if(total_range == 0) 100 else utility_ranges$range_util / total_range * 100
  )
  cat("\n=== CONJOINT ATTRIBUTE IMPORTANCE ===\n")
  print(importance_scores)
  
} else {
  cat("\nSkipping Conjoint Analysis: Required columns (FeatureScore, PreferredPrice) not found\n")
}
# ---------------------------------------------------------------------


# --- MARKET RESPONSE MODELING ----------------------------------------
# Build a linear regression model to examine what drives sign-up likelihood.
# Here, we use FeatureScore and GymVisitsPerMonth as predictors.
response_model <- lm(SignUpLikelihood ~ FeatureScore + GymVisitsPerMonth, data = df)
model_summary <- summary(response_model)

response_plot <- ggplot(df, aes(x = FeatureScore, y = SignUpLikelihood)) +
  geom_point(color = color_palette$points, alpha = 0.6) +
  geom_smooth(method = "lm", color = color_palette$line, se = TRUE) +
  labs(title = "Market Response: Sign-Up Likelihood vs Feature Score",
       x = "Feature Score", y = "Sign-Up Likelihood") +
  annotate("text", 
           x = max(as.numeric(as.character(df$FeatureScore)), na.rm = TRUE) * 0.6,
           y = max(df$SignUpLikelihood, na.rm = TRUE) * 0.9,
           label = glue::glue("FeatureScore Effect: {round(coef(response_model)['FeatureScore'], 4)}\nGymVisits Effect: {round(coef(response_model)['GymVisitsPerMonth'], 2)}\nR² = {round(model_summary$r.squared, 2)}")) +
  theme_minimal()
print(response_plot)
# ---------------------------------------------------------------------


# --- FINAL RESULTS REPORTING -----------------------------------------
cat("\n=== FINAL RESULTS ===\n")

cat("\nCLUSTER PROFILES:\n")
print(cluster_profile, n = Inf)
cat("\nInterpretation: For example, clusters with higher average gym visits may indicate highly engaged members, while differences in top plan choices and preferred class times reveal distinct member segments.\n")

cat("\nMARKET RESPONSE MODEL SUMMARY:\n")
print(model_summary)

if (model_summary$coefficients["FeatureScore", "Pr(>|t|)"] > 0.05) {
  cat("\nWARNING: FeatureScore effect is not statistically significant (p =",
      round(model_summary$coefficients["FeatureScore", "Pr(>|t|)"], 3),
      "). Consider additional predictors or non-linear terms.\n")
}
# ---------------------------------------------------------------------

