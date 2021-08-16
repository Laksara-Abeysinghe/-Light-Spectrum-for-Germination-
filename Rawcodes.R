(df <- read.csv("light.csv"))

library(dplyr)
Lights <- as.factor(df$factors)
x <- list(
  df$Germinated_seeds...,
  df$Total_seedling_Length, 
  df$Total_Length, 
  df$Leaf_Length,
  df$Leaf_Width
)

models <- function(x) {
  ANOV <- vector("list", length(x))
  for (i in seq_along(x)) {
    ANOV[[i]] <- anova(lm(x[[i]] ~ Lights))
  }
  ANOV
} 

results <- models(x)
names(results) <- c( "Germinated_seeds...", "Total_seedling_Length", 
                     "Total_Length", "Leaf_Length", "Leaf_Width")
results




p_val <- vector("double", length(results))
concl <- vector("double", length(results))
for (i in seq_along(results)) {
  p_val[[i]] <- results[[i]][[5]][1]
  concl[[i]] <- ifelse(p_val[[i]] < 0.05, "Significant", "Not_Significant")
}

P_Results <- tribble(
  ~Factor,
  "Germinated_seeds...", "Total_seedling_Length", 
  "Total_Length", "Leaf_Length", "Leaf_Width"
) %>%
  mutate(P_Values = p_val, Conclusions = concl)

(P_Results)



comparisions <- function(x) {
  comp <- vector("list", length(x))
  for (i in seq_along(x)) {
    comp[[i]] <- TukeyHSD(aov(lm(x[[i]] ~ Lights)))
  }
  comp
} 
mean_com <- comparisions(x)
names(mean_com) <- c( "Germinated_seeds...", "Total_seedling_Length", 
                      "Total_Length", "Leaf_Length", "Leaf_Width")
mean_com



