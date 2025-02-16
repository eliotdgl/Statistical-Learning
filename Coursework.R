library(readxl)
happiness <- read_excel(path = "DataForFigure2.1WHR2023.xls")

# Keep only wanted columns
happiness <- happiness[, c(1,2,6,7,8,9,10,11)]


# Add regions of the world from another dataset
data_regions <- read.csv('WHR_2023.csv')
regions_world <- data_regions['region']
happiness <- cbind(happiness[, 1], regions_world, happiness[, -1])

# Rename columns
names(happiness) <- c("Country", "Region", "Happiness_score", "Logged_GDP_per_capita",
                      "Social_support", "Healthy_life_expectancy", "Freedom_to_make_life_choices",
                      "Generosity", "Perceptions_of_corruption")

View(happiness)



# Correlation Matrix (FIGURE 1)
library(corrplot)

happiness_for_cor <- happiness[complete.cases(happiness), ]
correlation_matrix <- cor(happiness_for_cor[, c('Happiness_score', 'Logged_GDP_per_capita', 'Social_support', 'Healthy_life_expectancy', 
                                                'Freedom_to_make_life_choices', 'Generosity', 'Perceptions_of_corruption')])
corrplot(correlation_matrix, method='number', type='upper')


regions = unique(happiness$Region)

custom_colors <- c('red', 'blue', 'green', 'orange', 'purple', 'cyan', 'magenta', 'brown', 'darkgreen', 'darkblue')
region_colors <- setNames(custom_colors[1:length(regions)], regions)



# Happiness scores by region
library(ggplot2)

# Boxplot of happiness scores by region (FIGURE 3)
ggplot(happiness, aes(x = Region, y = Happiness_score, fill = Region)) +
  geom_boxplot() +
  labs(x = '', y = 'Happiness score', title = 'Happiness score by region') +
  scale_fill_manual(values = region_colors) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Region"))



# Multidimensional Scaling
library('MASS')
  
  # General (FIGURE 2)
data <- happiness[, -c(1,2)] # Remove 'Country' and 'Region'

data.dist <- dist(data, method = 'euclidean')

mds <- isoMDS(data.dist)

plot(mds$points, type = 'n', xlab = 'Dimension 1', ylab = 'Dimension 2', main = 'MDS Plot for World Happiness in 2023')
text(mds$points, labels = rownames(happiness), cex = 0.9, col = region_colors[happiness$Region])
legend('bottomright', legend = regions, col = region_colors, pch = 19, title = 'Region', cex = 0.55)

# Cases of: Lebanon (#136 compared to Jordan #123) / Botswana (#132 higher gpd and social support) /
# Singapore (#25) / Israel (#4 start of war on October so maybe dataset made before as high Happiness Score != war?)


  # Happiness and GDP (FIGURE 3)
data_hg <- happiness[, c(3,4)]
View(data_hg)

data_hg.dist <- dist(data_hg, method = 'euclidean')

mds_hg <- isoMDS(data_hg.dist)

plot(mds_hg$points, type = 'n', xlab = 'Dimension 1', ylab = 'Dimension 2', main = 'MDS for Happiness and GDP in 2023')
text(mds_hg$points, labels = rownames(happiness), cex = 0.9, col = region_colors[happiness$Region])
legend('bottomleft', legend = regions, col = region_colors, pch = 19, title = 'Region', cex = 0.55)

# Same cases.


  # FMLC and PC (FIGURE 5)
data_fp <- happiness[, c(7,9)]
View(data_fp)

data_fp.dist <- dist(data_fp, method = 'euclidean')

mds_fp <- isoMDS(data_fp.dist)

plot(mds_fp$points, type = 'n', xlab = 'Dimension 1', ylab = 'Dimension 2', main = 'MDS for Freedom to make life choices and Perceptions of corruption in 2023')
text(mds_fp$points, labels = rownames(happiness), cex = 0.9, col = region_colors[happiness$Region])
legend('topleft', legend = regions, col = region_colors, pch = 19, title = 'Region', cex = 0.55)

# Cases: #25 / #56 / #58 / #82 / #137



"""
  # Happiness only
data_happiness <- happiness$Happiness Score

data_happiness.dist <- dist(data_happiness, method = 'euclidean') + 0.0001

mds_happiness <- isoMDS(data_happiness.dist)

plot(mds_happiness$points, type = 'n', xlab = 'Dimension 1', ylab = 'Dimension 2', main = 'MDS Plot for World Happiness in 2023')
text(mds_happiness$points, labels = rownames(happiness), cex = 0.9, col = region_colors[happiness$region])
"""


"""
  # GDP only
data_gdp <- happiness$Logged_GDP_per_capita

data_gdp.dist <- dist(data_gdp, method = 'euclidean') + 0.0001

mds_gdp <- isoMDS(data_gdp.dist)

plot(mds_gdp$points, type = 'n', xlab = 'Dimension 1', ylab = 'Dimension 2', main = 'MDS Plot for GDP PER CAPITA in 2023')
text(mds_gdp$points, labels = rownames(happiness), cex = 0.9, col = region_colors[happiness$region])
legend('topright', legend = regions, col = region_colors, pch = 19, title = 'Region', cex = 0.8)
"""


"""
  # Happiness and Social support
data_hsp <- happiness[, c(3,5)]
View(data_hsp)

data_hsp.dist <- dist(data_hsp, method = 'euclidean')

mds_hsp <- isoMDS(data_hsp.dist)

plot(mds_hsp$points, type = 'n', xlab = 'Dimension 1', ylab = 'Dimension 2', main = 'MDS for Happiness and Social support in 2023')
text(mds_hsp$points, labels = rownames(happiness), cex = 0.9, col = region_colors[happiness$region])
legend('bottomright', legend = regions, col = region_colors, pch = 19, title = 'Region', cex = 0.6)
"""

