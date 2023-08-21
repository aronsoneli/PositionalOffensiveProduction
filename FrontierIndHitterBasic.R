# Load the required libraries
library(rvest)
library(xml2)
 
# Set the URL of the website you want to scrape
urlHitting <- "https://www.frontierleague.com/sports/bsb/2022-23/players?sort=avg&view=&pos=h&r=0"

webpage <- read_html(urlHitting)

table_data <- html_table(html_nodes(webpage, "table")[1], fill = TRUE)

desired_table <- table_data[[1]]

library(dplyr)

colnames(desired_table)[8] <- "Doubles"

colnames(desired_table)[9] <- "Triples"
library(tidyr)

desired_table$Doubles <- as.numeric(as.character(desired_table$Doubles))
desired_table$Triples <- as.numeric(as.character(desired_table$Triples))
desired_table$hr <- as.numeric(as.character(desired_table$hr))

# Replace NA with zero in the specific columns
desired_table$Doubles[is.na(desired_table$Doubles)] <- 0
desired_table$Triples[is.na(desired_table$Triples)] <- 0
desired_table$hr[is.na(desired_table$hr)] <- 0



desired_table$Singles <- desired_table$h - desired_table$Doubles - desired_table$Triples - desired_table$hr

FrontierHittersInd <- desired_table

FrontierHittersInd <- FrontierHittersInd[-119, ]

FrontierHitters23 <- read.csv("~/Downloads/frontier_all_hitters23.csv")
FrontierHitters23 <- FrontierHitters23[!(FrontierHitters23$NAME == "TOTALS" | grepl("Tie-breaker", FrontierHitters23$NAME)), ]
FrontierHitters23 <- FrontierHitters23 %>%
  filter(PA > 100, !grepl("Team Totals", NAME))

# Load the required library
library(ggplot2)
library(plotly)
# Assuming you have a data frame named "FrontierHittersInd"

# Create the scatter plot
# ggplot(FrontierHittersInd, aes(x = Singles, y = bb)) +
#   geom_point() +
#   labs(x = "Singles", y = "bb", title = "title")

# breaks <- c(100, 200, 300, 400, Inf)
# labels <- c("100-199", "200-299", "300-399", "400+")
# 
# # Create a new variable 'PA_range' to store the range labels for each plate appearance
# FrontierHitters23$PA_range <- cut(FrontierHitters23$PA, breaks = breaks, labels = labels, include.lowest = TRUE)
# 
# # Create the scatter plot with different shapes based on the 'PA_range' variable
# ggplot(FrontierHitters23, aes(x = wOBA, y = oWAR, shape = PA_range)) +
#   geom_point(size = 4) +
#   labs(x = "wOBA", y = "oWAR", title = "Offensive WAR vs Weighted on Base Average") +
#   scale_shape_manual(name = "Plate Appearances", values = c(16, 17, 15, 3, 4)) +
#   guides(shape = guide_legend(override.aes = list(size = 4)))  # Optional: Adjust shape size


# Categorizing Positions
FrontierHitters23 <- FrontierHitters23 %>%
  mutate(Position = case_when(
    grepl("1B|2B|3B|SS|INF", POS) ~ "Infielder",
    grepl("OF", POS) ~ "Outfielder",
    grepl("C", POS) ~ "Catcher",
    TRUE ~ "Utlity"
  ))
library(png)
image_path <- "~/Downloads/FrontierLeagueLogo.png"  
image <- readPNG(image_path)
#install.packages("grid")
library(grid)

p <- ggplot(FrontierHitters23, aes(x = wOBA, y = oWAR, color = Position)) +
  geom_point() +
  labs(x = "wOBA", y = "oWAR", title = "Offensive Production By Position") +
  scale_color_discrete(name = "Position") + theme_classic()

# Create a graphical object for the image
image_grob <- rasterGrob(image, width = unit(1.3, "in"), height = unit(1.3, "in"))

# Combine the scatterplot and image using annotation_custom
combined_plot <- p +
  annotation_custom(image_grob, xmin = .25, xmax = .35, ymin = 4.2, ymax = 5.5)

# Print the combined plot
print(combined_plot)



# plot_ly(data = FrontierHitters23, x = ~wOBA, y = ~oWAR, text = ~NAME, type = "scatter", mode = "markers") %>%
#   layout(title = "Offensive WAR vs Weighted on Base Average",
#          xaxis = list(title = "wOBA"),
#          yaxis = list(title = "oWAR"),
#          hovermode = "closest")

