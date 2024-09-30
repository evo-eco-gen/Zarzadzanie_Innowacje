#### STATISTICAL ANNALYSIS OF PREDICTORS OF INNOVATION AT THE COUNTRY LEVEL
#### ATTACHMENT TO THE FINAL ESSAY FOR SPIW IX

# read in the data.
inn<-read.csv("leadership_innovation_trim.csv", header=TRUE)
attach(inn)
summary(inn)
autocratic <- as.numeric(autocratic)
autonomous <- as.numeric(autonomous)
self_protective <- as.numeric(self_protective)

# Examine the distribution of the variables.
shapiro.test(log(PPT+0.0001))
hist(PPT_normalised)
boxplot(PPT_normalised)
shapiro.test(log(autonomous))

library(MASS)
boxcox_autocratic <- boxcox(lm(autocratic ~ 1))
lambda_autocratic <- boxcox_autocratic$x[which.max(boxcox_autocratic$y)]
autocratic_transformed <- (autocratic^lambda_autocratic - 1) / lambda_autocratic


# Fit the GLM: beta regression
library(betareg)

model_formulas <- list(
  betareg_1 <- betareg(PPT_scaled ~ GERD, data = inn),
  betareg_2 <- betareg(PPT_scaled ~ HDI, data = inn),
  betareg_3 <- betareg(PPT_scaled ~ RQ, data = inn),
  betareg_4 <- betareg(PPT_scaled ~ HDI + GERD + RQ, data = inn),
  betareg_5 <- betareg(PPT_scaled ~ HDI * GERD * RQ, data = inn),
  betareg_6 <- betareg(PPT_scaled ~ autocratic, data = inn),
  betareg_7 <- betareg(PPT_scaled ~ autonomous, data = inn),
  betareg_8 <- betareg(PPT_scaled ~ self_protective, data = inn),
  betareg_9 <- betareg(PPT_scaled ~ autocratic + autonomous + self_protective, data = inn),
  betareg_10 <- betareg(PPT_scaled ~ autocratic * autonomous * self_protective, data = inn),
  betareg_11 <- betareg(PPT_scaled ~ autocratic + autonomous + self_protective + HDI + GERD + RQ, data = inn),
  betareg_12 <- betareg(PPT_scaled ~ autocratic * autonomous * self_protective + HDI * GERD * RQ, data = inn)
)

# Fit the models and store the results
fitted_models <- lapply(model_formulas, function(f) betareg(f, data = inn))

# Extract AIC values for each model
aic_values <- sapply(fitted_models, AIC)

# Print AIC values and select the best model
print(aic_values)
best_model <- names(which.min(aic_values))
cat("Best model is:", best_model, "\n")

# Examine the relation for a subset of the data:
summary(lm(inn_vhigh_HDI$PPT ~ inn_vhigh_HDI$HDI + inn_vhigh_HDI$GERD + inn_vhigh_HDI$RQ))

# Create Plot 1:
# Load necessary libraries
library(ggplot2)
library(cowplot)
library(ggrepel)

# Create the first plot (self_protective)
self <- ggplot(inn, aes(x = self_protective, y = PPT+0.001, size = GERD)) +
  geom_point(data = subset(inn, CEE == "no"), aes(color = "Non-CEE"), alpha = 0.7) +
  geom_point(data = subset(inn, CEE == "yes"), aes(color = "CEE"), alpha = 0.7) +
  scale_color_manual(values = c("CEE" = "green", "Non-CEE" = "blue")) +
  labs(x = "Wskaźnik samozachowawczego zarządzania", y = "Patenty na 1000 mieszkańców", 
       color = "Europa Śr-Wsch", size = "% R&D w PKB") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "darkgrey"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'black'),
        legend.background = element_rect(fill = "lightgrey", color = NA),
        legend.key = element_rect(color = "gray", fill = "white"),
        legend.title = element_text(color = "black"),
        legend.text = element_text(color = "black"),
        axis.text.x = element_text(size = 8)  # Make x-axis labels smaller
  ) +
  geom_text_repel(data = subset(inn, COUNTRIES == "Poland"), 
                  aes(label = "Polska"), 
                  nudge_x = 0.35, nudge_y = 0.5, 
                  size = 5, color = "yellow", 
                  segment.color = "grey", segment.size = 0.5)

# Create the second plot (autonomous) without the y-axis label and without the legend
autonomous <- ggplot(inn, aes(x = autonomous, y = PPT+0.001, size = GERD)) +
  geom_point(data = subset(inn, CEE == "no"), aes(color = "Non-CEE"), alpha = 0.7) +
  geom_point(data = subset(inn, CEE == "yes"), aes(color = "CEE"), alpha = 0.7) +
  scale_color_manual(values = c("CEE" = "green", "Non-CEE" = "blue")) +
  labs(x = "Wskaźnik autonomicznego zarządzania", y = NULL,  # Remove y-axis label
       color = "Europa Śr-Wsch", size = "% R&D w PKB") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "darkgrey"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'black'),
        legend.position = "none",  # Suppress the legend in the second plot
        axis.text.x = element_text(size = 8)  # Make x-axis labels smaller
  ) +
  geom_text_repel(data = subset(inn, COUNTRIES == "Poland"), 
                  aes(label = "Polska"), 
                  nudge_x = 0.35, nudge_y = 0.5, 
                  size = 5, color = "yellow", 
                  segment.color = "grey", segment.size = 0.5)

# Extract the shared legend from the first plot
legend <- get_legend(self)

# Combine the two plots side by side without the legend
combined_plots <- plot_grid(
  self + theme(legend.position = "none"),  # Remove the legend from the first plot
  autonomous,  # Second plot without the y-axis label
  ncol = 2, rel_widths = c(1, 1)  # Adjust column widths as needed
)

# Create a title as a separate plot object
title <- ggdraw() + 
  draw_label("Innowacyjność gospodarki nie jest związana z kulturą zarządzania", fontface = 'bold', x = 0.5, hjust = 0.5) +
  theme(plot.margin = margin(10, 0, 0, 0))  # Adjust the margin for alignment

# Add the shared legend to the right of the second plot
final_plot <- plot_grid(
  title, 
  plot_grid(combined_plots, legend, ncol = 2, rel_widths = c(1, 0.25)),  # Same legend panel width
  ncol = 1, rel_heights = c(0.1, 1)  # Title on top
)

# Display the final plot with title, aligned legend, and smaller x-axis labels
final_plot

