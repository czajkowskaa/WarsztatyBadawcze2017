library("dplyr")
library("ggplot2")

plot_df <- read.csv("auc_plot.csv")

ggplot(plot_df, aes(x, y)) +
  geom_line(aes(colour = type)) +
  labs(title = "Krzywa ROC", x = "False positive rate", y = "True positive rate", colour = "Metoda i model") +
  scale_colour_manual(labels = c("Lasy losowe, regresja logistyczna", 
                                 "Lasy losowe, krokowy", 
                                 "Regresja logistyczna, regresja logistyczna", 
                                 "Regresja logistyczna, krokowy"), 
                      values = c("black", "blue2", "green3", "red2"))
