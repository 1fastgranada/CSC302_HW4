# Q3 Please run the following code and observe how the parameters are used in the resulting figure. Then perform the steps
#    in the following bullet points. Notice that we are getting the data frame, Cars93, from MASS module. So, we’re not
#    reading a dataset for this question.

#    (a) Use "lm", "glm", "gam" methods in the geom_smooth() function to create three figures.
#    (b) Set the se parameter to TRUE to show the standard error (shaded area around the fitted line)
#    (c) For every method above change the color of the line with the following color codes: #8fe388, #fe8d6d, #7c6bea
#    (d) Please search for the method to add a title to your ggplot figure and add titles for each figure to indicate the method that you used for smoothing.
#    (e) Please search for the theme() function for ggplot and change the font size of the titles to 14 and match their colors with the line colors you used above.

cars93 <- MASS::Cars93
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "#0072B2") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")

# lm method
plot_lm <- ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "lm", formula = y ~ x, color = "#8fe388") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  ggtitle("Linear Model (lm)") +
  theme(plot.title = element_text(size = 14, color = "#8fe388"))

# glm method
plot_glm <- ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "glm", formula = y ~ x, color = "#fe8d6d") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  ggtitle("Generalized Linear Model (glm)") +
  theme(plot.title = element_text(size = 14, color = "#fe8d6d"))

# gam method
plot_gam <- ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "gam", formula = y ~ s(x), color = "#7c6bea") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  ggtitle("Generalized Additive Model (gam)") +
  theme(plot.title = element_text(size = 14, color = "#7c6bea"))

# Display the plots
print(plot_lm)
print(plot_glm)
print(plot_gam)


##############################
# Q4 Please inspect the following code which can be also found in TimeSeries_Trends.R and try to run how it generates
#    three time series in a single plot. Then, modify the start date and the manual coloring as you want to get a different
#    version of the chart. Please indicate what you changed and submit the figure you created as a response to this question.

# Load the lubridate package because ymd() caused an error without it
library(lubridate)

# Load data
load("I:/.shortcut-targets-by-id/1ehWwunuAo7CE1Vk2JYkUnQMmxh5pph3C/DATA/preprint_growth.rda") #please change the path if needed

preprint_growth %>% filter(archive == "bioRxiv") %>%
  filter(count > 0) -> biorxiv_growth

preprints<-preprint_growth %>% filter(archive %in%
  c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))

preprints_final <- filter(preprints, date == ymd("2017-01-01"))

ggplot(preprints) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis( #this part is for the second y axis
      breaks = preprints_final$count, #and we use the counts to position our labels
      labels = c("arXivq-bio", "PeerJPreprints", "bioRxiv"),
      name = NULL)
  ) +
  scale_x_date(name = "year",
               limits = c(ymd("2015-01-01"), ymd("2017-01-01"))) +  # Changed the start of the limit to "2015-01-01"
  scale_color_manual(values = c("#D52B1E", "#000000", "#0000FF"),   # changed the colors to red, black, and blue
                     name = NULL) +
  theme(legend.position = "none")



