theme_ludwig <-  theme_gray() +
  theme(
    #Background
    panel.background = element_rect(fill="white", linetype="blank"),
    #Axis
    axis.line = element_line(linetype = "solid", color= "#dddddd"),
    axis.title = element_text(
      color = rgb(105, 105, 105, maxColorValue = 255), size = rel(.8), face = "bold"),
    axis.text = element_text(
      color = rgb(105, 105, 105, maxColorValue = 255), size = rel(.8), face = "bold"),
    #Panel
    panel.grid.major = element_blank(), 
    panel.grid.major.y = element_line(linetype = "solid", color= "#f2f2f2"),
    panel.grid.minor = element_blank(),
    #Strip
    strip.background = element_blank(),
    strip.text = element_text(color="#333333"),
    strip.background.x = element_rect(linetype = "solid", color= "#dddddd"),
    #Legend
    legend.title = element_text(color = "#333333", size = rel(1), face = "bold"),
    legend.box.margin = margin(t = 20, r = 0, b = 0, l = 20, unit = "pt"))