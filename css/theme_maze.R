theme_maze <- function (base_size = 14, base_family = "Source Sans Pro", cols = c("#000000", "#666666")) {
  theme(rect = element_rect(fill = "#FFFFFF", linetype = 0, colour = NA), 
        text = element_text(size = base_size, family = base_family), 
        plot.title = element_text(size = rel(1.3), hjust = 0, color = cols[1]),
        plot.subtitle = element_text(size = rel(1), hjust = 0, color = cols[1]),
        plot.caption = element_text(hjust = 0, size = rel(.8), margin = unit(c(2, 0, 0, 0), unit = "mm"), color = cols[2]),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "#777777", size = rel(0.2)), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.border = element_blank(), 
        axis.text = element_text(colour = "black"), 
        axis.title.x = element_text(hjust = 0.5, size = rel(1), margin = unit(c(2, 0, 0, 0), "mm")), 
        axis.title.y = element_text(hjust = 0.5, size = rel(1)), 
        axis.line.x = element_line(color = "#666666", size = rel(0.5)),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        legend.title=element_blank(),
        legend.text = element_text(colour = "black", size= rel(1)), 
        legend.margin = margin(0, 0, 0, 0), 
        legend.position = "right", 
        legend.key = element_rect(fill = "#FFFFFF00"))
}

theme_box_maze <- function(base_size = 12, base_family = "Source Sans Pro") {
  theme_maze(base_size, base_family) +
  theme(rect = element_rect(fill = "#ecf0f3"),
        plot.margin = unit(c(0.5, 0.25, 0.25, 0.25), "cm"))
}

colorMaze <- c("#103399", "#c5000c", "#ff9911", "#105510", "#0088d1", "#ff440e",    
               "#ffd322", "#55991c", "#88ccff", "#b0d000", "#aa11ff", "#ff11aa")

colorMazeSoft <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", 
                   "#a65628", "#f781bf")

colorMazePastel <- c("#D7F1DE", "#C9E6E0", "#BEDEE9", "#F7C9C9", "#FED8CF")

colorMazeRed <- c("#660000", "#990000", "#cc0000", "#e27373", "#e29393")

colorMazeBlue <- c("#0000a8", "#2048c8", "#3474dc", "#4898f0", "#8ab8ee")

# 600x900 png
# ggsave("file_name.svg", width =12, height = 9)
# Change line color (define color): scale_colour_manual(values=colorMaze)
# Use one color: scale_color_brewer(palette="Blues", direction=-1)
# Change bar color (define fill): scale_fill_manual(values=colorMaze)
# Change line size: geom_line(size=1.5)
# Start y-at exactly 0: scale_y_continuous(expand = c(0, 0), limits=c(0,NA))
# Change legend line size: guides(colour = guide_legend(override.aes = list(size=5)))
# Change legend position: theme(legend.position="bottom")
# Add legend next to line: use ggrepel
# No legend: scale_colour_discrete(guide = "none") 
# Change legend names: scale_color_manual(labels = c("20-49 emp ", "50-149 emp ", "150+ emp "))
# Change labels for any order and integer, scale_x_continuous(breaks=seq(2005, 2015, 2)) 
# Add titles:  labs(title=" ", subtitle= " ", caption = " ", x=" ", y=" ") 
# Add legend title: scale_color_manual(name=" ") or scale_fill_manual(name=" ")
# X axis along 0 y: geom_hline(yintercept=0, size=0.1) + theme(axis.line.x=element_line(color="gray"))
# Use object names (for example, fn)  in a title - fn should be character, use as.character(.)
#   ptit <- bquote(paste(bold("Figure "), bold(.(fn)), ". GDP per capita")); labs(title=ptit)
#   ctit <- bquote(paste(italic("Source : "), .(fn[zz])))


