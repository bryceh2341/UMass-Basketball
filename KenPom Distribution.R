library(XML)
library(rvest)
library(stringr)
library(dplyr)

setwd("C:/Users/Bryce Haase/Desktop/UMass Basketball")

theme_bryce <- function () {
  theme_minimal(base_size=12, base_family="Consolas") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

getKenPomYearData <- function(year)
{
  theUrl <- paste0("https://kenpom.com/index.php?y=", as.character(year))
  page <- read_html(theUrl)
  tables <- page %>% html_nodes("table") %>% html_table()
  data <- as.data.frame(tables[1])
  
  colnames(data) <- c("Rank", "Team", "Conf", "Record", "AdjEM", "AdjO", 
                      "AdjO_R", "AdjD", "AdjD_R", "AdjT", "AdjT_R",
                      "Luck", "Luck_R", "SoS_AdjEM", "SoS_AdjEM_R", 
                      "OppO", "OppO_R", "OppD", "OppD_R", "NC_AdjEM", "NC_AdjEM_R")
  
  data <- data %>% filter(!str_detect(Rank, "Rk")) # Remove label row
  
  data <- data %>% filter(nchar(as.character(Rank)) > 0) # Remove empty rank rows.
  data$Year = year
  
  return(data)
}

kenPomData <- getKenPomYearData(2023)
kenPomData[c("AdjEM", "AdjO", "AdjD")] <- sapply(kenPomData[c("AdjEM", "AdjO", "AdjD")], as.numeric)
kenPomData <- kenPomData %>%
  mutate(OPercentile = round(pnorm((AdjO-mean(kenPomData$AdjO))/sd(kenPomData$AdjO)), 4)*100) %>%
  mutate(DPercentile = round(pnorm(((AdjD-mean(kenPomData$AdjD))*-1)/sd(kenPomData$AdjD)), 4)*100) %>%
  arrange(desc(AdjD)) #%>%
  #slice(1:100)

ggplot(kenPomData, aes(x=Year, y=AdjD)) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="white", alpha = .5) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1.3, binwidth = 1, fill="orange", alpha = 0.5) +
  geom_point(data = . %>% filter(Team == kenPomData$Team[kenPomData$Team == "Tennessee"]),
             aes(fill = "gold"),
             size = 3,
             shape = 21,
             show.legend = FALSE) +
  geom_text_repel(data = . %>% filter(Team == kenPomData$Team[kenPomData$Team == "Tennessee"]),
                  aes(label = "Tennessee", color = "orange"),
                  show.legend = FALSE,
                  color = "orange",
                  fontface = 'bold',
                  family = "Consolas",
                  nudge_x = 0.1,
                  nudge_y = 1.5) +
  scale_y_reverse() +
  #scale_y_continuous(breaks = seq(80, 120, by = 10)) +
  theme_bryce() + 
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 20, hjust = .5, color="black"),
        plot.subtitle = element_text(face = 'bold', size = 15, hjust = .5, color="black"),
        plot.caption = element_text(size = 10, hjust = .5, color="black"),
        axis.title = element_text(face = "bold.italic", color = "black"),
        axis.text.y = element_text(face = "bold.italic", color = "black", size = 12)) +
  labs(title = "Distribution of Adjusted Defense",
       subtitle = "2022-23 Regular Season",
       y = "Adjusted Defensive Efficiency",
       caption = "Data per KenPom")

ggsave("C:\\Users\\Bryce Haase\\Desktop\\UMass Basketball\\KenPom Defense.png", width = 7, height = 5, dpi = 300, limitsize = F)