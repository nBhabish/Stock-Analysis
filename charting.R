#Loading libraries
library(tidyverse)
library(ggplot2)
library(dplyr)

load("returnsLong.RData")


ticker <- "AMZN"

chartingData <- returnsLong %>% 
  filter(Ticker == ticker, Date >= "2020-09-30")


candleStick <- ggplot(chartingData)+
  geom_boxplot(aes(as.character(Date), Value, fill = Movement),
               color = "#D0D3D4", width = 0.1)+
  scale_fill_manual(values = c(Up = "#0066ff", Down ="#ffff00"))+
  xlab("Date")+
  ylab("Stock Price")+
  labs(
    title = paste0(chartingData$Name[1], " (", ticker,")"),
    subtitle = chartingData$Sector[1],
    caption = "Source: Yahoo Finance"
  )+
  scale_y_continuous(labels = scales::dollar)+
  theme(
    plot.background = element_rect(fill = "#17202A"),
    panel.background = element_rect(fill = "#17202A"),
    axis.title.x = element_text(color = "#ffffff"),
    axis.title.y = element_text(color = "#ffffff"),
    axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(color = "#ffffff"),
    plot.title = element_text(color = "#ffffff"),
    plot.subtitle = element_text(color = "#ffffff"),
    plot.caption = element_text(color = "#ffffff", face = "italic", size = 6),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#273746"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
    
  )


