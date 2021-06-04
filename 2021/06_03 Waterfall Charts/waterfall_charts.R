library(tidyverse)
library(tidytuesdayR)
library(ggplot2)
library(ggtext)
library(showtext)
library(extrafont)
library(ragg)

#add fonts
font_add(family = "regular", "Rubik-VariableFont_wght.ttf")
font_add(family = "title", "BebasNeue-Regular.ttf")
showtext_auto()


my_theme <- theme(
  #titles
  plot.title=element_text(family="title", vjust=0.5,
                          hjust=0.5, size=65, color="white"),
  plot.caption=element_text(family="regular", size=35, color="#cccccc",
                            vjust=0.5, hjust=0.5),
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title.x = element_text(size=35, family="regular", color="#cccccc"),
  axis.title.y = element_text(size=30, family="regular", color="#cccccc"),
  axis.text.y = element_text(size=20, family="regular", color="#cccccc"),
  axis.text.x = element_text(size=20, family="regular", color="#cccccc"),
  #legend
  legend.position = "top",
  legend.direction = "horizontal",
  legend.background = element_rect(fill="black", color="black"),
  legend.text = element_text(size=25, family="regular", color="#cccccc"))


##SPANISH MIGRATION##

#load spanish migration data
migration <- read.csv("spain_migration.csv")
#rename columns
names(migration) <- c("year", "net_migration")

#waterfall plot
changes <- migration %>%
  #create variables
  mutate(id=row_number(),   #use row id for positioning
         year=year,
         start=lag(net_migration),    #lag() function for time series
         end=net_migration,
         dif=net_migration-lag(net_migration),
         change_type=case_when(    #increase/decrease/no change between data points
           dif>0 ~"INCREASE",
           dif==0 ~ "NO CHANGE",
           dif<0 ~ "DECREASE"
         ))

yr1962 <- changes %>% filter(year==1962)
yr2017 <- changes %>% filter(year==2017)

#(migration_waterfall <- changes %>% 
#  ggplot() +
  #change bars
  #geom_rect(data=changes,
  #          aes(x=year,
  #              fill=change_type,
  #              xmin = id - 0.45, 
  #              xmax = id + 0.45,
  #              ymin = end,
  #              ymax = start)) +
  #first bar
  #geom_rect(data=yr1962,
  #          aes(xmin = year - 4.5, 
  #              xmax = year + 4.5,
  #              ymin = net_migration,
  #              ymax = 0), fill="#888888") +
  #last bar
  #geom_rect(data=yr2017,
  #          aes(xmin = year - 4.5, 
  #              xmax = year + 4.5,
  #              ymin = start,
  #              ymax = end), fill="#888888") +
  #color scale
  #scale_fill_manual(values = c("#E73F3F", "#76AA43"), na.translate=FALSE) +
  #scale_y_continuous(limits = c(-200000, 3000000), labels=scales::comma) +
  #scale_x_continuous(breaks=changes$year, limits=c(1960,2019)) +
  #my_theme +
  #labels
  #labs(x="YEAR\n", y="CHANGE IN NET MIGRATION",
  #     caption="Source: The World Bank | Moriah Taylor | Twitter: moriah_taylor58 | GitHub: moriahtaylor1",
  #     title="Spanish Immigration and Emigration",
  #     fill="")

##NVIDIA STOCK##
stock_theme <- theme(
  #titles
  plot.title=element_text(family="title", vjust=0.5,
                          hjust=0.5, size=65, color="white"),
  plot.caption=element_text(family="regular", size=35, color="#cccccc",
                            vjust=0.5, hjust=0.5),
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title.x = element_text(size=35, family="regular", color="#cccccc"),
  axis.title.y = element_text(size=30, family="regular", color="#cccccc"),
  axis.text.y = element_text(size=20, family="regular", color="#cccccc"),
  axis.text.x = element_text(size=20, family="regular", color="#cccccc", angle=90),
  #legend
  legend.position = "top",
  legend.direction = "horizontal",
  legend.background = element_rect(fill="black", color="black"),
  legend.text = element_text(size=25, family="regular", color="#cccccc"))

nvda <- read.csv("NVDA_data.csv")

nvda <- nvda %>% 
  mutate(month = lubridate::month(date, label=TRUE)) %>%
  mutate(year = lubridate::year(date))

names(nvda) <- c("stock_date", "stock_open", "stock_high", "stock_low", "stock_close", 
                 "stock_volume", "stock_name", "stock_month", "stock_year")

aug_2015 <- nvda %>% filter(stock_month=="Aug"&stock_year==2015)

aug_2015_changes <- aug_2015 %>%
  #create variables
  mutate(id=row_number(),   #use row id for positioning
         stock_date=stock_date,
         start=lag(stock_close),    #lag() function for time series
         end=stock_close,
         dif=stock_close-lag(stock_close),
         change_type=case_when(    #increase/decrease/no change between data points
           dif>0 ~"INCREASE",
           dif==0 ~ "NO CHANGE",
           dif<0 ~ "DECREASE"
         ))

aug_2015_first <- aug_2015_changes[1,]   #first row
aug_2015_last <- aug_2015_changes[nrow(aug_2015_changes),]

(stock_waterfall <- aug_2015_changes %>%
    ggplot() +
    #change bars
    geom_rect(data=aug_2015_changes,
              aes(x=stock_date,
                  fill=change_type,
                  xmin = id - 0.45, 
                  xmax = id + 0.45,
                  ymin = end,
                  ymax = start)) +
    #first bar
    geom_rect(data=aug_2015_first,
              aes(x=stock_date,
                  xmin = id - 0.45, 
                  xmax = id + 0.45,
                  ymin = 20,
                  ymax = stock_close), fill="#cccccc") +
    #last bar
    geom_rect(data=aug_2015_last,
              aes(x=stock_date,
                  xmin = id - 0.45, 
                  xmax = id + 0.45,
                  ymin = 20,
                  ymax = stock_close), fill="#cccccc") +
    #color scale
    scale_fill_manual(values = c("#f3722c", "#43aa8b"), na.translate=FALSE) +
    ylim(20,24) +
    stock_theme +
    #labels
    labs(x="DATE", y="CLOSING PRICE",
         caption="\nSource: Kaggle | Moriah Taylor | Twitter: moriah_taylor58 | GitHub: moriahtaylor1",
         title="NVIDIA STOCK: AUG 2015",
         fill=""))

ggsave("nvda_stock_waterfall.png",
       plot=stock_waterfall,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))
