

library(tidyverse)
library(gganimate)
library(here)
library(lubridate)

int <- interval(ymd("2023-12-01"), ymd("2024-01-15"))


ymd("2001-05-03") %within% int # TRUE

dat <- read_csv(here("oa_export_feb.csv")) |> 
  mutate(
    is_paper=!is.na(first_name), 
    cum_count=cumsum(as.numeric(is_paper)), 
    count=as.numeric(is_paper),
    id=row_number(),
    date=dmy(date),
  ) |>
  arrange(id) |> 
  group_by(id) |> 
  summarise(count=first(cum_count), date=first(date)) |>
  mutate(
    is_shit=if_else(date %within% int, "CRIPES!",""),
    show_count=if_else(id %in% c(1,180), as.character(count), "")
    )
  
  mutate(pause_time = case_when(date == "2024-01-08" ~ 61, TRUE~ 1),
         is_shit=if_else(pause_time==1,"", "CRIPES!")) 

  
  
p <- dat |> 
  ggplot() +
  geom_point(aes(x=0, y=-.01, size=count)) +
  scale_size(guide="none", range=c(2,80)) +
  scale_x_continuous(limits=c(-.5,.5)) +
  scale_y_continuous(limits=c(-.8,.6)) +
  geom_text(aes(x=0,y=-.75, label=paste0("# papers")), size=12) +
  geom_text(aes(x=0,y=.45, label=is_shit), size=12, colour="#b30000") +
  labs(title = '{frame_time}') +
  transition_time(date) +
  ease_aes('linear') +
  theme(
    plot.title=element_text(size=35, family="Iosevka", hjust = .5),
    legend.position = "bottom",
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text=element_blank(),
    panel.grid=element_blank(),
    strip.text=element_blank(),
    axis.line = element_blank()
  )



animate(p, renderer = gifski_renderer("./anim.gif", loop = FALSE),res=300, nframes=180, width=1500, height=1200,  fps=16)


