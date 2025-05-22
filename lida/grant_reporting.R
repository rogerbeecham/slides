library(tidyverse)
library(here)
library(ggtext)
source(here("data", "book_theme.R"))

dat <- read_csv(here("data", "lida_grant_reporting2.csv"))
# Leeds Price is the figures quoted

grants <- read_csv(here("data", "lida_grants2.csv"))

grant : 134498 > successful

faculty_levels <- dat |> 
  rename(value=`Leeds Price (£)`) |> 
  filter(
    `Phase Type`=="Live Award", 
    str_detect(Faculty, "Faculty of "),
    Grant %in% (grants |> filter(`Phase Type`=="Live Award") |>  pull(Grant))
  ) |>
  mutate(
    Faculty=str_remove(Faculty, "Faculty of "),
    award_date=parse_date_time(`Research Start`, "dmy"), 
    is_new=if_else(award_date > as.Date("2024-10-01"), "> Nov 24 start", "Pre Nov 24 start")
  ) |> 
  group_by(Faculty) |> 
  summarise(tot=sum(value, na.rm=T))








plot <- dat |> 
    rename(value=`Leeds Price (£)`) |> 
    filter(
      `Phase Type`=="Live Award", 
      str_detect(Faculty, "Faculty of "),
      Grant %in% (grants |> pull(Grant))
      ) |>
     mutate(
       Faculty=str_remove(Faculty, "Faculty of "),
       award_date=parse_date_time(`Research Start`, "dmy"), 
       is_new=if_else(award_date > as.Date("2024-10-01"), "Starts post Nov 24", "Starts pre Nov 24")
       ) |> 
    group_by(Faculty, is_new) |> 
    summarise(tot=sum(value, na.rm=T)) |> 
    mutate(Faculty=factor(Faculty, levels=faculty_levels |> arrange(tot) |> pull(Faculty))) |> 
    ggplot(aes(y=Faculty, x=tot)) +
    geom_col(aes(fill=is_new)) +
    geom_text(data=faculty_levels |> 
                mutate(tot=if_else(tot>1e-6, paste0("£", round(tot/1e6,2), "m"), paste0("£", tot/1e3, "k") )), 
              aes(label=tot, x=60*1e+6), hjust="right", colour="#525252") +
  
     annotate("richtext", x=70*1e+6,y=7.5, hjust="left",vjust="top", label.colour = NA, family="Avenir", size=5, colour="#525252", fill="transparent",
           label="<span style='font-size: 20px;'> Nov 24 -- May 25 </span> <br>
           <span style='font-family:\"Avenir Heavy\"'>£9.15m</span> ~ 7 new awards <br>
           <span style='font-family:\"Avenir Heavy\"'>£1.7m</span> ~ 5 pending</span> <br>
           <span style='font-family:\"Avenir Heavy\"'>~75%</span> involves LASER</span> <br>" ) + 
  
  
  annotate("richtext", x=70*1e+6,y=3.5, hjust="left",vjust="top", label.colour = NA, family="Avenir", size=4, colour="#525252", fill="transparent",
           label="Projects may: <br>
           -- use Laser <br>-- employ LIDA DS / DAT <br>-- have LIDA researcher as P/Co-I <br>-- written w/ LIDA RIS") +
  
    
    annotate("richtext", x=14*1e+6,y=5.3, hjust="left",vjust="top", label.colour = NA, family="Avenir", size=4, colour="#525252", fill="transparent",
           label="starts after<br>Nov 24") + 
  
  annotate("curve", x=18*1e+6, xend=10*1e+6, y=5.3, yend=6, linewidth=.2, colour="#525252", arrow = arrow(length = unit(0.02, "npc")), curvature = 0.4 )  +
  
  labs(title="Live awards", caption="Data to May 2025") +
  
    scale_fill_manual(values=c("#525252", "#bdbdbd"), guide="none") +
    scale_x_continuous(
      # labels=scales::label_dollar(scale = 1e-6, prefix = "£", suffix = "m"), 
      # breaks = c(15*1e+6, 30*1e+6, 45*1e+6),
      limits=c(0,1.2*1e+8), expand = c(0, 0)
      ) +
    theme(
      axis.title.x=element_blank(), axis.title.y=element_blank(), legend.title = element_blank(),
      axis.text.x=element_blank(), axis.line = element_blank(), axis.text.y=element_text(size=14)
    )

ggsave("lida_awards.png", plot, width=9, height=4, dpi=600)

dat |> 
  rename(value=`Leeds Price (£)`) |> 
  filter(
    `Phase Type`=="Live Award", 
    # str_detect(Faculty, "Faculty of "),
    Grant %in% (grants |> pull(Grant))
  ) |>
  mutate(
    Faculty=str_remove(Faculty, "Faculty of "),
    award_date=parse_date_time(`Research Start`, "dmy"), 
    is_new=award_date > as.Date("2024-10-01")
  ) |> 
  group_by(Faculty, is_new) |> 
  summarise(tot=n()) |> 
  ggplot() +
  geom_col(aes(y=Funder, x=tot, fill=is_new)) +

  
  
  
  

dat |> mutate(date=parse_date_time(`Research Start`, "dmy")) |> 
  filter(year(date) > 2024) |> View()
