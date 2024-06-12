```{r}
#| eval: false
#| echo: false
ny_day <- ny_trips |>
  filter(date(start_time) == "2020-06-13") |>
  mutate(hour=hour(start_time)) |> 
  group_by(user_type, hour) |>
  summarise(count=n()) |> ungroup() |> 
  group_by(user_type) |> 
  mutate(
    cum_count=cumsum(count),
    is_breached=cum_count>1000,
    hours_breached=cumsum(as.numeric(is_breached))
  )

ny_day |> 
  ggplot(aes(x=hour, y=cum_count, colour=user_type)) +
  geom_line(aes(group=user_type)) 


ny_day |> 
  ggplot(aes(x=hour, y=cum_count, colour=user_type)) +
  geom_line(aes(group=user_type)) +
  scale_colour_manual(values=c("#e31a1c", "#1f78b4")) 

ny_day |> 
  ggplot(aes(x=hour, y=cum_count, colour=user_type)) +
  geom_line(aes(group=user_type), alpha=.5, linewidth=.7) +
  scale_colour_manual(values=c("#e31a1c", "#1f78b4")) 

ny_day |> 
  ggplot(aes(x=hour, y=cum_count, colour=user_type)) +
  geom_line(aes(group=user_type), alpha=.5, linewidth=.7) +
  geom_line(data = . %>% filter(is_breached), aes(group=user_type), linewidth=1) +
  scale_colour_manual(values=c("#e31a1c", "#1f78b4"))  


ny_day |> 
  ggplot(aes(x=hour, y=cum_count, colour=user_type)) +
  geom_line(aes(group=user_type), alpha=.5, linewidth=.7) +
  geom_line(data = . %>% filter(is_breached), aes(group=user_type), linewidth=1) +
  geom_text(
    data = . %>% filter(hour == 23),
    aes(label=user_type), hjust=0, colour="#000000"
  ) +
  scale_colour_manual(values=c("#e31a1c", "#1f78b4")) +
  guides(colour="none")

ny_day |> 
  ggplot(aes(x=hour, y=cum_count, colour=user_type)) +
  geom_line(aes(group=user_type), alpha=.5, linewidth=.7) +
  geom_line(data = . %>% filter(is_breached), aes(group=user_type), linewidth=1) +
  geom_text(
    data = . %>% filter(hour == 23),
    aes(label=user_type), hjust=0, colour="#000000"
  ) +
  scale_colour_manual(values=c("#e31a1c", "#1f78b4")) +
  scale_x_continuous(limits=c(0,26), breaks=c(0,7,12,17,21)) +
  guides(colour="none")

ny_day |> 
  ggplot(aes(x=hour, y=cum_count, colour=user_type)) +
  geom_line(aes(group=user_type), alpha=.5, linewidth=.7) +
  geom_line(data = . %>% filter(is_breached), aes(group=user_type), linewidth=1) +
  geom_text(
    data = . %>% filter(hour == 23),
    aes(label=user_type), hjust=0, colour="#000000"
  ) +
  geom_text(
    data = . %>% filter(hours_breached==1),
    aes(label=paste0(hour,":00"), y=0), colour="#000000", size=3
  ) +
  geom_segment(
    data = . %>% filter(hours_breached==1),
    aes(x=hour, y=0, xend=hour, yend=cum_count), colour="#000000", linewidth=.2
  ) +
  scale_colour_manual(values=c("#e31a1c", "#1f78b4")) +
  scale_x_continuous(limits=c(0,26), breaks=c(0,7,12,17,21)) +
  guides(colour="none")


ny_temporal |>
  group_by(user_type) |>
  mutate(user_count=sum(count)) |>
  group_by(user_type, day) |>
  summarise(count=sum(count), user_count=first(user_count), prop=count/user_count) |>
  select(user_type, day, prop) 




ny_day |> 
  ggplot(aes(x=hour, y=cum_count, colour=user_type)) +
  geom_line(aes(group=user_type), linewidth=.8, alpha=.5) +
  geom_line(data = . %>% filter(is_breached), aes(group=user_type), linewidth=1) +
  geom_text(
    data = . %>% filter(hour == 23),
    aes(label=user_type), hjust=0, colour="#000000"
  ) +
  geom_text(
    data = . %>% filter(hours_breached==1),
    aes(label=paste0(hour,":00"), y=0), colour="#000000", size=3
  ) +
  scale_colour_manual(values=c("#e31a1c", "#1f78b4")) +
  scale_x_continuous(limits=c(0,26), breaks=c(0,7,12,17,21)) +
  labs(x="", y="trip counts", title="Cumulative trip counts", subtitle="-- Sat 13th June")  +
  guides(colour="none")



ggsave(here("img", "sat.png"), plot=sat, width=6, height=4.5) 


ny_day <- ny_trips |>
  filter(date(start_time) == "2020-06-16") |>
  mutate(
    hour=hour(start_time)) |> 
  group_by(user_type, hour) |>
  summarise(count=n()) |> ungroup() |> 
  group_by(user_type) |> 
  mutate(
    cum_count=cumsum(count),
    is_breached=cum_count>1000,
    hours_breached=cumsum(as.numeric(is_breached))
  )

tue <- ny_day |> 
  ggplot(aes(x=hour, y=cum_count, colour=user_type)) +
  geom_line(aes(group=user_type), linewidth=.8, alpha=.5) +
  geom_line(data = . %>% filter(is_breached), aes(group=user_type), linewidth=1) +
  geom_text(
    data = . %>% filter(hour == 23),
    aes(label=user_type), hjust=0, colour="#000000"
  ) +
  geom_text(
    data = . %>% filter(hours_breached==1),
    aes(label=paste0(hour,":00"), y=0), colour="#000000", size=3
  ) +
  scale_colour_manual(values=c("#e31a1c", "#1f78b4")) +
  scale_x_continuous(limits=c(0,26), breaks=c(0,7,12,17,21)) +
  labs(x="", y="trip counts", title="Cumulative trip counts", subtitle="-- Tue 16th June")  +
  guides(colour="none") 

ggsave(here("img", "tue.png"), plot=tue, width=6, height=4.5) 