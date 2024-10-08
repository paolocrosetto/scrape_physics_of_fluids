## Analysis of the Physics of FLuids Turnaround data
##

#### 0. Libraries ####
library(tidyverse)
library(hrbrthemes)
library(ggbeeswarm)
library(gt)
library(gtExtras)


#### 1. Plot: N Articles ####
## How many articles did PoF publish every year? 
## a simple plot
links <- read_csv("all_articles.csv")

plotme <- links %>% 
  group_by(year) %>% 
  tally()

plotme %>% 
  mutate(year = year + 1988) %>% 
  ggplot()+
  aes(year, n)+
  geom_col()+
  hrbrthemes::theme_ipsum_inter()+
  labs(title = "Physics of Fluids, N papers by year 2004-24",
       subtitle = "Including Letters and Errata, 2024: Jan-Sep",
       x = "Year", y = "N papers")+
  theme(plot.title.position = "plot", 
        plot.background = element_rect(fill = "white", color = "white"))
ggsave("N_links.png", width = 12/1.3, height = 9/1.3, units = "in", dpi = "retina")



#### 2. Turnaround times ####

### get data
df <- read_csv("PoF_scraped.csv")

df <- df %>% 
  mutate(TAT = accepted - received)


### clean the basic datasets

## raw data
TATs <- df %>% 
  filter(type == "Research Article") %>% 
  mutate(year = 1988 + volume, 
         month = issue) %>% 
  select(year, month, type, TAT) %>% 
  mutate(date = make_date(year= year, month = month, day = 15))

## means
TAT_means <- TATs %>% 
  group_by(year) %>% 
  summarise(m = mean(TAT, na.rm = T), 
            s = sd(TAT, na.rm = T), 
            Nn = n(),
            N = paste0("N:\n", n())) %>% 
  mutate(label = paste0(round(m), " days (", round(s, 1), ")"))


#### 3. Main plot that visualizes distriubtions, means, and N papers ####


## plot
TATs %>% 
  ggplot(aes(x = year, y = TAT))+
  geom_quasirandom(alpha = .7, color = "grey70", fill = "#d2fff6", pch = 21)+
  stat_summary(color = "chartreuse4")+
  coord_cartesian(ylim = c(0,365))+
  geom_label(data = TAT_means, 
            aes(x = year, y = m - 12, label = round(m)), 
            alpha = .5)+
  geom_label(data = TAT_means, 
             aes(x = year, y = 372, label = N), 
             color = "white",fill = "grey90", alpha = 0.5)+
  geom_text(data = TAT_means, 
             aes(x = year, y = 372, label = N), 
             )+
  theme_ipsum_gs()+
  scale_x_continuous(breaks = seq(2004,2024), 
                     labels = c("2004", "05", "06", "07", "08", "09", "10", 
                                "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "2024"), 
                      )+
  labs(y = "Turnaround times", 
       x = "Year", 
       title = "Physics of Fluids: turnaround times in days for accepted Research Articles", 
       subtitle = "Submission to acceptance · each point is a paper · yearly means in green & text · number of papers up top · 161 papers with TAT > 1 year not shown", 
       caption = "data scraping and analysis by @paolocrosetto")+
  theme(plot.title.position = "plot", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"), 
        plot.subtitle = element_text(size = 11))
ggsave("PoF_TAT_visualizer.png", width = 12/1, height = 9/1, units = "in", dpi = 300)


#### 4. Ancillary radar plot of monthly data ####
base_plot <- TATs %>% 
  group_by(year, month) %>% 
  summarise(TAT = mean(TAT)) %>% 
  ungroup() %>% 
  mutate(TAT = as.numeric(TAT), 
         TAT= case_when(year != 2004 & month == 1 ~ (TAT + lag(TAT))/2, 
                        month == 12               ~ (TAT + lead(TAT))/2,  
                        TRUE                      ~ TAT)) %>% 
  ggplot()+
  aes(month, TAT, color = year, group = year)+
  geom_line(linewidth = 1.5)+
  coord_polar()+
  ylim(c(0, 200))+
  scale_color_steps2(name = "", 
                     low = "#4DAF4A", mid = "#ffff03", high = "#aa0000", midpoint = 2014, 
                     breaks = seq(2004, 2024, 2))+ 
  guides(color = guide_colorsteps(barwidth = 20, barheight = 1.5, draw.llim = F, draw.ulim = F)) + 
  scale_x_continuous(breaks = seq(1:11),
                     labels = c("Jan\nDec", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov"))+
  geom_text(data = tibble(x = 1, y = seq(25, 175, 25), label = seq(25, 175, 25)),
            aes(x = x, y = y, label = label), 
            color = "grey60", 
            vjust = 0.5, hjust = 0.5,
            inherit.aes = F)+
  labs(title = "Physics of Fluids: Research Articles monthly turnaround times", 
       subtitle = "Submission to acceptance in days · monthly means")

## with a light theme
base_plot+
  theme_ipsum_ps()+
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.line.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 13), 
        panel.grid.minor.x = element_blank(),
        plot.title.position = "plot", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        plot.background = element_rect(fill = "white", color = "white"))
ggsave("PoF_TAT_monthly_radar.png", width = 9/1, height = 9/1, units = "in", dpi = 300)

## with a dark theme
base_plot+
  theme_ft_rc()+
  theme(legend.position = "bottom", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.line.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 13), 
        panel.grid.minor.x = element_blank(),
        plot.title.position = "plot", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        plot.background = element_rect(fill = "#252a32", color = "#252a32"))
ggsave("PoF_TAT_monthly_radar_dark.png", width = 9/1, height = 9/1, units = "in", dpi = 300)


## variance / distro
time_comparison_colors <- c("#377EB8", "#529684", "#4DAF4A")
TATs %>% 
  filter(year %in% c(2016, 2019, 2022)) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x = TAT, color = year))+
  geom_density()+
  theme_ipsum_gs()+
  scale_color_manual(name = "", values = time_comparison_colors)+
  labs(y = "Density", 
       x = "Turnaround times", 
       title = "Distribution of turnaround times at Physics of Fluids", 
       subtitle = "Submission to acceptance, conditional on acceptance -- 'Research Articles' only", 
       caption = "data scraped from PoF website by @paolocrosetto")+
  theme(plot.title.position = "plot", 
        legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"))
ggsave("PoF_distribution.png", width = 12/1.2, height = 9/1.2, units = "in", dpi = 300)

#### 5. A simple table ####
TAT_means %>% 
  select(year, Nn, label) %>% 
  gt() %>% 
  gt_theme_538() %>% 
  cols_label("label" = "Mean Tunraround (st.dev.)", "Nn" = "Published papers") %>% 
  cols_align(align = "right", columns = label) %>% 
  tab_style(style = cell_text(align = "right"), 
            locations = cells_body(columns = label)) %>% 
  tab_header(title = "Turnaround times (submission to acceptance) at PoF", 
             subtitle = "Data scraped from PoF website")

