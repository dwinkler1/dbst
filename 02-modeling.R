## -----------------------------------------------------------------------------
#| code-fold: false
#| echo: true
library(ggplot2)
library(scales)
time_series_ggstyle <- list(
  scale_y_continuous(labels = label_number(scale_cut = cut_si(' ')), expand = c(0, 0.1)),
  theme_bw(base_size = 20),
  theme(
    axis.title.x = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = 'top'
  )
)


## -----------------------------------------------------------------------------
#| code-line-numbers: "14,19,22-23"
#| fig-align: center
#| fig-width: 18
#| fig-height: 8
library(readr)
library(stringr)
library(tidyverse)
library(data.table)
options(scipen = 99999)
charts <- fread("data/charts_at_global.csv.gz")
ts <- str_detect(tolower(charts$artistName), "taylor swift")
charts_ts <- charts[ts, ]
filter(charts_ts, format(day, "%Y") == "2019" & region == "global") |>
  group_by(day) |>
  mutate(streams = sum(streams)) |>
  ggplot(aes(x = day, y = streams)) +
  geom_line() +
  scale_x_date(
    breaks = seq(as.Date("2019-01-01"), as.Date("2019-12-31"), "month"),
    date_labels = "%b"
  ) +
  geom_vline(xintercept = as.Date("2019-08-23"), color = "red") +
  annotate("text", x = as.Date("2019-08-20"), label = "Release of 'Lover'", y = 40000000, colour = "red", angle = 90, size = 8) +
  ggtitle("Taylor Swift Streams", subtitle = "Songs in top 200 - 2019") +
  time_series_ggstyle


## -----------------------------------------------------------------------------
#| cache: true
library(zoo)
library(prophet)
total_streams <- charts |>
  filter(region == "global") |>
  group_by(day) |>
  summarize(y = sum(streams)) |>
  mutate(ds = as.Date(day)) |>
  select(-day)
total_streams_model <- filter(total_streams, ds <= as.Date("2020-12-31"), ds >= as.Date("2019-01-01")) 
total_streams_holdout <- filter(total_streams, ds >= as.Date("2021-01-01"))
mod <- prophet(total_streams_model,
               holidays = data.frame(
                 holiday = "christmas",
                 ds = c(
                    as.Date("2019-12-25"), 
                    as.Date("2020-12-25"), 
                    as.Date("2021-12-25")),
                 lower_window = -1, upper_window = 0
               ),
               daily.seasonality = FALSE
)
future <- make_future_dataframe(mod, periods = 365)
forecast <- predict(mod, future)
plot(mod, forecast) +
  labs(
    y = "Streams",
    title = "Prediction of total global streams of top 200",
    subtitle = "Observed: 2019-2020, forecast: 2021 (holdout: red)"
  ) +
  time_series_ggstyle +
  geom_point(data = total_streams_holdout, 
  aes(x = as.POSIXct(ds), y = y), color = 'red')


## -----------------------------------------------------------------------------
#| code-line-numbers: "3"
set.seed(123)
xy <- data.frame(x = rnorm(100000))
xy$y <- 0.5 * xy$x^2 + 2 * xy$x^4
ggplot(xy, aes(x = x, y = y)) +
geom_line() +
geom_smooth(method = "lm", color = "blue") +
labs(title = expression(y == 0.5 * x^2 + 2 * x^4), subtitle = "Non-linear relation") +
annotate("text",
x = -1, y = 25,
label = paste0(
"Best linear fit. Correlation: ",
round(cor(xy$x, xy$y), 3)
), hjust = 0, color = "blue", size =8
) +
time_series_ggstyle


## -----------------------------------------------------------------------------
set.seed(42)
xy <- data.frame(x = rnorm(1000), y = rnorm(1000))
xy$obs <- abs(xy$x + xy$y) < 0.5 + runif(1000,0,2)
ggplot(xy, aes(x=x, y=y)) +
    geom_point(aes(color=obs)) +
    geom_smooth(data = xy[xy$obs,], 
    method = 'lm', se = FALSE, color = "#00BFC4") +
    geom_smooth(method = 'lm', se = FALSE) +
    time_series_ggstyle +
    labs(color = "Observed",
        title = "Restaurant and location quality", 
        subtitle="Survivor bias",
        y = "Restaurant Quality", x = "Location Quality") +
    annotate('text', 
            x = 2, y = -0.2, hjust=0,
            label = "Population regression line",
            color = "blue", size = 8 ) +
    theme(axis.title.x = element_text())


## -----------------------------------------------------------------------------
library(datasauRus)
library(kableExtra)
suppressPackageStartupMessages(library(dplyr))
data <- datasaurus_dozen %>%
filter(dataset %in% c(
"away",
"bullseye",
"circle",
"dino",
"high_lines",
"wide_lines",
"x_shape",
"star"
))
data %>%
group_by(dataset) %>%
summarize(
mean_x    = round(mean(x), 2),
mean_y    = round(mean(y), 2),
std_dev_x = round(sd(x), 2),
std_dev_y = round(sd(y), 2),
corr_x_y  = round(cor(x, y), 2)
) %>%
mutate(dataset = stringr::str_replace(dataset, "_", " ")) %>%
kbl(
col.names =
c("data", "mean x", "mean y", "sd x", "sd y", "corr x,y"),
format = "html", table.attr = "style='width:100%;'"
) %>%
column_spec(1, width = "3cm")


## -----------------------------------------------------------------------------
#| fig-height: 14
library(ggplot2)
library(colorspace)
ggplot(data, aes(x = x, y = y, colour = dataset)) +
geom_point(size = 4.5) +
theme_void() +
theme(
legend.position = "none",
strip.text.x = element_text(size = 30)
) +
facet_wrap(~dataset, nrow = 4) +
scale_color_discrete_qualitative(palette = "Dynamic")


## -----------------------------------------------------------------------------
library(modelsummary)
library(gt)
set.seed(1)
N <- 5000
genre <- rbinom(N, 1, 0.5)
perfect_rating <- as.factor(rbinom(N, 5, 0.8 - 0.7 * genre)>4)
streams <- rexp(N, 0.01 - 0.003 * genre) |> floor()
modelsummary(
    list(lm(streams~perfect_rating),
    lm(streams~genre),
    lm(streams~perfect_rating + genre)),
    coef_rename = c("perfect_ratingTRUE" = "perfect rating"),
    stars = TRUE,
    statistic = "{p.value}",
    gof_map = NA)


## -----------------------------------------------------------------------------
set.seed(42)
range_normalize <- function(x, min_range = 0, max_range = 100){
  x_norm <- min_range + (x - min(x)) * (max_range - min_range) / (max(x) - min(x))
}
grades <- 1 + rbinom(N, 4, 0.04)
self_esteem <- range_normalize(100 * (1/grades + rnorm(N)))
happyness <- range_normalize(5 + self_esteem + rnorm(N))
modelsummary(
    list(lm(happyness~grades),
    lm(happyness~self_esteem),
    lm(happyness~grades+self_esteem)),
    coef_rename = c("self_esteem" = "self esteem"),
    stars = TRUE,
    statistic = "{p.value}",
    gof_map = NA)


## -----------------------------------------------------------------------------
set.seed(14)
restaurant_rating <- 25 + 10*rnorm(N)
location_rating <- 5*rnorm(N)
survival_probability <- range_normalize(0.8*restaurant_rating + 0.8 * location_rating + 10*rnorm(N), 0, 100)
modelsummary(
    list(lm(restaurant_rating~location_rating),
    lm(restaurant_rating~survival_probability),
    lm(restaurant_rating~survival_probability + location_rating)),
    coef_rename = c(
                    "location_rating" = "location rating",
                    "survival_probability" = "survival prob."
                    ),
    stars = TRUE,
    output = 'gt',
    statistic = "{p.value}",
    gof_map = NA) |>
    tab_style(style = cell_fill(color='lavenderblush3'),
              locations = cells_body(rows = 3)
    )


## ----first-merm, fig.width=5, fig.height=2.5----------------------------------
library(ggdag)
library(dagitty)
library(tidyverse)
dagify(y ~ x, x ~ z, exposure = "x", outcome = "y",
       coords = list(x = c(x = 1, y = 1.5, z = 1), y = c(x=1, y = 1, z=0))
) %>% 
  tidy_dagitty() %>%
  ggdag(text_size = 8, node_size = 10) +
  geom_dag_edges() +
  annotate("text", x = 1.2, y = 1, vjust=1, label= "x causes y", size=5 ) + 
  annotate("text", x = 1, y = 0.5, hjust=-0.1, label="z causes x", size = 5) +
  theme_dag()


## ----fig.height=1.8, fig.width=6----------------------------------------------
dagify(z ~ x, y2 ~ z, a ~ x, a ~ y3, x ~ d, y1 ~ d,
       coords = list(x = c(x = 1, z = 1.5, y2 = 2, a = 1.5, y3 = 2, d = 1.5, y1 = 2), 
                     y = c(x = 1, y2 = 1, z = 1, a = 0, y3 = 0, d = 2, y1 = 2))
) %>% 
  tidy_dagitty() %>%
  ggdag(text_size = 3, node_size = 5) +
  geom_dag_edges() +
  theme_dag() +
  labs(title= "Causal Pitchfork", subtitle = "x and y2 are d-connected but x and y1/y3 are not") +
  theme(title = element_text(size = 8))


## -----------------------------------------------------------------------------
#| fig-width: 6
#| fig-height: 2.5
med <- dagify( x ~ d, y1 ~ d,
       coords = list(x = c(x = 1, z = 1.5, y = 2, a = 1.5, b = 2, d = 1.5, y1 = 2), 
                     y = c(x = 1, y = 1, z = 1, a = 0, b = 0, d = 2, y1 = 2))
) %>% 
  tidy_dagitty() %>%
  mutate(fill = ifelse(name == "d", "Confounder", "variables of interest")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point(size=7, aes(color = fill)) + 
  geom_dag_edges(show.legend = FALSE)+
  geom_dag_text() +
  theme_dag() +
  theme(legend.title  = element_blank(),
        legend.position = "top") 
med


## -----------------------------------------------------------------------------
#| fig-width: 6
#| fig-height: 2
med <- dagify(z ~ x, y2 ~ z,
       coords = list(x = c(x = 1, z = 1.5, y2 = 2), y = c(x=1, y2 = 1, z=1))
) %>% 
  tidy_dagitty() %>%
  mutate(fill = ifelse(name == "z", "Mediator", "variables of interest")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point(size=7, aes(color = fill)) + 
  geom_dag_edges(show.legend = FALSE)+
  geom_dag_text() +
  theme_dag() +
  theme(legend.title  = element_blank(),
        legend.position = "top") 
med


## -----------------------------------------------------------------------------
#| fig-width: 6
#| fig-height: 2
dagify(a ~ x, a ~ y,
  coords = list(x = c(x = 1, y = 2, a = 1.5), y = c(x = 1, y = 0,  a = 0))
) |>
  tidy_dagitty() |>
  mutate(fill = ifelse(name == "a", "Collider", "variables of interest")) |>
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(size = 7, aes(color = fill)) +
  geom_dag_edges(show.legend = FALSE) +
  geom_dag_text() +
  theme_dag() +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )


## -----------------------------------------------------------------------------
#| fig-align: center
library(ggdag)
library(dagitty)
library(tidyverse)
dagify(y ~ n + z + b + c, 
       x ~  z + a + c,
       n ~ x,
       z ~ a + b, exposure = "x", outcome = "y",
       coords = list(x = c(n = 2, x = 1, y = 3, a = 1, z = 2, c = 2, b = 3), y = c(x = 2, y = 2, a = 3, z = 3, c = 1, b = 3, n = 2))) %>% 
  tidy_dagitty() %>%
  ggdag(text_size = 8, node_size = 12) +
  geom_dag_edges() +
  theme_dag()


## -----------------------------------------------------------------------------
#| fig-align: center
library(ggpubr)
p1 <- dagify(y ~ x + U2,
       a ~ U1 + U2,
       x ~ U1,
      coords = list(x = c(x = 1, y = 2, a = 1.5, b = 1.5, U1 = 1, U2 = 2), y = c(x=1, y = 1,  a = 1.5, b = 0, U1 = 2, U2 = 2))
) %>% 
  tidy_dagitty() %>%
  mutate(fill = ifelse(name %in% c("U1", "U2"), "Unobserved", "Observed")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point(size=12, 
                 aes(color = fill)
                 ) + 
  geom_dag_edges(show.legend = FALSE)+
  geom_dag_text() +
  theme_dag() +
  theme(legend.title  = element_blank(),
        legend.position = "bottom") +
  labs(title = "M-Bias")
p2 <- dagify(y ~ a + U,
       a ~ x + U,
      coords = list(x = c(x = 1, y = 2, a = 1.5, b = 1.5, U = 1.7, U2 = 2), y = c(x=1, y = 1,  a = 1, b = 0, U = 2, U2 = 2))
) %>% 
  tidy_dagitty() %>%
  mutate(fill = ifelse(name %in% c("U"), "Unobserved", "Observed")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point(size=12, 
                 aes(color = fill)
                 ) + 
  geom_dag_edges(show.legend = FALSE)+
  geom_dag_text() +
  theme_dag() +
  theme(legend.title  = element_blank(),
        legend.position = "bottom") +
  labs(title = "Post-treatment Bias")
ggarrange(p1, p2)


## -----------------------------------------------------------------------------
#| fig-align: center
p1 <- dagify(y ~ x ,
       a ~ x + y,
      coords = list(x = c(x = 1, y = 2, a = 1.5, b = 1.5, U1 = 1, U2 = 2), y = c(x=1, y = 1,  a = 1.5, b = 0, U1 = 2, U2 = 2))
) %>% 
  tidy_dagitty() %>%
  #mutate(fill = ifelse(name %in% c("U1", "U2"), "Unobserved", "Observed")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point(size=12, 
                 #aes(color = fill)
                 ) + 
  geom_dag_edges(show.legend = FALSE)+
  geom_dag_text() +
  theme_dag() +
  theme(legend.title  = element_blank(),
        legend.position = "bottom") +
  labs(title = "Selection Bias")
p2 <-  dagify(y ~ x ,
       a ~ y,
      coords = list(x = c(x = 1, y = 2, a = 1.5, b = 1.5, U1 = 1, U2 = 2), y = c(x=1, y = 1,  a = 1.5, b = 0, U1 = 2, U2 = 2))
) %>% 
  tidy_dagitty() %>%
  #mutate(fill = ifelse(name %in% c("U1", "U2"), "Unobserved", "Observed")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point(size=12, 
                 #aes(color = fill)
                 ) + 
  geom_dag_edges(show.legend = FALSE)+
  geom_dag_text() +
  theme_dag() +
  theme(legend.title  = element_blank(),
        legend.position = "bottom") +
  labs(title = "Case-control Bias")
ggarrange(p1, p2)

