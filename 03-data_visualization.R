## -----------------------------------------------------------------------------
#| message: false
library(datasauRus)
library(dplyr)
library(dagitty)
library(ggdag)
library(ggplot2)
library(gt)
library(stringr)
library(tidyr)
library(scales)

tab_ds_data <- datasaurus_dozen |>
  filter(dataset %in% c('dino', 'star')) |>
  group_by(dataset) |>
  mutate(id = 1:n()) |>
  pivot_wider(
    names_from = dataset, 
    values_from = c(x,y),
    names_vary = 'slowest'
  ) |>
  select(-id) |> 
  head(10) 
tab_ds_data |>  
  gt() |>
  cols_label(
    x_dino = 'x',
    y_dino = 'y',
    x_star = 'x',
    y_star = 'y'
  ) |>
  fmt_number() |>
  tab_spanner(
    label = "Dino",
    columns = ends_with("dino")
  ) |>
  cols_align('center') |> 
  tab_spanner(
    label = "Star",
    columns = ends_with("star")
  ) |> 
  tab_options(
    table.width = pct(85),
    table.font.size = 35) 


## -----------------------------------------------------------------------------
tab_ds_data |> 
knitr::kable(
  col.names = c(
    'Dino: x',
    'Dino: y',
    'Star: x',
    'Star: y'),
    digits = 2)


## -----------------------------------------------------------------------------
datasaurus_dozen |>
  filter(dataset %in% c('dino', 'star')) |>
  group_by(dataset) |>
  summarize(
    avg_x = round(mean(x), digits = 2), 
    sd_x = round(sd(x), digits = 2),
    avg_y = round(mean(y), digits = 2),
    sd_y = round(sd(y), digits = 2),
    cor_xy = round(cor(x,y), digits = 2)
    ) |>
  mutate(dataset = str_to_title(dataset)) |> 
  gt() |>
  cols_label(
    avg_x = "Mean of x",
    sd_x = "Std. Dev. of x",
    avg_y = "Mean of y",
    sd_y = "Std. Dev. of y",
    cor_xy = "Correlation") |> 
  tab_options(
    table.width = pct(85),
    table.font.size = 35) 


## -----------------------------------------------------------------------------
#| fig-align: center
datasaurus_dozen |>
  filter(dataset %in% c('dino', 'star')) |>
  mutate(dataset = str_to_title(dataset)) |>
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~dataset) +
  theme_minimal() +
  theme(
    panel.grid=element_blank(),
    strip.text=element_text(size=35),
    axis.ticks=element_blank(),
    axis.text=element_blank(),
    axis.title=element_text(size=30)
  )


## -----------------------------------------------------------------------------
#| code-fold: show
library(datasauRus)
library(dplyr)
library(ggplot2)
library(gt)
library(stringr)
library(tidyr)
filter(datasaurus_dozen, dataset %in% c('x_shape', 'bullseye')) |>
  str(give.attr = FALSE)


## -----------------------------------------------------------------------------
#| fig-width: 18
#| fig-height: 9
#| fig-align: center
related_data <- data.frame(
  x = c(rnorm(100, 0, 0.1), rnorm(100, 0.5, 0.1)), 
  y = c(rnorm(100, 0, 0.1), rnorm(100, 0.5, 0.1)))
ggplot(related_data, aes(x = x , y = y)) +
  geom_point(size = 1.2) +

  theme_minimal() +
  theme(
    plot.title=element_text(size=35),
    axis.ticks.x=element_blank(),
    axis.text=element_text(size=30),
    axis.title=element_text(size=30)
  )


## -----------------------------------------------------------------------------
#| fig-width: 18
#| fig-height: 9
#| fig-align: center
related_data$g <- as.factor(ifelse(rbinom(200, 1, prob = 0.5) == 1, 'F', 'M'))
ggplot(related_data, aes(x = x , y = y, color = g, shape = g)) +
  geom_point(size = 4) +
  guides(colour = guide_legend(override.aes = list(size=7))) +
  theme_minimal() +
  theme(
    plot.title=element_text(size=35),
    axis.ticks.x=element_blank(),
    axis.text=element_text(size=30),
    axis.title=element_text(size=30),
    legend.title = element_blank(),
    legend.position = 'top',
    legend.text = element_text(size = 25)
  )


## -----------------------------------------------------------------------------
#| code-line-numbers: "7"
#| fig-width: 18
#| fig-height: 9
#| fig-align: center
library(palmerpenguins)
library(forcats)
fct_revfreq <- \(x) fct_rev(fct_infreq(x))
ggplot(penguins, aes(x = fct_revfreq(species))) +
  geom_bar(stat = "count") +
  scale_y_continuous(
    expand = expansion(c(0,0.05)),
    breaks = seq(10,150, by = 20)) + 
  labs(
    title = "Number of observations by species",
    x = "Penguin Species"
    ) +
  theme_bw() +
    theme(
    plot.title=element_text(size=35),
    legend.text=element_text(size=25),
    axis.ticks.x=element_blank(),
    axis.text=element_text(size=30),
    axis.title=element_text(size=30)
  )


## -----------------------------------------------------------------------------
#| fig-width: 18
#| fig-height: 9
#| fig-align: center
ggplot(penguins, aes(x = fct_revfreq(species), color = sex, fill = sex)) +
  geom_bar(stat = "count") +
  scale_y_continuous(
    expand = expansion(c(0,0.05)),
    breaks = seq(10,150, by = 20)) + 
  labs(
    title = "Number of observations by species and sex",
    x = "Penguin Species"
    ) +
  theme_bw() +
    theme(
    plot.title=element_text(size=35),
    axis.ticks.x=element_blank(),
    axis.text=element_text(size=30),
    axis.title=element_text(size=30),
    legend.position='top',
    legend.text=element_text(size=25),
    legend.title=element_blank()
  )


## -----------------------------------------------------------------------------
#| code-line-numbers: "2"
#| fig-width: 18
#| fig-height: 9
#| fig-align: center
ggplot(penguins, aes(x = fct_revfreq(species), color = sex, fill = sex)) +
  geom_bar(stat = "count", position = 'dodge') +
  scale_y_continuous(
    expand = expansion(c(0,0.05)),
    breaks = seq(10,150, by = 20)) + 
  labs(
    title = "Number of observations by species and sex",
    x = "Penguin Species"
    ) +
  theme_bw() +
    theme(
    plot.title=element_text(size=35),
    axis.ticks.x=element_blank(),
    axis.text=element_text(size=30),
    axis.title=element_text(size=30),
    legend.position='top',
    legend.text=element_text(size=25),
    legend.title=element_blank()
  )


## -----------------------------------------------------------------------------
#| fig-width: 12
#| fig-heigt: 4
#| fig-align: center
#| dpi: 600
set.seed(1)
xy = expand.grid(seq(0,1, length.out = 5), seq(0,1, length.out = 5))
text = sample(c("a", "b", "c", "d", "e"), 25, replace = TRUE)
par(mar = c(0.1,0,0,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n',type = 'n', xaxt = 'n', yaxt = 'n')
text(xy[,1], xy[,2], text, cex = 3)


## -----------------------------------------------------------------------------
#| fig-align: center
#| fig-width: 12
#| fig-heigt: 4
#| dpi: 600
par(mar = c(0,0,0,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n',type = 'n',  xaxt = 'n', yaxt = 'n')
text(xy[,1], xy[,2], text, cex = 3, col = ifelse(text == "b", "black", "gray"))


## -----------------------------------------------------------------------------
#| fig-width: 12
#| fig-heigt: 4
#| dpi: 600
par(mar = c(0,0,0,0))
text_cyr <- sample(c("ш", "ц", "ж", "є", "ґ"), 25, replace = TRUE)
plot(c(0, 1), c(0, 1), ann = F, bty = 'n',type = 'n',  xaxt = 'n', yaxt = 'n')
text(xy[,1], xy[,2], text_cyr, cex = 3, col = ifelse(text_cyr == "ш", "black", "gray"))


## -----------------------------------------------------------------------------
#| fig-width: 18
#| fig-height: 9
#| fig-align: center
library(gapminder)
library(dplyr)
library(scales)
gm_last <- gapminder |>
  group_by(country) |>
  slice_max(year) |>
  mutate(hl = as.factor(ifelse(country %in% c("China", "United States"), country, 'other')))
ggplot(gm_last, aes(y = lifeExp, x = gdpPercap, size = pop, color = hl)) +
  geom_point() +
  scale_size(
    name = "Population",
    labels = label_number(scale_cut = cut_short_scale()),
    ) +
  geom_text(
    aes(label = country, x = gdpPercap, y = lifeExp),
    data = filter(gm_last, country %in% c('China', 'United States')),
    hjust = c(-0.1, 0.5),
    vjust = c(4, 2),
    size = 12
    ) +
  geom_segment(
  aes(
    x = gdpPercap + c(200, 0), 
    y = lifeExp - 1, 
    xend = gdpPercap+c(1000, 0), 
    yend = lifeExp-c(5.5, 2)),
  data = filter(gm_last, country %in% c('China', 'United States')),
  size = 0.5
  ) +
  scale_x_continuous(labels = label_comma()) +
  scale_color_manual(values = c('gray20', 'red', 'darkblue')) +
  guides(colour = 'none') +
  labs(y = "Life Expectancy", x = "GDP/Capita") +
  theme_bw() +
    theme(
    plot.title=element_text(size=35),
    axis.ticks.x=element_blank(),
    axis.text=element_text(size=30),
    axis.title=element_text(size=30),
    legend.position='top',
    legend.text=element_text(size=25),
    legend.title=element_text(size=25)
  )


## -----------------------------------------------------------------------------
#| fig-width: 18
#| fig-height: 9
#| fig-align: center
penguins |>
  group_by(species) |>
  summarize(count = n()) |>
ggplot(aes(x = "", y = count, fill = species)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of observations by species",
    ) +
  theme_bw() +
    theme(
    plot.title=element_text(size=35),
    axis.ticks=element_blank(),
    axis.text=element_blank(),
    axis.title=element_blank(),
    legend.position='top',
    legend.text=element_text(size=25),
    legend.title=element_blank()
  ) +
  coord_polar("y", start=0) 


## -----------------------------------------------------------------------------
#| code-line-numbers: "5,8"
#| fig-width: 18
#| fig-height: 9
#| fig-align: center
library(datasets)
library(scales)
ggplot(data.frame(abb = state.abb, area = state.area), aes(x = abb, y = area)) +
  geom_bar(stat = 'identity') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(
    expand = expansion(c(0,0.05)),
    labels = label_comma(suffix = " sq mi")) + 
  theme_bw() +
    theme(
    plot.title=element_text(size=35),
    axis.ticks.x=element_blank(),
    axis.text=element_text(size=20),
    axis.title=element_text(size=30),
    axis.title.x=element_blank()
  )


## -----------------------------------------------------------------------------
#| code-line-numbers: "3,5,12-17"
#| fig-width: 34
#| fig-height: 14
#| fig-align: center
library(colorspace)
library(forcats)
state_areas <- data.frame(abb = state.abb, area = state.area, name = state.name) |>
  mutate(abb = fct_reorder(abb, area))
ggplot(state_areas, aes(x = abb, y = area, 
  color = abb == 'MO', fill = abb == 'MO')) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(
    expand = expansion(c(0,0.05)), breaks = seq(50000,600000, by = 100000),
    labels = label_comma(suffix = " sq mi")) + 
  scale_color_manual(values = c('gray85', 'red')) +
  scale_fill_manual(values = c('gray20', 'red')) +
  geom_text(
    aes(label=abb, y = area), 
    position=position_dodge(width=0.9), 
    vjust=-0.50, 
    color = ifelse(state_areas$abb=='MO', 'red', 'gray20'),
    size = 8)+
  labs(title = "US state areas") +
  theme_bw() +
    theme(
    plot.title=element_text(size=45),
    axis.ticks.x=element_blank(),
    axis.text=element_text(size=35),
    axis.text.x=element_blank(),
    axis.title=element_blank(),
    legend.position='none'
  )


## -----------------------------------------------------------------------------
library(maps)
library(sf)
library(colorspace)
us_map_data <- map("state", fill=TRUE, plot =FALSE)

usa <- st_as_sf(map("state", fill=TRUE, plot =FALSE))
usa <- merge(
  usa, 
  mutate(state_areas, ID = str_to_lower(name)),
  )
ggplot(usa) +
  geom_sf(aes(fill = area), color = "#2b2b2b", size=0.125) +
  coord_sf(crs = st_crs(6350)) +
  geom_sf_text(aes(label = abb, color = abb == 'MO')) +
  scale_color_manual(values = c('gray20', 'red')) +
  guides(color = 'none') + 
  ggthemes::theme_map() +
  scale_fill_binned_sequential(
    palette = "Heat", 
    labels = label_comma(suffix = " sq mi") 
    )


## -----------------------------------------------------------------------------
#| fig-width: 40
#| fig-height: 25
#| fig-align: center
## Example using "shapefile"
### e.g. for EU download here https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
eu <- st_read("data/NUTS_RG_60M_2021_3035.shp/", quiet = TRUE)
eu0 <- filter(eu, LEVL_CODE == 0)
## Transform to longitude and latitude
eu0 <- eu0 |> st_transform(4326)
eu0_box <- eu0 |> st_bbox()
eu0_box <- eu0_box + c(50, 20, 0, -10)
ggplot(st_crop(eu0, eu0_box))+ 
  geom_sf(fill = 'white', lwd = 1) +
  geom_sf(data = filter(eu0, CNTR_CODE == 'AT'), color = 'red', lwd =1.5) + 
  coord_sf(crs = st_crs(3035)) +
  geom_sf_text(aes(label = CNTR_CODE, color = CNTR_CODE == 'AT'), size = 15) +
  scale_color_manual(values = c('gray20', 'red')) +
  guides(color = 'none') + 
  ggthemes::theme_map(base_size = 12) 


## -----------------------------------------------------------------------------
#| fig-width: 24
#| fig-height: 12
charts <- arrow::read_parquet(
    "data/chart_data/spotify_charts.parquet")  |> 
    filter(country %in% c("de", "fr")) |>
    group_by(country, date) |>
    summarize(total_streams = sum(streams)) 
charts |>
    ggplot(aes(x = date, y = log(total_streams))) +
    geom_point(aes(color = country), size = 4) +
    guides(colour = guide_legend(override.aes = list(size=7))) +
    scale_color_discrete_qualitative(palette = "Dark 2") +
    theme_classic() +
    labs(y = "log(total streams)", title = "Total streams 2023/24", subtitle = "Top 200 songs") +
    theme(
        legend.text = element_text(size = 35),
        legend.title = element_blank(),
        axis.text=element_text(size=35),
        axis.title=element_text(size=35),
        axis.title.x=element_blank(),
        legend.position = 'top',
        plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 35)
    )


## -----------------------------------------------------------------------------
#| fig-width: 24
#| fig-height: 12
charts <- arrow::read_parquet(
    "data/chart_data/spotify_charts.parquet")  |> 
    filter(country %in% c("de", "fr")) |>
    group_by(country, date) |>
    summarize(total_streams = sum(streams)) 
charts |>
    ggplot(aes(x = date, y = log(total_streams), color = country)) +
    geom_point(size = 4) +
    geom_line() +
    guides(colour = guide_legend(override.aes = list(size=7))) +
    scale_color_discrete_qualitative(palette = "Dark 2") +
    theme_classic() +
    labs(y = "log(total streams)", title = "Total streams 2023/24", subtitle = "Top 200 songs") +
    theme(
        legend.text = element_text(size = 35),
        legend.title = element_blank(),
        axis.text=element_text(size=35),
        axis.title=element_text(size=35),
        axis.title.x=element_blank(),
        legend.position = 'top',
        plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 35)
    )


## -----------------------------------------------------------------------------
#| fig-width: 24
#| fig-height: 12
#| fig-align: center
penguins |>
ggplot(aes(x = bill_length_mm, y = bill_depth_mm, #color = species
)) +
    geom_point(size = 4) +
    geom_smooth(method = 'lm', se = FALSE) +
    guides(colour = guide_legend(override.aes = list(size=7))) +
    theme_classic() +
    labs(y = "Bill depth", x = "Bill length") +
    theme(
        axis.text=element_text(size=35),
        axis.title=element_text(size=35),
    )


## -----------------------------------------------------------------------------
#| fig-width: 24
#| fig-height: 12
#| fig-align: center
penguins |>
ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
    geom_point(size = 4) +
    geom_smooth(method = 'lm', se = FALSE) +
    guides(colour = guide_legend(override.aes = list(size=7))) +
    theme_classic() +
    labs(y = "Bill depth", x = "Bill length") +
    theme(
        legend.text = element_text(size = 35),
        legend.title = element_blank(),
        axis.text=element_text(size=35),
        axis.title=element_text(size=35),
        legend.position = 'top',
    )


## -----------------------------------------------------------------------------
#| fig-width: 24
#| fig-height: 12
#| fig-align: center
penguins |>
ggplot(aes(
  y = bill_length_mm, 
  x = fct_reorder(species, bill_length_mm, .fun = median, .na_rm = TRUE))) +
    geom_boxplot(na.rm = TRUE) +
    theme_classic() +
    labs(y = "Bill length") +
    theme(
        legend.text = element_text(size = 35),
        legend.title = element_blank(),
        axis.text=element_text(size=35),
        axis.title=element_text(size=35),
        axis.title.x=element_blank(),
        legend.position = 'top',
    )


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
#| fig-width: 5
#| fig-height: 3
exper <- dagify( x ~ d, y ~ d, y ~ x,
       coords = list(x = c(x = 1, d = 1.5, y = 2), 
                     y = c(x = 0, d = 1, y = 0))
) |> 
  tidy_dagitty() %>%
  mutate(fill = ifelse(name == "d", "Confounder", "variables of interest")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point(size=7, aes(color = fill)) + 
  geom_dag_edges(
    show.legend = FALSE, 
    aes(
      edge_color = ifelse(name == 'd', 'gray80', 'darkgreen'),
      edge_linetype = ifelse(name == 'd', 'dashed', 'solid')
      ))+
  geom_dag_text() +
  theme_dag() +
  theme(legend.title  = element_blank(),
        legend.position = "top") +
  geom_text(aes(x = 1.5, y = 1, label = '← kept constant'), hjust = -0.3)
exper


## -----------------------------------------------------------------------------
#| fig-width: 7
#| fig-height: 8
#| fig-align: center
library(ggstatsplot)
library(palmerpenguins)
ggbetweenstats(
  data = penguins,
  x = species,
  y = body_mass_g,
  type = 'nonparametric',
  violin.args = list(width = 0, linewidth=0),
  point.args = list(alpha = 0),
  bf.message = FALSE
) + labs(y = "body mass")


## -----------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 5
#| fig-align: center
ggcoefstats(
  lm(body_mass_g ~ species + sex, data = penguins)
) + labs(caption = 'Error bars show 95% confidence interval')


## -----------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 5
#| fig-align: center
body_mass_mod <- lm(body_mass_g ~ species + sex, data = penguins)
tidy_coefs <- broom::tidy(body_mass_mod)
tidy_coefs$df.error <- body_mass_mod$df.residual
tidy_coefs <- confint(body_mass_mod) |> 
  data.frame() |> 
  dplyr::rename(conf.low = 'X2.5..', conf.high = 'X97.5..') |>
  cbind(tidy_coefs) |>
  mutate(term = str_replace_all(
    term, c(
      sexmale = "male (vs. female)",
      speciesGentoo = "Gentoo (vs. Adelie)",
      speciesChinstrap = "Chinstrap (vs. Adelie)"
    )))
ggcoefstats(
  tidy_coefs, 
  statistic = 't',
  exclude.intercept = TRUE
  ) + 
  labs(title = 'DV: body mass (g)', caption = 'Intercept omitted; error bars show 95% confidence interval') +
  scale_x_continuous(labels = label_number(suffix = 'g')) +
  theme(axis.title.y = element_blank())


## -----------------------------------------------------------------------------
#| fig-width: 6
#| fig-height: 6
#| fig-align: center
#| code-fold: false
corrs <- palmerpenguins::penguins |> 
  drop_na() |> 
  select(bill_length_mm, bill_depth_mm, flipper_length_mm) |>
  cor()
cor_df <- data.frame(cor = c(corrs), var1 = factor(col(corrs)), var2 = factor(row(corrs)))
ggplot(cor_df, aes(var1, var2, fill = cor)) + 
  geom_tile() + 
  coord_fixed() +
  ylab("variable") +
  scale_x_discrete(position = "top", name = "variable") +
  scale_fill_continuous_diverging("Blue-Red 3")


## -----------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 8
#| fig-align: center
#| code-fold: false
#| output-location: column
ggplot(penguins, 
  aes(x = fct_revfreq(species), 
      color = sex, fill = sex)) +
  geom_bar(stat = "count", position = 'dodge') +
  scale_fill_discrete_qualitative("pastel 1") +
  scale_y_continuous(
    expand = expansion(c(0,0.05)),
    breaks = seq(10,150, by = 20)) + 
  labs(
    title = "Number of observations by species and sex",
    x = "Penguin Species"
    ) +
  theme_bw() +
    theme(
    plot.title=element_text(size=35),
    axis.ticks.x=element_blank(),
    axis.text=element_text(size=30),
    axis.title=element_text(size=30),
    legend.position='top',
    legend.text=element_text(size=25),
    legend.title=element_blank()
  )


## -----------------------------------------------------------------------------
#| fig-align: center
#| code-fold: false
#| output-location: column
#| fig-height: 10
swatchplot(diverging_hcl(7, 'Red-Green'), cvd = TRUE)

