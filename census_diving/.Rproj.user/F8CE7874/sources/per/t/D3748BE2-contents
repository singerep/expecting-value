library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(ggplot2)
library(scales)
library(ineq)

census_api_key("11d46bc70e375d39b67b4b4919a0099934aecbc7")

tracts <- list()
for (state_code in unique(fips_codes$state_code)[1:51]) {
  print(state_code)
  state_tracts <- get_acs(
    geography = "tract",
    state = state_code,
    year = 2021, 
    survey = "acs5",
    variables = c("B19013_001", "B25040_001", "B25040_002", "B25040_003", "B25040_004", "B25040_005", "B25040_006", "B25040_007", "B25040_008"),
    # table = c("B25040"),
    geometry = TRUE, 
    output = "wide"
  )
  tracts <- bind_rows(tracts, state_tracts)
}

tracts <- tracts %>%
  shift_geometry() %>%
  rename("Total" = B25040_001E, "Gas" = B25040_002E, "Tank" = B25040_003E, "Electric" = B25040_004E, "Oil" = B25040_005E, "Coal" = B25040_006E, "Wood" = B25040_007E, "Solar" = B25040_008E)

counties <- list()
for (year in c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021)) {
  year_counties <- get_acs(
    geography = "county",
    year = year, 
    survey = "acs5", 
    table = c("B25040"),
    geometry = TRUE,
    output = "wide"
  ) %>%
    shift_geometry()
  year_counties["year"] <- year
  counties <- bind_rows(counties, year_counties)
}

counties <- counties %>%
  rename("Total" = B25040_001E, "Gas" = B25040_002E, "Tank" = B25040_003E, "Electric" = B25040_004E, "Oil" = B25040_005E, "Coal" = B25040_006E, "Wood" = B25040_007E, "Solar" = B25040_008E)

states <- list()
for (year in c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021)) {
  year_states <- get_acs(
    geography = "state",
    year = year, 
    survey = "acs1", 
    table = c("B25040"),
    output = "wide"
  )
  year_states["year"] <- year
  states <- bind_rows(states, year_states)
}

states <- states %>%
  rename("Total" = B25040_001E, "Gas" = B25040_002E, "Tank" = B25040_003E, "Electric" = B25040_004E, "Oil" = B25040_005E, "Coal" = B25040_006E, "Wood" = B25040_007E, "Solar" = B25040_008E) %>%
  left_join(., get_acs(geography = "state", variables = c("B01001_001"), year = "2021", survey = "acs1",  geometry = TRUE, output = "wide") %>%
  select(GEOID, geometry)) %>%
  st_as_sf() %>%
  shift_geometry()

states %>%
  filter(GEOID != "72" & year == "2021") %>%
  as_tibble() %>%
  select("NAME", "Gas", "Tank", "Electric", "Oil", "Coal", "Wood", "Solar") %>%
  pivot_longer(cols = c("Gas", "Tank", "Electric", "Oil", "Coal", "Wood", "Solar"), names_to = "energy") %>%
  mutate(energy = fct_relevel(energy, c("Gas", "Electric", "Tank", "Oil", "Wood", "Solar", "Coal"))) %>%
  group_by(energy) %>%
  summarise(total = sum(value)) %>%
  ggplot() +
    geom_bar(aes(x=energy, y=total, fill=energy), stat="identity") +
    geom_text(aes(x=energy, y=total, label=percent(total/sum(total))), size=3, vjust=-0.4) +
    scale_y_continuous("Total Occupied Housing Units", labels = scales::label_number(scale = 1e-6, suffix = "m")) +
    labs(title = "Heating Fuel Distribution", caption = "Ethan Singer | American Community Survey") +
    xlab("Energy Source") +
    theme(legend.position = "none")
ggsave("B25040_008/current_distr.png")

states %>%
  filter(GEOID != "72") %>%
  as_tibble() %>%
  select("NAME", "Gas", "Tank", "Electric", "Oil", "Coal", "Wood", "Solar", "year") %>%
  pivot_longer(cols = c("Gas", "Tank", "Electric", "Oil", "Coal", "Wood", "Solar"), names_to = "energy") %>%
  mutate(energy = fct_relevel(energy, c("Gas", "Electric", "Tank", "Wood", "Oil", "Solar", "Coal"))) %>%
  group_by(year, energy) %>%
  summarise(total = sum(value)) %>%
  ggplot() +
    geom_bar(aes(x=year, y=total, fill=energy), position="fill", stat="identity") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous("% of Occupied Housing Units", labels = scales::percent) +
    labs(title = "Heating Fuel Distribution by Year", fill="Energy Source", caption = "Ethan Singer | American Community Survey") +
    xlab("Year") +
    theme(legend.position = "bottom", legend.margin = margin(t=-0.2, b=0.2, unit = "cm")) + 
    guides(fill = guide_legend(nrow = 1))
ggsave("B25040_008/distr_by_year.png")

states %>%
  filter(GEOID != "72") %>%
  # mutate(prop = Solar/Total) %>%
  # arrange(prop) %>%
  # select(NAME, prop)
  st_simplify(dTolerance = 1000) %>%
  ggplot() +
    geom_sf(aes(fill=Solar/Total), color=NA) +
    scale_fill_gradient2(low = "black", mid = "#FDB813", high = "#ffdd00", labels=scales::percent, midpoint = 0.02, guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
    labs(title = "Proportion of Housing Units Using Solar Energy for Heat", caption = "Ethan Singer | American Community Survey") +
    theme(
      panel.grid.major = element_blank(), 
      axis.text=element_blank(), 
      axis.ticks=element_blank(), 
      legend.position = c(0.08, 0.7), 
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.key.height = unit(1, 'cm')
    )
ggsave("B25040_008/states.png")

counties %>%
  filter(!startsWith(GEOID, "72")) %>%
  filter(year == "2021") %>%
  as_tibble() %>%
  mutate(prop = Solar/Total, state = substr(GEOID, 1, 2)) %>%
  arrange(desc(prop)) %>%
  select(NAME, prop)
  # filter(prop == 0) %>%
  # select(state) %>%
  # unique()
  # st_simplify(dTolerance = 1000) %>%
  # ggplot() +
  # geom_sf(aes(fill=Solar/Total), color=NA) +
  # scale_fill_gradient2(low = "black", mid = "#FDB813", high = "#ffdd00", name = "", labels=scales::percent, midpoint = 0.03, guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
  # labs(title = "Proportion of Housing Units Using Solar Energy for Heat", caption = "Ethan Singer | American Community Survey") +
  # theme(panel.grid.major = element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), legend.position = c(0.08, 0.72), legend.background = element_blank(), legend.key.height = unit(1, 'cm'))
ggsave("B25040_008/counties.png")

tracts %>%
  filter(!startsWith(GEOID, "72") & year == "2021") %>%
  as_tibble() %>%
  select("NAME", "Solar") %>%
  pivot_longer(cols = c("Solar"), names_to = "energy") %>%
  group_by(energy) %>%
  arrange(value) %>%
  mutate(cum_tract_prop = 100*row_number()/n(), cum_house_prop = 100*cumsum(value)/sum(value)) %>%
  # filter(cum_tract_prop > 99) %>%
  # arrange(cum_house_prop)
  ggplot() +
    geom_line(aes(x=cum_tract_prop, y=cum_house_prop, color=energy)) +
    annotate("rect", xmin = 0, xmax = 86.4, ymin = 0, ymax = 100, alpha = .1, fill = "blue") +
    annotate("rect", xmin = 95, xmax = 100, ymin = 0, ymax = 100, alpha = .1, fill = "red") +
    annotate("curve", x = 75, y = 75, xend = 85.4, yend = 60, curvature = 0.4, arrow = arrow(length = unit(0.02, "npc"))) +
    annotate("text", x = 60, y = 81.5, label = "86% of Census tracts have\n0 solar-heated housing units", size=3) +
    annotate("curve", x = 83, y = 35, xend = 94, yend = 50, curvature = -0.4, arrow = arrow(length = unit(0.02, "npc"))) +
    annotate("text", x = 67, y = 31.5, label = "5% of Census tracts have\n73% of U.S. solar-heated housing units", size=3) +
    xlab("Cumulative % of Census Tracts") +
    ylab("Cumulative % of Housing Units\nUsing Solar Energy") +
    labs(title = 'Heat from Solar Energy Lorenz Curve', caption = "Ethan Singer | American Community Survey") +
    theme(legend.position = "none")
ggsave("B25040_008/solar_lorenz.png")

tracts %>%
  filter(!startsWith(GEOID, "72") & year == "2021") %>%
  as_tibble() %>%
  select("NAME", "Gas", "Tank", "Electric", "Oil", "Coal", "Wood", "Solar") %>%
  pivot_longer(cols = c("Gas", "Tank", "Electric", "Oil", "Coal", "Wood", "Solar"), names_to = "energy") %>%
  mutate(energy = fct_relevel(energy, c("Gas", "Electric", "Tank", "Wood", "Oil", "Solar", "Coal"))) %>%
  group_by(energy) %>%
  # summarise(gini = ineq(value, type = "Gini"))
  arrange(value) %>%
  mutate(cum_tract_prop = 100*row_number()/n(), cum_house_prop = 100*cumsum(value)/sum(value)) %>%
  ggplot() +
    annotate("segment", x = 0, y = 0, xend = 100, yend = 100, lty="dashed") +
    geom_line(aes(x=cum_tract_prop, y=cum_house_prop, color=energy)) +
    xlab("Cumulative % of Census Tracts") +
    ylab("Cumulative % of Housing Units\nUsing Energy Source") +
    labs(title = 'Energy Source Usage Lorenz Curves', color="Energy Source", caption = "Ethan Singer | American Community Survey") +
    theme(legend.position = "bottom", legend.margin = margin(t=-0.2, b=0.2, unit = "cm")) + 
    guides(color = guide_legend(nrow = 1))
ggsave("B25040_008/all_lorenz.png")

tracts %>%
  as_tibble() %>%
  filter(!startsWith(GEOID, "72") & year == "2021") %>%
  mutate(income_group = 10000*((B19013_001E %/% 10000))) %>%
  group_by(income_group) %>%
  summarize(avg = sum(Solar)/sum(Total)) %>%
  drop_na() %>%
  ggplot() +
    geom_bar(aes(x=income_group, y=avg), stat="identity", fill = "#FDB813", color="black") +
    scale_x_continuous(labels = scales::label_number(scale = 1e-3, suffix='k'), breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "% of Housing Units Using Solar Heating by Income", caption = "Ethan Singer | American Community Survey") +
    xlab("Income Bracket") +
    ylab("% of Housing Units\nUsing Solar Heating")
ggsave("B25040_008/solar_by_income.png")
