library(tidyverse)
library(ggthemes)
library(gridExtra)

libor <- read_csv(here::here("./data/USDONTD156N.csv"),
                  col_types = "Dn",
                  col_names = c("date", "libor")) %>%
    drop_na()
effr  <- read_csv(here::here("./data/EFFR.csv"),
                  col_types =  "Dn",
                  col_names = c("date", "effr")) %>%
    drop_na()

rates <- libor %>%
    full_join(effr, by = "date") %>%
    pivot_longer(cols = -date,
                 names_to = "source",
                 values_to = "rate")

diffs <- libor %>%
    full_join(effr, by = "date") %>%
    mutate(diff = libor - effr)

# gg_rate

gg_rate <- rates %>%
    ggplot(aes(x = date, y = rate, group = source)) +
    geom_line(aes(col = source)) +
    theme_few() +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1))

gg_diff <- diffs %>%
    ggplot(aes(x = date, y = diff)) +
    geom_line() +
    ylab("LIBOR - EFFR") +
    theme_few()

grid.arrange(gg_rate, gg_diff,
             ncol = 1,
             top = "Comparison of LIBOR and Effective Federal Funds Rate")
