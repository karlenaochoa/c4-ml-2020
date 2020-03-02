library(tidyverse)

d <- read_csv(here::here("data", "raw-data", "math1718.csv"))

# d %>% 
#   count(is.na(rit_tot))

# d %>% 
#   filter(rit_tot < 10) %>% 
#   View()

d <- d %>% 
  filter(rit_tot > 0)

rnorm_v <- function(mean_v, sd_v) {
  if(length(mean) != length(sd)) {
    stop("vectors are of different lengths")
  }
  map2_dbl(mean_v, sd_v, rnorm, n = 1)
}

set.seed(8675309)
d <- d %>% 
  mutate(score = rnorm_v(rit_tot, sem_tot))

# Do some double checking
ggplot(d, aes(score, rit_tot)) +
  geom_point() +
  facet_wrap(~srt_tst_typ, scales = "free")

d %>% 
  group_by(attnd_schl_inst_id) %>% 
  summarize(mean_raw = mean(rit_tot),
            mean_sim = mean(score)) %>% 
  ggplot(aes(mean_sim, mean_raw)) +
  geom_point()

d %>% 
  group_by(attnd_dist_inst_id) %>% 
  summarize(mean_raw = mean(rit_tot),
            mean_sim = mean(score)) %>% 
  ggplot(aes(mean_sim, mean_raw)) +
  geom_point()

# fix it up for final output
## redo cut scores
maxes <- d %>% 
  group_by(tst_bnch, pl5g_tot) %>% 
  summarize(cut = max(rit_tot)) %>% 
  filter(pl5g_tot < 4) %>% 
  filter(!(grepl("^X", tst_bnch) & pl5g_tot == 2))

classify <- function(score, cuts) {
  if(length(cuts) == 1) {
    if(score <= cuts) { 
      out <- 1
    } else {
      out <- 2 
    }
  }

  if(length(cuts) > 1) {
    if(score <= cuts[1]) { 
      out <- 1
    } else if(score > cuts[1] & score <= cuts[2]) {
      out <- 2
    } else if(score > cuts[2] & score <= cuts[3]) {
      out <- 3
    } else {
      out <- 4
    } 
  }
  out
}

cuts <- maxes %>% 
  group_by(tst_bnch) %>% 
  nest() %>% 
  mutate(cuts = map(data, ~sort(.x$cut))) %>% 
  select(-data)

d <- left_join(d, cuts) %>% 
  mutate(score = round(score),
         classification = map2_dbl(score, cuts, classify))
d %>% 
  count(srt_tst_typ, pl5g_tot, classification) %>% 
  filter(srt_tst_typ == "T") %>% 
  ggplot(aes(classification, pl5g_tot)) +
  geom_tile(aes(fill = n)) +
  colorspace::scale_fill_continuous_diverging(palette = "Blue-Red 3") +
  theme_minimal()

# write out file
d %>% 
  rowid_to_column("id") %>% 
  select(-rit_tot, -cuts, -pl5g_tot) %>% 
  select(id:tst_valid_fg, score, sem_tot, classification, everything()) %>% 
  write_csv(here::here("data", "state-test-simulated.csv"))
