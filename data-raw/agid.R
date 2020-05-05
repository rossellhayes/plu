library(dplyr)
library(readr)
library(stringr)
library(tibble)

list <- read_lines("data-raw/infl.txt") %>%
  enframe(name = NULL, value = "line") %>%
  mutate(
    singular = str_extract(line, "[A-Za-z]+"),
    line     = str_squish(str_replace(line, singular, "")),
    part     = str_extract(str_extract(line, ".+:"), "[A-Z]")
  ) %>%
  filter(part == "N") %>%
  mutate(
    line = str_squish(
      str_replace_all(line, c("\\{.*?\\}" = "", ".*?:" = "", "[^A-Za-z ]" = ""))
    ),
    guessed_plural = pluralize(singular, irregulars = "none")
  ) %>%
  rowwise() %>%
  mutate(
    line = list(as.character(str_split(line, " ", simplify = TRUE)))
  ) %>%
  select(-part) %>%
  bind_rows(
    tribble(
      ~singular, ~line,
      "emoji",   c("emojis", "emoji"),
      "ghoti",   c("ghoti", "ghotis"),
      "octopus", c("octopuses", "octopodes")
    ) %>%
      mutate(guessed_plural = pluralize(singular, irregulars = "none"))
  ) %>%
  group_by(singular) %>%
  slice_tail() %>%
  rowwise()

moderate_list <- list %>%
  mutate(plural = line[[1]]) %>%
  filter(guessed_plural != plural, plural != "") %>%
  ungroup() %>%
  select(singular, plural)

conservative_list <- list %>%
  mutate(line = list(unlist(line)[!unlist(line) == guessed_plural])) %>%
  filter(length(line) != 0) %>%
  mutate(plural = line[[1]]) %>%
  ungroup() %>%
  select(singular, plural)

liberal_list <- list %>%
  filter(!guessed_plural %in% line) %>%
  mutate(plural = line[[1]]) %>%
  ungroup() %>%
  select(singular, plural)

verb_list <- read_lines("data-raw/infl.txt") %>%
  enframe(name = NULL, value = "line") %>%
  mutate(
    plural = str_extract(line, "[A-Za-z]+"),
    line   = str_squish(str_replace(line, plural, "")),
    part   = str_extract(str_extract(line, ".+:"), "[A-Z]")
  ) %>%
  filter(part == "V", !plural %in% c("be", "wit")) %>%
  mutate(
    line     = str_squish(str_replace_all(line, ".*:|\\{.*?\\}|[^A-Za-z ]", "")),
    singular = str_extract(line, "[A-Za-z]+$")
  ) %>%
  filter(!singular %in% list$singular) %>%
  group_by(plural) %>%
  slice_tail() %>%
  select(singular, plural) %>%
  bind_rows(
    tribble(
      ~ singular, ~ plural,
      "a",        "",
      "an",       "",
      "is",       "are",
      "was",      "were",
      "isn't",    "aren't",
      "wasn't",   "weren't"
    )
  )

moderate_list     <- bind_rows(moderate_list, verb_list)
conservative_list <- bind_rows(conservative_list, verb_list)
liberal_list      <- bind_rows(liberal_list, verb_list)

use_data(
  moderate_list, conservative_list, liberal_list,
  internal = TRUE, overwrite = TRUE
)
