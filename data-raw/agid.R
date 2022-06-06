library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(usethis)

list <- read_lines("data-raw/infl.txt") %>%
  enframe(name = NULL, value = "line") %>%
  mutate(
    singular = stringr::str_extract(line, "[A-Za-z]+"),
    line     = stringr::str_squish(stringr::str_replace(line, singular, "")),
    part     = stringr::str_extract(stringr::str_extract(line, ".+:"), "[A-Z]")
  ) %>%
  filter(part == "N") %>%
  mutate(
    line = stringr::str_squish(
      stringr::str_replace_all(
        line, c("\\{.*?\\}" = "", ".*?:" = "", "[^A-Za-z ]" = "")
      )
    ),
    guessed_plural = plu::ralize(singular, irregulars = "none")
  ) %>%
  rowwise() %>%
  mutate(
    line = list(as.character(stringr::str_split(line, " ", simplify = TRUE)))
  ) %>%
  select(-part) %>%
  bind_rows(
    tribble(
      ~singular, ~line,
      "emoji",   c("emojis",     "emoji"),
      "ghoti",   c("ghoti",      "ghotis"),
      "lasagna", c("lasagnas",   "lasagne"),
      "octopus", c("octopuses",  "octopodes"),
      "octopus", c("platypuses", "platypodes")
    ) %>%
      mutate(guessed_plural = plu::ralize(singular, irregulars = "none"))
  ) %>%
  group_by(singular) %>%
  slice(n()) %>%
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
    plural = stringr::str_extract(line, "[A-Za-z]+"),
    line   = stringr::str_squish(stringr::str_replace(line, plural, "")),
    part   = stringr::str_extract(stringr::str_extract(line, ".+:"), "[A-Z]")
  ) %>%
  filter(part == "V", !plural %in% c("be", "wit")) %>%
  mutate(
    line = stringr::str_squish(
      stringr::str_replace_all(line, ".*:|\\{.*?\\}|[^A-Za-z ]", "")
    ),
    singular = stringr::str_extract(line, "[A-Za-z]+$")
  ) %>%
  filter(!singular %in% list$singular) %>%
  group_by(plural) %>%
  slice(n()) %>%
  select(singular, plural)

grammar_list <- tribble(
  ~ singular, ~ plural,
  "a",        "",
  "an",       "",
  "I",        "we",
  "I'm",      "we're",
  "me",       "us",
  "myself",   "ourselves",
  "my",       "our",
  "mine",     "ours",
  "thou",     "you",
  "thee",     "you",
  "yourself", "yourselves",
  "thyself",  "yourselves",
  "thy",      "your",
  "thine",    "yours",
  "he",       "they",
  "he's",     "they're",
  "she",      "they",
  "she's",    "they're",
  "it",       "they",
  "it's",     "they're",
  "him",      "them",
  "her",      "them",
  "himself",  "themselves",
  "herself",  "themselves",
  "itself",   "themselves",
  "themself", "themselves",
  "his",      "their",
  "its",      "their",
  "hers",     "theirs",
  "this",     "these",
  "that",     "those",
  "is",       "are",
  "was",      "were",
  "isn't",    "aren't",
  "wasn't",   "weren't",
  "does",     "do"
)

moderate_list     <- bind_rows(moderate_list, verb_list, grammar_list)
conservative_list <- bind_rows(conservative_list, verb_list, grammar_list)
liberal_list      <- bind_rows(liberal_list, verb_list, grammar_list)

str_to_sentence <- function(string) {
  sub("^(.*?)(\\p{L})(.*)$", "\\1\\U\\2\\E\\3", string, perl = TRUE)
}

moderate_list <- moderate_list %>%
  bind_rows(
    mutate(
      moderate_list,
      singular = str_to_sentence(singular),
      plural = str_to_sentence(plural)
    )
  ) %>%
  arrange(singular) %>%
  distinct()

conservative_list <- conservative_list %>%
  bind_rows(
    mutate(
      conservative_list,
      singular = str_to_sentence(singular),
      plural = str_to_sentence(plural)
    )
  ) %>%
  arrange(singular) %>%
  distinct()

liberal_list <- liberal_list %>%
  bind_rows(
    mutate(
      liberal_list,
      singular = str_to_sentence(singular),
      plural = str_to_sentence(plural)
    )
  ) %>%
  arrange(singular) %>%
  distinct()

usethis::use_data(
  moderate_list, conservative_list, liberal_list,
  internal = TRUE, overwrite = TRUE
)
