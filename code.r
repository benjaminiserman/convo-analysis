repo <- "http://cran.us.r-project.org"

dependencies <- c("lsa", "LSAfun", "tidyverse")
new_packages <-
  dependencies[!(dependencies %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}

library(lsa)
library(LSAfun)
library(tidyverse)
library(dplyr)

test <- function(v) {
  print(paste("test: ", v))
  n()
}

input_data <- read.csv("Totally Real Conversation Data - Sheet1.csv")
aggregated_text_by_participant <- input_data %>%
  group_by(ID, F.ID, M.ID, Date.Number) %>%
  summarize(Event = paste(Event, collapse = " "))

for (i in seq_len(nrow(aggregated_text_by_participant))) {
  file_to_write <- file(paste(
    "participant_text_for_",
    aggregated_text_by_participant[[i, "ID"]],
    ".txt",
    sep = ""
  ))
  writeLines(aggregated_text_by_participant[[i, "Event"]], file_to_write)
  close(file_to_write)
}

tvectors <- textmatrix(getwd())
convos <- input_data %>%
  group_by(F.ID, M.ID) %>%
  summarize(F.ID = F.ID[1], M.ID = M.ID[1])

for (i in seq_len(nrow(convos))) {
  f_text <- aggregated_text_by_participant %>%
    filter(
      ID == convos[[i, "F.ID"]],
      F.ID == convos[[i, "F.ID"]],
      M.ID == convos[[i, "M.ID"]]
    )

  m_text <- aggregated_text_by_participant %>%
    filter(
      ID == convos[[i, "M.ID"]],
      F.ID == convos[[i, "F.ID"]],
      M.ID == convos[[i, "M.ID"]]
    )

  result <- costring(
    f_text[[1, "Event"]],
    m_text[[1, "Event"]],
    tvectors = tvectors
  )
  print(paste(
    "Result for ",
    convos[[i, "F.ID"]],
    " & ",
    convos[[i, "M.ID"]],
    ": ",
    result,
    sep = ""
  ))
}