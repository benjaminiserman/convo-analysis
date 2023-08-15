repo <- "http://cran.us.r-project.org"

# install missing dependencies
dependencies <- c("lsa", "LSAfun", "tidyverse")
new_packages <-
  dependencies[!(dependencies %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# imports
library(lsa)
library(LSAfun)
library(tidyverse)
library(dplyr)

# aggregate text by participant
# note that this currently treats conversations from different days separately
input_data <- read.csv("convos/Totally Real Conversation Data - Sheet1.csv")
aggregated_text_by_participant <- input_data %>%
  group_by(ID, F.ID, M.ID, Date.Number) %>%
  summarize(Event = paste(Event, collapse = " "))

# write participant text to files for LSA
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

# tvectors <- textmatrix(paste(getwd()[1], "/convos", sep = ""))
load("TASA.rda")

# create a list of pairs of participants who conversed
convo_participant_pairs <- input_data %>%
  group_by(F.ID, M.ID) %>%
  summarize(F.ID = F.ID[1], M.ID = M.ID[1])

for (i in seq_len(nrow(convo_participant_pairs))) {
  # find conversations from this pair
  f_text <- aggregated_text_by_participant %>%
    filter(
      ID == convo_participant_pairs[[i, "F.ID"]],
      F.ID == convo_participant_pairs[[i, "F.ID"]],
      M.ID == convo_participant_pairs[[i, "M.ID"]]
    )

  m_text <- aggregated_text_by_participant %>%
    filter(
      ID == convo_participant_pairs[[i, "M.ID"]],
      F.ID == convo_participant_pairs[[i, "F.ID"]],
      M.ID == convo_participant_pairs[[i, "M.ID"]]
    )

  # get cosine similarity result for conversation
  # note we are currently only acting on the first matched convo
  result <- costring(
    f_text[[1, "Event"]],
    m_text[[1, "Event"]],
    tvectors = TASA
  )

  print(paste(
    "Result for ",
    convo_participant_pairs[[i, "F.ID"]],
    " & ",
    convo_participant_pairs[[i, "M.ID"]],
    ": ",
    result,
    sep = ""
  ))
}