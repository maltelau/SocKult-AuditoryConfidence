
library(tidyverse)
library(purrr)
library(lubridate)
# cleaning the data
datafile = "data/confidence_audio (accessed 2017-04-26).csv"


data <- read.csv(datafile,
    na.strings=c("NA", "NaN"),
    stringsAsFactors = F) %>%
    select( participant.id_in_session, participant.code, participant.time_started,
            player.answer, player.correct_individual, group.consensus, group.correct_consensus, group.vowel,
            group.id_in_subsession,
            group.stimulus, group.intensity, subsession.round_number, session.code) %>%
    # filter so we only have trials with answers given
    filter(map_lgl(player.answer, ~nchar(.) > 0),
           complete.cases(player.correct_individual)) %>%
    # filter out the one random answer
    group_by(participant.code) %>%
    filter(n() > 1) %>%
    # clean up the group correct column
    mutate(group.correct_consensus = ifelse(nchar(group.consensus) == 1, group.correct_consensus, player.correct_individual),
           group.consensus         = ifelse(nchar(group.consensus) == 1, group.consensus, player.answer),
           # fix timezone
           participant.time_started = ymd_hms(participant.time_started, tz="UTC") + hours(2)) %>%
    # rename
    rename(
        participant = participant.code,
        intensity = group.intensity,
        vowel = group.vowel,
        group = group.id_in_subsession) %>%
    ungroup() %>%
    mutate(participant_id = as.numeric(factor(participant)),
           group_id = as.numeric(factor(group)))



individual <- data %>%
    group_by(participant, group, group_id, vowel, intensity) %>%
    summarise(k = sum(player.correct_individual),
              n = n())

group <- data %>%
    group_by(group, group_id, vowel, intensity) %>%
    summarise(k = sum(group.correct_consensus),
              n = n()) %>%
    arrange(group, intensity, vowel)

best_individuals = individual %>%
    group_by(participant, group) %>%
    summarise(k = sum(k), n = sum(n), prop = k / n) %>%
    ungroup() %>% group_by(group) %>%
    do({dat = .
        .[.$prop == max(.$prop), c("participant", "prop")] %>%
            mutate(other = min(dat$prop),
                   similarity = prop / other) %>%
            select(-c(prop, other))})

only_best_individuals <- best_individuals %>%
    left_join(individual) %>%
    ungroup() %>%
    select(-participant) %>%
    arrange(group, intensity, vowel)


write.csv(individual, "data/individual.csv", row.names = F)
write.csv(only_best_individuals, "data/best_individual.csv", row.names = F)
write.csv(group, "data/group.csv", row.names = F)





