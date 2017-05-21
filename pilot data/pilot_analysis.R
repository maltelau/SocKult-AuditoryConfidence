


# estimating the psychometric function
library(tidyverse)
library(lubridate)
library(rethinking)



sess <- "6luh1k5a"
# perf[c(29:32),]
setwd("~/fourth semester/Social and Cultural Dynamics in Cognition/exam code/pilot data")
perf <- read_csv("confidence_audio (accessed 2017-04-03).csv")



perf <- perf %>%
    filter(session.code %in% sess,
           complete.cases(player.correct_individual)) %>%
    mutate(time = ymd_hms(participant.time_started)) %>%
    select(participant.code, player.answer, group.consensus, group.correct_consensus, group.vowel, subsession.round_number,
           player.correct_individual, group.stimulus, group.intensity)


perf %>%
    filter(group.vowel != "a") %>%
    group_by(group.intensity) %>%
    mutate(Individual = sum(player.correct_individual) / n()) %>%
    filter(complete.cases(group.consensus)) %>%
    # dplyr::mutate(n.consensus = sum(complete.cases(group.consensus))) %>%
    summarise(Individual = Individual[[1]],
              Individual_grp_trials = sum(player.correct_individual) / n(),
              Group = sum(group.correct_consensus) / n()) %>%
    gather("who", "hitrate", Individual:Group) %>%

    # change this to show the others
    filter(who == "Individual") %>%


    ggplot(aes(group.intensity, hitrate)) +
    geom_line(stat="summary", fun.y=mean) +
    geom_hline(aes(yintercept = .2), linetype="dashed") +
    scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1)) +
    labs(x = "Formant intensity (dB)", y = "Average hit-rate") +
    guides(colour = guide_legend(title="")) +
    theme_classic()


ggplot(perf, aes(group.intensity, player.correct_individual, colour=participant.code)) +
    geom_point(stat="summary", fun.y=mean) +
    # geom_pointrange(stat="summary", fun.data=mean_cl_boot) +
    scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1)) +
    labs(x = "Formant intensity (dB)",
         y = "Average hit-rate (.95 ci)",
         title = "Psychometric function for 'which vowel did you perceive'",
         subtitle = "n = 4, ntrials ~ 34") +
    guides(colour = guide_legend(title="")) +
    geom_hline(aes(yintercept=.2), linetype="dashed") +
    theme_classic()





perf.summary <- perf %>%
    filter(complete.cases(group.intensity)) %>%
    group_by(group.intensity, participant.code) %>%
    summarise(k = sum(player.correct_individual, na.rm=T),
              n = n()) %>%
    rename(x = group.intensity,
           id = participant.code) %>%
    ungroup() %>%
    mutate(x = scale(x)[,1]) %>%
    as.data.frame()


f1 <- alist(
    k ~ dbinom(n, theta),
    logit(theta) <- a + b * x,
    a[id] ~ dnorm(10,5),
    b[id] ~ dnorm(10,5)
)


m1 <- map2stan(f1, data=perf.summary, chains = 4, cores = 4, iter = 10000, warmup=2000, WAIC=F)
s <- extract.samples(m1)# %>%
    # as_data_frame() %>%
    # mutate(pse = -a / b)


dens(-s$a[,1] / s$b[,1], xlim=c(-2e2, 2e2))
dens(-s$a[,2] / s$b[,2], xlim=c(-2e2, 2e2))
dens(-s$a[,3] / s$b[,3], xlim=c(-2e2, 2e2))
dens(-s$a[,4] / s$b[,4], xlim=c(-2e2, 2e2))



pdata <- data_frame(id = 1:4) %>%
    mutate(
    a = purrr::map(id, ~s$a[,.x]),
    b = purrr::map(id, ~s$b[,.x])) %>%
    unnest() %>%
    filter(id == 2)
    mutate(pse = -a / b) %>%
    group_by(id) %>%
    summarise(d = list(density(pse)),
              x = purrr::map(d, ~.$x),
              y = purrr::map(d, ~.$y)) %>%
    unnest(x,y) %>%
    filter(id == 3)


ggplot(pdata, aes(x,y,  colour=factor(id), group=factor(id))) +
    geom_line() +
    # facet_wrap(~id, scales="free") +
    coord_cartesian(xlim = c(-1e2,1e2))
# ??????????????


