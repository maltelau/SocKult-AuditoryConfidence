## Run all models


iter = 2000
chains = 4
cores = 4


best_individual <- read.csv("data/best_individual.csv")
group <- read.csv("data/group.csv") %>% mutate(prop = k / n)
text <- read.csv("data/text_data.csv")


f <- function(x){ array(as.integer(x), dim = length(x))}
full.data <- list(
    N = nrow(group),
    N_group = length(unique(group$group)),
    N_vowel = 4,
    k_gro = f(group$k),
    n_gro = f(group$n),
    k_ind = f(best_individual$k),
    n_ind = f(best_individual$n),
    group = group$group_id,
    intensity = group$intensity,
    vowel = as.numeric(factor(group$vowel))) %>%
    # add all the predictors, and rescale them
    c(text %>% select(-group) %>% mutate_all(function(x){scale(x)[,1]}))

save("full.data", file = "data/full.data.Rdata")

# intercept <- stan(
#     file = "models/intercept.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("intercept", file="models/intercept.model")


# conf_align_local <- stan(
#     file = "models/conf_align_local.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("conf_align_local", file="models/conf_align_local.model")

# indiscriminate_align_local <- stan(
#     file = "models/indiscriminate_align_local.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("indiscriminate_align_local", file="models/indiscriminate_align_local.model")


# conf_align_global <- stan(
#     file = "models/conf_align_global.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("conf_align_global", file="models/conf_align_global.model")


# 
# cosine <- stan(
#     file = "models/cosine.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("cosine", file="models/cosine.model")

# self_l <- stan(
#     file = "models/self_l.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("self_l", file="models/self_l.model")
# 
# align_l <- stan(
#     file = "models/align_l.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("align_l", file="models/align_l.model")
# 
# synergy_l <- stan(
#     file = "models/synergy_l.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("synergy_l", file="models/synergy_l.model")


# self_entr <- stan(
#     file = "models/self_entr.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("self_entr", file="models/self_entr.model")
# 
# align_entr <- stan(
#     file = "models/align_entr.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("align_entr", file="models/align_entr.model")
# 
# synergy_entr <- stan(
#     file = "models/synergy_entr.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("synergy_entr", file="models/synergy_entr.model")


# self_l_and_local_confidence <- stan(
#     file = "models/self_l_and_local_confidence.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("self_l_and_local_confidence", file="models/self_l_and_local_confidence.model")
# 
# self_l_and_cosine <- stan(
#     file = "models/self_l_and_cosine.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("self_l_and_cosine", file="models/self_l_and_cosine.model")

# self_l_cosine_local_confidence <- stan(
#     file = "models/self_l_cosine_local_confidence.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("self_l_cosine_local_confidence", file="models/self_l_cosine_local_confidence.model")

# cosine_local_confidence <- stan(
#     file = "models/cosine_local_confidence.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("cosine_local_confidence", file="models/cosine_local_confidence.model")

# self_l_x_local_confidence <- stan(
#     file = "models/self_l_x_local_confidence.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("self_l_x_local_confidence", file="models/self_l_x_local_confidence.model")



# self_l_x_cosine <- stan(
#     file = "models/self_l_x_cosine.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("self_l_x_cosine", file="models/self_l_x_cosine.model")



# cosine_x_local_confidence <- stan(
#     file = "models/cosine_x_local_confidence.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("cosine_x_local_confidence", file="models/cosine_x_local_confidence.model")



# self_l_x_local_confidence2 <- stan(
#     file = "models/self_l_x_local_confidence2.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("self_l_x_local_confidence2", file="models/self_l_x_local_confidence2.model")


# self_l_x_cosine2 <- stan(
#     file = "models/self_l_x_cosine2.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("self_l_x_cosine2", file="models/self_l_x_cosine2.model")
# 
# 
# cosine_x_local_confidence_2 <- stan(
#     file = "models/cosine_x_local_confidence_2.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("cosine_x_local_confidence_2", file="models/cosine_x_local_confidence_2.model")
# 
# 
# self_l_x_cosine_conf_local <- stan(
#     file = "models/self_l_x_cosine_conf_local.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("self_l_x_cosine_conf_local", file="models/self_l_x_cosine_conf_local.model")
# 
# 
# 
# self_l_x_cosine_conf_local_full <- stan(
#     file = "models/self_l_x_cosine_conf_local_full.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("self_l_x_cosine_conf_local_full", file="models/self_l_x_cosine_conf_local_full.model")

# self_l_x_local_confidence_only <- stan(
#     file = "models/self_l_x_local_confidence_only.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("self_l_x_local_confidence_only", file="models/self_l_x_local_confidence_only.model")

# type_diff <- stan(
#     file = "models/type_diff.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("type_diff", file="models/type_diff.model")


# param_type_diff <- stan(
#     file = "models/param_type_diff.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("param_type_diff", file="models/param_type_diff.model")


# param_synergy_l <- stan(
#     file = "models/param_synergy_l.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("param_synergy_l", file="models/param_synergy_l.model")
# 
# param_local_indiscriminate <- stan(
#     file = "models/param_local_indiscriminate.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("param_local_indiscriminate", file="models/param_local_indiscriminate.model")
# 
# param_global_conf <- stan(
#     file = "models/param_global_conf.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("param_global_conf", file="models/param_global_conf.model")
# 
# 
# param_align_entr <- stan(
#     file = "models/param_align_entr.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("param_align_entr", file="models/param_align_entr.model")
# 
# 
# param_align_l <- stan(
#     file = "models/param_align_l.stan",
#     data = full.data,
#     chains = chains, cores = cores, iter = iter,
#     init = 0)
# 
# save("param_align_l", file="models/param_align_l.model")
