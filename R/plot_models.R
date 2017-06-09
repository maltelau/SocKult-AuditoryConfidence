

library(stringr)
library(tidyverse)
library(rstan)
library(rethinking)
library(scales)

# 
# model_list = data.frame(filename = list.files(path = "models/", pattern = "*.model")) %>%
#     filter(filename != "conf_align_local.model")
#     #[1,] %>% data.frame(filename = .)
# 
# model_names <- list(
#     "align_entr" = "RQA: Alignment ENTR",
#     "align_l"    = "RQA: Alignment L",
#     "conf_align_global" = "Global confidence convergence",
#     "cosine"     = "Cosine between word sets",
#     "indiscriminate_align_local" = "Local indiscriminate alignment",
#     "intercept"  = "Intercept only (Null Model)",
#     "self_entr"  = "RQA: Self-consistency ENTR",
#     "self_l"     = "RQA: Self-consistency L",
#     "synergy_entr" = "RQA: Synergy L",
#     "synergy_l"  = "RQA: Syngery ENTR",
#     "type_diff"  = "Difference in vocabulary size"
#     
# )




plot_predictive <- function(model, title) {
    post <- extract.samples(model)
    p <- expand.grid(par = seq(-3,3,3),
                intensity = seq(5,23,3),
                group = 1:9,
                vowel = 1:4) %>%
        mutate(collective_benefit = purrr::map(par, function(par) {
            mu = post$col_a + post$col_b * par
            rnorm(length(mu), mu, post$collective_sigma)
        })) %>%
        mutate(theta_ind = purrr::pmap(list(group, intensity, vowel), function(gro,int,vow, coll) {
            
            iA = post$ia + post$ia_vowel[,vow] + post$ia_group[,gro]
            iB = post$ib + post$ib_vowel[,vow] + post$ib_group[,gro]
            return(inv_logit(iA + iB * int))
        }), 
        theta_gro = purrr::pmap(list(group, intensity, vowel, collective_benefit), function(gro,int,vow, coll) {
            gA = post$ga# + post$ga_vowel[,vow] + post$ga_group[,gro]
            
            gb_group_theta = inv_logit(post$ib + post$ib_vowel[,vow] + post$ib_group[,gro]) * coll
            gb_group = logit(gb_group_theta) - (post$gb + post$gb_vowel[,vow])
            
            gB = post$gb + gb_group + post$gb_vowel[,vow]
            return(inv_logit(gA + gB * int))
        })) %>%
        gather("who", "pred", theta_ind, theta_gro) %>%
        mutate(who = ifelse(who == "theta_ind", "Individual", "Group")) %>%
        group_by(intensity, who, par) %>%
        summarise(pred = list(unlist(pred))) %>%
        unnest(pred) %>%
        ungroup() %>%
        filter(complete.cases(pred)) %>%
        mutate(par = factor(par, labels = c("Low (-3 SD)", "Average", "High (+3 SD)"))) %>%
        ggplot(aes(intensity, colour = who, fill = who, group = stringr::str_c(who, intensity))) +
        geom_violin(aes(y=pred), alpha=.3, position = position_identity(), scale = "width") +
        facet_grid(par ~ .) +
        labs(x = "Intensity (dB)", y = "Proportion correct answers",
             title = title) +
        guides(fill = guide_legend(title = element_blank()),
               colour = guide_legend(title = element_blank())) +
        scale_y_continuous(labels = percent)
    p
    ggsave(str_c("plots/pred_", title, ".png"))
    rm(p)
}


model_list %>%
    plyr::a_ply(1, function(ro) {
        model = stringr::str_sub(ro$filename, 1, -7)
        # print(str_c("plots/pred_", model_names[model], ".png"))
        if (file.exists(str_c("plots/pred_", model_names[model], ".png"))) {
            print(paste0("Plot already exists: ", model))
            return(NULL)
        }
        print(paste0("Plotting model: ", model))
        load(str_c("models/", ro$filename))

        try(plot_predictive(get(model), model_names[model]))
        rm(list = model)
    })



# load("models/conf_align_local.model")


# plot_predictive(conf_align_local, "Local confidence alignment")
