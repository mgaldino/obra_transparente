## Bayesian propensity score

library("rstan")
library("arm")
library("rstanarm")
library("survey")
library(broom)
library(broom.mixed)
# library(ggdag)
library(MetBrewer)

df_balancing_aux %>%
  group_by(indicator_muni_ot,tipo_do_projeto ) %>%
  summarise(n()) %>% View()

df_balancing_aux1 <- df_balancing_aux %>%
  mutate(tipo_do_projeto = ifelse(grepl("Espaço Educativo", tipo_do_projeto), 
                                  "Espaço Educativo - x salas", tipo_do_projeto),
         concluida_num = as.numeric(concluida))

df_balancing_aux_se <- df_balancing_aux1 %>%
  filter(uf.x %in% c("SP", "PR", "SP", "SC", "RS", "MG"))

pre_dados <- df_balancing_aux %>%
  dplyr::select(id, concluida) %>%
  rename(concluida_pre = concluida) %>%
  mutate(id = as.numeric(id))


data_pos <- obras_fim_seg_fase %>%
  mutate(municipio1 = tolower(iconv(municipio, "UTF-8",  "ASCII//TRANSLIT"))) %>%
  inner_join(aux_muni1, by = join_by(municipio1  == name_muni1, uf == abbrev_state)) %>%
  inner_join(ipea_final, by = join_by(code_muni == tcode)) %>%
  mutate(perc_pea18 =pea18/populacao,
         log_pop = log(populacao),
         log_renda = log(rpc*populacao),
         log_rpc = log(rpc)) %>%
  inner_join(pre_dados, by = "id") %>%
  dplyr::filter(concluida_pre == FALSE) %>%
  mutate(concluida_num = as.numeric(concluida))


df_balancing_aux_post_se <- data_pos %>%
  filter(uf.x %in% c("SP", "PR", "SP", "SC", "RS", "MG"))
  
# these are the no redundancy covariates with and without state covariate
covs.nr <- c('perc_pea18', 'ext_pobres', 'log_rpc', 'p10_ricos_desigualdade', 'freq_escola_15_1', 'analfabetismo',
             'log_renda', 'log_pop', 'percentual_de_execucao', 'tipo_do_projeto')

covs.nr.st <- c(covs.nr, "uf.x")

# pscore estimation formula with original covariates

ps_spec <- formula(indicator_muni_ot ~ log_rpc  + ext_pobres + freq_escola_15_1 + analfabetismo +
                     log_renda   +  p10_ricos_desigualdade +
                       + percentual_de_execucao)

#pscore estimation formula with state added

ps_spec.st <- update(ps_spec, . ~ uf.x)

# Estimation of pscores using stan_glm
set.seed(1234)
ps_fit_1 <- stan_glm(ps_spec, family=binomial(link='logit'), 
                     data=df_balancing_aux_se, algorithm='optimizing', refresh=0, iter = 2000)


set.seed(1234)
ps_fit_2 <- stan_glm(ps_spec, family=binomial(link='logit'), 
                     data=df_balancing_aux_post_se, algorithm='optimizing', refresh=0, iter = 2000)
# extrqacting logit pscores
pscores <- apply(posterior_linpred(ps_fit_1), 2, mean)

pscores2 <- apply(posterior_linpred(ps_fit_2), 2, mean)

# matching
#matching without replacement, original formula

matches <- matching(z=df_balancing_aux_se$indicator_muni_ot, score=pscores, replace=FALSE)
matched <- df_balancing_aux_se[matches$match.ind,]

matches2 <- matching(z=df_balancing_aux_post_se$indicator_muni_ot, score=pscores2, replace=FALSE)
matched2 <- df_balancing_aux_post_se[matches$match.ind,]

#matching with replacement, original formula

matches.wr <- matching(z=df_balancing_aux_se$indicator_muni_ot, score=pscores, replace=TRUE)
wts.wr <- matches.wr$cnts

# estimating with pscore

te_spec_nr <- update(ps_spec, concluida_num ~ indicator_muni_ot + (1 | municipio) + .)

#treatment effect without replacement

set.seed(20)
reg_ps <- stan_lmer(te_spec_nr, data=df_balancing_aux_se[matches$match.ind,])

## After
set.seed(20)
reg_ps2 <- stan_lmer(te_spec_nr, data=df_balancing_aux_post_se[matches2$match.ind,])

# summary pre
summary(reg_ps)['indicator_muni_ot', 1:5]
pre_sims <- as.matrix(reg_ps)

b1_pre_sims <- as.matrix(reg_ps, 
                     pars = "indicator_muni_ot")




## pos
summary(reg_ps.wr)['indicator_muni_ot', 1:5]
summary(reg_ps2)['indicator_muni_ot', 1:5]


# posterior draws

##
# Before
##

# Posterior mean and SD of treatment
b1_mean_pre <- mean(b1_pre_sims) # posterior mean
b1_sd_pre <- sd(b1_pre_sims)       # posterior SD


# Posterior median and 95% credible interval
b1_quant_pre <- quantile(b1_pre_sims,   probs = c(0.025, 0.50, 0.975))

b1_quant_pre <- data.frame(t(b1_quant_pre))
names(b1_quant_pre) <- c("Q2.5", "Q50", "Q97.5")

# Combine summary statistics of posterior simulation draws
b1_df_pre_cred <- data.frame(variables = "ATT before project",
                             post_mean = b1_mean_pre,
                             post_sd = b1_sd_pre,
                             b1_quant_pre[2], 
                             b1_quant_pre[1],
                             b1_quant_pre[3])



##
# After
##

b1_sims_after <- as.matrix(reg_ps2, 
                     pars = "indicator_muni_ot")
  
# Compute mean, SD, median, and 95% credible interval of treatment

# Posterior mean and SD of treatment
b1_mean_after <- mean(b1_sims_after) # posterior mean
b1_sd_after <- sd(b1_sims_after)       # posterior SD

# Posterior median and 95% credible interval
b1_quant_after <- quantile(b1_sims_after,   probs = c(0.025, 0.50, 0.975))

b1_quant_after <- data.frame(t(b1_quant_after))
names(b1_quant_after) <- c("Q2.5", "Q50", "Q97.5")

# Combine summary statistics of posterior simulation draws
b1_df_after_cred <- data.frame(variables = "ATT After project",
                             post_mean = b1_mean_after,
                             post_sd = b1_sd_after,
                             b1_quant_after[2], 
                             b1_quant_after[1],
                             b1_quant_after[3])
b1_df <- bind_rows(b1_df_pre_cred, b1_df_after_cred)

b1_df %>% gt() %>%
  tab_header(
    title = "Effect of local OSB social monitoring on construction delivery rates") %>%
  cols_label(
    variables = "Variables",
    post_mean = "Posterior mean",
    post_sd = "Posterior sd",
    Q50 = "Posterior median",
    Q2.5 = "Posterior 2.5% CI",
    Q97.5 = "Posterior 97.5% CI"
  ) %>%
  fmt_number(
    columns = c(post_mean, post_sd, Q50, Q2.5, Q97.5 ),
    decimals = 3,
    use_seps = FALSE
  ) %>%
  tab_source_note(
    source_note = "452 observations after matching, with municipalities of same states as participants of Obra Transparente."
  ) 
# # visualization
# isfahan <- MetBrewer::met.brewer("Isfahan1")
# 
# 
# 
# ggplot() + 
#   geom_histogram(data = filter(ot_with_weights, indicator_muni_ot == 1), 
#                  bins = 50, aes(x = propensity), 
#                  fill = isfahan[2]) + 
#   geom_histogram(data = filter(ot_with_weights, indicator_muni_ot == 0), 
#                  bins = 50, aes(x = propensity, y = -after_stat(count)),
#                  fill = isfahan[6]) +
#   geom_hline(yintercept = 0) +
#   annotate(geom = "label", x = 0.1, y = 20, label = "Treated", 
#            fill = isfahan[2], color = "white", hjust = 0) +
#   annotate(geom = "label", x = 0.1, y = -20, label = "Untreated", 
#            fill = isfahan[6], color = "white", hjust = 0) +
#  # scale_y_continuous(label = abs) +
#   coord_cartesian(xlim = c(0.1, 0.8), ylim = c(-80, 100)) +
#   labs(x = "Propensity", y = "Relative Freq.")
# # Many more municipalities are in the control group (as we already know). We can also see that 
# # extracting (logit) pscores from the fit
# 
# ## Using IPWT
# 
# ggplot() + 
#   geom_histogram(data = filter(ot_with_weights, indicator_muni_ot == 1), 
#                  bins = 50, aes(x = propensity, weight = iptw), 
#                  fill = colorspace::lighten(isfahan[2], 0.35)) + 
#   geom_histogram(data = filter(ot_with_weights, indicator_muni_ot == 0), 
#                  bins = 50, aes(x = propensity, weight = iptw, y = -after_stat(count)),
#                  fill = colorspace::lighten(isfahan[6], 0.35)) +
#   geom_histogram(data = filter(ot_with_weights, indicator_muni_ot == 1), 
#                  bins = 50, aes(x = propensity), 
#                  fill = isfahan[2]) + 
#   geom_histogram(data = filter(ot_with_weights, indicator_muni_ot == 0), 
#                  bins = 50, aes(x = propensity, y = -after_stat(count)),
#                  fill = isfahan[6]) +
#   annotate(geom = "label", x = 0.8, y = 70, label = "Treated (actual)", 
#            fill = isfahan[2], color = "white", hjust = 1) +
#   annotate(geom = "label", x = 0.8, y = 90, label = "Treated (IPTW pseudo-population)", 
#            fill = colorspace::lighten(isfahan[2], 0.35), color = "white", hjust = 1) +
#   annotate(geom = "label", x = 0.8, y = -60, label = "Untreated (actual)", 
#            fill = isfahan[6], color = "white", hjust = 1) +
#   annotate(geom = "label", x = 0.8, y = -80, label = "Untreated (IPTW pseudo-population)", 
#            fill = colorspace::lighten(isfahan[6], 0.35), color = "white", hjust = 1) +
#   geom_hline(yintercept = 0, color = "white", linewidth = 0.25) +
#   scale_y_continuous(label = abs) +
#   coord_cartesian(xlim = c(0.1, 0.8), ylim = c(-80, 100)) +
#   labs(x = "Propensity", y = "Count")
# 
# 
# 
# pscores <- apply(posterior_linpred(ps_fit_1), 2, mean)
# pscores.st <- apply(posterior_linpred(ps_fit_1.st), 2, mean)