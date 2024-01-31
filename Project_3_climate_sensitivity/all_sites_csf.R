## master sheet of all sites
source("Project_3_climate_sensitivity/corre_spei_file_cleaning.R")


### CDR - biocon ####
cdr_biocon <- filter(n_sites, site_code == "CDR" & project_name == "BioCON")
# just n versus control

m.null <- lme(anpp ~ year*n, data = cdr_biocon, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = cdr_biocon,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = cdr_biocon,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = cdr_biocon, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = cdr_biocon,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = cdr_biocon,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = cdr_biocon, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.Ci
##make an lm for plotting
lm.null <- lm(anpp ~ year*n, data = cdr_biocon)

visreg(lm.null, xvar = "year", type = "conditional", by = "n", data = cdr_biocon, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = year, y = anpp), alpha = 0.2, data = cdr_biocon) +
  # facet_grid(p~n) +
  theme_bw() +
  labs(x="YEAR",
       y="ANPP")
## CDR - NutNet ####
cdr_nutnet <- filter(n_sites, site_code == "CDR" & project_name == "NutNet")

m.null_new <- lme(anpp ~ year*n*p, data = cdr_nutnet, random = ~1|uniqueID, method="ML")

m.La_int <- lme(anpp ~ spei + n*p, data = cdr_nutnet,random = ~1|uniqueID, method="ML")
m.La_n <- lme(anpp ~ spei + n, data = cdr_nutnet,random = ~1|uniqueID, method="ML")
m.La_p <- lme(anpp ~ spei + p, data = cdr_nutnet,random = ~1|uniqueID, method="ML")

m.Li_int <- lme(anpp ~ spei*n*p, data = cdr_nutnet,random = ~1|uniqueID, method="ML")
m.Li_n <- lme(anpp ~ spei*n, data = cdr_nutnet,random = ~1|uniqueID, method="ML")
m.Li_p <- lme(anpp ~ spei*p, data = cdr_nutnet,random = ~1|uniqueID, method="ML")

m.Qa_int <- lme(anpp ~ spei+n*p + I(spei^2), data = cdr_nutnet, random = ~1|uniqueID, method="ML")
m.Qa_n <- lme(anpp ~ spei+n + I(spei^2), data = cdr_nutnet, random = ~1|uniqueID, method="ML")
m.Qa_p <- lme(anpp ~ spei+p + I(spei^2), data = cdr_nutnet, random = ~1|uniqueID, method="ML")

m.Qi_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p, data = cdr_nutnet,random=~1|uniqueID,method="ML")
m.Qi_n <- lme(anpp ~ spei*n*p + I(spei^2)*n, data = cdr_nutnet,random=~1|uniqueID,method="ML")
m.Qi_p <- lme(anpp ~ spei*n*p + I(spei^2)*p, data = cdr_nutnet,random=~1|uniqueID,method="ML")

m.Ca_int <- lme(anpp ~ spei+n*p + I(spei^2) + I(spei^3),data = cdr_nutnet,random=~1|uniqueID, method="ML")
m.Ca_n <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = cdr_nutnet,random=~1|uniqueID, method="ML")
m.Ca_p <- lme(anpp ~ spei+p + I(spei^2) + I(spei^3),data = cdr_nutnet,random=~1|uniqueID, method="ML")

m.Ci_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n*p, data = cdr_nutnet, random = ~1|uniqueID, method="ML")
m.Ci_n <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n, data = cdr_nutnet, random = ~1|uniqueID, method="ML")
m.Ci_p <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*p, data = cdr_nutnet, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
min(AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
    [,2])
#Best model is m.Ca_int

fits <- data.frame("uniqueID" = names(fitted(object = m.Ca_int)),
                   "anpp_model_fits" = fitted(object = m.Ca_int))

# Drop rownames (purely for aesthetic reasons)
rownames(fits) <- NULL

# Bind onto "real" data
cdr_nutnet_fits <- dplyr::left_join(x = cdr_nutnet, y = fits, by = "uniqueID")

# Make desired plot
ggplot(data = cdr_nutnet_fits) +
  geom_point(aes(x = spei, y = anpp, color = trt_type), alpha = 0.2) +
  geom_smooth(aes(x = spei, y = anpp_model_fits, color = trt_type, fill = trt_type), method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = T) +
  #facet_wrap( ~ n) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP") +
  scale_y_continuous(limits = c(0,1000))
r.squaredGLMM(m.Ca_int)

#### cbgb - NutNet ####
cbgb <- filter(n_sites, site_code == "cbgb.us" & project_name == "NutNet")

m.null_new <- lme(anpp ~ year*n*p, data = cbgb, random = ~1|uniqueID, method="ML")

m.La_int <- lme(anpp ~ spei + n*p, data = cbgb,random = ~1|uniqueID, method="ML")
m.La_n <- lme(anpp ~ spei + n, data = cbgb,random = ~1|uniqueID, method="ML")
m.La_p <- lme(anpp ~ spei + p, data = cbgb,random = ~1|uniqueID, method="ML")

m.Li_int <- lme(anpp ~ spei*n*p, data = cbgb,random = ~1|uniqueID, method="ML")
m.Li_n <- lme(anpp ~ spei*n, data = cbgb,random = ~1|uniqueID, method="ML")
m.Li_p <- lme(anpp ~ spei*p, data = cbgb,random = ~1|uniqueID, method="ML")

m.Qa_int <- lme(anpp ~ spei+n*p + I(spei^2), data = cbgb, random = ~1|uniqueID, method="ML")
m.Qa_n <- lme(anpp ~ spei+n + I(spei^2), data = cbgb, random = ~1|uniqueID, method="ML")
m.Qa_p <- lme(anpp ~ spei+p + I(spei^2), data = cbgb, random = ~1|uniqueID, method="ML")

m.Qi_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p, data = cbgb,random=~1|uniqueID,method="ML")
m.Qi_n <- lme(anpp ~ spei*n*p + I(spei^2)*n, data = cbgb,random=~1|uniqueID,method="ML")
m.Qi_p <- lme(anpp ~ spei*n*p + I(spei^2)*p, data = cbgb,random=~1|uniqueID,method="ML")

m.Ca_int <- lme(anpp ~ spei+n*p + I(spei^2) + I(spei^3),data = cbgb,random=~1|uniqueID, method="ML")
m.Ca_n <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = cbgb,random=~1|uniqueID, method="ML")
m.Ca_p <- lme(anpp ~ spei+p + I(spei^2) + I(spei^3),data = cbgb,random=~1|uniqueID, method="ML")

m.Ci_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n*p, data = cbgb, random = ~1|uniqueID, method="ML")
m.Ci_n <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n, data = cbgb, random = ~1|uniqueID, method="ML")
m.Ci_p <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*p, data = cbgb, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
min(AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
    [,2])
#Best model is m.null

### KBS T7 ####
kellogg <- filter(n_sites, site_code == "KBS")
# just n versus control

m.null <- lme(anpp ~ year*n, data = kellogg, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = kellogg,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = kellogg,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = kellogg, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = kellogg,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = kellogg,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = kellogg, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.Ci
##make an lm for plotting
lm.null <- lm(anpp ~ year*n, data = kellogg)

visreg(lm.null, xvar = "year", type = "conditional", by = "n", data = kellogg, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = year, y = anpp), alpha = 0.2, data = kellogg) +
  # facet_grid(p~n) +
  theme_bw() +
  labs(x="YEAR",
       y="ANPP")
#### KNZ - BGP ####
knz_bgp <- filter(n_sites, site_code == "KNZ" & project_name == "BGP")

m.null_new <- lme(anpp ~ year*n*p, data = knz_bgp, random = ~1|uniqueID, method="ML")

m.La_int <- lme(anpp ~ spei + n*p, data = knz_bgp,random = ~1|uniqueID, method="ML")
m.La_n <- lme(anpp ~ spei + n, data = knz_bgp,random = ~1|uniqueID, method="ML")
m.La_p <- lme(anpp ~ spei + p, data = knz_bgp,random = ~1|uniqueID, method="ML")

m.Li_int <- lme(anpp ~ spei*n*p, data = knz_bgp,random = ~1|uniqueID, method="ML")
m.Li_n <- lme(anpp ~ spei*n, data = knz_bgp,random = ~1|uniqueID, method="ML")
m.Li_p <- lme(anpp ~ spei*p, data = knz_bgp,random = ~1|uniqueID, method="ML")

m.Qa_int <- lme(anpp ~ spei+n*p + I(spei^2), data = knz_bgp, random = ~1|uniqueID, method="ML")
m.Qa_n <- lme(anpp ~ spei+n + I(spei^2), data = knz_bgp, random = ~1|uniqueID, method="ML")
m.Qa_p <- lme(anpp ~ spei+p + I(spei^2), data = knz_bgp, random = ~1|uniqueID, method="ML")

m.Qi_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p, data = knz_bgp,random=~1|uniqueID,method="ML")
m.Qi_n <- lme(anpp ~ spei*n*p + I(spei^2)*n, data = knz_bgp,random=~1|uniqueID,method="ML")
m.Qi_p <- lme(anpp ~ spei*n*p + I(spei^2)*p, data = knz_bgp,random=~1|uniqueID,method="ML")

m.Ca_int <- lme(anpp ~ spei+n*p + I(spei^2) + I(spei^3),data = knz_bgp,random=~1|uniqueID, method="ML")
m.Ca_n <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = knz_bgp,random=~1|uniqueID, method="ML")
m.Ca_p <- lme(anpp ~ spei+p + I(spei^2) + I(spei^3),data = knz_bgp,random=~1|uniqueID, method="ML")

m.Ci_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n*p, data = knz_bgp, random = ~1|uniqueID, method="ML")
m.Ci_n <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n, data = knz_bgp, random = ~1|uniqueID, method="ML")
m.Ci_p <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*p, data = knz_bgp, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
min(AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
    [,2])


### KNZ - pplots ####
knz_pplots <- filter(n_sites, site_code == "KNZ" & project_name == "pplots")

m.null_new <- lme(anpp ~ year*n*p, data = knz_pplots, random = ~1|uniqueID, method="ML")

m.La_int <- lme(anpp ~ spei + n*p, data = knz_pplots,random = ~1|uniqueID, method="ML")
m.La_n <- lme(anpp ~ spei + n, data = knz_pplots,random = ~1|uniqueID, method="ML")
m.La_p <- lme(anpp ~ spei + p, data = knz_pplots,random = ~1|uniqueID, method="ML")

m.Li_int <- lme(anpp ~ spei*n*p, data = knz_pplots,random = ~1|uniqueID, method="ML")
m.Li_n <- lme(anpp ~ spei*n, data = knz_pplots,random = ~1|uniqueID, method="ML")
m.Li_p <- lme(anpp ~ spei*p, data = knz_pplots,random = ~1|uniqueID, method="ML")

m.Qa_int <- lme(anpp ~ spei+n*p + I(spei^2), data = knz_pplots, random = ~1|uniqueID, method="ML")
m.Qa_n <- lme(anpp ~ spei+n + I(spei^2), data = knz_pplots, random = ~1|uniqueID, method="ML")
m.Qa_p <- lme(anpp ~ spei+p + I(spei^2), data = knz_pplots, random = ~1|uniqueID, method="ML")

m.Qi_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p, data = knz_pplots,random=~1|uniqueID,method="ML")
m.Qi_n <- lme(anpp ~ spei*n*p + I(spei^2)*n, data = knz_pplots,random=~1|uniqueID,method="ML")
m.Qi_p <- lme(anpp ~ spei*n*p + I(spei^2)*p, data = knz_pplots,random=~1|uniqueID,method="ML")

m.Ca_int <- lme(anpp ~ spei+n*p + I(spei^2) + I(spei^3),data = knz_pplots,random=~1|uniqueID, method="ML")
m.Ca_n <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = knz_pplots,random=~1|uniqueID, method="ML")
m.Ca_p <- lme(anpp ~ spei+p + I(spei^2) + I(spei^3),data = knz_pplots,random=~1|uniqueID, method="ML")

m.Ci_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n*p, data = knz_pplots, random = ~1|uniqueID, method="ML")
m.Ci_n <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n, data = knz_pplots, random = ~1|uniqueID, method="ML")
m.Ci_p <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*p, data = knz_pplots, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
min(AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
    [,2])
## average the two top models (within 2 AICc values)
mod_list <- list(m.Ca_int, m.Ca_n) ## make a model list
m.final <- get.models(model.sel(mod_list), subset = 1)[[1]] ## take the full averaged model

fits <- data.frame("uniqueID" = names(fitted(object = m.final)),
                   "anpp_model_fits" = fitted(object = m.final))

# Drop rownames (purely for aesthetic reasons)
rownames(fits) <- NULL

# Bind onto "real" data
knz_pplots_fits <- dplyr::left_join(x = knz_pplots, y = fits, by = "uniqueID")
## make column for treatments for plotting
knz_pplots_fits <- knz_pplots_fits %>%
  mutate(N = rep("N"),
         P = rep("P")) %>%
  unite(trt, c("N", "n", "P", "p"), sep = "", remove = FALSE)
unique(knz_pplots_fits$trt)
sort(unique(knz_pplots_fits$trt))
knz_pplots_fits$trt <- factor(knz_pplots_fits$trt, levels = c("N0P0", "N0P2.5", "N0P5", "N0P10", "N10P0", "N10P2.5", "N10P5", "N10P10"))
knz_color_scale <- c("black", "#cbc9e2","#9e9ac8","#6a51a3", "#bae4b3","#74c476","#238b45", "#2171b5")
# Make desired plot
ggplot(data = knz_pplots_fits) +
  geom_point(aes(x = spei, y = anpp, color = trt), alpha = 0.4, size = 2) +
  geom_smooth(aes(x = spei, y = anpp_model_fits, color = trt, fill = trt), method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = F, linewidth = 2) +
  scale_color_manual(values = knz_color_scale) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP") 
r.squaredGLMM(m.final)

### KUFS - E2 ####
kufs_e2 <- filter(n_sites, site_code == "KUFS" & project_name == "E2")

m.null <- lme(anpp ~ year*trt_type, data = kufs_e2, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + trt_type, data = kufs_e2,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*trt_type, data = kufs_e2,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+trt_type + I(spei^2), data = kufs_e2, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type, data = kufs_e2,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+trt_type + I(spei^2) + I(spei^3),data = kufs_e2,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type + I(spei^3)*trt_type, data = kufs_e2, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.Ci,
## additive models are better fits than interactive models, generally

#Visualize CSF results---
# get a plot of estimated values from the model, by each depth
# visreg with ggplot graphics
# kufs$n_levels <- factor(kufs$n, levels = c("0", '4', "8", "15", "16"))
# kufs$p_levels <- factor(kufs$p, levels = c("0", '8'))
m.Ci_plot <- lm(anpp ~ spei*trt_type + I(spei^2)*trt_type + I(spei^3)*trt_type, data = kufs_e2)
lm.La <- lm(anpp ~ spei + trt_type, data = kufs_e2)

visreg(lm.La, xvar = "spei", type = "conditional", by = "trt_type", data = kufs_e2, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = spei, y = anpp, color = trt_type), alpha = 0.2, data = kufs_e2) +
  # facet_wrap(~p_levels) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP")+
  scale_y_continuous(limits = c(0,1500))

### KUFS -- E6 ####
kufs_e6 <- filter(n_sites, site_code == "KUFS" & project_name == "E6")


m.null_new <- lme(anpp ~ year*n*p, data = kufs_e6, random = ~1|uniqueID, method="ML")

m.La_int <- lme(anpp ~ spei + n*p, data = kufs_e6,random = ~1|uniqueID, method="ML")
m.La_n <- lme(anpp ~ spei + n, data = kufs_e6,random = ~1|uniqueID, method="ML")
m.La_p <- lme(anpp ~ spei + p, data = kufs_e6,random = ~1|uniqueID, method="ML")

m.Li_int <- lme(anpp ~ spei*n*p, data = kufs_e6,random = ~1|uniqueID, method="ML")
m.Li_n <- lme(anpp ~ spei*n, data = kufs_e6,random = ~1|uniqueID, method="ML")
m.Li_p <- lme(anpp ~ spei*p, data = kufs_e6,random = ~1|uniqueID, method="ML")

m.Qa_int <- lme(anpp ~ spei+n*p + I(spei^2), data = kufs_e6, random = ~1|uniqueID, method="ML")
m.Qa_n <- lme(anpp ~ spei+n + I(spei^2), data = kufs_e6, random = ~1|uniqueID, method="ML")
m.Qa_p <- lme(anpp ~ spei+p + I(spei^2), data = kufs_e6, random = ~1|uniqueID, method="ML")

m.Qi_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p, data = kufs_e6,random=~1|uniqueID,method="ML")
m.Qi_n <- lme(anpp ~ spei*n*p + I(spei^2)*n, data = kufs_e6,random=~1|uniqueID,method="ML")
m.Qi_p <- lme(anpp ~ spei*n*p + I(spei^2)*p, data = kufs_e6,random=~1|uniqueID,method="ML")

m.Ca_int <- lme(anpp ~ spei+n*p + I(spei^2) + I(spei^3),data = kufs_e6,random=~1|uniqueID, method="ML")
m.Ca_n <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = kufs_e6,random=~1|uniqueID, method="ML")
m.Ca_p <- lme(anpp ~ spei+p + I(spei^2) + I(spei^3),data = kufs_e6,random=~1|uniqueID, method="ML")

m.Ci_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n*p, data = kufs_e6, random = ~1|uniqueID, method="ML")
m.Ci_n <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n, data = kufs_e6, random = ~1|uniqueID, method="ML")
m.Ci_p <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*p, data = kufs_e6, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
min(AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
    [,2])
## average the two top models (within 2 AICc values)
mod_list <- list(m.Qa_int, m.Ca_int) ## make a model list
m.final <- get.models(model.sel(mod_list), subset = 1)[[1]] ## take the full averaged model

fits <- data.frame("uniqueID" = names(fitted(object = m.final)),
                   "anpp_model_fits" = fitted(object = m.final))

# Drop rownames (purely for aesthetic reasons)
rownames(fits) <- NULL

# Bind onto "real" data
kufs_e6_fits <- dplyr::left_join(x = kufs_e6, y = fits, by = "uniqueID")
## make column for treatments for plotting
unique(kufs_e6_fits$p)
kufs_e6_fits <- kufs_e6_fits %>% ## make column for plotting
  mutate(N = rep("N"),
         P = rep("P")) %>%
  unite(trt, c("N", "n", "P", "p"), sep = "", remove = FALSE)
unique(kufs_e6_fits$trt)
sort(unique(kufs_e6_fits$trt))
kufs_e6_fits$trt <- factor(kufs_e6_fits$trt, levels = c("N0P0", "N4P0", "N8P0", "N16P0", "N0P8", "N4P8", "N8P8", "N16P8"))
## one control, 3 N alone, one P alone, 3 NP
kufs_color_scale <- c("black","#bae4b3","#74c476","#238b45","#6a51a3","#bdd7e7","#6baed6", "#2171b5")
# Make desired plot
ggplot(data = kufs_e6_fits) +
  geom_point(aes(x = spei, y = anpp, color = trt), alpha = 0.4, size = 2) +
  geom_smooth(aes(x = spei, y = anpp_model_fits, color = trt, fill = trt), method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = F, linewidth = 2) +
  scale_color_manual(values = kufs_color_scale) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP") 
r.squaredGLMM(m.final)

<<<<<<< HEAD
=======
ggplot(kufs_e6, aes(x = spei, y = anpp, color = as.factor(p))) +
  geom_point() +
  facet_wrap(~n) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), span = 1, se = F) +
  labs(y = "ANPP")
min(kufs_e6$spei)
max(kufs_e6$spei)
<<<<<<< HEAD
=======
>>>>>>> 90542ff1d1a05d53734ec13c88077450733a4394

### niwot -- snow ####
niwot <- filter(n_sites, site_code == "NWT")
# just n versus control


## use trt_type
m.null <- lme(anpp ~ year*n, data = niwot, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = niwot,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = niwot,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = niwot, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = niwot,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = niwot,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = niwot, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
mod_list <- list(m.La, m.Qa) ## make a model list
m.final <- get.models(model.sel(mod_list), subset = 1)[[1]] ## take the full averaged model

fits <- data.frame("uniqueID" = names(fitted(object = m.final)),
                   "anpp_model_fits" = fitted(object = m.final))

# Drop rownames (purely for aesthetic reasons)
rownames(fits) <- NULL

# Bind onto "real" data
niwot_fits <- dplyr::left_join(x = niwot, y = fits, by = "uniqueID")
## make column for treatments for plotting

niwot_colors <- c("black", "#74c476")

ggplot(data = niwot_fits) +
  geom_point(aes(x = spei, y = anpp, color = trt_type), alpha = 0.4, size = 2) +
  geom_smooth(aes(x = spei, y = anpp_model_fits, color = trt_type, fill = trt_type), method = "lm", formula = y ~ x + I(x^2), se = F, linewidth = 2) +
  theme_bw() +
  scale_color_manual(values = niwot_colors) +
  scale_fill_manual(values = niwot_colors) +
  labs(x="SPEI",
       y="ANPP") 
ggplot(data = niwot_fits) +
  geom_point(aes(x = spei, y = anpp, color = trt_type), alpha = 0.4, size = 2) +
  geom_smooth(aes(x = spei, y = anpp_model_fits, color = trt_type), method = "lm", formula = y ~ x + I(x^2), se = F, linewidth = 2) +
  theme_bw() +
  scale_color_manual(values = niwot_colors) +
  labs(x="SPEI",
       y="ANPP") 
r.squaredGLMM(m.final)

### SERC - CXN ####
serc <- filter(n_sites, site_code == "SERC")
# just n versus control

m.null <- lme(anpp ~ year*n, data = serc, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n, data = serc,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n, data = serc,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n + I(spei^2), data = serc, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n + I(spei^2)*n, data = serc,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = serc,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*n + I(spei^2)*n + I(spei^3)*n, data = serc, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.null


###sier.us - NutNet ####
sier <- filter(n_sites, site_code == "sier.us" & project_name == "NutNet")


m.null_new <- lme(anpp ~ year*n*p, data = sier, random = ~1|uniqueID, method="ML")

m.La_int <- lme(anpp ~ spei + n*p, data = sier,random = ~1|uniqueID, method="ML")
m.La_n <- lme(anpp ~ spei + n, data = sier,random = ~1|uniqueID, method="ML")
m.La_p <- lme(anpp ~ spei + p, data = sier,random = ~1|uniqueID, method="ML")

m.Li_int <- lme(anpp ~ spei*n*p, data = sier,random = ~1|uniqueID, method="ML")
m.Li_n <- lme(anpp ~ spei*n, data = sier,random = ~1|uniqueID, method="ML")
m.Li_p <- lme(anpp ~ spei*p, data = sier,random = ~1|uniqueID, method="ML")

m.Qa_int <- lme(anpp ~ spei+n*p + I(spei^2), data = sier, random = ~1|uniqueID, method="ML")
m.Qa_n <- lme(anpp ~ spei+n + I(spei^2), data = sier, random = ~1|uniqueID, method="ML")
m.Qa_p <- lme(anpp ~ spei+p + I(spei^2), data = sier, random = ~1|uniqueID, method="ML")

m.Qi_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p, data = sier,random=~1|uniqueID,method="ML")
m.Qi_n <- lme(anpp ~ spei*n*p + I(spei^2)*n, data = sier,random=~1|uniqueID,method="ML")
m.Qi_p <- lme(anpp ~ spei*n*p + I(spei^2)*p, data = sier,random=~1|uniqueID,method="ML")

m.Ca_int <- lme(anpp ~ spei+n*p + I(spei^2) + I(spei^3),data = sier,random=~1|uniqueID, method="ML")
m.Ca_n <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = sier,random=~1|uniqueID, method="ML")
m.Ca_p <- lme(anpp ~ spei+p + I(spei^2) + I(spei^3),data = sier,random=~1|uniqueID, method="ML")

m.Ci_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n*p, data = sier, random = ~1|uniqueID, method="ML")
m.Ci_n <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n, data = sier, random = ~1|uniqueID, method="ML")
m.Ci_p <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*p, data = sier, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
min(AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
    [,2])
#Best model is m.null

### yarra NutNet ####
yarra <- filter(n_sites, site_code == "yarra.au")

m.null_new <- lme(anpp ~ year*n*p, data = yarra, random = ~1|uniqueID, method="ML")

m.La_int <- lme(anpp ~ spei + n*p, data = yarra,random = ~1|uniqueID, method="ML")
m.La_n <- lme(anpp ~ spei + n, data = yarra,random = ~1|uniqueID, method="ML")
m.La_p <- lme(anpp ~ spei + p, data = yarra,random = ~1|uniqueID, method="ML")

m.Li_int <- lme(anpp ~ spei*n*p, data = yarra,random = ~1|uniqueID, method="ML")
m.Li_n <- lme(anpp ~ spei*n, data = yarra,random = ~1|uniqueID, method="ML")
m.Li_p <- lme(anpp ~ spei*p, data = yarra,random = ~1|uniqueID, method="ML")

m.Qa_int <- lme(anpp ~ spei+n*p + I(spei^2), data = yarra, random = ~1|uniqueID, method="ML")
m.Qa_n <- lme(anpp ~ spei+n + I(spei^2), data = yarra, random = ~1|uniqueID, method="ML")
m.Qa_p <- lme(anpp ~ spei+p + I(spei^2), data = yarra, random = ~1|uniqueID, method="ML")

m.Qi_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p, data = yarra,random=~1|uniqueID,method="ML")
m.Qi_n <- lme(anpp ~ spei*n*p + I(spei^2)*n, data = yarra,random=~1|uniqueID,method="ML")
m.Qi_p <- lme(anpp ~ spei*n*p + I(spei^2)*p, data = yarra,random=~1|uniqueID,method="ML")

m.Ca_int <- lme(anpp ~ spei+n*p + I(spei^2) + I(spei^3),data = yarra,random=~1|uniqueID, method="ML")
m.Ca_n <- lme(anpp ~ spei+n + I(spei^2) + I(spei^3),data = yarra,random=~1|uniqueID, method="ML")
m.Ca_p <- lme(anpp ~ spei+p + I(spei^2) + I(spei^3),data = yarra,random=~1|uniqueID, method="ML")

m.Ci_int <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n*p, data = yarra, random = ~1|uniqueID, method="ML")
m.Ci_n <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n, data = yarra, random = ~1|uniqueID, method="ML")
m.Ci_p <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*p, data = yarra, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
min(AICc(m.null_new, m.La_int, m.La_n, m.La_p, m.Li_int, m.Li_n, m.Li_p, m.Qa_int, m.Qa_n, m.Qa_p, m.Qi_int, m.Qi_n, m.Qi_p, m.Ca_int, m.Ca_n, m.Ca_p, m.Ci_int, m.Ci_n, m.Ci_p)
    [,2])
<<<<<<< HEAD
# m.Ca_n
## none within 2

fits <- data.frame("uniqueID" = names(fitted(object = m.Ca_n)),
                   "anpp_model_fits" = fitted(object = m.Ca_n))

# Drop rownames (purely for aesthetic reasons)
rownames(fits) <- NULL

# Bind onto "real" data
yarra_fits <- dplyr::left_join(x = yarra, y = fits, by = "uniqueID")

# Make desired plot
ggplot(data = yarra_fits) +
  geom_point(aes(x = spei, y = anpp, color = trt_type), alpha = 0.2) +
  geom_smooth(aes(x = spei, y = anpp_model_fits, color = trt_type, fill = trt_type), method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = F) +
  #facet_wrap( ~ n) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP") 
r.squaredGLMM(m.Ca_n)

=======
>>>>>>> c30c6b0e6b00b6d6522218b0f626d3368c091d43
>>>>>>> 90542ff1d1a05d53734ec13c88077450733a4394
