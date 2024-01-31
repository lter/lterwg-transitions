## master sheet of all sites
source("Project_3_climate_sensitivity/corre_spei_file_cleaning.R")


### yarra NutNet ####
yarra <- filter(n_sites, site_code == "yarra.au")
## NutNet site -- use trt_type
m.null <- lme(anpp ~ year*trt_type, data = yarra, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + trt_type, data = yarra,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*trt_type, data = yarra,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+trt_type + I(spei^2), data = yarra, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type, data = yarra,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+trt_type + I(spei^2) + I(spei^3),data = yarra,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type + I(spei^3)*trt_type, data = yarra, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.Ca,  1780.543
## none within 2
## additive models are better fits than interactive models, generally
summary(m.Ca)
lm.Ca <- lm(anpp ~ spei+trt_type + I(spei^2) + I(spei^3), data = yarra)

visreg(lm.Ca, xvar = "spei", type = "conditional", by = "trt_type", data = yarra, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = spei, y = anpp, color = trt_type), alpha = 0.2, data = yarra) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP")

ggplot(yarra, aes(x = spei, y = anpp, color = trt_type)) +
  geom_point() +
#  facet_wrap(~p) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), span = 1, se = F) +
  labs(y = "ANPP")

#### KNZ - BGP ####
knz_bgp <- filter(n_sites, site_code == "KNZ" & project_name == "BGP")
unique(knz_bgp$n)
unique(knz_bgp$p)

## use trt_type
m.null <- lme(anpp ~ year*trt_type, data = knz_bgp, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + trt_type, data = knz_bgp,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*trt_type, data = knz_bgp,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+trt_type + I(spei^2), data = knz_bgp, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type, data = knz_bgp,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+trt_type + I(spei^2) + I(spei^3),data = knz_bgp,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type + I(spei^3)*trt_type, data = knz_bgp, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.null

lm.null <- lm(anpp ~ year*trt_type, data = knz_bgp)

visreg(lm.null, xvar = "year", type = "conditional", by = "trt_type", data = knz_bgp, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = year, y = anpp, color = trt_type), alpha = 0.2, data = knz_bgp) +
  theme_bw() +
  labs(x="YEAR",
       y="ANPP")
### KNZ - pplots ####
knz_pplots <- filter(n_sites, site_code == "KNZ" & project_name == "pplots")
unique(knz_pplots$n)
unique(knz_pplots$p)
knz_pplots <- knz_pplots %>%
  unite(n_p, c("n", "p"), sep = "-", remove = FALSE)


## use trt_type
m.null <- lme(anpp ~ year*n*p, data = knz_pplots, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n*p, data = knz_pplots,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n*p, data = knz_pplots,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n*p + I(spei^2), data = knz_pplots, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n*p + I(spei^2)*n*p, data = knz_pplots,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n*p + I(spei^2) + I(spei^3),data = knz_pplots,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n*p, data = knz_pplots, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.Ci
##make an lm for plotting
lm.Ca <- lm(anpp ~ spei+n*p + I(spei^2) + I(spei^3),data = knz_pplots)

visreg(lm.Ca, xvar = "spei", type = "conditional", by = "n", data = knz_pplots, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1, layout = c(4,2)) +
  geom_point(aes(x = spei, y = anpp), alpha = 0.2, data = knz_pplots) +
 # facet_grid(p~n) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP")

ggplot(knz_pplots, aes(x = spei, y = anpp, color = as.factor(n))) +
  geom_point() +
  facet_wrap(~p) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), span = 1, se = F) +
  labs(y = "ANPP")

### CDR - biocon ####
cdr_biocon <- filter(n_sites, site_code == "CDR" & project_name == "BioCON")
# just n versus control


## use trt_type
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
# use trt type


## use trt_type
m.null <- lme(anpp ~ year*trt_type, data = cdr_nutnet, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + trt_type, data = cdr_nutnet,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*trt_type, data = cdr_nutnet,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+trt_type + I(spei^2), data = cdr_nutnet, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type, data = cdr_nutnet,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+trt_type + I(spei^2) + I(spei^3),data = cdr_nutnet,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type + I(spei^3)*trt_type, data = cdr_nutnet, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.Ca
##make an lm for plotting
lm.Ca <- lm(anpp ~ spei+trt_type + I(spei^2) + I(spei^3),data = cdr_nutnet)

visreg(lm.Ca, xvar = "spei", type = "conditional", by = "trt_type", data = cdr_nutnet, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = spei, y = anpp), alpha = 0.2, data = cdr_nutnet) +
  # facet_grid(p~n) +
  theme_bw() +
  labs(x="YEAR",
       y="ANPP")
### SERC - CXN ####
serc <- filter(n_sites, site_code == "SERC")
# just n versus control


## use trt_type
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
##make an lm for plotting
lm.null <- lm(anpp ~ year*n, data = serc)

visreg(lm.null, xvar = "year", type = "conditional", by = "n", data = serc, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = year, y = anpp), alpha = 0.2, data = serc) +
  # facet_grid(p~n) +
  theme_bw() +
  labs(x="YEAR",
       y="ANPP")
#### cbgb - NutNet ####
cbgb <- filter(n_sites, site_code == "cbgb.us" & project_name == "NutNet")
# use trt type


## use trt_type
m.null <- lme(anpp ~ year*trt_type, data = cbgb, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + trt_type, data = cbgb,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*trt_type, data = cbgb,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+trt_type + I(spei^2), data = cbgb, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type, data = cbgb,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+trt_type + I(spei^2) + I(spei^3),data = cbgb,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type + I(spei^3)*trt_type, data = cbgb, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.null
##make an lm for plotting
lm.null <- lm(anpp ~ year*trt_type, data = cbgb)

visreg(lm.null, xvar = "year", type = "conditional", by = "trt_type", data = cbgb, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = year, y = anpp), alpha = 0.2, data = cbgb) +
  # facet_grid(p~n) +
  theme_bw() +
  labs(x="YEAR",
       y="ANPP")
###sier.us - NutNet ####
sier <- filter(n_sites, site_code == "sier.us" & project_name == "NutNet")
# use trt type


## use trt_type
m.null <- lme(anpp ~ year*trt_type, data = sier, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + trt_type, data = sier,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*trt_type, data = sier,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+trt_type + I(spei^2), data = sier, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type, data = sier,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+trt_type + I(spei^2) + I(spei^3),data = sier,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type + I(spei^3)*trt_type, data = sier, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.null
##make an lm for plotting
lm.null <- lm(anpp ~ year*trt_type, data = sier)

visreg(lm.null, xvar = "year", type = "conditional", by = "trt_type", data = sier, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = year, y = anpp), alpha = 0.2, data = sier) +
  # facet_grid(p~n) +
  theme_bw() +
  labs(x="YEAR",
       y="ANPP")
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
#Best model is m.Ci
##make an lm for plotting
lm.null <- lm(anpp ~ year*n, data = niwot)
lm.Ca <- lm(anpp ~ spei+n + I(spei^2) + I(spei^3),data = niwot)

visreg(lm.Ca, xvar = "spei", type = "conditional", by = "n", data = niwot, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = spei, y = anpp), alpha = 0.2, data = niwot) +
  facet_wrap(~n) +
  theme_bw() +
  labs(x="YEAR",
       y="ANPP")
### KBS T7 ####
kellogg <- filter(n_sites, site_code == "KBS")
# just n versus control


## use trt_type
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


m.null <- lme(anpp ~ year*n*p, data = kufs_e6, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n*p, data = kufs_e6,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n*p, data = kufs_e6,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n*p + I(spei^2), data = kufs_e6, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n*p + I(spei^2)*n*p, data = kufs_e6,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n*p + I(spei^2) + I(spei^3),data = kufs_e6,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n*p, data = kufs_e6, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Min AICc model is m.Ca: 
## additive models are better fits than interactive models, generally
summary(m.Ca)
#Visualize CSF results---

ggplot(kufs_e6, aes(x = spei, y = anpp, color = as.factor(p))) +
  geom_point() +
  facet_wrap(~n) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), span = 1, se = F) +
  labs(y = "ANPP")
min(kufs_e6$spei)
max(kufs_e6$spei)

## test out cedar creek nutnet site with N and P independent in the models ####
cdr_nutnet <- filter(n_sites, site_code == "CDR" & project_name == "NutNet")
# use trt type


## normal
m.null <- lme(anpp ~ year*trt_type, data = cdr_nutnet, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + trt_type, data = cdr_nutnet,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*trt_type, data = cdr_nutnet,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+trt_type + I(spei^2), data = cdr_nutnet, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type, data = cdr_nutnet,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+trt_type + I(spei^2) + I(spei^3),data = cdr_nutnet,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type + I(spei^3)*trt_type, data = cdr_nutnet, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])

## new model fits
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


ggplot(cdr_nutnet, aes(x = spei, y = anpp, color = trt_type)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = F) +
  theme_bw()


## try with cbgb -- see if it changes null ones ####
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


## try with yarra ####
yarra <- filter(n_sites, site_code == "yarra.au")
## NutNet site -- use trt_type
m.null <- lme(anpp ~ year*trt_type, data = yarra, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + trt_type, data = yarra,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*trt_type, data = yarra,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+trt_type + I(spei^2), data = yarra, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type, data = yarra,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+trt_type + I(spei^2) + I(spei^3),data = yarra,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type + I(spei^3)*trt_type, data = yarra, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])


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
