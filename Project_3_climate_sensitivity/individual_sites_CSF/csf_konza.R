##file for looking at CSF for Konza experiments

source("Project_3_climate_sensitivity/corre_spei_file_cleaning.R")


konza <- n_sites %>%
  filter(site_code == "KNZ")

bgp <- filter(konza, project_name == "BGP")
pplots <- filter(konza, project_name == "pplots")

### KONZA BGP PLOTS -- Control vs. N*P ####
unique(bgp$n)
unique(bgp$p)

## use trt_type
m.null <- lme(anpp ~ year*trt_type, data = bgp, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + trt_type, data = bgp,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*trt_type, data = bgp,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+trt_type + I(spei^2), data = bgp, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type, data = bgp,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+trt_type + I(spei^2) + I(spei^3),data = bgp,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type + I(spei^3)*trt_type, data = bgp, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.null

lm.null <- lm(anpp ~ year*trt_type, data = bgp)

visreg(lm.null, xvar = "year", type = "conditional", by = "trt_type", data = bgp, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = year, y = anpp, color = trt_type), alpha = 0.2, data = bgp) +
  theme_bw() +
  labs(x="YEAR",
       y="ANPP")

### KONZA PPLOTS -- multi P, +/- N ####
unique(pplots$n)
unique(pplots$p)


## use n*p
m.null <- lme(anpp ~ year*n*p, data = pplots, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n*p, data = pplots,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n*p, data = pplots,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n*p + I(spei^2), data = pplots, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n*p + I(spei^2)*n*p, data = pplots,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n*p + I(spei^2) + I(spei^3),data = pplots,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n*p, data = pplots, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Best model is m.Ci

ggplot(pplots, aes(x = spei, y = anpp, color = as.factor(n))) +
  geom_point() +
  facet_wrap(~p) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), span = 1, se = F) +
  labs(y = "ANPP")
