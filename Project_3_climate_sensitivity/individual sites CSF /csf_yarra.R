## CSF for Yarra NutNet site (Australia)
source("Project_3_climate_sensitivity/corre_spei_file_cleaning.R")


yarra <- n_sites %>%
  filter(site_code == "yarra.au")


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