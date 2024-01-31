##file for looking at CSF for KUFS experiments

source("Project_3_climate_sensitivity/corre_spei_file_cleaning.R")

### Nick -- the issues are in the KUFS E6 header 
kufs <- n_sites %>%
  filter(site_code == "KUFS")

## make df for each experiment
e2 <- kufs %>%
  filter(project_name == "E2")

e6 <- kufs %>%
  filter(project_name == "E6")


### KUFS E2 -- Control, Nitrogen ####
m.null <- lme(anpp ~ year*trt_type, data = e2, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + trt_type, data = e2,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*trt_type, data = e2,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+trt_type + I(spei^2), data = e2, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type, data = e2,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+trt_type + I(spei^2) + I(spei^3),data = e2,random=~1|uniqueID,method="ML")
m.Ci <- lme(anpp ~ spei*trt_type + I(spei^2)*trt_type + I(spei^3)*trt_type, data = e2, random = ~1|uniqueID, method="ML")

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
m.Ci_plot <- lm(anpp ~ spei*trt_type + I(spei^2)*trt_type + I(spei^3)*trt_type, data = e2)

## this one does what we want it to :)
visreg(m.Ci_plot, xvar = "spei", type = "conditional", by = "trt_type", data = e2, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) +
  geom_point(aes(x = spei, y = anpp, color = trt_type), alpha = 0.2, data = e2) +
  # facet_wrap(~p_levels) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP")+
  scale_y_continuous(limits = c(0,1500))

### KUFS E6 -- Control, multi N, +/- P ####

m.null <- lme(anpp ~ year*n*p, data = e6, random = ~1|uniqueID, method="ML")
m.La <- lme(anpp ~ spei + n*p, data = e6,random = ~1|uniqueID, method="ML")
m.Li <- lme(anpp ~ spei*n*p, data = e6,random = ~1|uniqueID, method="ML")
m.Qa <- lme(anpp ~ spei+n*p + I(spei^2), data = e6, random = ~1|uniqueID, method="ML")
m.Qi <- lme(anpp ~ spei*n*p + I(spei^2)*n*p, data = e6,random=~1|uniqueID,method="ML")
m.Ca <- lme(anpp ~ spei+n*p + I(spei^2) + I(spei^3),data = e6,random=~1|uniqueID, method="ML")
m.Ci <- lme(anpp ~ spei*n*p + I(spei^2)*n*p + I(spei^3)*n*p, data = e6, random = ~1|uniqueID, method="ML")

# model selection
AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)
min(AICc(m.null, m.La, m.Li, m.Qa, m.Qi, m.Ca, m.Ci)[,2])
#Min AICc model is m.Ca: 
## additive models are better fits than interactive models, generally
summary(m.Ca)

## try to plot the lme model; doesn't work
visreg(m.Ca, xvar = "spei", type = "conditional", by = "n", data = e6, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) 
geom_point(aes(x = spei, y = anpp), alpha = 0.2, data = e6) +
  #facet_grid(p~n) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP")

#make an lm model just for plotting -- same as m.Ca just no random effects
lm.Ca <- lm(anpp ~ spei+n*p + I(spei^2) + I(spei^3),data = e6)

visreg(lm.Ca, xvar = "spei", type = "conditional", by = "n", data = e6, gg = TRUE, partial = F, rug = F, overlay = TRUE, alpha = 1) + 
  geom_point(aes(x = spei, y = anpp), alpha = 0.2, data = e6) +
  #facet_grid(p~n) +
  theme_bw() +
  labs(x="SPEI",
       y="ANPP")
## can't see the P data; also can't see the N = 8 data for some reason, which is weird


## ggplot with geom_smooth just to visualize 
ggplot(e6, aes(x = spei, y = anpp, color = as.factor(p))) +
  geom_point() +
  facet_wrap(~n, labels = c("0g N", "4g N", "8g N", "16g N")) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), span = 1, se = F) +
  labs(y = "ANPP") +
  theme_bw()
