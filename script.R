## ================== ANÁLISE EXPLORATÓRIA ============================

# gráfico da figura 14
temp_ml %>% 
  mutate(doy = strftime(dataa, format = "%j") %>% as.numeric()) %>% 
  group_by(year,tree, doy) %>% 
  dplyr::summarise(sum = sum(ml, na.rm = T)/1000) %>% 
  mutate(year_tree = paste0(year,"/",tree)) %>% 
  mutate(year_tree = factor(year_tree, levels = c("2018/1", "2018/2", "2019/1", "2019/2", "2019/3", "2019/4", 
                                                  "2020/1", "2020/2", "2020/3", "2020/4", "2021/1", "2021/2", 
                                                  "2021/3", "2021/4"))) %>% 
  mutate(sum = ifelse(sum == 0, NA, sum)) %>%
  ggplot(aes(x = doy, y = fct_rev(year_tree), fill = sum)) +
  geom_tile(color="black", size=0.1) +
  coord_equal() +
  scale_fill_viridis_c(option = "C", direction = -1, na.value="white") +
  geom_vline(xintercept = c(90,120, 151), color = "black") +
  labs(x = "dia do ano", y = "ano/árvore", 
       fill = "produção de seiva (litros)") +
  theme_minimal() +
  theme(axis.ticks = element_line(color = "#EBEBEB"),
        axis.line = element_line(color = "#EBEBEB"),
        strip.text = element_text(face = "plain", size = 13),
        axis.title = element_text(face = "plain", size = 13),
        legend.title = element_text(face = "plain", size = 11),
        legend.text = element_text(face = "plain", size = 11),
        panel.background = element_rect(fill = "#f5f5f5",
                                        color = "#f5f5f5")) +
  theme(legend.key.width=unit(1.0,"cm"),
        legend.key.height=unit(.25,"cm")) +
  theme(legend.position="bottom")

## ================== MODELOS CONTÍNUOS COM ZERO ==========================

# seleção de filtro
temp_ml <- temp_ml %>% filter(hour %in% 9:17)
# adição da constante c = 0.001
temp_ml_cont <- temp_ml %>% mutate(ml = ifelse(ml == 0, 0.001, ml))

# modelos da árvore 1 em 2018
x <- temp_ml_cont %>% drop_na %>% filter(year == 2018, tree == 1)
mod_2018_1_ga <- gamlss(ml ~ 1, family = GA(),
                        data = x, trace = T, method = mixed(100,300))
mod_2018_1_gg <- gamlss(ml ~ 1, family = GG(),
                        data = x, trace = T, method = mixed(100,300))
mod_2018_1_gp <- gamlss(ml ~ 1, family = GP(),
                        data = x, trace = T, method = mixed(100,300))

## ================== MODELOS CONTÍNUOS SEM ZERO ==========================

# dados sem zero
temp_ml <- temp_ml %>% filter(hour %in% 9:17, ml != 0)

# modelos da árvore 1 em 2018
mod_2018_1_ga <- gamlss(ml ~ 1, family = GA(),
                        data = x, method = mixed(100,300))
mod_2018_1_gg <- gamlss(ml ~ 1, family = GG(),
                        data = x, trace = T, method = mixed(100,300))
mod_2018_1_wei <- gamlss(ml ~ 1, family = WEI(),
                         data = x, method = mixed(100,300))
mod_2018_1_exp <- gamlss(ml ~ 1, family = EXP(),
                         data = x, method = mixed(100,300))
mod_2018_1_log <- gamlss(ml ~ 1, family = LOGNO(),
                         data = x, method = mixed(100,300))

## =========== MODELOS CONTÍNUOS COM ZEROS AJUSTADOS =================

# modelos da árvore 1 em 2018
x <- temp_ml %>% drop_na %>% filter(year == 2018, tree == 1)
mod_2018_1_gp <- gamlssZadj(ml, family = GP, data = x, n.cyc=700)
mod_2018_1_gg <- gamlssZadj(ml, family = GG, data = x, n.cyc=500)
mod_2018_1_ga <- gamlssZadj(ml, family = GA, data = x, n.cyc=500)
mod_2018_1_wei <- gamlssZadj(ml, family = WEI, data = x, n.cyc=500)
mod_2018_1_log <- gamlssZadj(ml, family = LOGNO, data = x, n.cyc=500)

## ================== MODELOS DISCRETOS ============================

# modelos da árvore 1 em 2018
x <- df_cut %>% drop_na %>% filter(year == 2018, tree == 1)
mod_2018_1_nbi <- gamlss(ml ~ 1, family = NBI(), data = x, 
                         method = mixed(100,300))
mod_2018_1_zinbi <- gamlss(ml ~ 1, family = ZINBI(), data = x, 
                           method = mixed(100,300))
mod_2018_1_zisichel <- gamlss(ml ~ 1, family = ZISICHEL(), data = x, 
                              method = mixed(100,300))
mod_2018_1_zipig <- gamlss(ml ~ 1, family = ZIPIG(), data = x, 
                           method = mixed(100,300))

## ================== MODELOS CENSURADOS ===========================

# Modelo com censura parcial dos dados. Árvore 4, ano de 2021
cens1 <- gamlss(Surv(left, right, censo, type = "interval") ~ 1,
                data = vec,
                method = CG(400),
                family = cens(GA, type = "interval"))

# Gráfico quantil-quantil para os resíduos do modelo
resfit1 <- cens1$residuals
qqnorm(resfit1,
       xlab = "Theoretical Quantile", ylab = "Simple Quantile")
qqline(resfit1)

# Gera uma didtribuição Gama com censura intervalar
gen.cens(GA, type = "interval")

# gráfico para visualização da distribuição acumulada e curva do modelo ajustado
plot(ecdf((vec$left + vec$left) / 2), main = "")
plot(function(y) pGAic(Surv(y), mu = fitted.values(cens1, "mu")[1],
                       sigma = fitted.values(cens1, "sigma")[1]),
     from = 0, to = 800, add = T)


# Modelo com censura total dos dados. Árvore 4, ano de 2019
cens2 <- gamlss(Surv(left, left + 2, type = "interval2") ~ 1,
                data = vec,
                method = CG(400),
                family = cens(GG, type = "interval"))

# Gráfico quantil-quantil para os resíduos do modelo
resfit1 <- cens1$residuals
qqnorm(resfit1,
       xlab = "Theoretical Quantile", ylab = "Simple Quantile")
qqline(resfit1)

# Gera uma didtribuição Gama Generalizada com censura intervalar
gen.cens(GG, type = "interval")

# gráfico para visualização da distribuição acumulada e curva do modelo ajustado
plot(ecdf((vec$left + 1)), main = "")
plot(function(y) pGGic(Surv(y), mu = fitted.values(cens2, "mu")[1],
                       sigma = fitted.values(cens2, "sigma")[1],
                       nu = fitted.values(cens2, "nu")[1]),
     from = 0, to = 800, add = T)