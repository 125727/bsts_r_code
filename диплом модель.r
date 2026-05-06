library(readxl)
library(dplyr)
library(zoo)
library(CausalImpact)
library(car)


# импорт файла
df0 <- read_excel("/Users/tatya.a.kuznetsova/Desktop/диплом/финальная.xlsx")

# описательные статистики
library(psych)
stats <- describe(df0)[, c('n', 'mean', 'sd', 'min', 'max')]
stats

# перевод даты к нужному типа
if (is.numeric(df0$week_dt)) {
  df0$week_dt <- as.Date(df0$week_dt, origin = "1899-12-30")
} else if (inherits(df0$week_dt, c("POSIXct", "POSIXt"))) {
  df0$week_dt <- as.Date(df0$week_dt)
} else {
  df0$week_dt <- as.Date(as.character(df0$week_dt))
}


df <- df0 %>%
  select(week_dt, cnt_without_payment, cnt_without_payment_pledged, key_rate, cnt_utilized, cnt_apps, cnt_apps_rej, cnt_utilized_all, cnt_utilized_pledged, conv, conv_pledged, income, real_income, unemployment, active_clients)


# перевод остальных переменных к числовому типу и сортировка по датам
df <- df %>%
  mutate(
    cnt_without_payment = as.numeric(cnt_without_payment),
    cnt_without_payment_pledged = as.numeric(cnt_without_payment_pledged),
    cnt_utilized = as.numeric(cnt_utilized),
    cnt_utilized_all = as.numeric(cnt_utilized_all),
    cnt_utilized_pledged = as.numeric(cnt_utilized_pledged),
    key_rate = as.numeric(key_rate),
    cnt_apps = as.numeric(cnt_apps),
    cnt_apps_rej = as.numeric(cnt_apps_rej),
    income = as.numeric(income),
    real_income = as.numeric(real_income),
    unemployment = as.numeric(unemployment),
    active_clients = as.numeric(active_clients)
  ) %>%
  arrange(week_dt)


# динамика переменных
df

par(mfrow = c(2, 2))

plot(df$week_dt, df$cnt_without_payment, type = "l",
     main = "cnt_without_payment", xlab = "date", ylab = "value")

plot(df$week_dt, df$cnt_without_payment_pledged, type = "l",
     main = "cnt_without_payment_pledged", xlab = "date", ylab = "value")

plot(df$week_dt, df$cnt_utilized, type = "l",
     main = "cnt_utilized", xlab = "date", ylab = "value")

plot(df$week_dt, df$key_rate, type = "l",
     main = "key_rate", xlab = "date", ylab = "value")

plot(df$week_dt, df$cnt_apps, type = "l",
     main = "cnt_apps", xlab = "date", ylab = "value")

plot(df$week_dt, df$cnt_apps_rej, type = "l",
     main = "cnt_apps_rej", xlab = "date", ylab = "value")

plot(df$week_dt, df$conv, type = "l",
     main = "conv", xlab = "date", ylab = "value")


par(mfrow = c(1, 1))


# логарифиммирование переменных
df1 <- df %>%
  mutate(
    cnt_without_payment = log1p(cnt_without_payment),
    cnt_without_payment_pledged = log1p(cnt_without_payment_pledged),
    cnt_utilized = log1p(cnt_utilized),
    cnt_apps = log1p(cnt_apps),
    cnt_apps_rej = log1p(cnt_apps_rej),
    cnt_utilized_all = log1p(cnt_utilized_all),
    cnt_utilized_pledged = log1p(cnt_utilized_pledged)
  )



# индексация по датам
ts_data <- zoo(
  # x = df1[, c("cnt_without_payment", "cnt_without_payment_pledged", "key_rate")],
  # x = df1[, c("cnt_without_payment", "cnt_without_payment_pledged")],
  x = df1[, c("cnt_without_payment", "cnt_without_payment_pledged", "key_rate", "cnt_apps")],
  order.by = df$week_dt
)

ts_data

# корреляция переменных
print(round(cor(ts_data, use = "pairwise.complete.obs"), 3))


# вифы
y_name <- "cnt_without_payment"

pre.period  <- as.Date(c("2022-06-27", "2025-08-25")) # пример

pre_zoo <- window(ts_data, start = pre.period[1], end = pre.period[2])
pre_df  <- as.data.frame(pre_zoo)

# регрессия только для оценки мультиколлинеарности x
fit_lm <- lm(cnt_without_payment ~ ., data = ts_data)

vif_values <- car::vif(fit_lm)
vif_values


# диапазон дат
print(range(index(ts_data)))


# периоды (пре и пост)
pre.period  <- as.Date(c("2022-06-27", "2025-08-25"))
post.period <- as.Date(c("2025-09-01", "2025-12-15"))


# базовая модель
impact <- CausalImpact(ts_data, pre.period, post.period)


# результаты модели
print(summary(impact))
summary(impact, "report")


#график
plot(impact)



# кросс-временная валидация
library(CausalImpact)
library(Metrics)
library(zoo)

# оставляем только пре-период
ts_pre <- window(ts_data, start = pre.period[1], end = pre.period[2])
dates_pre <- index(ts_pre)
n_pre <- length(dates_pre)

# окна
initial_window <- 104 
horizon <- 4
step <- 4 

param_grid <- expand.grid(
  prior_level_sd = c(0.01, 0.03, 0.05, 0.07, 0.1),
  nseasons = c(1, 52) 
)

cv_results <- data.frame(
  prior_level_sd = numeric(), 
  nseasons = numeric(), 
  mean_mae = numeric(),
  mean_rmse = numeric()
)

for (i in 1:nrow(param_grid)) {
  
  sd_val <- param_grid$prior_level_sd[i]
  seas_val <- param_grid$nseasons[i]
  
  model_args <- list(prior.level.sd = sd_val, niter = 1000)
  if (seas_val > 1) {
    model_args$nseasons <- seas_val
    model_args$season.duration <- 1
  }
  
  fold_mae <- c()
  fold_rmse <- c()
  
  for (start_test_idx in seq(initial_window + 1, n_pre - horizon + 1, by = step)) {
    end_test_idx <- start_test_idx + horizon - 1
    
    cv_pre <- c(dates_pre[1], dates_pre[start_test_idx - 1])
    cv_post <- c(dates_pre[start_test_idx], dates_pre[end_test_idx])
    
    capture.output({
      impact_cv <- CausalImpact(ts_pre, cv_pre, cv_post, model.args = model_args)
    })
    
    # логарифмированные значения
    actuals_log <- as.numeric(coredata(window(ts_pre[, 1], start = cv_post[1], end = cv_post[2])))
    
    idx_start <- nrow(window(ts_pre, start = cv_pre[1], end = cv_pre[2])) + 1
    idx_end <- idx_start + horizon - 1
    predictions_log <- as.numeric(impact_cv$series$point.pred[idx_start:idx_end])
    
    # из логарифмов в реальные числа
    actuals_real <- expm1(actuals_log)
    preds_real <- expm1(predictions_log)
    
    # ошибки
    fold_mae <- c(fold_mae, mae(actuals_real, preds_real))
    fold_rmse <- c(fold_rmse, rmse(actuals_real, preds_real))
  }
  
  cv_results <- rbind(cv_results, data.frame(
    prior_level_sd = sd_val,
    nseasons = seas_val,
    mean_mae = mean(fold_mae, na.rm = TRUE),
    mean_rmse = mean(fold_rmse, na.rm = TRUE)
  ))
}

# сортируем по RMSE
cv_results <- cv_results[order(cv_results$mean_rmse), ]
print(cv_results)

best_sd <- cv_results$prior_level_sd[1]
best_seas <- cv_results$nseasons[1]

best_sd 
best_seas   
    


# финальная модель (целевая переменная - конверсия в невыплату)
ts_data3 <- zoo(
  x = df1[, c("conv", "key_rate", "conv_pledged", 'cnt_apps')],
  order.by = df$week_dt
)

ts_data3

# модель
impact3 <- CausalImpact(ts_data3, pre.period, post.period, alpha = 0.1,  model.args = list(niter = 10000, prior.level.sd = 0.1, nseasons = 1))


# результаты модели
print(summary(impact3))
summary(impact3, "report")

plot(impact3, metrics = c('original', 'pointwise'))



# тест Плацебо
fake_pre_period  <- as.Date(c("2022-06-27", "2025-05-05"))
fake_post_period <- as.Date(c("2025-05-12", "2025-08-25"))


ts_placebo_3 <- window(ts_data3, start = fake_pre_period[1], end = fake_post_period[2])


impact_placebo_3 <- CausalImpact(ts_placebo_3, fake_pre_period, fake_post_period, alpha = 0.1, model.args = list(niter = 10000, prior.level.sd = 0.1, nseasons = 1))
print(summary(impact_placebo_3))
plot(impact_placebo_3)


# коэффициенты

colMeans(impact3$model$bsts.model$coefficients != 0)
colMeans(impact3$model$bsts.model$coefficients)

residuals(impact3)

acf(residuals(impact3))




# анализ остатков модели
library(tseries)

# остатки
actuals <- impact3$series$response
preds <- impact3$series$point.pred
resids_pre <- na.omit(window(actuals - preds, start = pre.period[1], end = pre.period[2]))

# стандартизация
std_resids <- resids_pre / sd(resids_pre)

# тест Дики-Фуллера (на стационарность)
adf_result <- adf.test(std_resids, alternative = "stationary")
adf_result

# тест Стьюдента (на равенство среднего нулю)
t_result <- t.test(std_resids, mu = 0)
t_result

# тест Льюнга-Бокса 
lb_result <- Box.test(std_resids, type = "Ljung-Box")
lb_result


# графики
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2), oma = c(2, 2, 2, 2))

# стационарность
plot(std_resids, type = "l", col = "steelblue", lwd = 1.5,
     main = "Standardized residual", ylab = "", xlab = "Date")
abline(h = 0, col = "gray", lty = 1)

# гистограмма 
hist(std_resids, prob = TRUE, breaks = 15, col = "steelblue", border = "white",
     main = "Histogram plus estimated density", xlab = "", ylab = "")

lines(density(std_resids), col = "darkorange", lwd = 2)

curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "green3", lwd = 2)
legend("topright", legend = c("KDE", "N(0,1)", "Hist"),
       fill = c(NA, NA, "steelblue"), border = c(NA, NA, "white"),
       col = c("darkorange", "green3", NA), lwd = c(2, 2, NA), bty = "n", cex = 0.8)

# --- Normal Q-Q (Квантиль-Квантиль) ---
qqnorm(std_resids, main = "Normal Q-Q", pch = 16, col = "blue", 
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(std_resids, col = "red", lwd = 2)

# --- Коррелограмма (ACF) ---
acf(std_resids, main = "Correlogram", xlab = "Lag")


# Возвращаем стандартные настройки отображения (1 график на окно)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))







actuals <- impact3$series$response
preds <- impact3$series$point.pred

# пре-периодs
actuals_pre <- as.numeric(window(actuals, start = pre.period[1], end = pre.period[2]))
preds_pre <- as.numeric(window(preds, start = pre.period[1], end = pre.period[2]))


valid_idx <- !is.na(preds_pre) & !is.na(actuals_pre)
actuals_clean <- actuals_pre[valid_idx]
preds_clean <- preds_pre[valid_idx]

# метрики
mae_val <- mae(actuals_clean, preds_clean)
rmse_val <- rmse(actuals_clean, preds_clean)
mape_val <- mape(actuals_clean, preds_clean) * 100 # в проценты
r_squared <- cor(actuals_clean, preds_clean)^2     # псевдо-R2


quality_metrics_df <- data.frame(
  Metric = c("MAE", "RMSE", "MAPE (%)", "Pseudo R-squared"),
  Value = c(mae_val, rmse_val, mape_val, r_squared)
)

print(quality_metrics_df)






# 1. Извлекаем внутреннюю модель BSTS 
bsts_model <- impact3$model$bsts.model

# 2. Получаем стандартизированную сводку по модели
bsts_summary <- summary(bsts_model)

# 3. Извлекаем матрицу с коэффициентами и преобразуем в data.frame
coefficients_df <- as.data.frame(bsts_summary$coefficients)

colnames(coefficients_df) <- c(
  "Безусловное среднее", 
  "Безусловное SD", 
  "Условное среднее", 
  "Условное SD", 
  "Вероятность включения"
)

print("--- Апостериорные оценки коэффициентов модели ---")
print(coefficients_df)




-----
  
  


#---- зависимая переменная - утилизации ----
  
  
  
  
  
# конверсия в невыплату с утилизациями в ковариатах
ts_data6 <- zoo(
  x = df1[, c("cnt_utilized", "key_rate", "cnt_utilized_pledged", 'cnt_apps')],
  order.by = df$week_dt
)

ts_data6

# модель
impact6 <- CausalImpact(ts_data6, pre.period, post.period, alpha = 0.1,  model.args = list(niter = 10000, prior.level.sd = 0.1, nseasons = 1))


# результаты модели
print(summary(impact6))
summary(impact6, "report")

plot(impact6, metrics = c('original', 'pointwise'))





# кросс-временная валидация


# оставляем только пре-период
ts_pre <- window(ts_data6, start = pre.period[1], end = pre.period[2])
dates_pre <- index(ts_pre)
n_pre <- length(dates_pre)

# окна
initial_window <- 104 
horizon <- 4
step <- 4 

param_grid <- expand.grid(
  prior_level_sd = c(0.01, 0.03, 0.05, 0.07, 0.1),
  nseasons = c(1, 52) 
)

cv_results <- data.frame(
  prior_level_sd = numeric(), 
  nseasons = numeric(), 
  mean_mae = numeric(),
  mean_rmse = numeric()
)

for (i in 1:nrow(param_grid)) {
  
  sd_val <- param_grid$prior_level_sd[i]
  seas_val <- param_grid$nseasons[i]
  
  model_args <- list(prior.level.sd = sd_val, niter = 1000)
  if (seas_val > 1) {
    model_args$nseasons <- seas_val
    model_args$season.duration <- 1
  }
  
  fold_mae <- c()
  fold_rmse <- c()
  
  for (start_test_idx in seq(initial_window + 1, n_pre - horizon + 1, by = step)) {
    end_test_idx <- start_test_idx + horizon - 1
    
    cv_pre <- c(dates_pre[1], dates_pre[start_test_idx - 1])
    cv_post <- c(dates_pre[start_test_idx], dates_pre[end_test_idx])
    
    capture.output({
      impact_cv <- CausalImpact(ts_pre, cv_pre, cv_post, model.args = model_args)
    })
    
    # логарифмированные значения
    actuals_log <- as.numeric(coredata(window(ts_pre[, 1], start = cv_post[1], end = cv_post[2])))
    
    idx_start <- nrow(window(ts_pre, start = cv_pre[1], end = cv_pre[2])) + 1
    idx_end <- idx_start + horizon - 1
    predictions_log <- as.numeric(impact_cv$series$point.pred[idx_start:idx_end])
    
    # из логарифмов в реальные числа
    actuals_real <- expm1(actuals_log)
    preds_real <- expm1(predictions_log)
    
    # ошибки
    fold_mae <- c(fold_mae, mae(actuals_real, preds_real))
    fold_rmse <- c(fold_rmse, rmse(actuals_real, preds_real))
  }
  
  cv_results <- rbind(cv_results, data.frame(
    prior_level_sd = sd_val,
    nseasons = seas_val,
    mean_mae = mean(fold_mae, na.rm = TRUE),
    mean_rmse = mean(fold_rmse, na.rm = TRUE)
  ))
}

# сортируем по RMSE
cv_results <- cv_results[order(cv_results$mean_rmse), ]
print(cv_results)

best_sd <- cv_results$prior_level_sd[1]
best_seas <- cv_results$nseasons[1]

best_sd 
best_seas   


# финальная модель 
impact_final <- CausalImpact(ts_data6, pre.period, post.period,  model.args = list(niter = 10000, prior.level.sd = 0.3, nseasons = 1))
summary(impact_final)
plot(impact_final)





# плацебо
fake_pre_period  <- as.Date(c("2022-06-27", "2025-05-05"))
fake_post_period <- as.Date(c("2025-05-12", "2025-08-25"))


ts_placebo_6 <- window(ts_data6, start = fake_pre_period[1], end = fake_post_period[2])


impact_placebo_6 <- CausalImpact(ts_placebo_6, fake_pre_period, fake_post_period, alpha = 0.1, model.args = list(niter = 10000, prior.level.sd = 0.1, nseasons = 1))
print(summary(impact_placebo_6))
plot(impact_placebo_6)





actuals <- impact6$series$response
preds <- impact6$series$point.pred

# пре-периодs
actuals_pre <- as.numeric(window(actuals, start = pre.period[1], end = pre.period[2]))
preds_pre <- as.numeric(window(preds, start = pre.period[1], end = pre.period[2]))


valid_idx <- !is.na(preds_pre) & !is.na(actuals_pre)
actuals_clean <- actuals_pre[valid_idx]
preds_clean <- preds_pre[valid_idx]

# метрики
mae_val <- mae(actuals_clean, preds_clean)
rmse_val <- rmse(actuals_clean, preds_clean)
mape_val <- mape(actuals_clean, preds_clean) * 100 # в проценты
r_squared <- cor(actuals_clean, preds_clean)^2     # псевдо-R2


quality_metrics_df <- data.frame(
  Metric = c("MAE", "RMSE", "MAPE (%)", "Pseudo R-squared"),
  Value = c(mae_val, rmse_val, mape_val, r_squared)
)

print(quality_metrics_df)






# 1. Извлекаем внутреннюю модель BSTS 
bsts_model6 <- impact6$model$bsts.model

# 2. Получаем стандартизированную сводку по модели
bsts_summary6 <- summary(bsts_model6)

# 3. Извлекаем матрицу с коэффициентами и преобразуем в data.frame
coefficients_df6 <- as.data.frame(bsts_summary6$coefficients)

colnames(coefficients_df6) <- c(
  "Безусловное среднее", 
  "Безусловное SD", 
  "Условное среднее", 
  "Условное SD", 
  "Вероятность включения"
)

print("--- Апостериорные оценки коэффициентов модели ---")
print(coefficients_df6)





# 1. Извлекаем остатки на пре-периоде и стандартизируем их
actuals <- impact6$series$response
preds <- impact6$series$point.pred
resids_pre <- na.omit(window(actuals - preds, start = pre.period[1], end = pre.period[2]))

# Стандартизация (как на графике Python: Standardized residual)
std_resids <- resids_pre / sd(resids_pre)


# =====================================================================
# 2. ВЫВОД P-VALUE ТЕСТОВ
# =====================================================================

# Тест Дики-Фуллера (на стационарность)
adf_result <- adf.test(std_resids, alternative = "stationary")
cat(sprintf("p-value тест Дики-Фуллера = %f\n", adf_result$p.value))
adf_result

# Тест Стьюдента (на равенство среднего нулю)
t_result <- t.test(std_resids, mu = 0)
cat(sprintf("p-value тест Стьюдента = %f\n", t_result$p.value))
t_result

# Тест Льюнга-Бокса 
lb_result <- Box.test(std_resids, type = "Ljung-Box")
cat(sprintf("p-value тест Льюнга-Бокса = %f\n", lb_result1$p.value))
lb_result


# =====================================================================
# 3. ПОСТРОЕНИЕ 4-ПАНЕЛЬНОГО ГРАФИКА (plot_diagnostics)
# =====================================================================

# Задаем сетку 2x2 для графиков
# oma = c(2, 2, 2, 2) задает общую белую рамку вокруг всего рисунка
# mar = c(4, 4, 3, 2) настраивает поля для каждого графика: 3 — это верхнее поле под название
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2), oma = c(2, 2, 2, 2))

# --- Стандартизированные остатки во времени ---
plot(std_resids, type = "l", col = "steelblue", lwd = 1.5,
     main = "Standardized residual", ylab = "", xlab = "Date")
abline(h = 0, col = "gray", lty = 1) # Линия нуля

# --- Гистограмма и оценка плотности ---
hist(std_resids, prob = TRUE, breaks = 15, col = "steelblue", border = "white",
     main = "Histogram plus estimated density", xlab = "", ylab = "")
# Оценка плотности (KDE) - оранжевая линия
lines(density(std_resids), col = "darkorange", lwd = 2)
# Теоретическое нормальное распределение N(0,1) - зеленая линия
curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "green3", lwd = 2)
legend("topright", legend = c("KDE", "N(0,1)", "Hist"),
       fill = c(NA, NA, "steelblue"), border = c(NA, NA, "white"),
       col = c("darkorange", "green3", NA), lwd = c(2, 2, NA), bty = "n", cex = 0.8)

# --- Normal Q-Q (Квантиль-Квантиль) ---
qqnorm(std_resids, main = "Normal Q-Q", pch = 16, col = "blue", 
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(std_resids, col = "red", lwd = 2)

# --- Коррелограмма (ACF) ---
acf(std_resids, main = "Correlogram", xlab = "Lag")


# Возвращаем стандартные настройки отображения (1 график на окно)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))












# количество заявок на кредит
ts_data7 <- zoo(
  x = df1[, c("cnt_apps", "key_rate")],
  order.by = df$week_dt
)

ts_data7

# модель
impact7 <- CausalImpact(ts_data7, pre.period, post.period,  model.args = list(niter = 10000, prior.level.sd = 0.1, nseasons = 1))


# результаты модели
print(summary(impact7))
summary(impact7, "report")

plot(impact7, metrics = c('original', 'pointwise'))





# 1. Извлекаем внутреннюю модель BSTS 
bsts_model5 <- impact5$model$bsts.model

# 2. Получаем стандартизированную сводку по модели
bsts_summary5 <- summary(bsts_model5)

# 3. Извлекаем матрицу с коэффициентами и преобразуем в data.frame
coefficients_df5 <- as.data.frame(bsts_summary5$coefficients)

colnames(coefficients_df5) <- c(
  "Безусловное среднее", 
  "Безусловное SD", 
  "Условное среднее", 
  "Условное SD", 
  "Вероятность включения"
)

print("--- Апостериорные оценки коэффициентов модели ---")
print(coefficients_df5)

  
  


# конверсия в невыплату с другими ковариатами
ts_data8 <- zoo(
  # x = df1[, c("cnt_without_payment", "cnt_without_payment_pledged", "key_rate")],
  # x = df1[, c("cnt_without_payment", "cnt_without_payment_pledged")],
  x = df1[, c("conv", "key_rate", "conv_pledged", 'cnt_apps', 'income', 'real_income', 'unemployment', 'active_clients')],
  order.by = df$week_dt
)

ts_data8


# модель
impact8 <- CausalImpact(ts_data8, pre.period, post.period,  model.args = list(niter = 10000, prior.level.sd = 0.1, nseasons = 1))


# результаты модели
print(summary(impact8))
summary(impact8, "report")

plot(impact8, metrics = c('original', 'pointwise'))



# плацебо
fake_pre_period  <- as.Date(c("2022-06-27", "2025-05-05"))
fake_post_period <- as.Date(c("2025-05-12", "2025-08-25"))


ts_placebo_8 <- window(ts_data8, start = fake_pre_period[1], end = fake_post_period[2])


impact_placebo_8 <- CausalImpact(ts_placebo_8, fake_pre_period, fake_post_period, alpha = 0.1, model.args = list(niter = 10000, prior.level.sd = 0.1, nseasons = 1))
print(summary(impact_placebo_8))
plot(impact_placebo_8)

  


# 1. Извлекаем внутреннюю модель BSTS 
bsts_model8 <- impact8$model$bsts.model

# 2. Получаем стандартизированную сводку по модели
bsts_summary8 <- summary(bsts_model8)

# 3. Извлекаем матрицу с коэффициентами и преобразуем в data.frame
coefficients_df8 <- as.data.frame(bsts_summary8$coefficients)

colnames(coefficients_df8) <- c(
  "Безусловное среднее", 
  "Безусловное SD", 
  "Условное среднее", 
  "Условное SD", 
  "Вероятность включения"
)

print("--- Апостериорные оценки коэффициентов модели ---")
print(coefficients_df8)
