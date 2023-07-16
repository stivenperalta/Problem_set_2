###

### buscar la distancia al centroide de chapinero###

### agregar la llave del hogar ###




### Modelo elastic net ###

install.packages("glmnet")
library(glmnet)

x_e_l <- train_data %>%
  select(-price)
y_e_l <- train_data$price

x_e_l <- as.matrix(x_e_l)
y_e_l <- as.vector(y_e_l)

k2 <- 10
elastic_net <- cv.glmnet(x_e_l, y_e_l, alpha = 0.7, nfolds = k)

optimal_lambda_e_n <- elastic_net$lambda.min
e_n_coefficients <- coef(elastic_net, s = optimal_lambda_e_n)

elastic_net$lambda      # All lambda values
elastic_net$cvm         # Cross-validated mean squared error for each lambda
elastic_net$nzero       # Number of nonzero coefficients for each lambda

predicted_values_e_n <- predict(elastic_net, newx = x_e_l, s = "lambda.min")

mae_e_n <- mean(abs(y_e_l - predicted_values_e_n))
rmse_e_n <- sqrt(mean((y_e_l - predicted_values_e_n)^2))
#
### Con K=100

k3 <- 100
lasso_model3 <- cv.glmnet(x2, y2, alpha = 1, nfolds = k)

optimal_lambda3 <- lasso_model3$lambda.min
lasso_coefficients3 <- coef(lasso_model3, s = optimal_lambda3)

lasso_model3$lambda      # All lambda values
lasso_model3$cvm         # Cross-validated mean squared error for each lambda
lasso_model3$nzero       # Number of nonzero coefficients for each lambda

predicted_values3 <- predict(lasso_model3, newx = x2, s = "lambda.min")

mae3 <- mean(abs(y2 - predicted_values3))
rmse3 <- sqrt(mean((y2 - predicted_values3)^2))

