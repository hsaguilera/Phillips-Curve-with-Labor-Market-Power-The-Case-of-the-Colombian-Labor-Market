library(ggplot2)

# Data
data <- read.csv("C:/Investigation/wage phillips curve with labor market power/2_data/monthly_model_data.csv")
data_for <- read.csv("C:/Investigation/wage phillips curve with labor market power/2_data/monthly_model_data_for.csv")
data_inf <- read.csv("C:/Investigation/wage phillips curve with labor market power/2_data/monthly_model_data_inf.csv")


## Labor Market
data$dln_meansalary <- data$dln_meansalary * 100
data$unem <- data$unem * 100

data$high_mkp_factor <- factor(data$high_mkp, labels = c("Bajo Poder de Mercado Laboral", "Alto Poder de Mercado Laboral"))

ggplot(data, aes(x = unem, y = dln_meansalary, color = high_mkp_factor)) +
  geom_point(shape = 16, alpha = 0.4) +  # Usar diferentes formas para los grupos
  geom_smooth(method = "lm", se = FALSE, size = 1.4) +  # Agregar líneas de tendencia
  scale_color_manual(values = c("dodgerblue4", "#9932CC")) +  # Definir colores para los grupos
  labs(x = "Tasa de desempleo (%)", y = "Inflación salarial (%)") +  # Etiquetas de ejes
  theme_minimal() +  # Tema del gráfico
  guides(color = guide_legend(title = NULL)) +  # Título de la leyenda
  theme(legend.position="bottom")

ggsave("C:/Investigation/wage phillips curve with labor market power/4_results/graph/wage_philips_curve.png", device = "png")


## Formal Labor Market
data_for$dln_meansalary <- data_for$dln_meansalary * 100
data_for$unem <- data_for$unem * 100

data_for$high_mkp_factor <- factor(data_for$high_mkp, labels = c("Bajo Poder de Mercado Laboral", "Alto Poder de Mercado Laboral"))

ggplot(data_for, aes(x = unem, y = dln_meansalary, color = high_mkp_factor)) +
  geom_point(shape = 16, alpha = 0.4) +  # Usar diferentes formas para los grupos
  geom_smooth(method = "lm", se = FALSE, size = 1.4) +  # Agregar líneas de tendencia
  scale_color_manual(values = c("dodgerblue4", "#9932CC")) +  # Definir colores para los grupos
  labs(x = "Tasa de desempleo (%)", y = "Inflación salarial (%)") +  # Etiquetas de ejes
  theme_minimal() +  # Tema del gráfico
  guides(color = guide_legend(title = NULL)) +  # Título de la leyenda
  theme(legend.position="bottom")

ggsave("C:/Investigation/wage phillips curve with labor market power/4_results/graph/formal_wage_philips_curve.png", device = "png")


## Informal Labor Market
data_inf$dln_meansalary <- data_inf$dln_meansalary * 100
data_inf$unem <- data_inf$unem * 100

data_inf$high_mkp_factor <- factor(data_inf$high_mkp, labels = c("Bajo Poder de Mercado Laboral", "Alto Poder de Mercado Laboral"))

ggplot(data_inf, aes(x = unem, y = dln_meansalary, color = high_mkp_factor)) +
  geom_point(shape = 16, alpha = 0.4) +  # Usar diferentes formas para los grupos
  geom_smooth(method = "lm", se = FALSE, size = 1.4) +  # Agregar líneas de tendencia
  scale_color_manual(values = c("dodgerblue4", "#9932CC")) +  # Definir colores para los grupos
  labs(x = "Tasa de desempleo (%)", y = "Inflación salarial (%)") +  # Etiquetas de ejes
  theme_minimal() +  # Tema del gráfico
  guides(color = guide_legend(title = NULL)) +  # Título de la leyenda
  theme(legend.position="bottom")

ggsave("C:/Investigation/wage phillips curve with labor market power/4_results/graph/informal_wage_philips_curve.png", device = "png")
