library(readxl)

# Carga de datos
Anuncios <- readxl::read_excel(path= "setdatos.xlsx")

precio_original <- Anuncios$`Precio (€)`

# Si X ~ Lognormal, entonces Y = log(X) ~ Normal
precio_log <- log(precio_original)

# Estadísticos básicos de la variable transformada
n <- length(precio_log)
media_log <- mean(precio_log)
s_log <- sd(precio_log)      # Desviación típica muestral (S)
var_log <- var(precio_log)   # Varianza muestral (S^2)

cat("=== ESTADÍSTICOS DE LA VARIABLE TRANSFORMADA (Log) ===\n")
cat("Tamaño muestral (n):", n, "\n")
cat("Media de log(Precio):", round(media_log, 4), "\n")
cat("Desviación típica de log(Precio):", round(s_log, 4), "\n\n")

# CÁLCULO DE INTERVALOS DE CONFIANZA (95%)

# Intervalo para la media del logaritmo t-student
test_t <- t.test(precio_log, conf.level = 0.95)
ic_media <- test_t$conf.int 

cat("=== INTERVALOS DE CONFIANZA (95%) PARA LOS PARÁMETROS ===\n")
cat("1. Intervalo para la MEDIA (mu) del logaritmo:\n")
cat("   [", round(ic_media[1], 4), ";", round(ic_media[2], 4), "]\n")
cat("   (Estimación puntual media log:", round(media_log, 4), ")\n\n")


# Intervalo para la VARIANZA del logaritmo Chi-Cuadrado
alpha <- 0.05

chi_cuadrado_grande <- qchisq(1 - alpha/2, df = n - 1) 
chi_cuadrado_pequeno <- qchisq(alpha/2, df = n - 1)

numerador <- (n - 1) * s_log^2
lim_inf_var <- numerador / chi_cuadrado_grande
lim_sup_var <- numerador / chi_cuadrado_pequeno

cat("2. Intervalo para la varianza del logaritmo:\n")
cat("   [", round(lim_inf_var, 4), ";", round(lim_sup_var, 4), "]\n")
cat("   (Estimación puntual varianza log:", round(var_log, 4), ")\n")