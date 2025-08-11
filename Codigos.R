
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggpattern)

# CAPÍTULO 2: ORGANIZACIÓN DE DATOS ---------------------------------------

datos = read_excel('DATOS.xlsx')

datos |> rename(IM = 2,
                NV = 3,
                TC = 4) |> 
  mutate(IM = readr::parse_number(IM))-> datos

## Gráfico de barras -------------------------------------------------------

## Ejemplo 1 - Página 20

resumen <- datos %>%
  count(TC) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1))

ggplot(resumen, aes(x = TC, y = porcentaje)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Tarjeta de crédito usada",
       x = "Tarjeta de crédito",
       y = "%",
       caption = "Fuente: Elaboración propia") +
  ylim(0, 30) +
  theme_minimal() -> grafico1

ggsave(filename = "grafico1.jpg", plot = grafico1, width = 5, height  = 3)


## Gráfico circular --------------------------------------------------------

## Ejemplo 1 - Página 20

df <- data.frame(Tarjeta = datos$TC) %>%
  count(Tarjeta) %>%
  mutate(
    porcentaje = round(100 * n / sum(n), 1),
    etiqueta = paste0(Tarjeta, ": ", n, " \n (", porcentaje, "%)")
  )

df <- df %>%
  arrange(desc(Tarjeta)) %>%
  mutate(prop = n / sum(n),
         ypos = cumsum(prop) - 0.5 * prop)

ggplot(df, aes(x = "", y = prop, fill = Tarjeta)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(y = ypos, label = etiqueta), x = 1.3, size = 3) +  # fuera del centro
  labs(title = "Tarjeta de crédito usada",
       caption = "Fuente: Elaboración propia") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    aspect.ratio = 1
  ) -> grafico2

ggsave(filename = "grafico2.jpg", plot = grafico2, width = 5, height  = 3)

## Ejercicio 2 - Página 22

pizza <- data.frame(
  tamano = c("Chica", "Mediana", "Grande", "Familiar"),
  porcentaje = c(10, 20, 30, 40)
)

ggplot(pizza, aes(x = "", y = porcentaje, fill = tamano)) +
  geom_col(color = "black", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(porcentaje, "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_grey(start = 0.9, end = 0.3) + # escala de grises
  labs(title = "Distribución del tamaño de pizza vendida",
       fill = "Tamaño") +
  theme_void()


## Gráfico de varas --------------------------------------------------------

## Ejemplo 2 - Página 23

viajes <- datos |> count(NV) |> mutate(frecuencia_relativa = n/sum(n)*100)
viajes

ggplot(viajes, aes(x = NV, y = frecuencia_relativa)) +
  geom_col(fill = "steelblue", color = "black", width = 0.01) +
  labs(
    title = "Viajes al mes",
    x = "Viajes",
    y = "%",
    caption = "Fuente: Elaboración propia"
  ) +
  ylim(0, 40) +
  theme_minimal() -> grafico3

ggsave(filename = "grafico3.jpg", plot = grafico3, width = 5, height  = 3)


## Histograma --------------------------------------------------------------

## Ejemplo 3 - Página 28

cortes <- c(1.99, 3.90, 5.81, 7.72, 9.63, 11.54, 13.45)

datos$IM_intervalos <- cut(
  datos$IM,
  breaks = cortes,
  right = FALSE,  # para usar [a, b) como en tu notación
  include.lowest = TRUE
)

frecuencia <- datos %>%
  count(IM_intervalos) |> 
  mutate(porc = n/sum(n)*100)

ggplot(frecuencia, aes(x = IM_intervalos, y = porc)) +
  geom_col(fill = "forestgreen", color = "black", width  = 1) +
  labs(
    title = "Ingreso Mensual",
    x = "Intervalos de ingreso (miles de soles)",
    y = "%",
    caption = "Fuente: Elaboración propia"
  ) +
  theme_minimal()-> grafico4

ggsave(filename = "grafico4.jpg", plot = grafico4, width = 5, height  = 3)


## Polígono de frecuencias -------------------------------------------------

## Ejemplo 3 - Página 28

marcas <- c(1.035, 2.945, 4.855, 6.765, 8.675, 10.585, 12.495, 14.405)

frecuencias <- c(0, 11, 20, 10, 1, 2, 1, 0)

df <- data.frame(MarcaClase = marcas,
                 Frecuencia = frecuencias)

ggplot(df, aes(x = MarcaClase, y = Frecuencia)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(shape = 18, color = "blue", size = 3) +  # diamantes
  geom_text(aes(label = Frecuencia), vjust = -0.8, size = 4) +
  scale_x_continuous(
    breaks = marcas,  # Solo mostrar las marcas de clase
    labels = round(marcas, 3)  # Opcional: redondear etiquetas
  ) +
  scale_y_continuous(limits = c(0,22.5)) +
  labs(
    title = "Ingreso mensual",
    x = "Marca de clase del ingreso mensual",
    y = "Frecuencia",
    caption = "Fuente: Elaboración propia",
  ) +
  theme_minimal() -> grafico5

ggsave(filename = "grafico5.jpg", plot = grafico5, width = 5, height  = 3)

# CAPÍTULO 5: VARIABLES ALEATORIAS ----------------------------------------

## Variable aleatoria discreta ---------------------------------------------

## Ejercicio propuesto 4 - Página 136

datos <- data.frame(asientos = c(0, 2, 5, 6, 8),
                    dias = c(5, 8, 4, 1, 2))

ggplot(datos, aes(x = asientos, y = dias)) +
  geom_segment(aes(xend = asientos, y = 0, yend = dias)) +
  geom_point(shape = 18, size = 3, color = "blue") +
  scale_x_continuous(breaks = 0:9, limits = c(0, 9)) +
  scale_y_continuous(breaks = 0:9, limits = c(0, 9)) +
  labs(
    title = "Gráfico del número de asientos libres (Diciembre)",
    x = "Número de asientos",
    y = "Número de días"
  ) +
  theme_minimal()-> grafico6

ggsave(filename = "grafico6.jpg", plot = grafico6, width = 5, height  = 3)

## Ejercicio propuesto 10 - Página 138

x  <- c(0, 2, 4, 6, 8)
fx <- c(0.25, 0.30, 0.25, 0.15, 0.05)

df <- data.frame(x = x,
                 fx = fx)

ggplot(df, aes(x = x, y = fx)) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = fx), color = "black") +
  geom_point(shape = 18, size = 4, color = "blue") +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  scale_y_continuous(limits = c(0, 0.35), breaks = seq(0, 0.35, 0.05)) +
  labs(
    title = "Distribución del número de reclamos",
    x = "Número de reclamos",
    y = "f(x)"
  ) +
  theme_minimal() -> grafico7a

ggsave(filename = "grafico7a.jpg", plot = grafico7a, width = 5.5, height  = 3)

## Variable aleatoria continua ---------------------------------------------

## Práctica dirigida 7 - Página 134

c  <- 1/275
x1 <- seq(50, 55, length.out = 100)
f1 <- c * x1

x2 <- seq(55, 60, length.out = 100)
f2 <- c * (60 - x2)

df <- rbind(
  data.frame(x = 48, f = 0),                  # antes del inicio
  data.frame(x = 50, f = 0),
  data.frame(x = x1, f = f1),
  data.frame(x = x2, f = f2),
  data.frame(x = 60, f = 0),
  data.frame(x = 62, f = 0)                   # después del final
)

ggplot(df, aes(x = x, y = f)) +
  geom_polygon(fill = "white", color = "blue") +
  geom_line(size = 0.5, color = "blue") +
  labs(
    title = "Función de densidad f(y)",
    x = "y",
    y = "f(y)"
  ) +
  scale_x_continuous(breaks = 48:62)+
  theme_minimal() -> grafico7

ggsave(filename = "grafico7.jpg", plot = grafico7, width = 5, height  = 3)


# CAPÍTULO 6: DISTRIBUCIONES DISCRETAS ------------------------------------

## Distribución Binomial ---------------------------------------------------

## Ejemplo 1 - Página 141

n  <- 20
p  <- 0.75
x  <- 0:n
px <- dbinom(x, size = n, prob = p)

df <- data.frame(
  x = x, 
  px = px, 
  highlight = ifelse(x > 18, "X > 18", "Otro")
)

ggplot(df, aes(x, px)) +
  geom_col(aes(fill = highlight), width = 0.20, color = "white") +
  geom_point(aes(fill = highlight), color = "white", shape = 21, size = 2) +
  scale_fill_manual(values = c("Otro" = "gray80", "X > 18" = "black")) +
  labs(
    title = "Distribución Binomial (n=20, p=0.75)",
    x = "Número de clientes que compran",
    y = "Probabilidad",
    fill = ""
  ) +
  theme_minimal() -> grafico8

ggsave(filename = "grafico8.jpg", plot = grafico8, width = 5, height  = 3)

## Ejemplo 2 - Página 141

n  <- 5
p  <- 0.4
x  <- 0:n
px <- dbinom(x, size = n, prob = p)

df <- data.frame(x = x, px = px)

ggplot(df, aes(x = x, y = px)) +
  geom_col(width = 0.015, fill = "gray70", color = "black") +
  geom_point(size = 3) +
  labs(
    title = "Distribución Binomial (n = 5, p = 0.4)",
    x = "Número de máquinas que requieren ajustes",
    y = "Probabilidad"
  ) +
  theme_minimal() -> grafico9

ggsave(filename = "grafico9.jpg", plot = grafico9, width = 5, height  = 3)

## Ejemplo 3 - Página 142

n  <- 20
p  <- 0.20
x  <- 0:n
px <- dbinom(x, size = n, prob = p)

df <- data.frame(
  x = x,
  px = px,
  highlight = ifelse(x > 2, "X > 2", "Otro")
)

ggplot(df, aes(x = x, y = px, fill = highlight)) +
  geom_col(width = 0.20, color = "white") +
  geom_point(aes(fill = highlight), color = "white", shape = 21, size = 2) +
  scale_fill_manual(values = c("Otro" = "gray80", "X > 2" = "black")) +
  scale_x_continuous(breaks = 0:20)+
  labs(
    title = "Distribución Binomial (n=20, p=0.20)",
    x = "Número de alumnos que se dan de baja",
    y = "Probabilidad",
    fill = ""
  ) +
  theme_minimal() -> grafico10

ggsave(filename = "grafico10.jpg", plot = grafico10, width = 5, height  = 3)


## Distribución Hipergeométrica --------------------------------------------

## Ejemplo 4 - Página 147

N  <- 15
A  <- 3
n  <- 2
x  <- 0:n
px <- dhyper(x, m = A, n = N - A, k = n)  # m=éxitos en población

df <- data.frame(x, px)

ggplot(df, aes(x = factor(x), y = px)) +
  geom_col(width = 0.025, fill = "black", color = "white") +
  geom_point(color = "black", size = 2) +
  geom_text(aes(label = round(px, 4)), vjust = -0.4, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Distribución Hipergeométrica  X ~ Hiper(15, 3, 2)",
    x = "Número de contenedores que NO cumplen (x)",
    y = "P(X = x)"
  ) +
  theme_minimal(base_size = 13)-> grafico11

ggsave(filename = "grafico11.jpg", plot = grafico11, width = 5.5, height  = 3)

## Ejemplo 5 - Página 147

N  <- 13      # total de profesionales
A  <- 4       # agrónomos en la población
n  <- 3       # tamaño de la muestra
x  <- 0:n
px <- dhyper(x, m = A, n = N - A, k = n)  # pmf

df <- data.frame(x, px)

ggplot(df, aes(x = factor(x), y = px)) +
  geom_col(width = 0.025, fill = "black", color = "white") +
  geom_point(aes(y = px), size = 2, color = "#1B4F72") +
  geom_text(aes(label = round(px, 4)), vjust = -0.4, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Distribución Hipergeométrica  X ~ Hiper(13, 4, 3)",
    x = "Número de ingenieros agrónomos seleccionados (x)",
    y = "P(X = x)"
  ) +
  theme_minimal(base_size = 13)-> grafico12

ggsave(filename = "grafico12.jpg", plot = grafico12, width = 5.5, height  = 3)

## Distribución Poisson ----------------------------------------------------

## Ejemplo 6 - Página 150

lambda <- 10
x <- 0:25   # valores razonables para graficar
px <- dpois(x, lambda = lambda)

df <- data.frame(x, px)

ggplot(df, aes(x = factor(x), y = px)) +
  geom_col(fill = "black", color = "white", width = 0.15) +
  geom_point(size = 1, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = expression(paste("Distribución Poisson  X ", " ~ Pois(10)")),
    x = "Número de mensajes por minuto (x)",
    y = "P(X = x)"
  ) +
  theme_minimal(base_size = 13)-> grafico13

ggsave(filename = "grafico13.jpg", plot = grafico13, width = 5.5, height  = 3)

## Ejemplo 7 - Página 151

lambda <- 1.2
x <- 0:8
px <- dpois(x, lambda = lambda)

df <- data.frame(x, px)

ggplot(df, aes(x = factor(x), y = px)) +
  geom_col(fill = "black", color = "white", width = 0.05) +
  geom_point(size = 1, color = "black") +
  geom_text(aes(label = round(px, 4)), vjust = -0.4, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = expression(paste("Distribución Poisson  X ", " ~ Pois(1.2)")),
    x = "Número de individuos en 2 km² (x)",
    y = "P(X = x)"
  ) +
  theme_minimal(base_size = 13) -> grafica14

ggsave(filename = "grafico14.jpg", plot = grafica14, width = 5.5, height  = 3)


# CAPÍTULO 7: DISTRIBUCIONES CONTINUAS Y MUESTRALES -----------------------
## Distribución Normal -----------------------------------------------------

## Página 158

mu <- 0
sigma <- 1
x <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 1000)
df <- data.frame(x = x, y = dnorm(x, mean = mu, sd = sigma))

ggplot(df, aes(x, y)) +
  geom_line(size = 1, color = "black") +
  # líneas punteadas en mu-sigma, mu, mu+sigma
  geom_vline(xintercept = c(mu - sigma, mu, mu + sigma),
             linetype = "dashed", color = "blue") +
  # Etiquetas en caja (usar parse=TRUE)
  annotate("label", x = mu - sigma, y = 0.02,
           label = "mu - sigma", parse = TRUE,
           fill = "white", label.size = 0.3, size = 4) +
  annotate("label", x = mu + sigma, y = 0.02,
           label = "mu + sigma", parse = TRUE,
           fill = "white", label.size = 0.3, size = 4) +
  # Texto "campana de Gauss" horizontal
  annotate("text", x = mu - 4*sigma, y = 0.75*max(df$y),
           label = "campana de Gauss", color = "blue",
           hjust = 0, size = 4) +
  # Solo μ en el eje X (sin números)
  scale_x_continuous(breaks = mu, labels = expression(mu)) +
  labs(x = "x", y = expression(f(x))) +
  coord_cartesian(ylim = c(0, max(df$y) + 0.05)) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) -> grafica15

ggsave(filename = "grafico15.jpg", plot = grafica15, width = 5.5, height  = 3)


## Distribución Normal Estándar --------------------------------------------

## Página 159

x  <- seq(-3, 3, length.out = 2000)
df <- data.frame(x = x, y = dnorm(x))

p <- function(a, b) round(100*(pnorm(b) - pnorm(a)), 1)

lab_34   <- paste0(p(-1,0),  "%")   # ~34.1
lab_34_r <- paste0(p(0,1),   "%")   # ~34.1
lab_136L <- paste0(p(-2,-1), "%")   # ~13.6
lab_136R <- paste0(p(1,2),   "%")   # ~13.6
lab_2L   <- paste0(round(100*(pnorm(-2)),1), "%")  # ~2.3
lab_2R   <- paste0(round(100*(1-pnorm(2)),1), "%") # ~2.3

ggplot(df, aes(x, y)) +
  geom_line(color = "steelblue", linewidth = 1) +
  
  geom_area(data = subset(df, x >= -1 & x <= 0),
            fill = "steelblue", alpha = 0.25) +
  geom_area(data = subset(df, x >= 0 & x <= 1),
            fill = "steelblue", alpha = 0.25) +
  geom_area(data = subset(df, x >= -2 & x < -1),
            fill = "steelblue", alpha = 0.15) +
  geom_area(data = subset(df, x > 1 & x <= 2),
            fill = "steelblue", alpha = 0.15) +
  geom_area(data = subset(df, x <= -2),
            fill = "steelblue", alpha = 0.08) +
  geom_area(data = subset(df, x >=  2),
            fill = "steelblue", alpha = 0.08) +
  
  geom_vline(xintercept = c(-2, -1, 0, 1, 2),
             linetype = c("solid","solid","dashed","solid","solid"),
             color = c("gray40","gray40","gray40","gray40","gray40")) +
  
  annotate("text", x = -0.5, y = 0.14, label = lab_34, size = 4.2) +
  annotate("text", x =  0.5, y = 0.14, label = lab_34_r, size = 4.2) +
  annotate("text", x = -1.5, y = 0.08, label = lab_136L, size = 4.2) +
  annotate("text", x =  1.5, y = 0.08, label = lab_136R, size = 4.2) +
  annotate("text", x = -2.45, y = 0.03, label = lab_2L, size = 3.8) +
  annotate("text", x =  2.45, y = 0.03, label = lab_2R, size = 3.8) +
  
  labs(
    title = "Distribución Normal Estándar",
    x = "z", y = "f(z)"
  ) +
  coord_cartesian(xlim = c(-3,3), ylim = c(0, 0.42)) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())  -> grafica16

ggsave(filename = "grafico16.jpg", plot = grafica16, width = 5.5, height  = 3)

## Ejemplo 2 - Página 160

mu    <- 167.85
sigma <- 10.37
xcut  <- 179

x  <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 2000)
df <- data.frame(x = x, y = dnorm(x, mean = mu, sd = sigma))

shade <- subset(df, x <= xcut)

p_left <- 0.8599
z      <- (xcut - mu) / sigma

ggplot(df, aes(x, y)) +
  geom_line(color = "black", linewidth = 1) +
  geom_area(data = shade, aes(y = y),
            fill = "steelblue", alpha = 0.35) +
  geom_vline(xintercept = mu, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = xcut, color = "firebrick") +
  annotate("text", x = mu, y = max(df$y)*0.98,
           label = expression(mu==167.85), vjust = -0.2, size = 4) +
  annotate("text", x = xcut, y = max(df$y)*0.6,
           label = sprintf("x = 179\nz = %.2f", z), color = "firebrick",
           hjust = -0.1, size = 4) +
  annotate("label", x = mu - 0.6*sigma, y = max(df$y)*0.2,
           label = sprintf("P(X < 179) = %.4f", p_left),
           fill = "white") +
  labs(title = "Normal N(167.85, 10.37^2) y área P(X < 179)",
       x = "Estatura (cm)", y = "f(x)") +
  coord_cartesian(xlim = c(mu - 4*sigma, mu + 4*sigma),
                  ylim = c(0, max(df$y)*1.05)) +
  theme_minimal(base_size = 13) -> grafica17

ggsave(filename = "grafico17.jpg", plot = grafica17, width = 5.5, height  = 3)

## Ejercicio 3 - Página 160

mu <- 1.5
sigma <- 0.1565
x_max <- 2
z_val <- (x_max - mu) / sigma  # valor Z

x <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 1000)
y <- dnorm(x, mean = mu, sd = sigma)
df <- data.frame(x, y)

df_fill <- subset(df, x <= x_max)

ggplot(df, aes(x, y)) +
  geom_line(size = 1, color = "blue") +
  geom_area(data = df_fill, aes(x, y), fill = "skyblue", alpha = 0.5) +
  geom_vline(xintercept = mu, linetype = "dashed", color = "black") +
  geom_vline(xintercept = x_max, linetype = "dashed", color = "red") +
  annotate("text", x = mu, y = max(y) * 0.9, label = expression(mu), vjust = -1) +
  annotate("text", x = x_max, y = max(y) * 0.9, label = "2 min", vjust = -1, color = "red") +
  labs(title = expression(paste("Distribución de ", bar(X))),
       subtitle = bquote(P(bar(X) <= 2) == 0.9993 ~~~ "(" * Z == .(round(z_val, 2)) * ")"),
       x = "Tiempo medio (min)", y = "Densidad") +
  theme_minimal(base_size = 14) -> grafica18

ggsave(filename = "grafico18.jpg", plot = grafica18, width = 5.5, height  = 3)


# CAPÍTULO 8: INFERENCIA ESTADÍSTICA --------------------------------------
## Intervalos de confianza -------------------------------------------------
## Simulación de intervalos de confianza para la media - Página 179
## Página 179
set.seed(22)
mu <- 50
sigma <- 10
n <- 20
nsim <- 100
alpha <- 0.05

lower <- upper <- numeric(nsim)
contains_mu <- logical(nsim)

for (i in 1:nsim) {
  x <- rnorm(n, mean = mu, sd = sigma)
  xbar <- mean(x)
  s <- sd(x)
  se <- s / sqrt(n)
  tcrit <- qt(1 - alpha/2, df = n-1)
  lower[i] <- xbar - tcrit * se
  upper[i] <- xbar + tcrit * se
  contains_mu[i] <- (lower[i] <= mu & upper[i] >= mu)
}

covg <- mean(contains_mu) * 100

df <- data.frame(
  sim = 1:nsim,
  lower = lower,
  upper = upper,
  contains = factor(contains_mu, levels = c(TRUE, FALSE),
                    labels = c("Contiene μ", "No contiene μ"))
)

ggplot(df, aes(x = sim)) +
  # Intervalos
  geom_linerange(aes(ymin = lower, ymax = upper, linetype = contains), linewidth = 0.18) +
  # Puntos en los extremos
  geom_point(aes(y = lower, shape = contains), size = 1.0) +
  geom_point(aes(y = upper, shape = contains), size = 1.0) +
  # Línea de la media verdadera
  geom_hline(yintercept = mu, linetype = "22", linewidth = 0.9) +
  scale_linetype_manual(values = c("solid", "longdash")) +     # B/N: sólido vs. discontinua
  scale_shape_manual(values = c(16, 4)) +                      # sólido vs. X
  coord_cartesian(ylim = range(c(lower, upper))) +
  labs(
    title = "Simulación de 100 IC al 95% para la media (t de Student)",
    subtitle = paste0("n = ", n, ", μ = ", mu,", σ = ", sigma, ", nivel = 95%  |  Cobertura observada ≈ ", round(covg, 1), "%"),
    x = "Número de simulación",
    y = "Media"
  ) +
  guides(linetype = guide_legend(title = "Intervalo"),
         shape    = guide_legend(title = "Intervalo")) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )-> grafica19

ggsave(filename = "grafico19.jpg", plot = grafica19, width = 5.5, height  = 3)

## Simulación de intervalos de confianza para la vaiancia - Página 179

## Página 179

set.seed(15)

sigma <- 10          # Desviación estándar verdadera
var_true <- sigma^2  # Varianza verdadera
n <- 20
nsim <- 100
alpha <- 0.05

lower <- numeric(nsim)
upper <- numeric(nsim)
contains_var <- logical(nsim)

for (i in 1:nsim) {
  x <- rnorm(n, mean = 50, sd = sigma)
  s2 <- var(x)
  
  chi_lower <- qchisq(1 - alpha/2, df = n - 1)
  chi_upper <- qchisq(alpha/2, df = n - 1)
  
  lower[i] <- (n - 1) * s2 / chi_lower
  upper[i] <- (n - 1) * s2 / chi_upper
  
  contains_var[i] <- (lower[i] <= var_true & upper[i] >= var_true)
}

covg <- mean(contains_var) * 100

df <- data.frame(
  sim = 1:nsim,
  lower = lower,
  upper = upper,
  contains = factor(contains_var, levels = c(TRUE, FALSE),
                    labels = c("Contiene sigma²", "No contiene sigma²"))
)

ggplot(df, aes(x = sim)) +
  geom_linerange(aes(ymin = lower, ymax = upper, linetype = contains), linewidth = 0.18) +
  geom_point(aes(y = lower, shape = contains), size = 1) +
  geom_point(aes(y = upper, shape = contains), size = 1) +
  geom_hline(yintercept = var_true, linetype = "22", linewidth = 0.9) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  scale_shape_manual(values = c(16, 4)) +
  coord_cartesian(ylim = range(c(lower, upper))) +
  labs(
    title = "Simulación de 100 IC al 95% para la varianza (χ²)",
    subtitle = paste0("n = ", n, ", σ² = ", sigma**2, ", nivel = 95%  |  Cobertura observada ≈ ", round(covg, 1), "%"),
    x = "Simulación",
    y = "Varianza"
  ) +
  guides(linetype = guide_legend(title = "Intervalo"),
         shape    = guide_legend(title = "Intervalo")) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) -> grafica20

ggsave(filename = "grafico20.jpg", plot = grafica20, width = 5.5, height  = 3)

## Simulación de intervalos de confianza para la proporción - Página 180

set.seed(3)

p_true <- 0.30     # proporción verdadera
n <- 60            # tamaño muestral por simulación
nsim <- 100
alpha <- 0.05
z <- qnorm(1 - alpha/2)

lower <- upper <- numeric(nsim)
contains_p <- logical(nsim)

for (i in 1:nsim) {
  x <- rbinom(1, size = n, prob = p_true)
  phat <- x / n
  
  denom <- 1 + z^2 / n
  center <- (phat + z^2/(2*n)) / denom
  half   <- z * sqrt( (phat*(1 - phat)/n) + z^2/(4*n^2) ) / denom
  lower[i] <- max(0, center - half)
  upper[i] <- min(1, center + half)
  
  contains_p[i] <- (lower[i] <= p_true & upper[i] >= p_true)
}

covg <- mean(contains_p) * 100

df <- data.frame(
  sim = 1:nsim,
  lower = lower,
  upper = upper,
  contains = factor(contains_p, levels = c(TRUE, FALSE),
                    labels = c("Contiene pi", "No contiene pi"))
)

ggplot(df, aes(x = sim)) +
  geom_linerange(aes(ymin = lower, ymax = upper, linetype = contains), linewidth = 0.18) +
  geom_point(aes(y = lower, shape = contains), size = 1) +
  geom_point(aes(y = upper, shape = contains), size = 1) +
  geom_hline(yintercept = p_true, linetype = "22", linewidth = 0.9) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  scale_shape_manual(values = c(16, 4)) +
  coord_cartesian(ylim = c(0, 0.7)) +
  labs(
    title = "100 IC al 95% para la proporción",
    subtitle = paste0("n = ", n, ", nivel = 95%  |  Cobertura observada ≈ ", round(covg, 1), "%"),
    x = "Número simulación",
    y = "Proporción"
  ) +
  guides(linetype = guide_legend(title = "Intervalo"),
         shape    = guide_legend(title = "Intervalo")) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) -> grafica21

ggsave(filename = "grafico21.jpg", plot = grafica21, width = 5.5, height  = 3)


## Práctica dirigida 9 -----------------------------------------------------

datos_enaho = readr::read_csv('Enaho01-2025-200.csv', locale = readr::locale(encoding = "latin1"))
library(dplyr)
set.seed(2)
datos_enaho |> 
  select(DOMINIO,P207,P208A,P208B,P209) |> 
  rename(SEXO = 2,
         ESTCIVIL = 5) |> 
  mutate(P208A = ifelse(is.na(P208A),0,P208A),
         P208B = ifelse(is.na(P208B),0,P208B),
         SOLTERO = ifelse(ESTCIVIL == 6, "SÍ","NO"),
         EDAD  = P208A + P208B/12,
         ZONA  = case_when(DOMINIO %in% 1:3 ~ "COSTA",
                           DOMINIO %in% 4:6 ~ "SIERRA",
                           DOMINIO == 7 ~ "SELVA",
                           DOMINIO == 8 ~ "LIMA Y CALLAO")) |> 
  slice_sample(n = 200) -> datos_enaho_ok

datos_enaho_ok |> 
  filter(SOLTERO %in% c("SÍ","NO")) |> 
  group_by(ZONA) |> 
  summarise(n = n(),
            EdadMedia = mean(EDAD),
            EdadMediana = median(EDAD),
            EdadDesv = sd(EDAD))

datos_enaho_ok |> 
  filter(SOLTERO %in% c("SÍ","NO")) |> 
  summarise(n = n(),
            EdadMedia = mean(EDAD),
            EdadMediana = median(EDAD),
            EdadDesv = sd(EDAD))

df_sum <- datos_enaho_ok %>%
  count(ZONA, SOLTERO) |> 
  filter(SOLTERO %in% c("SÍ","NO"))

ggplot(df_sum, aes(x = ZONA, y = n, fill = SOLTERO)) +
  geom_col(position = "dodge", colour = "black") +  # borde negro para distinguir
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3) +
  labs(title = "Distribución de personas solteras por zona",
       x = "Dominio",
       y = "Frecuencia",
       fill = "¿Es soltero(a)?") +
  scale_fill_grey(start = 0.8, end = 0.2) +         # escala de grises
  theme_minimal()-> grafica22

ggsave(filename = "grafico22.jpg", plot = grafica22, width = 5.5, height  = 3)


## Prueba de hipótesis -----------------------------------------------------

## Ejemplo 8a - Página 196

alpha <- 0.05
df <- 9
t_crit <- qt(alpha, df) # valor crítico para cola izquierda

x <- seq(-4, 4, length.out = 500)
y <- dt(x, df)

df_plot <- data.frame(x = x, y = y)

rechazo <- subset(df_plot, x <= t_crit)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 1) +
  geom_area(data = rechazo, aes(x = x, y = y), fill = "grey80", alpha = 0.8) +
  geom_vline(xintercept = t_crit, linetype = "dotdash") +
  geom_hline(yintercept = 0)+
  annotate("label", x = t_crit, y = -0.1, 
           label = paste0("t = ", round(t_crit, 3)), 
           vjust = -0.5, 
           fill = "white", 
           label.size = 0.3) +
  annotate("text", x = -3, y = 0.15, label = "Zona de\nrechazo de H0", size = 4) +
  annotate("text", x = 0, y = 0.15, label = "Zona de no\nrechazo de H0", size = 4) +
  annotate("text", x = -2.25, y = 0.02, label = "0.05", size = 3) +
  annotate("text", x = 0, y = 0.05, label = "0.95", size = 3) +
  scale_y_continuous(breaks = seq(0,0.4,0.1)) +
  theme_minimal() +
  labs(title = "Distribución t de Student",
       subtitle = paste0("Prueba unilateral izquierda, α = ", alpha, ", gl = ", df),
       x = "t", y = "Densidad")-> grafica23

ggsave(filename = "grafico23.jpg", plot = grafica23, width = 5.5, height  = 3)

## Ejemplo 8b - Página 197

alpha  <- 0.05
df     <- 9
alpha2 <- alpha/2

chi_l <- qchisq(alpha2, df)
chi_r <- qchisq(1 - alpha2, df)

x <- seq(0, qchisq(0.999, df), length.out = 800)
y <- dchisq(x, df)
df_plot <- data.frame(x, y)

rechazo_izq <- subset(df_plot, x <= chi_l)
rechazo_der <- subset(df_plot, x >= chi_r)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 0.9) +
  geom_area(data = rechazo_izq, aes(x, y), fill = "grey75", alpha = 0.8) +
  geom_area(data = rechazo_der, aes(x, y), fill = "grey75", alpha = 0.8) +
  geom_vline(xintercept = c(chi_l, chi_r), linetype = "dashed") +
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks = seq(0,0.1,0.05)) +
  
  annotate("label",
           x = chi_l, y = -0.05,
           label = paste0("chi[", alpha2, "]^2==", round(chi_l, 3)),
           vjust = -0.5, fill = "white", label.size = 0.3, parse = TRUE) +
  annotate("label",
           x = chi_r, y = -0.05,
           label = paste0("chi[", 1 - alpha2, "]^2==", round(chi_r, 3)),
           vjust = -0.5, fill = "white", label.size = 0.3, parse = TRUE) +

  annotate("text", x = 7.5, y = max(y)*0.55,
           label = "Zona de no\nrechazo de H0", size = 4) +
  annotate("text", x = chi_l*0.6, y = max(y)*0.15, label = "0.025", size = 3) +
  annotate("text", x = 8, y = max(y)*0.15, label = "0.95", size = 3) +
  annotate("text", x = chi_r*1.1, y = max(y)*0.15, label = "0.025", size = 3) +
  
  labs(title = "Distribución Chi-cuadrado",
       subtitle = paste0("Prueba bilateral, \u03B1 = ", alpha, ", gl = ", df),
       x = expression(chi^2), y = "Densidad") +
  theme_minimal()  -> grafica24

ggsave(filename = "grafico24.jpg", plot = grafica24, width = 5.5, height  = 3)

## Ejemplo 8c - Página 198

alpha <- 0.03
z_crit <- qnorm(1 - alpha)   # z_{0.97}
x <- seq(-4, 4, length.out = 800)
y <- dnorm(x)
df_plot <- data.frame(x, y)

rechazo <- subset(df_plot, x >= z_crit)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 1) +
  geom_area(data = rechazo, aes(x, y), fill = "grey70", alpha = 0.9) +
  geom_vline(xintercept = z_crit, linetype = "dashed") +
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks = seq(0,0.4,0.2)) +
  
  annotate("text", x = 0, y = max(y)*0.5,
           label = "Zona de no\nrechazo de H0", size = 4) +  
  annotate("text", x = 3, y = max(y)*0.5,
           label = "Zona de \nrechazo de H0", size = 4) +  
  annotate("text", x = 0, y = max(y)*0.15,
           label = "0.97", size = 3) +  
  annotate("text", x = 2.5, y = max(y)*0.15,
           label = "0.03", size = 3) +  
  
  annotate("label",
           x = z_crit, y = -0.175,
           label = paste0("Z[0.97] == ", 1.89),
           vjust = -0.5, parse = TRUE, fill = "white", label.size = 0.3) +
  
  coord_cartesian(ylim = c(-0.175, max(y)*1.1)) +
  labs(title = "Distribución Normal Estándar",
       subtitle = expression(paste("Prueba unilateral derecha,  ", alpha, " = 0.03")),
       x = "z", y = "Densidad") +
  theme_minimal()  -> grafica25

ggsave(filename = "grafico25.jpg", plot = grafica25, width = 5.5, height  = 3)


## Ejemplo 9a - Página 199

alpha <- 0.025
df <- 27
t_crit <- qt(1-alpha, df) # valor crítico para cola derecha

x <- seq(-4, 4, length.out = 500)
y <- dt(x, df)

df_plot <- data.frame(x = x, y = y)

rechazo <- subset(df_plot, x >= t_crit)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 1) +
  geom_area(data = rechazo, aes(x = x, y = y), fill = "grey80", alpha = 0.8) +
  geom_vline(xintercept = t_crit, linetype = "dotdash") +
  geom_hline(yintercept = 0)+
  annotate("label",
           x = t_crit, y = -0.1,
           label = paste0("t[0.975] == ", round(t_crit,3)),
           vjust = -0.5, parse = TRUE, fill = "white", label.size = 0.3) +
  annotate("text", x = 3.25, y = 0.15, label = "Zona de\nrechazo de H0", size = 4) +
  annotate("text", x = 0, y = 0.15, label = "Zona de no\nrechazo de H0", size = 4) +
  annotate("text", x = 2.5, y = 0.06, label = "0.03", size = 3) +
  annotate("text", x = 0, y = 0.06, label = "0.97", size = 3) +
  scale_y_continuous(breaks = seq(0,0.4,0.1)) +
  theme_minimal() +
  labs(title = "Distribución t de Student",
       subtitle = paste0("Prueba unilateral derecha, α = ", alpha, ", gl = ", df),
       x = "t", y = "Densidad") -> grafica26

ggsave(filename = "grafico26.jpg", plot = grafica26, width = 4.5, height  = 3)

## Ejercicio 9b - Página 199

alpha <- 0.05
z_crit <- qnorm(alpha)   # z_{0.05}
x <- seq(-4, 4, length.out = 800)
y <- dnorm(x)
df_plot <- data.frame(x, y)

rechazo <- subset(df_plot, x <= z_crit)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 1) +
  geom_area(data = rechazo, aes(x, y), fill = "grey70", alpha = 0.9) +
  geom_vline(xintercept = z_crit, linetype = "dashed") +
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks = seq(0,0.4,0.2)) +
  
  annotate("text", x = 0, y = max(y)*0.5,
           label = "Zona de no\nrechazo de H0", size = 4) +  
  annotate("text", x = -3, y = max(y)*0.5,
           label = "Zona de \nrechazo de H0", size = 4) +  
  annotate("text", x = 0, y = max(y)*0.15,
           label = "0.95", size = 3) +  
  annotate("text", x = -2.5, y = max(y)*0.15,
           label = "0.05", size = 3) +  

  annotate("label",
           x = z_crit, y = -0.125,
           label = paste0("Z[0.05] == ", round(z_crit,3)),
           vjust = -0.5, parse = TRUE, fill = "white", label.size = 0.3) +
  
  coord_cartesian(ylim = c(-0.15, max(y)*1.1)) +
  labs(title = "Distribución Normal Estándar",
       subtitle = expression(paste("Prueba unilateral izquierda,  ", alpha, " = 0.05")),
       x = "z", y = "Densidad") +
  theme_minimal() -> grafica27

ggsave(filename = "grafico27.jpg", plot = grafica27, width = 4.5, height  = 3)


## Ejemplo 10 - Página 200

alpha <- 0.05
df <- 15
t_crit <- qt(alpha, df) # valor crítico para cola izquierda

x <- seq(-4, 4, length.out = 500)
y <- dt(x, df)

df_plot <- data.frame(x = x, y = y)

rechazo <- subset(df_plot, x <= t_crit)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 1) +
  geom_area(data = rechazo, aes(x = x, y = y), fill = "grey80", alpha = 0.8) +
  geom_vline(xintercept = t_crit, linetype = "dotdash") +
  geom_hline(yintercept = 0)+
  annotate("label",
           x = t_crit, y = -0.1,
           label = paste0("t[0.05] == ", round(t_crit,3)),
           vjust = -0.5, parse = TRUE, fill = "white", label.size = 0.3) +
  annotate("text", x = -3.1, y = 0.15, label = "Zona de\nrechazo de H0", size = 4) +
  annotate("text", x = 0, y = 0.15, label = "Zona de no\nrechazo de H0", size = 4) +
  annotate("text", x = -2.5, y = 0.06, label = "0.05", size = 3) +
  annotate("text", x = 0, y = 0.06, label = "0.95", size = 3) +
  scale_y_continuous(breaks = seq(0,0.4,0.1)) +
  theme_minimal() +
  labs(title = "Distribución t de Student",
       subtitle = paste0("Prueba unilateral izquierda, α = ", alpha, ", gl = ", df),
       x = "t", y = "Densidad") -> grafica28

ggsave(filename = "grafico28.jpg", plot = grafica28, width = 4.5, height  = 3)


alpha  <- 0.05
df     <- 15

chi_crit <- qchisq(alpha, df)

# Curva χ²
x <- seq(0, qchisq(0.999, df), length.out = 800)
y <- dchisq(x, df)
df_plot <- data.frame(x, y)

rechazo <- subset(df_plot, x <= chi_crit)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 0.9) +
  geom_area(data = rechazo, aes(x, y), fill = "grey75", alpha = 0.8) +
  geom_vline(xintercept = chi_crit, linetype = "dashed") +
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks = seq(0,0.1,0.05)) +

  annotate("label",
           x = chi_crit, y = -0.035,
           label = paste0("chi[", alpha, "]^2==", round(chi_crit, 3)),
           vjust = -0.5, fill = "white", label.size = 0.3, parse = TRUE) +
 
  annotate("text", x = 3, y = max(y)*0.5,
           label = "Zona de \nrechazo de H0", size = 3) +
  annotate("text", x = 14, y = max(y)*0.5,
           label = "Zona de no\nrechazo de H0", size = 3) +
  annotate("text", x = chi_crit*0.3, y = max(y)*0.15, label = "0.05", size = 3) +
  annotate("text", x = 14, y = max(y)*0.15, label = "0.95", size = 3) +
  labs(title = "Distribución Chi-cuadrado",
       subtitle = paste0("Prueba unilateral izquierda, \u03B1 = ", alpha, ", gl = ", df),
       x = expression(chi^2), y = "Densidad") +
  theme_minimal()  -> grafica29

ggsave(filename = "grafico29.jpg", plot = grafica29, width = 4.5, height  = 3)

## Ejemplo 11 - Página 201

alpha <- 0.05
z_crit <- qnorm(1 - alpha)   # z_{0.95}
x <- seq(-4, 4, length.out = 800)
y <- dnorm(x)
df_plot <- data.frame(x, y)

# zona de rechazo (cola derecha)
rechazo <- subset(df_plot, x >= z_crit)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 1) +
  geom_area(data = rechazo, aes(x, y), fill = "grey70", alpha = 0.9) +
  geom_vline(xintercept = z_crit, linetype = "dashed") +
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks = seq(0,0.4,0.2)) +
  
  annotate("text", x = 0, y = max(y)*0.5,
           label = "Zona de no\nrechazo de H0", size = 4) +  
  annotate("text", x = 3, y = max(y)*0.5,
           label = "Zona de \nrechazo de H0", size = 4) +  
  annotate("text", x = 0, y = max(y)*0.15,
           label = "0.95", size = 3) +  
  annotate("text", x = 2.5, y = max(y)*0.15,
           label = "0.05", size = 3) +  
  
  annotate("label",
           x = z_crit, y = -0.175,
           label = paste0("Z[0.95] == ", round(z_crit,2)),
           vjust = -0.5, parse = TRUE, fill = "white", label.size = 0.3) +
  
  coord_cartesian(ylim = c(-0.175, max(y)*1.1)) +
  labs(title = "Distribución Normal Estándar",
       subtitle = expression(paste("Prueba unilateral derecha,  ", alpha, " = 0.05")),
       x = "z", y = "Densidad") +
  theme_minimal()  -> grafica30

ggsave(filename = "grafico30.jpg", plot = grafica30, width = 4.5, height  = 3)


## Ejemplo 13a - Página 205

alpha <- 0.10
gl1 <- 15
gl2 <- 11

F_crit_inf <- qf(alpha/2, gl1, gl2) |> round(3)
F_crit_sup <- qf(1-alpha/2, gl1, gl2) |> round(2)

x <- seq(0, 4, length.out = 500)
y <- df(x, gl1, gl2)

df_plot <- data.frame(x = x, y = y)

rechazo_izq <- subset(df_plot, x <= F_crit_inf)
rechazo_der <- subset(df_plot, x >= F_crit_sup)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 1) +
  geom_area(data = rechazo_izq, fill = "grey80", alpha = 0.8) +
  geom_area(data = rechazo_der, fill = "grey80", alpha = 0.8) +
  geom_vline(xintercept = c(F_crit_inf, F_crit_sup), linetype = "dotted") +
  geom_hline(yintercept = 0)+
  annotate("label", x = F_crit_inf, y = -0.2,
           label = paste0("F[1] == ", round(F_crit_inf, 3)),
           parse = TRUE, vjust = -0.5, fill = "white", label.size = 0.3) +
  annotate("label", x = F_crit_sup, y = -0.2,
           label = paste0("F[2] == ", round(F_crit_sup, 3)),
           parse = TRUE, vjust = -0.5, fill = "white", label.size = 0.3) +
  annotate("text", x = 1, y = max(y) * 0.35,
           label = "Zona de \n no rechazo H0", size = 4) +
  annotate("text", x = 1, y = 0.05, label = "0.90", size = 3) +
  annotate("text", x = F_crit_inf * 0.75, y = 0.05, label = "0.05", size = 3) +
  annotate("text", x = F_crit_sup * 1.05, y = 0.05, label = "0.05", size = 3) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme_minimal() +
  labs(title = "Distribución F",
       subtitle = paste0("Prueba bilateral, α = ", alpha, ", gl1 = ", gl1, ", gl2 = ", gl2),
       x = "F", y = "Densidad")  -> grafica31

ggsave(filename = "grafico31.jpg", plot = grafica31, width = 5.5, height  = 3)



## Ejemplo 13b - Página 206

alpha <- 0.90
df <- 26
t_crit <- qt(alpha, df) # valor crítico para cola derecha

x <- seq(-4, 4, length.out = 500)
y <- dt(x, df)

df_plot <- data.frame(x = x, y = y)

rechazo <- subset(df_plot, x >= t_crit)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 1) +
  geom_area(data = rechazo, aes(x = x, y = y), fill = "grey80", alpha = 0.8) +
  geom_vline(xintercept = t_crit, linetype = "dotdash") +
  geom_hline(yintercept = 0)+
  annotate("label",
           x = t_crit, y = -0.15,
           label = paste0("t[0.9] == ", round(t_crit,3)),
           vjust = -0.5, parse = TRUE, fill = "white", label.size = 0.3) +
  annotate("text", x = 3, y = 0.15, label = "Zona de\nrechazo de H0", size = 4) +
  annotate("text", x = 0, y = 0.15, label = "Zona de no\nrechazo de H0", size = 4) +
  annotate("text", x = 1.7, y = 0.05, label = "0.1", size = 3) +
  annotate("text", x = 0, y = 0.05, label = "0.9", size = 3) +
  scale_y_continuous(breaks = seq(0,0.4,0.1)) +
  theme_minimal() +
  labs(title = "Distribución t de Student",
       subtitle = paste0("Prueba unilateral derecha, α = ", 1-alpha, ", gl = ", df),
       x = "t", y = "Densidad") -> grafica32

ggsave(filename = "grafico32.jpg", plot = grafica32, width = 5.5, height  = 3)


## Ejemplo 13c - Página 207

alpha <- 0.04
z_crit <- qnorm(alpha)   # Z_{0.04}
x <- seq(-4, 4, length.out = 800)
y <- dnorm(x)
df_plot <- data.frame(x, y)

rechazo <- subset(df_plot, x <= z_crit)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 1) +
  geom_area(data = rechazo, aes(x, y), fill = "grey70", alpha = 0.9) +
  geom_vline(xintercept = z_crit, linetype = "dashed") +
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks = seq(0,0.4,0.2)) +
  
  annotate("text", x = 0, y = max(y)*0.5,
           label = "Zona de no\nrechazo de H0", size = 4) +  
  annotate("text", x = -3, y = max(y)*0.5,
           label = "Zona de \nrechazo de H0", size = 4) +  
  annotate("text", x = 0, y = max(y)*0.15,
           label = "0.96", size = 3) +  
  annotate("text", x = -3, y = max(y)*0.15,
           label = "0.04", size = 3) +  
  
  annotate("label",
           x = z_crit, y = -0.15,
           label = paste0("Z[0.04] == ", round(z_crit,2)),
           vjust = -0.5, parse = TRUE, fill = "white", label.size = 0.3) +
  
  coord_cartesian(ylim = c(-0.15, max(y)*1.1)) +
  labs(title = "Distribución Normal Estándar",
       subtitle = expression(paste("Prueba unilateral izquierda,  ", alpha, " = 0.04")),
       x = "z", y = "Densidad") +
  theme_minimal() -> grafica33

ggsave(filename = "grafico33.jpg", plot = grafica33, width = 4.5, height  = 3)

## Ejemplo 14a - Página 208

alpha <- 0.025
df <- 16
t_crit <- qt(alpha, df) # valor crítico para cola izquierda

x <- seq(-4, 4, length.out = 500)
y <- dt(x, df)

df_plot <- data.frame(x = x, y = y)

rechazo <- subset(df_plot, x <= t_crit)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 1) +
  geom_area(data = rechazo, aes(x = x, y = y), fill = "grey80", alpha = 0.8) +
  geom_vline(xintercept = t_crit, linetype = "dotdash") +
  geom_hline(yintercept = 0)+
  annotate("label",
           x = t_crit, y = -0.1,
           label = paste0("t[0.025] == ", round(t_crit,3)),
           vjust = -0.5, parse = TRUE, fill = "white", label.size = 0.3) +
  annotate("text", x = -3.3, y = 0.15, label = "Zona de\nrechazo de H0", size = 3.5) +
  annotate("text", x = 0, y = 0.15, label = "Zona de no\nrechazo de H0", size = 3.5) +
  annotate("text", x = -3.3, y = 0.06, label = "0.05", size = 3) +
  annotate("text", x = 0, y = 0.06, label = "0.95", size = 3) +
  scale_y_continuous(breaks = seq(0,0.4,0.1)) +
  theme_minimal() +
  labs(title = "Distribución t de Student",
       subtitle = paste0("Prueba unilateral izquierda, α = ", alpha, ", gl = ", df),
       x = "t", y = "Densidad") -> grafica34

ggsave(filename = "grafico34.jpg", plot = grafica34, width = 4.5, height  = 3)

## Ejemplo 14b - Página 209

alpha <- 0.10
gl1 <- 16
gl2 <- 21

F_crit_inf <- qf(alpha/2, gl1, gl2) |> round(3)
F_crit_sup <- qf(1-alpha/2, gl1, gl2) |> round(2)

x <- seq(0, 3, length.out = 500)
y <- df(x, gl1, gl2)

df_plot <- data.frame(x = x, y = y)

rechazo_izq <- subset(df_plot, x <= F_crit_inf)
rechazo_der <- subset(df_plot, x >= F_crit_sup)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 1) +
  geom_area(data = rechazo_izq, fill = "grey80", alpha = 0.8) +
  geom_area(data = rechazo_der, fill = "grey80", alpha = 0.8) +
  geom_vline(xintercept = c(F_crit_inf, F_crit_sup), linetype = "dotted") +
  geom_hline(yintercept = 0)+
  annotate("label", x = F_crit_inf, y = -0.2,
           label = paste0("F[1] == ", round(F_crit_inf, 2)),
           parse = TRUE, vjust = -0.5, fill = "white", label.size = 0.3) +
  annotate("label", x = F_crit_sup, y = -0.2,
           label = paste0("F[2] == ", round(F_crit_sup, 2)),
           parse = TRUE, vjust = -0.5, fill = "white", label.size = 0.3) +
  annotate("text", x = 1, y = max(y) * 0.45,
           label = "Zona de \n no rechazo H0", size = 4) +
  annotate("text", x = 1, y = 0.2, label = "0.90", size = 3) +
  annotate("text", x = F_crit_inf * 0.35, y = 0.2, label = "0.05", size = 3) +
  annotate("text", x = F_crit_sup * 1.075, y = 0.2, label = "0.05", size = 3) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme_minimal() +
  labs(title = "Distribución F",
       subtitle = paste0("Prueba bilateral, α = ", alpha, ", gl1 = ", gl1, ", gl2 = ", gl2),
       x = "F", y = "Densidad") -> grafica35

ggsave(filename = "grafico35.jpg", plot = grafica35, width = 5.5, height  = 3)


alpha <- 0.10
df <- 37
t_crit <- qt(1-alpha, df) # valor crítico para cola derecha

x <- seq(-4, 4, length.out = 500)
y <- dt(x, df)

df_plot <- data.frame(x = x, y = y)

rechazo <- subset(df_plot, x >= t_crit)

ggplot(df_plot, aes(x, y)) +
  geom_line(size = 1) +
  geom_area(data = rechazo, aes(x = x, y = y), fill = "grey80", alpha = 0.8) +
  geom_vline(xintercept = t_crit, linetype = "dotdash") +
  geom_hline(yintercept = 0)+
  annotate("label",
           x = t_crit, y = -0.1,
           label = paste0("t[0.9] == ", round(t_crit,3)),
           vjust = -0.5, parse = TRUE, fill = "white", label.size = 0.3) +
  annotate("text", x = 3.3, y = 0.15, label = "Zona de\nrechazo de H0", size = 3.5) +
  annotate("text", x = 0, y = 0.15, label = "Zona de no\nrechazo de H0", size = 3.5) +
  annotate("text", x = 1.65, y = 0.06, label = "0.1", size = 3) +
  annotate("text", x = 0, y = 0.06, label = "0.9", size = 3) +
  scale_y_continuous(breaks = seq(0,0.4,0.1)) +
  theme_minimal() +
  labs(title = "Distribución t de Student",
       subtitle = paste0("Prueba unilateral derecha, α = ", alpha, ", gl = ", df),
       x = "t", y = "Densidad") -> grafica36

ggsave(filename = "grafico36.jpg", plot = grafica36, width = 4.5, height  = 3)
