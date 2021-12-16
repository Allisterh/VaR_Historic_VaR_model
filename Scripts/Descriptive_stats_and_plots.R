
# obtain mean,median,mode,sd,kurtosis,skewness,rank,min,max
require(moments)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#function to make code simpler
descriptive<- function(df,columna=log_rend){
  columna<- enquo(columna)
  df %>%
    summarize(Media = mean(!! columna),
              Mediana = median(!! columna),
              Moda = Mode(!! columna),
              Desviacion = sd(!! columna),
              Curtosis = kurtosis(!! columna),
              Asimetria = skewness(!! columna),
              Rango = max(!! columna)-min(!! columna),
              Minimo = min(!! columna),
              Maximo = max(!! columna))%>%
    pivot_longer(cols = everything(),names_to = "Descriptiva",values_to = "Valores")
}
descriptive_SAN<- descriptive(SAN.MC)
descriptive_DBK<- descriptive(DBK.DE)
descriptive_MSFT<- descriptive(MSFT)
#plot histograms of the stocks (indivdual)
histogram_SAN<- SAN.MC %>%
  ggplot(aes(x=log_rend))+
  geom_histogram(binwidth = 0.006,color="black")+
  labs(x = "Rangos de Utilidad",y = "Frecuencias",title = "Histograma de Rendimientos SAN.MC")
histogram_DBK<- DBK.DE %>%
  ggplot(aes(x=log_rend))+
  geom_histogram(binwidth = 0.006,color="black")+
  labs(x = "Rangos de Utilidad",y = "Frecuencias",title = "Histograma de Rendimientos DBK.DE")
histogram_MSFT<- MSFT %>%
  ggplot(aes(x=log_rend))+
  geom_histogram(binwidth = 0.006,color="black")+
  labs(x = "Rangos de Utilidad",y = "Frecuencias",title = "Histograma de Rendimientos MSFT")
histogram_SAN
histogram_DBK
histogram_MSFT
#plot density of stocks and density of normal distribution of size n with same mean and sd (individual)
density_SAN<- SAN.MC %>%
  ggplot(aes(x=log_rend))+
  geom_density(mapping = aes(x=normal), data = SAN.MC, fill = "grey", alpha = 0.85)+
  geom_density(fill = "red", alpha=0.7)+
  labs(x = "Rangos de Utilidad",y = "Frecuencias",title = "Distribución de Rendimientos SAN.MC")
density_DBK<- DBK.DE %>%
  ggplot(aes(x=log_rend))+
  geom_density(mapping = aes(x=normal), data = DBK.DE, fill = "grey", alpha = 0.85)+
  geom_density(fill = "blue", alpha=0.789)+
  labs(x = "Rangos de Utilidad",y = "Frecuencias",title = "Distribución de Rendimientos DBK.DE")
density_MSFT<- MSFT %>%
  ggplot(aes(x=log_rend))+
  geom_density(mapping = aes(x=normal), data = MSFT, fill = "grey", alpha = 0.85)+
  geom_density(fill = "green", alpha=0.789)+
  labs(x = "Rangos de Utilidad",y = "Frecuencias",title = "Distribución de Rendimientos MSFT")
density_SAN
density_DBK
density_MSFT
#save plots
ggsave(filename = "Plots/Histograma_SAN.jpeg", plot = histogram_SAN,
       dpi = "retina")
ggsave(filename = "Plots/Histograma_DBK.jpeg", plot = histogram_DBK,
       dpi = "retina")
ggsave(filename = "Plots/Histograma_MSFT.jpeg", plot = histogram_MSFT,
       dpi = "retina")
ggsave(filename = "Plots/Distribucion_SAN.jpeg", plot = density_SAN,
       dpi = "retina")
ggsave(filename = "Plots/Distribucion_DBK.jpeg", plot = density_DBK,
       dpi = "retina")
ggsave(filename = "Plots/Distribucion_MSFT.jpeg", plot = density_MSFT,
       dpi = "retina")


