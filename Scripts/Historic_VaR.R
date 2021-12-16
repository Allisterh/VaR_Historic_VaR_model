#function to calculate VaR.
#VaR is just a percentile, however R interpolates the percentiles, I want to return the true percentile value if exists.
# The historic VaR function ca be defined as follows:
Historic_VaR<- function(df,columna=log_rend,alpha){
  columna<- enquo(columna)
  if (is.integer(nrow(df)*alpha)) #if interger return the exact percentile 
  {(df%>%arrange(!! columna)%>%slice(., round(nrow(.)*alpha))%>%pull(!! columna))} else 
  {(df%>%pull(!! columna)%>%quantile(alpha))} #if not approximate
}
# we want to calculate the 99%, 95% and 90% VaR
alphas<- c(0.01,0.05,0.1)
SAN_VaR<- Historic_VaR(SAN.MC,alpha = alphas)
DBK_VaR<- Historic_VaR(DBK.DE, alpha = alphas)
MSFT_VaR<- Historic_VaR(MSFT,alpha = alphas)

#tibble with 3 stocks VaR's
stock_VaRs<- tibble(VaR = c("99%","95%","90%"),SAN=SAN_VaR,
                    DBK=DBK_VaR,MSFT_VaR)

#plot 3 distributions with Var's

SAN_rend<- SAN.MC$log_rend
DBK_rend<- DBK.DE$log_rend
MSFT_rend<- MSFT$log_rend
#make same length as SAN
length(DBK_rend) = length(SAN_rend)
length(MSFT_rend) = length(SAN_rend)
#tibble of returns
returns<- tibble(SAN=SAN_rend,DBK=DBK_rend,MSFT=MSFT_rend)
#plot left tails with vertical lines representing VaR's
Tails_Vars<- returns %>%
  pivot_longer(cols = everything(), names_to = "Stock", #tidy format to plot
               values_to = "return") %>%
  ggplot(aes(x=return, fill = Stock))+
  geom_density(alpha = 0.5)+
  labs(x="Rangos de Utilidad",y="Frecuencias",title ="Valor en Riesgo")+
  geom_vline(data = data.frame(xsan=SAN_VaR[1],Stock="SAN"), aes(xintercept = xsan,color="red"))+  #add each VaR value to make a vertical line
  geom_vline(data = data.frame(xdbk=DBK_VaR[1],Stock="DBK"), aes(xintercept = xdbk,color="red"))+
  geom_vline(data = data.frame(xmsft=MSFT_VaR[1],Stock="MSFT"), aes(xintercept = xmsft,color="red"))+
  geom_vline(data = data.frame(xsan=SAN_VaR[2],Stock="SAN"), aes(xintercept = xsan,color="blue"))+
  geom_vline(data = data.frame(xdbk=DBK_VaR[2],Stock="DBK"), aes(xintercept = xdbk,color="blue"))+
  geom_vline(data = data.frame(xmsft=MSFT_VaR[2],Stock="MSFT"), aes(xintercept = xmsft,color="blue"))+
  geom_vline(data = data.frame(xsan=SAN_VaR[3],Stock="SAN"), aes(xintercept = xsan,color="green"))+
  geom_vline(data = data.frame(xdbk=DBK_VaR[3],Stock="DBK"), aes(xintercept = xdbk,color="green"))+
  geom_vline(data = data.frame(xmsft=MSFT_VaR[3],Stock="MSFT"), aes(xintercept = xmsft,color="green"))+
  coord_cartesian(xlim = c(-0.21,-0.0225), ylim = c(0,16))+
  facet_grid(Stock ~ .)+
  theme(legend.position = "none")
Tails_Vars

ggsave(filename = "Plots/VaR.jpeg", plot = Tails_Vars,
       dpi = "retina")

