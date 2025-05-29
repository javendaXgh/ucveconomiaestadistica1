library(tidyverse)
nombres <- c("María Fernanda Cuervo",
             "Ricardo A. Rivas L.", 
             "Antonio Silva", 
             "Marco Zapata", 
             # "Carlos Diz", 
             "Kleyner Tirrito", 
             "Aris Alzuro",
             "David Pizarro",
             "Gabriel Carrillo",
             "Yonahan Fuentes",
             "Ibrahim Muñoz")

ci <- c(27988880, 
        25363777, #25.363.777
        27588842,#27588842 
        31184708, 
        # 29625588, 
        28136114,
        27111755,
        31187123, 
        28448377,
        6692514,
        28443606)

participantes <- tibble(Nombre= str_to_title(nombres),
                        ci= ci,ci2=ci)%>%
  arrange(nombres)%>%
  mutate(ci=format(ci, nsmall=0, big.mark=".", decimal.mark=','),
         `N.`= 1:nrow(.))%>%
  select(`N.`, Nombre, ci,ci2)%>%
  rename(`cédula de identidad`=ci)
