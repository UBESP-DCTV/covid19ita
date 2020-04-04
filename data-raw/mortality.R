library(tidyverse)

decessi_url <- paste0(
  "https://www.istat.it/",
  "it/files/2020/03/",
  "Tavola-sintetica-decessi.xlsx"
)

utils::download.file(data_url, dest_url)


dec<-read.csv("Tavola-sintetica-decessi.csv")


levels(dec$NOME_REGIONE)[levels(dec$NOME_REGIONE)%in%"Trentino-Alto Adige/Südtirol"]<-"Trentino A.A."

levels(dec$NOME_REGIONE)[levels(dec$NOME_REGIONE)%in%"Valle d'Aosta/Vallée d'Aoste"]<-"V.d'Aosta"

levels(dec$NOME_REGIONE)[levels(dec$NOME_REGIONE)%in%"Friuli-Venezia Giulia" ]<-"Friuli V.G."

levels(dec$NOME_REGIONE)[levels(dec$NOME_REGIONE)%in%"Emilia-Romagna" ]<-"Emilia R."
dt<-aggregate(list(m19=dec$M,
                   m20=dec$M.1,

                   f19=dec$F,

                   f20=dec$F.1),
              by=list(dec$NOME_REGIONE),
              sum,na.rm=T)
it<-c(Group.1=factor("Italia"),
      apply(dt[,!colnames(dt)%in%"Group.1"],2,sum))

dt<-rbind(dt,it)

dt$rel.var.M<-round(((dt$m20-dt$m19)/dt$m19)*100,2)
dt$rel.var.F<-round(((dt$f20-dt$f19)/dt$f19)*100,2)

colnames(dt)<-c("Regione",
                "Maschi 2019",
                "Maschi 2020",
                "Femminine 2019",
                "Femmine 2020",
                "Variazione relativa maschi 2019",
                "Variazione relativa femmine 2020")
