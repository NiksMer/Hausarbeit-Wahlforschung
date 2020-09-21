###############################
#####  Reproduktionsskript ####
#####       Hausarbeit     ####
#####     Nikos Mertens    ####
###############################

remove(list=ls())

#install.packages("tidyverse")
#install.packages("scales")
library("tidyverse")
library("scales")


# Working Directory setzen
setwd("C:/Users/niksm/Documents/Uni/Mainz/Master/SS 2020/Hausarbeit Wahlforschung")


# Funktionen erstellen
mutate_na <- function(x){
  x %>%
    mutate_if(is.numeric, na_if, -66) %>%
    mutate_if(is.numeric, na_if, -77) %>%
    mutate_if(is.numeric, na_if, -99) %>%
    mutate_if(is.numeric, na_if, 0) %>%
    mutate_if(is.numeric, na_if, 99) %>%
    mutate_if(is.character,na_if,"-66") %>%
    mutate_if(is.character,na_if,"-99") %>%
    mutate_if(is.character,na_if,"-77")
}

Vgl_Plot <- function(df,group,col,fill_v){
  df %>%
    group_by({{ group }},{{ fill_v }}) %>%
    summarise(Mittelwert=mean({{col}}, na.rm=TRUE),n=n(),sd1=sd({{ col }},na.rm = TRUE),
              error=sd1/sqrt(n),min=Mittelwert-error,max=Mittelwert+error) %>%
    drop_na() %>%
    ggplot(aes(x={{ group }},y=Mittelwert,fill={{ fill_v }}))+
    geom_bar(stat="identity",position = "dodge2")+
    scale_fill_manual(values=c("lightblue2","darkolivegreen3"))+
    geom_errorbar(aes(ymin=min,ymax=max),alpha=0.4,width=0.5,position = position_dodge(.9))+
    xlab("Treatment-Gruppen") +
    scale_y_continuous(labels = percent)+
    geom_text(aes(label=percent(Mittelwert, accuracy = 0.1, decimal.mark = ",")), size=5,position=position_dodge(width=0.9)
              ,colour="black",fontface="bold.italic")+
    theme_classic()+
    theme(legend.position='bottom')
}

Small_Vgl_Plot <- function(df,group,col){
  df %>%
    group_by({{ group }}) %>%
    summarise(Mittelwert=mean({{col}}, na.rm=TRUE),n=n(),sd1=sd({{ col }},na.rm = TRUE),
              error=sd1/sqrt(n),min=Mittelwert-error,max=Mittelwert+error) %>%
    drop_na() %>%
    ggplot(aes(x={{ group }},y=Mittelwert))+
    geom_bar(stat="identity",position = "dodge2", fill="lightblue2")+
    geom_errorbar(aes(ymin=min,ymax=max),alpha=0.4,width=0.5,position = position_dodge(.9))+
    xlab("Treatment-Gruppen") +
    scale_y_continuous(labels = percent)+
    geom_text(aes(label=percent(Mittelwert, accuracy = 0.1, decimal.mark = ",")), size=5,position=position_dodge(width=0.9)
              ,colour="black",fontface="bold.italic")+
    theme_classic()
}



# Rohdaten einlesen
df_raw <- read_csv("data_project_826927_2020_09_03.csv") %>%
dplyr::  select(lfdn,v100,v_150,F5,F6,F7,v_92,v_93,v_94,v_95,v_96,v_97,v_98,F18
         ,v_25,F15,F19,F3,F4,F20,F21,F22,F23,F24,
         dupl1_v_38) %>%
  mutate_na()

# Variablen rekodieren und umbennen
df <- df_raw %>%
  mutate(ID=lfdn) %>%
  mutate(`Politisches Interesse`=case_when(
    v100==1 ~ 4,
    v100==2 ~ 3,
    v100==3 ~ 2,
    v100==4 ~ 1,
    v100==5 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate(LiRe=v_150-1) %>%
  mutate(`Inanspruchnahme Corona-Hilfen`=F5-1) %>%
  mutate(`Art der Hilfe`=F6) %>%
  mutate(`Arbeitsplätze 800k`=case_when(
    F7==1 ~ 4,
    F7==2 ~ 3,
    F7==3 ~ 2,
    F7==4 ~ 1,
    F7==5 ~ 0,
    TRUE ~ NA_real_  )) %>%
  mutate(`Arbeitsplätze 1000k`=case_when(
    v_92==1 ~ 4,
    v_92==2 ~ 3,
    v_92==3 ~ 2,
    v_92==4 ~ 1,
    v_92==5 ~ 0,
    TRUE ~ NA_real_  )) %>%
  mutate(`Klimaschutz 40k`=case_when(
    v_93==1 ~ 4,
    v_93==2 ~ 3,
    v_93==3 ~ 2,
    v_93==4 ~ 1,
    v_93==5 ~ 0,
    TRUE ~ NA_real_  )) %>%
  mutate(`Klimaschutz 100k`=case_when(
    v_94==1 ~ 4,
    v_94==2 ~ 3,
    v_94==3 ~ 2,
    v_94==4 ~ 1,
    v_94==5 ~ 0,
    TRUE ~ NA_real_  )) %>%
 mutate(`Mehrwertsteuer 1`=case_when(
   v_95==1 ~ 4,
   v_95==2 ~ 3,
   v_95==3 ~ 2,
   v_95==4 ~ 1,
   v_95==5 ~ 0,
   TRUE ~ NA_real_  )) %>%
  mutate(`Mehrwertsteuer 2`=case_when(
    v_96==1 ~ 4,
    v_96==2 ~ 3,
    v_96==3 ~ 2,
    v_96==4 ~ 1,
    v_96==5 ~ 0,
    TRUE ~ NA_real_  )) %>%
  mutate(`Mehrwertsteuer 3`=case_when(
    v_97==1 ~ 4,
    v_97==2 ~ 3,
    v_97==3 ~ 2,
    v_97==4 ~ 1,
    v_97==5 ~ 0,
    TRUE ~ NA_real_  )) %>%
  mutate(`Mehrwertsteuer 4`=case_when(
    v_98==1 ~ 4,
    v_98==2 ~ 3,
    v_98==3 ~ 2,
    v_98==4 ~ 1,
    v_98==5 ~ 0,
    TRUE ~ NA_real_  )) %>%
  mutate(`Weitreichende Hilfspakete`=F18-1) %>%
  mutate(`Performanz Bundesregierung`=v_25-1) %>%
  mutate(Sonntagsfrage=case_when(
    F15<=8 ~ F15,
    F15==9 ~ NA_real_,
    TRUE ~ NA_real_  )) %>%
  mutate(`größere Anschaffungen`=case_when(
    F19==1 ~ 1,
    F19==2|F19==3 ~ 0,
    TRUE ~ NA_real_  )) %>%
  mutate(Parteiidentifikation=F3) %>%
  mutate(`Stärke PID`=F4-1) %>%
  mutate(Geburtsjahr=F20) %>%
  mutate(Alter=2020-F20) %>%
  mutate(Alter=case_when(
    Alter>=18 & Alter<=85 ~ Alter,
    TRUE ~ NA_real_
  )) %>%
  mutate(Geschlecht=F21) %>%
  mutate(Bildungsabschluss=F22-1) %>%
  mutate(Bundesland=F23) %>%
  mutate(Erwärbstätigkeit=F24) %>%
  mutate(Kommentar=dupl1_v_38) %>%
  mutate(Dummy_Mehrwertsteuer=case_when(
    is.na(`Mehrwertsteuer 1`)==FALSE ~ 1, 
    is.na(`Mehrwertsteuer 2`)==FALSE ~ 2, 
    is.na(`Mehrwertsteuer 3`)==FALSE ~ 3, 
    is.na(`Mehrwertsteuer 4`)==FALSE ~ 4, 
    TRUE ~ NA_real_
  )) %>%
  mutate(Dummy_Kaufprämie=case_when(
    is.na(`Arbeitsplätze 800k`)==FALSE ~ 1, 
    is.na(`Arbeitsplätze 1000k`)==FALSE ~ 2, 
    is.na(`Klimaschutz 40k`)==FALSE ~ 3, 
    is.na(`Klimaschutz 100k`)==FALSE ~ 4, 
    TRUE ~ NA_real_
  )) %>%
  mutate(Kombi_Kaufprämie=case_when(
    is.na(`Arbeitsplätze 800k`)==FALSE ~ `Arbeitsplätze 800k`,
    is.na(`Arbeitsplätze 1000k`)==FALSE ~ `Arbeitsplätze 1000k`,
    is.na(`Klimaschutz 40k`)==FALSE ~ `Klimaschutz 40k`,
    is.na(`Klimaschutz 100k`)==FALSE ~ `Klimaschutz 100k`,
    TRUE ~ NA_real_
  )) %>%
  mutate(Kombi_Mehrwertsteuer=case_when(
    is.na(`Mehrwertsteuer 1`)==FALSE ~ `Mehrwertsteuer 1`,
    is.na(`Mehrwertsteuer 2`)==FALSE ~ `Mehrwertsteuer 2`,
    is.na(`Mehrwertsteuer 3`)==FALSE ~ `Mehrwertsteuer 3`,
    is.na(`Mehrwertsteuer 4`)==FALSE ~ `Mehrwertsteuer 4`,
    TRUE ~ NA_real_
  )) %>%
  mutate(Mehrwertsteuer_Zustimmung=case_when(
    Kombi_Mehrwertsteuer<=2 ~ 0,
    Kombi_Mehrwertsteuer>=3 ~ 1,
    TRUE ~ NA_real_
  )) %>%
  mutate(Kaufprämie_Zustimmung=case_when(
    Kombi_Kaufprämie<=2 ~ 0,
    Kombi_Kaufprämie>=3 ~ 1,
    TRUE ~ NA_real_
  )) %>%
  mutate(Hohe_Zufriedenheit=case_when(
    `Performanz Bundesregierung`>=3 ~ 1,
    `Performanz Bundesregierung`<=2 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(Hohe_Ablehnung=case_when(
    `Weitreichende Hilfspakete`<=1 ~ 1,
    `Weitreichende Hilfspakete`>=2 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  dplyr::  select(-lfdn,-v100,-v_150,-F5,-F6,-F7,-v_92,
         -v_93,-v_94,-v_95,-v_96,-v_97
         ,-v_98,-F18,-v_25,-F15,-F19,
         -F3,-F4,-F20,-F21,-F22,-F23,-F24,-dupl1_v_38)

# Numerisch abspeichern
df_t <- df

# Faktoren erstellen
df$Dummy_Kaufprämie <- factor(df$Dummy_Kaufprämie,levels=c(1,2,3,4),labels=c("Gruppe 1", "Gruppe 2", "Gruppe 3", "Gruppe 4"),ordered=FALSE)
df$Dummy_Mehrwertsteuer <- factor(df$Dummy_Mehrwertsteuer,levels=c(1,2,3,4),labels=c("Gruppe 1", "Gruppe 2", "Gruppe 3", "Gruppe 4"),ordered=FALSE)
df$`Inanspruchnahme Corona-Hilfen` <- factor(df$`Inanspruchnahme Corona-Hilfen`,labels = c("Keine Inanspruchnahme","Inanspruchnahme"),ordered=FALSE)

# Für Plots mit Faktoren abspeichern
df_p <- df

# Graphische Vergleiche
df <- df_p

##H1a, H1b
png("H1.png",units = "cm",width = 16,height = 11, res = 200)
Small_Vgl_Plot(df,group = Dummy_Kaufprämie,col=Kaufprämie_Zustimmung)+
  ylab("Anteil Befragte: positive Bewertung Kaufprämie")
dev.off()
##H2a, H2b
png("H2.png",units = "cm",width = 16,height = 11, res = 200)
Small_Vgl_Plot(df,group = Dummy_Mehrwertsteuer,col=Mehrwertsteuer_Zustimmung)+
  ylab("Anteil Befragte: positive Bewertung MwSt.-Senkung")
dev.off()

##H3a
png("H3a.png",units = "cm",width = 16,height = 11, res = 200)
Vgl_Plot(df,group = Dummy_Mehrwertsteuer,col=`größere Anschaffungen`,fill_v = `Inanspruchnahme Corona-Hilfen`)+
  ylab("Anteil Befragte: größere Anschaffungen geplant")
dev.off()

##H3b
png("H3b.png",units = "cm",width = 16,height = 11, res = 200)
Small_Vgl_Plot(df,group = Dummy_Mehrwertsteuer,col=`größere Anschaffungen`)+
  ylab("Anteil Befragte: größere Anschaffungen geplant")
dev.off()

##H4a
df_temp <- df %>%
  mutate(Mehrwertsteuer_Zustimmung=factor(Mehrwertsteuer_Zustimmung,levels=c(0,1),labels=c("Nicht positiv","Positiv"),ordered=FALSE)) %>%
  rename(`Bewertung Mehrwertsteuersenkung`=Mehrwertsteuer_Zustimmung)
png("H4a.png",units = "cm",width = 16,height = 11, res = 200)
Vgl_Plot(df_temp,group = Dummy_Mehrwertsteuer,col=Hohe_Zufriedenheit,fill_v = `Bewertung Mehrwertsteuersenkung`)+
  ylab("Anteil Befragte: Zufrieden mit Bundesregierung")
dev.off()

##H4b
png("H4b.png",units = "cm",width = 16,height = 11, res = 200)
Small_Vgl_Plot(df,group = Dummy_Mehrwertsteuer,col=Hohe_Zufriedenheit)+
  ylab("Anteil Befragte: Zufrieden mit Bundesregierung")
dev.off()

##H5
png("H5.png",units = "cm",width = 16,height = 11, res = 200)
Small_Vgl_Plot(df,group = Dummy_Mehrwertsteuer,col=Hohe_Ablehnung)+
  ylab("Anteil Befragte: Ablehnung größerer Hilfspakete")
dev.off()

# Hypothesenüberprüfung
df<- df_t

## Hypothesen H1a und H1b
df %>% summarise_each(funs(t.test(.[Dummy_Kaufprämie == 1], .[Dummy_Kaufprämie == 2])$p.value), vars = Kaufprämie_Zustimmung) # 1 vs 2
df %>% summarise_each(funs(t.test(.[Dummy_Kaufprämie == 1], .[Dummy_Kaufprämie == 3])$p.value), vars = Kaufprämie_Zustimmung) # 1 vs 3
df %>% summarise_each(funs(t.test(.[Dummy_Kaufprämie == 1], .[Dummy_Kaufprämie == 4])$p.value), vars = Kaufprämie_Zustimmung) # 1 vs 4
df %>% summarise_each(funs(t.test(.[Dummy_Kaufprämie == 2], .[Dummy_Kaufprämie == 3])$p.value), vars = Kaufprämie_Zustimmung) # 2 vs 3
df %>% summarise_each(funs(t.test(.[Dummy_Kaufprämie == 2], .[Dummy_Kaufprämie == 4])$p.value), vars = Kaufprämie_Zustimmung) # 2 vs 4
df %>% summarise_each(funs(t.test(.[Dummy_Kaufprämie == 3], .[Dummy_Kaufprämie == 4])$p.value), vars = Kaufprämie_Zustimmung) # 3 vs 4

## Hypothesen H2a und H2b
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 2])$p.value), vars = Mehrwertsteuer_Zustimmung) # 1 vs 2 sig
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 2])$statistic), vars = Mehrwertsteuer_Zustimmung) # 1 vs 2 sig

df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 3])$p.value), vars = Mehrwertsteuer_Zustimmung) # 1 vs 3
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 3])$statistic), vars = Mehrwertsteuer_Zustimmung) # 1 vs 3

df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 4])$p.value), vars = Mehrwertsteuer_Zustimmung) # 1 vs 4 sig
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 4])$statistic), vars = Mehrwertsteuer_Zustimmung) # 1 vs 4 sig

df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 2], .[Dummy_Mehrwertsteuer == 3])$p.value), vars = Mehrwertsteuer_Zustimmung) # 2 vs 3
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 2], .[Dummy_Mehrwertsteuer == 3])$statistic), vars = Mehrwertsteuer_Zustimmung) # 2 vs 3

df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 2], .[Dummy_Mehrwertsteuer == 4])$p.value), vars = Mehrwertsteuer_Zustimmung) # 2 vs 4
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 2], .[Dummy_Mehrwertsteuer == 4])$statistic), vars = Mehrwertsteuer_Zustimmung) # 2 vs 4

df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 3], .[Dummy_Mehrwertsteuer == 4])$p.value), vars = Mehrwertsteuer_Zustimmung) # 3 vs 4
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 3], .[Dummy_Mehrwertsteuer == 4])$statistic), vars = Mehrwertsteuer_Zustimmung) # 3 vs 4

## Hypothese H3a

### Keine Hilfe
df_ohne <- df %>%
  filter(`Inanspruchnahme Corona-Hilfen`==0)
### Hilfe
df_mit <- df %>%
  filter(`Inanspruchnahme Corona-Hilfen`==1)

#### ohne Hilfe vs mit Hilfe
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==1],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==1]) # Gruppe 1 ohne vs mit
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==2],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==2]) # Gruppe 2 ohne vs mit
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==3],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==3]) # Gruppe 3 ohne vs mit
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==4],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==4]) # Sig Gruppe 4 ohne vs mit
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==4],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==4])

#### Ohne
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==1],df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==2]) # Ohne: Gruppe 1 vs Gruppe 2
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==1],df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==3]) # Ohne: Gruppe 1 vs Gruppe 3
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==1],df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==4]) # Ohne: Gruppe 1 vs Gruppe 4
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==2],df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==3]) # Ohne: Gruppe 2 vs Gruppe 3
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==2],df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==4]) # Ohne: Gruppe 2 vs Gruppe 4
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==3],df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==4]) # Sig Ohne: Gruppe 3 vs Gruppe 4


#### Mit
t.test(df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==1],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==2]) # Mit: Gruppe 1 vs Gruppe 2
t.test(df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==1],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==3]) # Mit: Gruppe 1 vs Gruppe 3
t.test(df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==1],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==4]) # Mit: Gruppe 1 vs Gruppe 4
t.test(df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==2],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==3]) # Mit: Gruppe 2 vs Gruppe 3
t.test(df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==2],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==4]) # Mit: Gruppe 2 vs Gruppe 4
t.test(df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==3],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==4]) # Mit: Gruppe 3 vs Gruppe 4

#### Ohne Gruppe 1 vs Mit Gruppen 2,3,4
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==1],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==2]) # Ohne Gruppe 1 vs Mit Gruppe 2
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==1],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==3]) # Ohne Gruppe 1 vs Mit Gruppe 3
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==1],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==4]) # Ohne Gruppe 1 vs Mit Gruppe 4

### Ohne Gruppe 2 vs Mit Gruppen 3,4
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==2],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==3]) # Ohne Gruppe 2 vs Mit Gruppe 3
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==2],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==4]) # Ohne Gruppe 2 vs Mit Gruppe 4

### Ohne Gruppe 3 vs Mit Gruppe 4
t.test(df_ohne$`größere Anschaffungen`[df_ohne$Dummy_Mehrwertsteuer==3],df_mit$`größere Anschaffungen`[df_mit$Dummy_Mehrwertsteuer==4]) # Ohne Gruppe 3 vs Mit Gruppe 4

## Hypothese H3b
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 2])$p.value), vars = `größere Anschaffungen`) # 1 vs 2
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 3])$p.value), vars = `größere Anschaffungen`) # 1 vs 3
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 4])$p.value), vars = `größere Anschaffungen`) # 1 vs 4
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 2], .[Dummy_Mehrwertsteuer == 3])$p.value), vars = `größere Anschaffungen`) # 2 vs 3
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 2], .[Dummy_Mehrwertsteuer == 4])$p.value), vars = `größere Anschaffungen`) # 2 vs 4
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 3], .[Dummy_Mehrwertsteuer == 4])$p.value), vars = `größere Anschaffungen`) # 3 vs 4

## Hypothese H4a
df_Zustimmung <- df %>%
  filter(Mehrwertsteuer_Zustimmung==1)
df_Ablehnung <- df %>%
  filter(Mehrwertsteuer_Zustimmung==0)

#### Ablehnung Hilfe vs Zustimmung Hilfe
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==1],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==1]) # Sig Gruppe 1 Ablehnung vs Zustimmung
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==2],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==2]) # p=0,08 Gruppe 2 Ablehnung vs Zustimmung
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==3],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==3]) # Gruppe 3 Ablehnung vs Zustimmung
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==4],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==4]) # Sig Gruppe 4 Ablehnung vs Zustimmung

#### Ablehnung
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==1],df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==2]) # Ablehnung: Gruppe 1 vs Gruppe 2
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==1],df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==3]) # Ablehnung: Gruppe 1 vs Gruppe 3
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==1],df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==4]) # Ablehnung: Gruppe 1 vs Gruppe 4
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==2],df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==3]) # Ablehnung: Gruppe 2 vs Gruppe 3
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==2],df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==4]) # Ablehnung: Gruppe 2 vs Gruppe 4
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==3],df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==4]) # Ablehnung: Gruppe 3 vs Gruppe 4


#### Zustimmung
t.test(df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==1],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==2]) # Zustimmung: Gruppe 1 vs Gruppe 2
t.test(df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==1],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==3]) # Sig Zustimmung: Gruppe 1 vs Gruppe 3
t.test(df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==1],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==4]) # Zustimmung: Gruppe 1 vs Gruppe 4
t.test(df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==2],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==3]) # Zustimmung: Gruppe 2 vs Gruppe 3
t.test(df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==2],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==4]) # p=0,09 Zustimmung: Gruppe 2 vs Gruppe 4
t.test(df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==3],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==4]) # Sig Zustimmung: Gruppe 3 vs Gruppe 4

#### Ablehnung Gruppe 1 vs Zustimmung Gruppen 2,3,4
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==1],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==2]) # Sig Ablehnung Gruppe 1 vs Zustimmung Gruppe 2
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==1],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==3]) # Ablehnung Gruppe 1 vs Zustimmung Gruppe 3
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==1],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==4]) # Sig Ablehnung Gruppe 1 vs Zustimmung Gruppe 4

### Ablehnung Gruppe 2 vs Zustimmung Gruppen 3,4
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==2],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==3]) # Ablehnung Gruppe 2 vs Zustimmung Gruppe 3
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==2],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==4]) # Sig Ablehnung Gruppe 2 vs Zustimmung Gruppe 4

### Ablehnung Gruppe 3 vs Zustimmung Gruppe 4
t.test(df_Ablehnung$Hohe_Zufriedenheit[df_Ablehnung$Dummy_Mehrwertsteuer==3],df_Zustimmung$Hohe_Zufriedenheit[df_Zustimmung$Dummy_Mehrwertsteuer==4]) # Sig Ablehnung Gruppe 3 vs Zustimmung Gruppe 4

## Hypothese H4b
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 2])$p.value), vars = Hohe_Zufriedenheit) # 1 vs 2
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 3])$p.value), vars = Hohe_Zufriedenheit) # 1 vs 3
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 4])$p.value), vars = Hohe_Zufriedenheit) # 1 vs 4 sig
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 2], .[Dummy_Mehrwertsteuer == 3])$p.value), vars = Hohe_Zufriedenheit) # 2 vs 3
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 2], .[Dummy_Mehrwertsteuer == 4])$p.value), vars = Hohe_Zufriedenheit) # 2 vs 4 p=0,07
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 3], .[Dummy_Mehrwertsteuer == 4])$p.value), vars = Hohe_Zufriedenheit) # 3 vs 4 sig

## Hypothese H5
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 2])$p.value), vars = Hohe_Ablehnung) # 1 vs 2
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 3])$p.value), vars = Hohe_Ablehnung) # 1 vs 3
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 1], .[Dummy_Mehrwertsteuer == 4])$p.value), vars = Hohe_Ablehnung) # 1 vs 4 sig
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 2], .[Dummy_Mehrwertsteuer == 3])$p.value), vars = Hohe_Ablehnung) # 2 vs 3
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 2], .[Dummy_Mehrwertsteuer == 4])$p.value), vars = Hohe_Ablehnung) # 2 vs 4
df %>% summarise_each(funs(t.test(.[Dummy_Mehrwertsteuer == 3], .[Dummy_Mehrwertsteuer == 4])$p.value), vars = Hohe_Ablehnung) # 3 vs 4 p=0,09

