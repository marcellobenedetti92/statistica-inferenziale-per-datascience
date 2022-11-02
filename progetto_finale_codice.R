dati <- read.csv("neonati.csv")
attach(dati)

# trasformo le variabili qualitative in fattori per verificare i livelli di ciascuna variabile qualitativa

fumatrici_factor <- as.factor(Fumatrici)
tipo.parto_factor <- as.factor(Tipo.parto)
ospedale_factor <- as.factor(Ospedale)
sesso_factor <- as.factor(Sesso)

levels(fumatrici_factor)
levels(tipo.parto_factor)
levels(ospedale_factor)
levels(sesso_factor)


# INDICI DI POSIZIONE

# MODA
table(Anni.madre)
# da rimuovere due osservazioni, relative a donne con 0 e 1 anno di età, che non potrebbero in alcun modo essere incinte

dati_madri_reali <- subset(dati,Anni.madre!="0" & Anni.madre!="1")

detach(dati)

attach(dati_madri_reali)

fumatrici_factor <- as.factor(Fumatrici)
tipo.parto_factor <- as.factor(Tipo.parto)
ospedale_factor <- as.factor(Ospedale)
sesso_factor <- as.factor(Sesso)

levels(fumatrici_factor)
levels(tipo.parto_factor)
levels(ospedale_factor)
levels(sesso_factor)


# calcolo della MODA
which.max(table(dati_madri_reali$Anni.madre))
df_Anni.madre <- as.data.frame(table(Anni.madre))
# moda = 30 anni (alla riga 18)

which.max(table(N.gravidanze))
df_N.gravidanze <- as.data.frame(table(N.gravidanze))
# rimuovo le osservazioni con 0 gravidanze dato che lo studio vuole indagare se è possibile prevedere il peso del neonato alla nascita, quindi non è possibile avere un dataset di partenza con 0 gravidanze e il peso  del bambino





# NO! Commento di Giuseppe Dejan Lucido: Le gravidanze segnate sono quelle precedenti a quella attuale, quindi sono da tenere le osservazioni con 0 gravidanze





# ERRORE: dati_madri_reali_2 <- subset(dati_madri_reali, N.gravidanze!="0")

attach(dati_madri_reali)

# ricalcolo la moda per Anni.madre
which.max(table(Anni.madre))
df_Anni.madre_2 <- as.data.frame(table(Anni.madre))
# la moda rimane 30 (in questo df alla riga 18)

# ricalcolo la moda per N.gravidanze
which.max(table(N.gravidanze))
df_N.gravidanze_2 <- as.data.frame(table(N.gravidanze))
# la moda è 1 gravidanza

table(Fumatrici)
# fortunatamente la moda è 0, quindi sono in numero maggiore le donne che non fumano in gravidanza

which.max(table(Gestazione))
df_Gestazione <- as.data.frame(table(Gestazione))
# la moda è 40 settimane

which.max(table(Peso))
df_Peso <- as.data.frame(table(Peso))
# la moda è 3300

which.max(table(Lunghezza))
df_Lunghezza <- as.data.frame(table(Lunghezza))
# la moda è 500

which.max(table(Cranio))
df_Cranio <- as.data.frame(table(Cranio))
# la moda è 340

table(Tipo.parto)
# la moda è Naturale

table(Ospedale)
# la moda è osp2

table(Sesso)
# la moda è F


# MIN e MAX
range(Anni.madre)
range(N.gravidanze)
range(Gestazione)
range(Peso)
range(Lunghezza)
range(Cranio)


# MEDIANA

median(Anni.madre) # 28
median(N.gravidanze) # 1
median(Gestazione) # 39
median(Lunghezza) # 500
median(Cranio) # 340
median(Peso) # 3300


# QUARTILI E PERCENTILI
quantile(Anni.madre)
quantile(N.gravidanze)
quantile(Gestazione)
quantile(Peso)
quantile(Lunghezza)
quantile(Cranio)

quantile(Gestazione,seq(0,1,0.01))
quantile(Peso,seq(0,1,0.01))
quantile(Lunghezza,seq(0,1,0.01))
quantile(Cranio,seq(0,1,0.01))
quantile(Anni.madre,seq(0,1,0.01))
quantile(N.gravidanze,seq(0,1,0.01))


# MEDIA ARITMETICA

round(mean(Anni.madre),0) # 28
round(mean(N.gravidanze),0) # 1
round(mean(Gestazione),1) # 39
mean_lunghezza <- round(mean(Lunghezza),2) # 494.7
mean_cranio <- round(mean(Cranio),2) # 340.03
round(mean(Peso),2) # 3284.18

# in questo caso mediana e media si muovono più o meno allo stesso modo
# con una differenza più marcata per la variabile N.gravidanze



# INDICI DI VARIABILITÀ

# INTERVALLO DI VARIAZIONE

max(Anni.madre)-min(Anni.madre)
max(N.gravidanze)-min(N.gravidanze)
max(Gestazione)-min(Gestazione)
max(Lunghezza)-min(Lunghezza)
max(Cranio)-min(Cranio)
max(Peso)-min(Peso)


# RANGE INTERQUARTILE

IQR(Anni.madre)
IQR(N.gravidanze)
IQR(Gestazione)
IQR(Lunghezza)
IQR(Cranio)
IQR(Peso)

# boxplot delle variabili quantitative

par(mfrow=c(2,3))
boxplot(Anni.madre)
boxplot(N.gravidanze)
boxplot(Gestazione)
boxplot(Lunghezza)
boxplot(Cranio)
boxplot(Peso)

# boxplot delle variabili quantitative condizionate

par(mfrow=c(1,1))

# ERRORE boxplot(Peso~Anni.madre)
# ERRORE boxplot(Peso~N.gravidanze)

# ERRORE boxplot(Peso~Gestazione) # si intravede una correlazione tra queste 2 variabili
# in particolare, tra i 30 e i 31, e tra i 33 e 34 anni, si nota un aumento considerevole del IQR
cor(Peso,Gestazione) # correlazione nella misura del 59% circa

# ERRORE boxplot(Peso~Lunghezza)
# come naturale, la lunghezza influisce sul peso
cor(Peso,Lunghezza)
# nella misura del 79% circa

#ERRORE boxplot(Peso~Cranio)
# anche qui, come sopra, si nota una correlazione
cor(Peso,Cranio)
# nella misura del 70% circa

boxplot(Gestazione~Fumatrici)
# il boxplot delle madri non fumatrici presenta un numero considerevolmente maggiore di outliers, rispetto al boxplot delle madri fumatrici --> le donne fumatrici campionate qui non sembrano risentire di nascite premature

boxplot(Peso~Fumatrici)

boxplot(Peso~Sesso)

boxplot(Peso~Ospedale)
# solo per scrupolo

boxplot(Peso~Tipo.parto)

# i boxplot finora plottati mi portano a credere che un primo modello di regressione lineare potrebbe essere multipla considerando come variabili esplicative la Lunghezza e il Cranio





# NO! Commento di Giuseppe Dejan Lucido: i boxplot condizionati tra due quantitative non si possono vedere. Si usano solo tra una variabile quantitativa e una qualitativa o in classi! Dovevi usare gli scatterplot.





# VARIANZA e DEVIAZIONE STANDARD

var(Anni.madre)
var(N.gravidanze)
var(Gestazione)
var(Lunghezza)
var(Cranio)
var(Peso)

round(sd(Anni.madre),0)
round(sd(N.gravidanze),0)
round(sd(Gestazione),1)
round(sd(Lunghezza),2)
round(sd(Cranio),2)
round(sd(Peso),2)


# COEFFICIENTE DI VARIAZIONE

CV <- function(x){
  return(sd(x)/mean(x)*100)
}

round(CV(Anni.madre),0)
round(CV(N.gravidanze),0)
round(CV(Gestazione),1)
round(CV(Lunghezza),2)
round(CV(Cranio),2)
round(CV(Peso),2)
# il numero delle gravidanze presenta una variabilità considerevolmente maggiore rispetto alle altre


# INDICE DI ETEROGENEITÀ DI GINI per le variabili qualitative

gini.index <- function(x){
  ni=table(x)
  fi=ni/length(x)
  fi2=fi^2
  J = length(table(x))
  
  gini = 1-sum(fi2)
  gini.normalizzato = gini/((J-1)/J)
  
  return(gini.normalizzato)
}

gini.index(Fumatrici) # bassa eterogeneità --> forse per questo che la correlazione con la variabile Peso risulta bassa
gini.index(Tipo.parto) # alta eterogeneità
gini.index(Ospedale) # alta eterogeneità --> quasi equidistribuzione
gini.index(Sesso) # alta eterogeneità --> quasi equidistribuzione


# INDICI DI FORMA sulle variabili continue

install.packages("moments")
library(moments)

plot(density(Gestazione))
plot(density(Lunghezza))
plot(density(Cranio))
plot(density(Peso))

attach(dati_madri_reali)

skewness(Lunghezza) # distribuzione asimettrica negativa come da grafico
skewness(Cranio) # distribuzione asimettrica negativa come da grafico
skewness(Peso) # distribuzione asimettrica negativa come da grafico

kurtosis(Lunghezza)-3 # distribuzione leptocurtica
kurtosis(Cranio)-3 # distribuzione leptocurtica 
kurtosis(Peso)-3 # distribuzione leptocurtica

# verifico che le distribuzioni di cui sopra siano effettivamente più allungate rispetto a una distribuzione normale
x.Lunghezza <- rnorm(100000,494.07,26.33) # inserendo media e ds di Lunghezza del campione
lines(density(x.Lunghezza),col=2)

x.Cranio <- rnorm(100000,340.03,16.43) # inserendo media e ds di Cranio del campione
lines(density(x.Cranio),col=2)

x.Peso <- rnorm(100000,3284.18,525.23) # inserendo media e ds di Peso del campione
lines(density(x.Peso),col=2)



# Saggia l’ipotesi che la media del peso e della lunghezza di questo campione di neonati siano significativamente uguali a quelle della popolazione


# PESO

mu_peso_popolazione <- 3300 # fonte: Google

t.test(Peso,
       mu = mu_peso_popolazione,
       conf.level = 0.95,
       alternative = "two.sided")
# non si rifiuta l'ipotesi nulla di uguaglianza tra medie dato che 3300 rientra nell'intervallo


# LUNGHEZZA

mu_lunghezza_popolazione <- 500 # fonte: Google

t.test(Lunghezza,
       mu = mu_lunghezza_popolazione,
       conf.level = 0.95,
       alternative = "two.sided")
# si rifiuta l'ipotesi nulla di uguaglianza tra medie dato che 500 è al di fuori dell'intervallo



# 5) Per le stesse variabili, o per altre per le quali ha senso farlo, verifica differenze significative tra i due sessi

t.test(data=dati_madri_reali,
       Peso~Sesso)
# si rifiuta l'ipotesi nulla di uguaglianza tra medie di campioni per Peso~Sesso

t.test(data=dati_madri_reali,
       Lunghezza~Sesso)
# si rifiuta l'ipotesi nulla di uguaglianza tra medie di campioni per Lunghezza~Sesso a causa del valore del p-value
# P.S. by Dejan: infine l'intervallo non è riferito alla statistica test, ma alla quantità stimata
# la statistica t "cade" - si distribuisce - come una t di student e fa riferimento solo a quella



# 6) Si vocifera che in alcuni ospedali si facciano più parti cesarei, sai verificare questa ipotesi?

install.packages("dplyr")
library(dplyr)

freq_osp1 <- dati_madri_reali %>%
  select(Ospedale,Tipo.parto) %>% 
  filter(Ospedale=="osp1") %>% 
  group_by(Tipo.parto) %>% 
  summarise(freq_ass = n()) %>% 
  mutate(freq_rel = round(freq_ass/sum(freq_ass),3))

freq_osp2 <- dati_madri_reali %>%
  select(Ospedale,Tipo.parto) %>% 
  filter(Ospedale=="osp2") %>% 
  group_by(Tipo.parto) %>% 
  summarise(freq_ass = n()) %>% 
  mutate(freq_rel = round(freq_ass/sum(freq_ass),3))

freq_osp3 <- dati_madri_reali %>%
  select(Ospedale,Tipo.parto) %>% 
  filter(Ospedale=="osp3") %>% 
  group_by(Tipo.parto) %>% 
  summarise(freq_ass = n()) %>%
  mutate(freq_rel = round(freq_ass/sum(freq_ass),3))

# domanda: COME POSSO CREARE UNA SOLA TABELLA? HO PROVATO A TOGLIERE FILTER, MA NON RIMANE LA COLONNA OSP





# risposta di Giuseppe Dejan Lucido:
Tipo.parto_per_osp <- table(Ospedale, Tipo.parto)
chisq.test(Tipo.parto_per_osp)




# le proporzioni per parti cesarei sono pressoché identiche tra ospedali



# ANALISI MULTIDIMENSIONALE

# 1) Ricordati qual è l’obbiettivo dello studio e indaga le relazioni a due a due, soprattutto con la variabile risposta

plot(Gestazione,Peso,pch=20)
cor(Gestazione,Peso) #0.59

plot(Lunghezza,Peso,pch=20)
cor(Lunghezza,Peso) #0.80
plot(Cranio,Peso,pch=20)
cor(Cranio,Peso) #0.70

plot(Gestazione,Lunghezza,pch=20)
cor(Gestazione,Lunghezza) #0.62
plot(Gestazione,Cranio,pch=20)
cor(Gestazione,Cranio) #0.46
plot(Cranio,Lunghezza,pch=20)
cor(Cranio,Lunghezza) #0.60


# 2) Crea un modello di regressione lineare multipla con tutte le variabili e commenta i coefficienti e il risultato ottenuto

# asimmetria di Peso, già calcolata, abbastanza vicina allo 0 (-0.6) --> simmetrica
# curtosi, già calcolata, a 1.8 --> distribuzione leptocurtica (più allungata rispetto alla normale)

# test di shapiro per verificare che non si rifiuta l'ipotesi di normalità per la variabile risposta:
shapiro.test(Peso)
# si rifiuta l'ipotesi di normalità -->
# RICORDARSI DI VERIFICARE LA DISTRIBUZIONE NORMALE DEI RESIDUI

# matrice di correlazione per verificare le ipotesi di correlazione finora sondate
# rimuovo le variabili chr dal dataset per la funzione
dati_madri_reali_cormatrix <- subset(dati_madri_reali[1:7],select = -Fumatrici)
round(cor(dati_madri_reali_cormatrix),2)
# si tratta in realtà di una sintesi degli indici di correlazione calcolati assieme agli scatter plots sopra


# unisco indici e scatterplot

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# apro una nuova finestra
dev.new()

# aggiungo altri aspetti al grafico e lo lancio nella nuova finestra grafica creata
pairs(dati_madri_reali_cormatrix,upper.panel = panel.smooth, lower.panel = panel.cor)

mod1_peso <- lm(Peso ~
             Anni.madre +
             N.gravidanze +
             Gestazione +
             Lunghezza +
             Cranio,
           data = dati_madri_reali)

summary(mod1_peso)

# CONSIDERAZIONI
# le variabili esplicative hanno tutte un effetto di incremento significativo sulla variabile risposta, tranne Anni.madre che posso provare a togliere dal modello, anche in ragione del fatto che valore del p-value è maggiore del livello di significatività fissato allo 0.05

mod2_peso <- lm(Peso ~
                  N.gravidanze +
                  Gestazione +
                  Lunghezza +
                  Cranio,
                data = dati_madri_reali)

summary(mod2_peso)

# L'R^2 adj è rimasto invariato, provo a togliere anche la variabile N.gravidanze che non mostrava una correlazione significativa con la nostra variabile risposta e riporta qui un p-value superiore allo 0.05

mod3_peso <- lm(Peso ~
                  Gestazione +
                  Lunghezza +
                  Cranio,
                data = dati_madri_reali)

summary(mod3_peso)

# l'R^2 adj è leggermente sceso, ma non in maniera significativa da portarci a preferire il modello precedente (sopratutto considerando gli insegnamenti di Dejan-Occam)

# dal grafico-matrice di correlazione si può notare un leggero effetto quadratico della variabile Gestazione
# la stessa, in aggiunta, mostra una stima in valore assoluto maggiore delle altre e il p-value è sotto la soglia del 5% --> variabile significativa
# provo ad aggiungere al modello l'effetto quadratico

mod4_peso <- update(mod3_peso,~.+I(Gestazione^2))

summary(mod4_peso)

# l'R^2 adj è tornato al valore dei primi modelli, con un leggero miglioramento rispetto al mod_3, tale però da non portarci a preferirlo, dato che il p-value per la Gestazione supera ora il livello di significatività, e quindi non risulta significativo

# posso provare quindi a togliere anche la variabile esplicativa Gestazione al modello

mod5_peso <- lm(Peso ~
                  Lunghezza +
                  Cranio,
                data = dati_madri_reali)

summary(mod5_peso)
# ho quindi ora due variabili esplicative significative:
# ogni mm di lunghezza aumenta di circa 11g il peso del bambino
# ogni mm di circonferenza del cranio aumenta di circa 12g il peso del bambino


# posso saggiare l'ipotesi che le due variabili abbiano anche un effetto sinergico, per cui:

mod6_peso <- lm(Peso ~
                  Lunghezza +
                  Cranio +
                  Lunghezza:Cranio,
                data = dati_madri_reali)

summary(mod6_peso)

# R^2 adj per questo modello ridotto --> posso fare a meno dell'interazione


# utilizzo i criteri ANOVA e BIC (preferibile all'AIC in quanto non sovrastima modelli sovraparametrizzati)

anova(mod1_peso,mod2_peso,mod3_peso,mod4_peso,mod5_peso,mod6_peso)
# il p-value "migliore" (più vicino allo 0) è relativo al modello 5

BIC(mod1_peso,mod2_peso,mod3_peso,mod4_peso,mod5_peso,mod6_peso)
# in questo caso il modello 2 sembrerebbe essere migliore del 5 che lo segue al secondo posto

# tengo il modello 2


# per essere sicuri che non ci siano problemi di multicollinearità (regressori con correlazioni molto elevate fra loro) utilizziamo la funzione:
install.packages("car")
library(car)
vif(mod2_peso)
# valori sotto 5 --> non indicano problemi


# DIAGNOSTICA SUI RESIDUI

par(mfrow=c(2,2))
plot(mod2_peso)

# [PRIMO GRAFICO] i punti NON sembrano sparsi casualmente intorno alla media di 0
# [SECONDO GRAFICO] i punti seguono una distribuzione normale
# [TERZO GRAFICO] nuvola non casuale di punti --> varianza non costante?
# [QUARTO GRAFICO] un solo punto (1551) leggermente sotto la soglia di allarme di 1


# leverage
lev <- hatvalues(mod2_peso)
plot(lev)
p = sum(lev)
soglia = 2*p/1403
abline(h=0.004, col=2)
lev[lev>soglia]

#outliers
outlierTest(mod2_peso)
# le osservazioni classificate come outliers sono: 1551, 155 e 1306

# 69 valori leverage e 3 valori outliers

cook <- cooks.distance(mod2_peso)
plot(cook)
max(cook)
# la distanza di Cook massima non supera di poco la soglia di allarme di 1


# 6) Quanto ti sembra buono il modello per fare previsioni? Abbastanza buono, forse troppi valori di leva


# 7) Fai la tua migliore previsione per il peso di una neonata, considerato che la madre è alla terza gravidanza e partorirà alla 39esima settimana. Niente misure dall’ecografia.

new <- data.frame(N.gravidanze=c(3), Gestazione=c(39), Lunghezza=c(mean_lunghezza), Cranio=c(mean_cranio))

predict(mod2_peso, newdata = new)

# peso medio bambino dopo 39 settimane: 3300 (fonte: https://www.pampers.it/39-settimana-di-gravidanza) -->
# sembra un buon modello


# provo un'altra previsione con un valore di Gestazione 

new <- data.frame(N.gravidanze=c(3), Gestazione=c(28), Lunghezza=c(mean_lunghezza), Cranio=c(mean_cranio))

predict(mod2_peso, newdata = new)