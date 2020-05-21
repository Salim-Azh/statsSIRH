####   ****Installation de packages
install.packages("VennDiagram")
library("VennDiagram")
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(ggmosaic)

### import de données (questionnaire branding IG polytech Montpellier)
donneeIni=read.csv("BrandingIG.csv",sep = ",",encoding = "UTF-8")

## Préparation des données
donnee=donneeIni[,3:11]
colnames(donnee)=c("sexe","influence.For","priorite.For","prerequis.For","def.Inf.avantFor",
               	"def.Inf.apresFor","satisfaction.For","PlaquetteFemme","IG.Femme")
donnee$sexe = recode_factor(donnee$sexe,
                             	"un homme" = "Homme",
                             	"une femme" = "Femme"
          	)
         	 
  donnee$satisfaction = recode_factor(donnee$satisfaction.For,
                                     	"1" = "Tres satisfait",
                                     	"2" = "Satisfait",
                                     	"3" = "Peu satisfait",
                                     	"4" = "Pas satisfait"
                                    	 
          	)

##Gestion definition informaticien
def.inf=donnee[,c("def.Inf.avantFor","def.Inf.apresFor")]
#creation var modif.defInf qui prend "Non" si l'etudiant a vrai pour (definition avant == definition apres) sinon "Oui"
def.inf=mutate(def.inf,
           	modif.defInf=case_when(
             	(def.Inf.apresFor=="Comme un gamer")==TRUE & str_detect(def.Inf.avantFor,"Comme un gamer")==TRUE ~ "Non",
             	(def.Inf.apresFor=="Comme un geek")==TRUE & str_detect(def.Inf.avantFor,"Comme un geek")==TRUE ~ "Non",
             	(def.Inf.apresFor=="Comme un no-life")==TRUE & str_detect(def.Inf.avantFor,"Comme un no-life")==TRUE ~ "Non",
             	(def.Inf.apresFor=="Comme une personne résolvant des problèmes")==TRUE & str_detect(def.Inf.avantFor,"Comme une personne résolvant des problèmes")==TRUE ~ "Non",
             	(def.Inf.apresFor=="Comme un scientifique")==TRUE & str_detect(def.Inf.avantFor,"Comme un scientifique")==TRUE ~ "Non",
             	(def.Inf.apresFor=="Comme un analyste")==TRUE & str_detect(def.Inf.avantFor,"Comme un analyste")==TRUE ~ "Non",
             	TRUE~ "Oui"),
           	modif.defInf=as.factor(modif.defInf)
)

#Gestion de Autre pour modif.defInf
def.inf=mutate(def.inf,
           	temp_avant = tolower(str_replace_all(def.Inf.avantFor, c("Comme un gamer"= "","Comme un geek"= "","Comme un no-life"= "",
                                                           	"Comme une personne résolvant des problèmes"= "",
                                                           	"Comme un scientifique"= "","Comme un analyste"= "",";"=""," "=""))),
           	temp_apres = tolower(str_replace_all(def.Inf.apresFor, c("Comme un gamer"= "","Comme un geek"= "","Comme un no-life"= "",
                                                           	"Comme une personne résolvant des problèmes"= "",
                                                           	"Comme un scientifique"= "","Comme un analyste"= "",";"=""," "=""))),

           	temp_modif = ifelse(modif.defInf == "Oui" & temp_avant != "" & temp_apres !="" & temp_avant == temp_apres, "Non", "Oui"),
           	modif.defInf = ifelse(modif.defInf == "Non" | temp_modif =="Non", "Non", "Oui")
                    	 
)
def.inf = def.inf[-(which(colnames(def.inf) == "temp_avant"))]
def.inf = def.inf[-(which(colnames(def.inf) == "temp_apres"))]
def.inf = def.inf[-(which(colnames(def.inf) == "temp_modif"))]
reponseDefInformatique =  recode_factor(def.inf$modif.defInf,
                                    	"Oui" = "Change",
                                    	"Non" = "Ne change pas"
)
def.inf=mutate(def.inf,
           	defInfAbr=case_when(
             	(def.Inf.apresFor=="Comme un gamer")==TRUE ~ "Gamer",
             	(def.Inf.apresFor=="Comme un geek")==TRUE ~ "Geek",
             	(def.Inf.apresFor=="Comme un no-life")==TRUE ~ "No-life",
             	(def.Inf.apresFor=="Comme une personne résolvant des problèmes")==TRUE ~ "Problem solver",
             	(def.Inf.apresFor=="Comme un scientifique")==TRUE ~ "Scientifique",
             	(def.Inf.apresFor=="Comme un analyste")==TRUE  ~ "Analyste",
             	TRUE~ "Autre")
)

def.inf$defInfAbr <- factor(def.inf$defInfAbr,levels=c("Scientifique",
                                                   	"Analyste",
                                                   	"Geek",
                                                   	"Problem solver",
                                                   	"No-life",
                                                   	"Gamer",
                                                   	"Autre")
)

##Gestion Influence
influence=donnee$influence.For # affectation de la variable
influence=as.data.frame(influence) # conversion en dataframe
influence=mutate(influence,
             	plaquette=ifelse((influence=="Plaquette papier de l'école") | str_detect(influence,"Plaquette papier de l'école")==TRUE,"Oui","Non"),
             	site_web=ifelse((influence=="Sur le site de Polytech Montpellier")| str_detect(influence,"Sur le site de Polytech Montpellier")==TRUE,"Oui","Non"),
             	temp = str_replace_all(influence, c(";" = "", "Sur le site de Polytech Montpellier" = "", "Plaquette papier de l'école" = "")),
             	ami=ifelse((temp =="Par un(e) ami(e)") ,"Oui","Non"),
             	autre=ifelse((temp == "Par un(e) ami(e)") | temp == "","Non","Oui"),
)

influence = influence[-(which(colnames(influence) == "temp"))]

#### Visualisation

###barplot
grapheUnivar=function(vecteur,titreGraphe,nomVariable){
  ggplot()+  
	geom_bar(aes(x = vecteur,y = round((..count..)/sum(..count..)*100,1),fill=vecteur),position = "dodge")+
	ylab("Frequence")+ xlab(nomVariable) + labs(fill = nomVariable)  + labs(title = titreGraphe)+
	theme(panel.grid.major = element_line(colour = "dodgerblue",
                                      	size = 0.5, linetype = "dotdash"),
      	axis.title = element_text(family = "serif",size = 18, face = "italic", colour = "black"),
      	axis.text = element_text(family = "serif", size = 15, face = "bold"),
      	axis.text.x = element_blank(),
      	plot.title = element_text(family = "serif"),
      	legend.text = element_text(family = "serif"),
      	legend.title = element_text(family = "serif"),
      	panel.background = element_rect(fill = "grey"),
      	plot.background = element_rect(fill = "beige")
	)
 
}

## mosaic plot
couleurs = c("beige","brown","cyan","grey","mint","charcoal","blue","green","yellow")
graphe_mosaic = function(v1, v2, main_, xlab_, ylab_){
   nbModalite = length(levels(v2))
   mosaicplot(v1~v2,
         	main = main_,
         	xlab = xlab_,
         	ylab = ylab_,
         	las = 1,
         	border = "chocolate",
         	color = couleurs[1:nbModalite])
}
