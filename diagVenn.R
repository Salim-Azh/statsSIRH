nbPlaquette=length(which(influence$plaquette=="Oui"))
nbSite=length(which(influence$site_web=="Oui"))
nbAmi=length(which(influence$ami=="Oui"))
nbAutre=length(which(influence$autre=="Oui"))
nbPlaquette_Site=length(which(influence$plaquette=="Oui" & influence$site_web=="Oui"))# inter(plaquette et site)
nbPlaquette_Ami=length(which(influence$plaquette=="Oui"& influence$Ami=="Oui"))
nbPlaquette_Autre=length(which(influence$plaquette=="Oui" & influence$autre=="Oui"))
nbSite_Ami=length(which(influence$site_web=="Oui" & influence$ami=="Oui"))
nbSite_Autre=length(which(influence$site_web=="Oui" & influence$autre=="Oui"))
nbAmi_Autre=length(which(influence$autre=="Oui" & influence$ami=="Oui"))
nbPlaquetteSiteAmi=length(which(influence$plaquette=="Oui" & influence$site_web=="Oui" & influence$Ami=="Oui"))
nbPlaquetteSiteAutre=length(which(influence$plaquette=="Oui" & influence$site_web=="Oui" & influence$autre=="Oui"))
nbPlaquetteAmiAutre=length(which(influence$plaquette=="Oui" & influence$ami=="Oui" & influence$autre=="Oui"))
nbSiteAmiAutre=length(which(influence$ami=="Oui" & influence$site_web=="Oui" & influence$autre=="Oui"))
nbPlaquetteSiteAmiAutre=length(which(influence$plaquette=="Oui" & influence$site_web=="Oui" & influence$ami=="Oui" & influence$autre=="Oui"))
draw.quad.venn(area1=nbPlaquette,
           	area2=nbSite,
           	area3=nbAmi,
           	area4=nbAutre,
           	n12=nbPlaquette_Site,
           	n13=nbPlaquette_Ami,
           	n14=nbPlaquette_Autre,
           	n23=nbSite_Ami,
           	n24=nbSite_Autre,
           	n34=nbAmi_Autre,
           	n123=nbPlaquetteSiteAmi,
           	n124=nbPlaquetteSiteAutre,
           	n134=nbPlaquetteAmiAutre,
           	n234=nbSiteAmiAutre,
           	n1234=nbPlaquetteSiteAmiAutre,
           	category = c("Plaquette Papier", "Site", "Ami","Autre"), # titre des cercles
           	fill = c("orange", "red", "green","blue"), # couleurs des cercles
           	cex = 2,cat.cex = 2, # taille de la police
           	lty = "blank" # traits
)
