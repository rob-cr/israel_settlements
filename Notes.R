#Notes
revgeocode(as.numeric(c(35.292417, 32.914671)), output = "all")
#>>Keine Postal_codes bei Israel hinterlegt, auch wenn exakte Koordinaten genutzt werden wie oben

##Gegentest mit Bonn:
geocode("Bonn, Germany", output = "all")
#Bei Suche nach (großen) Stadtnamen auch nicht (weil mehrere Postal codes pro Stadt)

revgeocode(as.numeric(c(7.098207, 50.73743)), output = "all")
#Aber bei Suche nach exakten Koordinaten (die von Bonn Beispiel oben)
#$results[[5]]$address_components[[1]]$long_name
#[1] "53111"

#$results[[5]]$address_components[[1]]$short_name
#[1] "53111"

#Aber: In Israel sind diese Daten nicht hinterlegt! (Auch unter keiner anderen Bezeichnung, wie man bei output sehen kann)