heleur21_mod11_12$pvm <- paste(heleur21_mod11_12$Date, heleur21_mod11_12$Hour, sep="_")

#Tehd??n datasta aikasarjat
tseur=ts(heleur21_mod11_12$Price)
plot(tseur, ylab="Electricity price (???)")

#Lasketaan tunneittainen vaihtelu
lagged=lag(tseur, k=1) #Tunti 1->Tunti 2
hdifference=(lagged-tseur) #Tunti 2-Tunti 1
hdifferenceabs=abs(hdifference)
plot(hdifference, xlab="Time (h)", ylab="Change in cost (???)") #Difference between two sequential hours (???)
plot(hdifferenceabs, xlab="Time (h)",ylab="Change in cost, absolute value (???)")

reldif=(hdifferenceabs/tseur)*100
dif=(hdifference/tseur)*100
plot(reldif, xlab="Time (h)", ylab="Relative difference (%)")
plot(dif, xlab="Time (h)", ylab="Relative difference (%)")

#keskiarvo 24h v?leinrja 24h abs. Kuinka paljon muutos on esim. vitosen kohdalla 5->6
maara1=61
tunnit <- rep(0,24)
for(i in 1:(24*maara1-1)){
  if(i%%24==0){tunnit[24]=tunnit[24]+reldif[i]}
  else{
    tunnit[i%%24]=tunnit[i%%24]+reldif[i]
}
}
hkeskiarvo=tunnit/maara1
plot(hkeskiarvo, xlab="Hour in a day", ylab="Change abs (%)", col="blue")
lines(hkeskiarvo, xlab="Hour in a day", ylab="Change abs (%)", type="l")
#vikan tunti heitt?? 1/61, koska siit? puuttuu yks datapiste
  
#keskiarvo viikkotasolla abs

maara2=56/7
plkm=24*7
paivat <- rep(0,plkm)
for(i in 1:(24*56)){
  if(i%%plkm==0){paivat[plkm]=paivat[plkm]+reldif[i]}
  else{
    paivat[i%%plkm]=paivat[i%%plkm]+reldif[i]
  }
}
plot(paivat/maara2, xlab="Hour in a week", ylab="Change abs (%)", col="blue")
lines(paivat/maara2, xlab="Hour in a week", ylab="Change abs (%)", type="l")

#keskiarvo 24h v?leinrja 24h. Kuinka paljon muutos on esim. vitosen kohdalla 5->6
maara1=61
tunnit <- rep(0,24)
for(i in 1:(24*maara1-1)){
  if(i%%24==0){tunnit[24]=tunnit[24]+dif[i]}
  else{
    tunnit[i%%24]=tunnit[i%%24]+dif[i]
  }
}
hkeskiarvo=tunnit/maara1
plot(hkeskiarvo, xlab="Hour in a day", ylab="Change (%)", col="blue")
lines(hkeskiarvo, xlab="Hour in a day", ylab="Change (%)", type="l")
#vikan tunti heitt?? 1/61, koska siit? puuttuu yks datapiste

#keskiarvo viikkotasolla

maara2=56/7
plkm=24*7
paivat <- rep(0,plkm)
for(i in 1:(24*56)){
  if(i%%plkm==0){paivat[plkm]=paivat[plkm]+dif[i]}
  else{
    paivat[i%%plkm]=paivat[i%%plkm]+dif[i]
  }
}
plot(paivat/maara2, xlab="Hour in a week", ylab="Change (%)", col="blue")
lines(paivat/maara2, xlab="Hour in a week", ylab="Change (%)", type="l")

#keskiarvo hinnalle p?iv?tasolla
maara1=61
tunnit <- rep(0,24)
for(i in 1:(24*maara1-1)){
  if(i%%24==0){tunnit[24]=tunnit[24]+tseur[i]}
  else{
    tunnit[i%%24]=tunnit[i%%24]+tseur[i]
  }
}
hkeskiarvo2=tunnit/maara1
plot(hkeskiarvo2, xlab="Hour in a day", ylab="Price (???)", col="blue")
lines(hkeskiarvo2, xlab="Hour in a day", ylab="Price (???)", type="l")

#keskiarvo hinnalle viikkotasolla
maara2=56/7
plkm=24*7
paivat <- rep(0,plkm)
for(i in 1:(24*56)){
  if(i%%plkm==0){paivat[plkm]=paivat[plkm]+tseur[i]}
  else{
    paivat[i%%plkm]=paivat[i%%plkm]+tseur[i]
  }
}
plot(paivat/maara2, xlab="Hour in a week", ylab="Price (???)", col="blue")
lines(paivat/maara2, xlab="Hour in a week", ylab="Price (???)", type="l")

#Kulutuksen aikasarjat
tsvalaistus=ts(Kotka_data$Valaistus/1000000)
tspuhallin=ts(Kotka_data$Puhallins?hk?/1000000)
tslammitys=ts(Kotka_data$L?mmityss?hk?/1000000)
tsyhteensa=tsvalaistus+tspuhallin+tslammitys

#keskiarvo valaistukselle p?iv?tasolla
maara1=61
tunnit <- rep(0,24)
for(i in 1:(24*maara1-1)){
  if(i%%24==0){tunnit[24]=tunnit[24]+tsvalaistus[i]}
  else{
    tunnit[i%%24]=tunnit[i%%24]+tsvalaistus[i]
  }
}
hkeskiarvo3=tunnit/maara1
plot(hkeskiarvo3, xlab="Hour in a day", ylab="Consumption lighting (MW)", col="blue")
lines(hkeskiarvo3, xlab="Hour in a day", ylab="Consumption lighting (MW)", type="l")

#keskiarvo valaistukselle viikkotasolla
maara2=56/7
plkm=24*7
paivat <- rep(0,plkm)
for(i in 1:(24*56)){
  if(i%%plkm==0){paivat[plkm]=paivat[plkm]+tsvalaistus[i]}
  else{
    paivat[i%%plkm]=paivat[i%%plkm]+tsvalaistus[i]
  }
}
plot(paivat/maara2, xlab="Hour in a week", ylab="Consumption lighting (MW)", col="blue")
lines(paivat/maara2, xlab="Hour in a week", ylab="Consumption lighting (MW)", type="l")

#keskiarvo puhaltimille p?iv?tasolla
maara1=61
tunnit <- rep(0,24)
for(i in 1:(24*maara1-1)){
  if(i%%24==0){tunnit[24]=tunnit[24]+tspuhallin[i]}
  else{
    tunnit[i%%24]=tunnit[i%%24]+tspuhallin[i]
  }
}
hkeskiarvo4=tunnit/maara1
plot(hkeskiarvo4, xlab="Hour in a day", ylab="Consumption fans (MW)", col="blue")
lines(hkeskiarvo4, xlab="Hour in a day", ylab="Consumption fans (MW)", type="l")

#keskiarvo puhaltimille viikkotasolla
maara2=56/7
plkm=24*7
paivat <- rep(0,plkm)
for(i in 1:(24*56)){
  if(i%%plkm==0){paivat[plkm]=paivat[plkm]+tspuhallin[i]}
  else{
    paivat[i%%plkm]=paivat[i%%plkm]+tspuhallin[i]
  }
}
plot(paivat/maara2, xlab="Hour in a week", ylab="Consumption fans (MW)", col="blue")
lines(paivat/maara2, xlab="Hour in a week", ylab="Consumption fans (MW)", type="l")

#keskiarvo l?mmitykselle p?iv?tasolla
maara1=61
tunnit <- rep(0,24)
for(i in 1:(24*maara1-1)){
  if(i%%24==0){tunnit[24]=tunnit[24]+tslammitys[i]}
  else{
    tunnit[i%%24]=tunnit[i%%24]+tslammitys[i]
  }
}
hkeskiarvo5=tunnit/maara1
plot(hkeskiarvo5, xlab="Hour in a day", ylab="Consumption heating (MW)", col="blue")
lines(hkeskiarvo5, xlab="Hour in a day", ylab="Consumption heating (MW)", type="l")

#keskiarvo l?mmitykselle viikkotasolla
maara2=56/7
plkm=24*7
paivat <- rep(0,plkm)
for(i in 1:(24*56)){
  if(i%%plkm==0){paivat[plkm]=paivat[plkm]+tsvalaistus[i]}
  else{
    paivat[i%%plkm]=paivat[i%%plkm]+tsvalaistus[i]
  }
}
plot(paivat/maara2, xlab="Hour in a week", ylab="Consumption heating (MW)", col="blue")
lines(paivat/maara2, xlab="Hour in a week", ylab="Consumption heating (MW)", type="l")

#keskiarvo yhteens? p?iv?tasolla
maara1=61
tunnit <- rep(0,24)
for(i in 1:(24*maara1-1)){
  if(i%%24==0){tunnit[24]=tunnit[24]+tsyhteensa[i]}
  else{
    tunnit[i%%24]=tunnit[i%%24]+tsyhteensa[i]
  }
}
hkeskiarvo6=tunnit/maara1
plot(hkeskiarvo6, xlab="Hour in a day", ylab="Consumption all (MW)", col="blue")
lines(hkeskiarvo6, xlab="Hour in a day", ylab="Consumption all (MW)", type="l")

hkeskiarvoyht=hkeskiarvo5+hkeskiarvo4 #puhaltimet + lammitys
plot(hkeskiarvo6, xlab="Hour in a day", ylab="Consumption (MW)", col="blue", ylim = c(0,1.5))
lines(hkeskiarvo6, type="l", col="blue")
lines(hkeskiarvo5, type="b", col="red") #lammitys
lines(hkeskiarvo5, type="l", col="red")
lines(hkeskiarvoyht, type="b", col="green")
lines(hkeskiarvoyht, type="l", col="green")

#keskiarvo yhteens? viikkotasolla
maara2=56/7
plkm=24*7
paivat <- rep(0,plkm)
for(i in 1:(24*56)){
  if(i%%plkm==0){paivat[plkm]=paivat[plkm]+tsyhteensa[i]}
  else{
    paivat[i%%plkm]=paivat[i%%plkm]+tsyhteensa[i]
  }
}
plot(paivat/maara2, xlab="Hour in a week", ylab="Consumption all (MW)", col="blue")
lines(paivat/maara2, xlab="Hour in a week", ylab="Consumption all (MW)", type="l")
