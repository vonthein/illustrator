# Icons
# "circles" are smoothed through 36 points
ellipsx <- c(0.5*sin(pi*(0:36)/18),NA)
ellipsy <- c(0.5*cos(pi*(0:36)/18),NA)
loop <- cbind(ellipsx,ellipsy)
cod <- matrix(c(
94,-14, 80,-12, 75,-20, 74,-20, 52,-18, 39,-19, 38,-19, 39,-19, 39,-21, 38,-24, 39,-27,
37,-25, 37,-23, 38,-18, 37,-23, 35,-24, 34,-26, 33,-24, 35,-19, 36,-17, 35,-19, 30,-19,
26,-18, 20,-16, 10,-14, 3,-11, 3,-13, 3,-10, 1,-9, 1,-8, 8,-6, 10,-7, 5,-4, 3,-3, 1,-2,
0,0, 1,2, 3,3, 5,4, 30,10, 33,10, 43,20, 48,23, 50,23, 51,16, 50,9, 53,9, 59,17, 64,16,
74,7, 94,10, 98,4, 101,4, 104,6, 116,13, 117,8, 117,0, 119,-1, 117,-2, 117,-10, 116,-15,
104,-8, 101,-6, 97,-6, 96,-10, 94,-14,
NA,NA, 13,4.8, 13.5,4.4, 13.7,4, 14,3.3, 13.7,2.6, 13,2.2, 12.5,2.6,
12.3,2.9, 12,3.5, 12.3,4,12.5,4.4, 13,4.8,
NA,NA, 33,10, 50,9, 53,9, 74,7, 98,4, 108,3, 110,2, 111,-1, 108,-4,97,-6, 80,-12, 52,-18,
NA,NA, 39,-5, 42,-10, 45,-13, 54,-11, 54,-9, 49,-3, 40,-1 ,NA,NA
),ncol=2,byrow=T)
cod <- cbind(cod[,1],cod[,2]+27)/8
Gadus.morhua <- cod; Dorsch <- cod
#
zebrafish <- matrix(c(
0,6.5, 0,5, 1,4, 3,3, 5,2, 6,1.7, 8,1.5, 6,3, 8,1.5, 9,0, 9.7,1, 10,2, 7,3, NA,NA, # Maul, Brust
11.3,1.6, 12,0, 13,1, 13.5, 1.5, 13.5,2.5, 11.5,2, NA,NA, # Brustflosse vorn
9.9,1.5, 11.4,1.5, NA,NA, # Bauch
13.5,1.5, 15,2, 24,4, 26,4, 32,1, 33,2, 33.5,3, 33,5, 32,5.5, 31.5,6, 31.5,7,
33,9, 33,11, 32,12, 31,12, 26,8, 20,8, 14,9, 5,8.5, 3,8, 1,7, 0,6.5, NA,NA, # Schwanz, R?cken
15,2, 16,1, 18,0, 19,0, 20,1, 22,3, 24,4, NA,NA, # Bauchflosse
14,9, 17.5,12, 19,12.2, 20,11, 20,8, NA,NA, # Rückenflosse
0,6.5, 0.7,6.5, 1.7,4.5, 2.8,4, 3.5,3.8, 4.5,4, 5,5, 5,6, 4,7, NA,NA, # Kopfschild
1.5,6.5, 1.7,6.2, 2.5,5.8, 2.8,6, 3,6.2, 3,6.4, 2.7,6.7, 2,7, 1.8,6.9,
1.5,6.5, NA,NA, # Augenring
5.5,1.8, 5.5,0, 6.5,0, 7.5,0.5, 7,1.6, NA,NA, # Brustflosse hinten
1.9,6.5, 2.6,6.2, 2.5,6.5, 1.9,6.5, NA,NA, # Auge
#5,3, 6,4, 6,5, NA,NA,# 6.5,7.5, 8,3, NA,NA, # Kieme, Streifenbeginn
6.7,7, 32,8, NA,NA, 7,6, 31,7, NA,NA, 7.5,5, 31,6, NA,NA, 8,4, 32,5, # Streifen
NA,NA)/2.5,ncol=2,byrow=T)
Danio.rerio <- zebrafish; Zebrabärbling <- zebrafish
#plot(zebrafish,type="l")
#polygon(zebrafish,col="white")
#
drosophila <- matrix(c(2,2.5, 1.5,3, 1.5,4, 2,4.5, 3,4.5, 3.5,4, 3.5,3,
3,2.5, 2,2.5, NA,NA,#Auge
2,1, 1,2, 0,1.5, 1,2, 0.5,3, 0,3, 0.5,3, 0.5,4, 0,4, 0.5,4, 1,4.5, 2,6, 3,6.5,
4,6, 4.5,5, 4.5,4, 4,3, 3,1.5, 2,1, NA,NA, #Kopf
4.5,5, 5,6, 6,7, 9,8, 13,9, 16,9, 18,8.5, 19,8, 19,7, 18.5,6.5, 19,6,
18.5,5.5, 17,4.7, 18,4, 18,2, 14,2, 10.5,3, 9,4, 7.5,3, 5.5,3, 5,3.3,
4.5,4, NA,NA, # Flügel, Brust und Hinterleib
18.5,6.5, 17,6, 14,5.8, 8,6, NA,NA, 17,4.7, 13,4.5, 8,5, NA,NA, #Fl?gel
6,3, 6,2.2, 4,2.2, 3,1.2, 1.5,0, 3,1, 4.1,2, 6.2,2, 6.2,3, NA,NA,
7,3, 7.5,0.5, 10.5,0, 7.6,0.6, 7.2,3, NA,NA,
7.5,3.2, 9.5,2.1, 12,3.5, 13.3,1.3, 17,1, 13.3,1.2, 12,3.4, 9.5,1.9, 7.5,3, NA,NA) # Beine
,ncol=2,byrow=T)*0.8
fly <- drosophila; Fruchtfliege <- fly
#plot(drosophila ,type="l");polygon(drosophila ,col="gold")
Drosophila.melanogaster <- fly
#
germ <- matrix(c(0.2,2, 0.6, 1.4, 1,1, 1.5,0.6, 1.7,0.5, 2,0.5, 4,1.3, 9,3, 13,4, 14,4.2,
14.5,4.5, 14.7,5, 14.5,6, 21,7, 28,7, 21,7, 14.5,6, 21,6.8, 28,6.9, 21,6.8,
14.5,6, 14,7, 13.5,7.2, 13,7.3, 11,7, 7,6, 1,4, 0.2,3.5, 0.1,3, 0.2,2, NA,NA)
,ncol=2,byrow=T)/2
#plot(germ ,type="l");polygon(germ ,col="lightgrey")
E.coli <- germ; Escherichia.coli <- germ
#
circ <- cbind(cos(2*pi*(-45:315)/360),sin(2*pi*(-45:315)/360))
yeast <- rbind(cbind(circ[c(15:180,200:345),1]+2,circ[c(15:180,200:345),2]*2+8),
cbind(circ[c(225:360,1:135),1]*0.5+2.9,circ[c(225:360,1:135),2]*0.5+6.5),NA, # top
cbind(circ[c(330:360),1]+1,circ[c(330:360),2]+4),
cbind(circ[c(1:180),1]+1.5,circ[c(1:180),2]+4.4),
cbind(circ[c(180:280),1]+1,circ[c(180:280),2]+4),
cbind(circ[c(185:360,1:105),1]+1.5,circ[c(185:360,1:105),2]+5)*0.5,
circ[330,]+c(1,4),NA, # left
cbind(circ[,1]+3,circ[,2])*0.5,NA, # bottom
cbind(circ[c(280:360,1:100),1]+6,circ[c(280:360,1:100),2]+1),
cbind(circ[100:280,1]+5,circ[100:280,2]+2),circ[280,]+c(6,1),NA, # bottom right
cbind(circ[,1]+6,circ[,2]*1.4+6)%*%
matrix(c(cos(-0.2),sin(-0.2),-sin(-0.2),cos(-0.2)),2),NA)# right
#plot(yeast ,type="l");polygon(yeast ,col="lightgrey")
saccharomyces <- yeast ; Saccharomyces.cerevisiae <- yeast; S.cerevisiae <- yeast
Hefe <- yeast
#
circ <- cbind(cos(2*pi*(0:360)/360),sin(2*pi*(0:360)/360))
egg <- rbind(cbind(circ[0:180,1]+1,circ[0:180,2]*2+1),
cbind(circ[180:360,1]+1,circ[180:360,2]+1),NA)*3
Ei <- egg
#plot(egg ,type="l");polygon(egg ,col="white")
#hen <- matrix(c( NA,NA)
#,ncol=2,byrow=T)/2
#Huhn <- hen
#
dish <- cbind(cos(2*pi*(-45:315)/360),sin(2*pi*(-45:315)/360))
dish <- 5*rbind(dish,NA,dish*0.9,NA)+5
Petrischale <- dish
#plot(dish,type="l");polygon(dish,col="gold")
#
fir <- matrix(c(0,0.5, 4,0, 8,0.5,
5.5,3, 7,3, 5,5.5, 6,5.5, 4.5,8, 4,9.5,
3.5,8, 2,5.5, 3,5.5, 1,3, 2.5,3, 0,0.5, NA,NA),16,2,T)
Nadelbaum <- fir; Tanne <- fir
#
bush <- matrix(c(1,0, 5,0, 6,2, 7,4, 7,6, 6,8.5, 4,10,
                 2,10, 0,8.5, -1,6, -1,4, 0,2, 1,0, NA,NA),14,2,T)
Busch <- bush
#
rat <- matrix(c(46,0, 0,0, 5,5, 8,7, 10,8, 12,7, 13,6, 20,10, 24,11,
27,11, 29,9, 30,8, 31,6, 32,0, 0,0, NA,NA),ncol=2,byrow=T)
Ratte <- rat; Rattus.norvegicus <- rat
mouse <- rat/3
Maus <- mouse; Mus.musculus <- mouse
#
man <- matrix(c(1.1,0, 1.8,0, 2.5,3.5, 3.2,0, 3.9,0,
 3.7,3.5, 3.8,7, 4,6.6, 4.7,4,
5,4, 4.5,6.5, 4.1,7.7, 3.5,7.8, 3,7.9, 3,8.2, 3.3,8.4, 3.5,8.9, 3.3,9.4, 2.8,9.7,
2.2,9.7, 1.7,9.4, 1.5,8.9, 1.7,8.4, 2,8.2, 2,7.9, 1.5,7.8, 0.9,7.7, 0.5,6.6, 0,4,
0.3,4, 1,6.5, 1.2,7, 1.3,3.5, 1.1,0, NA,NA),ncol=2,byrow=TRUE)
man[,1] <- man[,1]/2;
man <- 1.2*man; male <- man; Mann <- man
# plot(man,type="l")
woman <- matrix(c(1.3,0, 1.8,0, 2.5,3.5, 3.2,0, 3.7,0, 4.1,3.6, 4,4.5, 3.8,5.2, 3.6,5.5,
                3.8,7, 4,6.6, 4.7,4,
                5,4, 4.5,6.6, 4,7.6, 3.2,7.8, 3,7.9, 3,8.2, 3.4,8.5, 3.6,8.9, 3.4,9.4, 2.8,9.7,
                2.2,9.7, 1.6,9.4, 1.4,8.9, 1.6,8.5, 2,8.2, 2,7.9, 1.8,7.8, 1,7.6, 0.5,6.6, 0,4,
                0.3,4, 1,6.6, 1.2,7, 1.4,5.5, 1.2,5.2, 1, 4.5, 0.9,3.6, 1.3,0, NA,NA),ncol=2,byrow=TRUE)
woman[,1] <- woman[,1]/2;
woman <- 1.2*woman; female <- woman; Frau <- woman
# plot(woman,type="l") # plot(woman,type="b",pch=".")
human <- matrix(c(1.2,0, 1.9,0, 2.5,3.5, 3.1,0, 3.8,0,
                  3.8,0, 3.8,7, 4,6.6, 4.7,4,
                  5,4, 4.5,6.6, 4,7.6, 3.5,7.8, 3,8, 3,8.3, 3.5,8.5, 3.7,9, 3.5,9.5, 3,9.8,
                  2,9.8, 1.5,9.5, 1.3,9, 1.5,8.5, 2,8.3, 2,8, 1.5,7.8, 1,7.6, 0.5,6.6, 0,4,
                  0.3,4, 1,6.6, 1.2,7, 1.2,0, NA,NA),ncol=2,byrow=TRUE)
human[,1] <- human[,1]/2;
human <- 1.2*human
# plot(human,type="l")
Homo.sapiens <- human; H.sapiens <- human; Mensch <- human#
h <- length(human[,1])
family <- rbind(woman,
                matrix(c(2.33,0),ncol=2,nrow=h,T)+0.4*human,
                matrix(c(3.35,0),ncol=2,nrow=h,T)+0.7*human,
                matrix(c(5.3,0),ncol=2,nrow=length(man[,1]),T)+1.08*man)
family[,1] <- family[,1]*1.5
Familie <- family
# plot(family,type="l")
family2 <- rbind(0.4*human,
                matrix(c(0.55,0),ncol=2,nrow=length(woman[,1]),T)+woman,
                matrix(c(3.53,0),ncol=2,nrow=length(man[,1]),T)+1.08*man,
                matrix(c(6.6,0),ncol=2,nrow=h,T)+0.7*human)
family2[,1] <- family2[,1]*1.5
Familie2 <- family2
# plot(family2,type="l")

  man[,1] <-   man[,1]*1.5; Mann <- man
woman[,1] <- woman[,1]*1.5; Frau <- woman
human[,1] <- human[,1]*1.5; Mensch <- human; Homo.sapiens <- human; H.sapiens <- human


bone <- matrix(c(1.5,5, 1.5,4.5, 0.5,3, 0.5,2, 1,1, 3,0.5, 7,0.5,
10,1, 12,2, 15,3, 17,3.2, 30,3.2),
               ncol=2,byrow=TRUE)
bone <- rbind(bone,cbind(60-bone[12:1,1],   bone[12:1,2]),
                   cbind(60-bone[,1],    10-bone[,2]),
                   cbind(   bone[12:1,1],10-bone[12:1,2]),c(NA,NA))
bone <- cbind(bone[,1]/4,bone[,2]/1.5)
Knochen <- bone
vase <- matrix(c(6,10, 2,10, 3,9, 3,8, 2,6, 1,5, 0,4, 0,2, 1,1, 3,0, 5,0),ncol=2,byrow=TRUE)
vase <- rbind(vase,cbind(12-vase[10:1,1],vase[10:1,2]),NA)/1.5
Vase <- vase
#
shell <- matrix(c(2.5,0, 0.5,1.7, 0,2, 0,3, 1,4, 2,4.3,
4,4.3, 5,4, 6,3, 6,2, 5.5,1.7, 3.5,0, 2.5,0,
NA,NA, 2.5,1.0, 1.0,2.7, NA,NA, 2.8,1.5, 2,3.5,
NA,NA, 3.2,1.5, 4,3.5, NA,NA, 3.5,1.0, 5,2.7, NA,NA),ncol=2,byrow=TRUE)*1.5
Muschel <- shell
#
pig <- matrix(c(6,0, 5.7,1, 5,2, 4.5,2.5, 5,2, 1.3,2.5, 1,3, 2,4, 3,5, 4,6, 5,6.5,
6,7, 8,7.0, 10,7.0, 12,7, 13,6.5, 14,5.5, 14.3,5, 14.3,3, 14,2, 13.5,0, 13,0,
13,2, 12,2.5, 11,3, 10.5,3.5, 10,4.5, 10.5,3.5, 11,3, 10,2, 7,2, 7.3,3.3,
7,2, 6.5,0, 6,0, NA,NA, 2.6,4, 3,2.5, 4,4.4, NA,NA),ncol=2,byrow=TRUE)
Sus.scrofa <- pig; S.scrofa <- pig
Schwein <- pig
#
fruit <- matrix(c(3,0, 1,1, 0,2, 0,6, 1,7, 2,7, 3,6.5, 5,6.5, 6,7, 7,7, 8,6,
8,2, 7,1, 5,0, 3,0, NA,NA),ncol=2,byrow=TRUE)
Frucht <- fruit
#
rabbit <- matrix(c(1.999,0, 1.5,0.5, 2,1, 2.5,0.5, 2.001,0,
3,0.5, 4.5,1, 4.5,2.5, 4,3.3, 3.3,4, 2.5,4.5,
1.5,4.5, 0.7,4, 0,3.3, -0.5,2.5, -0.5,1, 1,0.5, 2,0, NA,NA,
1.5,5, 1,6, 1,8, 1.5,7, 2,5, 2.5,7, 3,8, 3,6, 2.5,5,
2.6,4.7, 2.5,4.5, 1.5,4.5, 1.4,4.7, 1.5,5,
NA,NA),ncol=2,byrow=TRUE)
#plot(rabbit,type="l");polygon(rabbit,col="brown")
Oryctolagus.cuniculus <- rabbit; O.cuniculus <- rabbit
Kaninchen <- rabbit
#
grain <- matrix(c(7,7, 3.5,3.5, 2.5,3.5, 1,3, 0,1, 0,0, 1,0, 3,1, 3.5,2.5,
3.5,3.5, NA,NA),ncol=2,byrow=TRUE)
wheat <- kronecker(rep(1,7),grain)+cbind(2.5,3.5*rep(0:6,each=11))
wheat <- rbind(wheat,NA,cbind(5-wheat[,1],wheat[,2]),
               matrix(c(2.5,2.5,0,-10),2,2),NA)*0.3+3
Triticum <- wheat; Weizen <- wheat
#plot(wheat,type="l",col="gold",lwd=1)
#
arabidopsis <- matrix(c(11,10, 9,14, 7,17, 5,18, 3,18, 2,17, 2,15, 3,14, 5,13, 9,12, 10,10,
10,9, 3,9, 2,8, 2,7, 3,6, 6,6, 10,8,
11,8, 10,6, 8,5, 7,3, 7,0, 8,0, 9,1, 10,4, 12,8, 10,4,
10,4, 11,3, 13,3, 14,4, 14,6, 13,8,
14,8, 15,6, 17,5, 19,5, 21,6, 22,7, 22,8, 21,9, 16,8, 15,9,
13,9, 15,9, 22,12, 24,14, 24,16, 23,17, 22,16, 20,14, 18,11, 15,10,
12,10, 15,12, 16,16, 16,17, 15,18, 14,18, 13,17, 13,12, 11,10,NA,NA,
12,10, 12,28, 11.5,28, 11.5,9, 10,9, NA,NA,
12,28, 12.5,30, 12.5,31, 12.5,32, 12,33, 11.5,32, 11.5,30, 12,28,
NA,NA),ncol=2,byrow=TRUE)/2
A.thaliana <- arabidopsis
Arabidopsis.thaliana <- arabidopsis
Ackerschmalwand <- arabidopsis
#plot(arabidopsis,type="l")
#polygon(arabidopsis,col="green")
#
starfish <- matrix(c(1,2.4, 0.6,3, 1,3.8, 2,4.7, 3,5.3, 4,5.7, 4.5,6, 4,6.8, 3.4,7,
2.6,7.2, 1.8,7.6, 2.5,8.2, 3,8.4, 4,8.6, 5,8.4, 5.7,8, 6.9,7.1, 8,7.8, 9.2,8.2,
10.3,8.3, 11.4,8.2, 11.4,8.2, 12.4,7.7, 11,7.3, 10.5,6.9, 9.6,6, 10,5.5, 11.4,5,
14,3, 13.2,2.8, 12,3, 11,3.3, 9.9,3.8, 9.5,3.7, 9.2,3, 9.1,2, 9.1,1, 9,0.8,
8,0.8, 7,1.9, 6.2,3.2, 5.7,3.9, 4.8,3.9, 4,3.6, 3,3, 2,2.5, 1,2.4, NA,NA),
ncol=2,,byrow=TRUE)
starfish <- cbind(0.65*starfish[,1],starfish[,2])
#plot(rbind(starfish,#starfish%*%matrix(c(-1,0,0,-1),2),
#starfish%*%matrix(c(-0.71,0.29,0.29,0.71),2)-1#,
#starfish%*%matrix(c(0.71,0.29,-0.29,0.71),2)-3.4
#),
#type="l");polygon(starfish,col="pink")
#
Seestern <- starfish
worm <- matrix(c(0,7, 0,7.2, 0.2,7.4, 01,7.5, 2,7.5, 3,7.3, 4,7, 5,6.2, 6,5.1,
7,4, 8,3, 9,2.2, 10,1.8, 11,1.8, 12,2, 15,2.6, 16,2.8, 17,2.8, 18,2.6, 19.5,2,
20,1.2, 20.4,0, 20,0.8, 19,1.3, 18,1.5, 17,1.6, 16,1.5, 15,1.4, 14,1,13,0.6, 12,0.3,
11,0.1, 10,0.2, 9,0.4, 8,1, 5,4, 4,5.2, 3,5.8, 2,6.2, 1,6.5,
0.5,6.7, 0.2,6.8, 0.1,6.9, 0,7, NA,NA),
ncol=2,,byrow=TRUE)
#plot(worm,type="l");polygon(worm,col="lightgray")
Caenorhabditis.elegans <- worm
C.elegans <- worm
Wurm <- worm
#
star <- matrix(c(0.3,1.5, 2,4, 1.5,1, 4,1.5, 2,-0.5, 3.5,-3, 1,-1.5, 0.3,-4.5),
ncol=2,,byrow=TRUE);star <- cbind(star[,1]-0.3,star[,2])
star <- rbind(star,star[8:1,]%*%matrix(c(-1,0,0,1),2,2),c(NA,NA))+4
#plot(rbind(star,#star%*%matrix(c(-1,0,0,-1),2),
#star%*%matrix(c(-0.71,0.29,0.29,0.71),2)-1#,
#star%*%matrix(c(0.71,0.29,-0.29,0.71),2)-3.4
#),
#type="l");polygon(star,col="yellow")
Stern <- star
#
wheel <- cbind(cos(2*pi*(-45:315)/360),sin(2*pi*(-45:315)/360))+1
Rad <- wheel
bike <- matrix(c(1,1, 1.7,3.2, 2.2,3.2, NA,NA, 1.5,2.5, 3.7,2.5, 3.3,1, 1.5,2.3, NA,NA,
3.5,3, 4,3, 4,2.9, 3.82,2.9, 3.7,2.5, 4.7,1, 3.3,1, NA,NA),ncol=2,byrow=TRUE)
bike <- rbind(wheel,NA,cbind(wheel[,1]+3.7,wheel[,2]),NA,bike)*2
Fahrrad <- bike
#plot(bike,type="l",col="darkblue",lwd=15)
#
w <- 2*wheel
flower <- rbind(wheel,NA,
cbind(w[1:250,1]-2.2,w[1:250,2]+1.4),NA,
cbind(w[73:322,1]-3.67,w[73:322,2]-1.41),NA,
cbind(c(w[c(145:360,1:36),1])-1.48,c(w[c(145:360,1:36),2])-3.63),NA,
cbind(c(w[c(217:360,1:108),1])+1.4,c(w[c(217:360,1:108),2])-2.2),NA,
cbind(c(w[c(289:360,1:180),1])+0.9,c(w[c(289:360,1:180),2])+0.9),NA)
flower <- flower+3.67
Blüte <- flower; Blume <- flower
#plot(flower,type="l",col="darkblue",lwd=1)
#polygon(flower,col="yellow")
#points(wheel[c(51,123,195,267,339),])

thetahat <- matrix(c(4,10, 6,12, 8,10, NA,NA,
                     1.5,4, 2.5,4, 2,2, 2.5,1, 3,0.5, 4,0, 5,0.2, 6,1, 7.5,4, 8,6, 8,7, 7,8.5, 6,9,
                     5,9, 4,8, 3.2,6, 3.2,5, 4,3.7, 6,3.3, 8,4, NA,NA),
                   ncol=2,byrow=TRUE)
thetadach <- thetahat
# plot(thetahat,type="l")#;polygon(thetahat,col="brown")
betahat <- matrix(c(3,12, 5,15, 7,12, NA,NA,
                    0,0, 4,9, 5,10, 6,9, 6,7, 5,6, 6,7, 7,6,
                    7,5, 6,4, 4,4, 3,5, NA,NA),ncol=2,byrow=TRUE)
# plot(betahat,type="l")#;polygon(betahat,col="brown")
betadach <- betahat
alphahat <- matrix(c(2,7, 4,9, 6,7, NA,NA,
                     6,6, 5,5, 3,1, 2,0, 1,0, 0,1, 0,2, 0.8,4.3, 2,5, 3,5,
                     5,1, 6,0, 7,1, NA,NA),ncol=2,byrow=TRUE)
# plot(alphahat,type="l")#;polygon(alphahat,col="brown")
alphadach <- alphahat
house <- matrix(c(0,4, 4,8, 8,4, NA,NA, 6,6, 6,7, NA,NA,
                  1,0, 1,5, 7,5, 7,0, 1,0, NA,NA,
                  6,0, 6,3, 5,3, 5,0, NA,NA,
                  2,1, 2,3, 4,3, 4,1, 2,1, NA,NA),
                ncol=2,byrow=TRUE)
Haus <- house
# plot(house,type="l");polygon(house,col="orange")
mill <- matrix(c(0.4,0, 0.4,10, 1,10, 1,4, 3,6, 3,4, 5,6, 5,4, 7,6, 7,0,
                 0.4,0, NA,NA),
                ncol=2,byrow=TRUE)
factory <- mill; Fabrik <- factory
# plot(mill,type="l");polygon(mill,col="orange")
mosque <- rbind(cbind(-loop[10:1,1],loop[10:1,2]),loop[1:10,], # cupola
                matrix(c(0.5,0.8, 0.55,1, 0.6,0.8, 0.6,-0.5, -0.6,-0.5, # minaretts
                         -0.6,0.8, -0.55,1, -0.5,0.8, -0.5,0,
                       NA,NA),
               ncol=2,byrow=TRUE)) + matrix(c(0.6,0),ncol=2,nrow=30,byrow=TRUE)
Moschee <- mosque
# (mosque,type="l");polygon(mosque,col="orange")
KKW <- mosque[-c(22,26,27,28),]
KKW [24,] <- c(+0.1,-0.5)
nuclear.powerplant <- KKW; Kernkraftwerk <- KKW
Atomkraftwerk <- Kernkraftwerk; AKW <- Atomkraftwerk; NPP <- AKW
# plot(AKW,type="l");polygon(AKW,col="orange")
car <- rbind(matrix(c(-0.5,0.5, -0.3,1.5, 2.3,1.5, 2.5,0.5, 2.6,0.5, 2.4,1.6, -0.4,1.6, -0.6,0.5, # roof
                      -0.6,-0.6, -0.4,-0.6, -0.4,-1, 0.4,-1, 0.4,-0.6, 1.6,-0.6, 1.6,-1, 2.4,-1, 2.4,-0.6, # wheels
                      2.6,-0.6, 2.6,0.5), # grill
                       ncol=2,byrow=TRUE),
              cbind(0.7*loop[-38,1]+2,0.7*loop[-38,2]),0.7*loop[-38,],c(-0.6,0.5), NA)  # lights
Auto <- car; Pkw <- Auto
# plot(car,type="l");polygon(car,col="orange")
bulb <- matrix(c(3.5,0, 3,0.25, 3.5,0.5, 3,0.75, 3.5,1, 3,1.25, 3.5,1.5, 3,1.75,
                 3,2, 3,2.5, 2,3.5, 1.8,4.7, 2,6, 2.8,7.1, 4,7.5,
                 5.2,7.1, 6,6, 6.2,4.7, 6,3.5, 5,2.5, 5,2, 4.5,1.75, 5,1.5,
                 4.5,1.25, 5,1, 4.5,0.75, 5,0.5, 4.25,0, 3.5,0,
                 NA,NA, 1.5,4.5, 0,4.5, NA,NA, 2,7, 0.5,8.5, NA,NA,
                 4,8, 4,10, NA,NA, 6,7, 7.5,8.5, NA,NA, 6.5,4.5, 8,4.5,
                 NA,NA),ncol=2,byrow=TRUE)
Glühbirne <- bulb
# plot(bulb,type="l",col="orange");polygon(bulb,col="yellow")
glas <- rbind(cbind(2 + 4 * loop[,1], 1 + loop[,2]), # Fuß
              matrix(c(NA,NA, 2.2,5.03, 2.2,1.3),ncol=2,byrow=TRUE), # Stil
              cbind(2 + 0.4 * loop[10:28,1],1.2 + 0.1 * loop[10:28,2]), # Stil unten
              matrix(c(1.8,1.3, 1.8,5.03, NA,NA),ncol=2,byrow=TRUE), # Stil links
              matrix(c(NA,NA, 4,12, 4,11),ncol=2,byrow=TRUE), # Kelch rechts
              cbind(2 + 4 * loop[10:27,1],9 + 8 * loop[10:27,2]), # Kelch unten
              matrix(c(0,11, 0,12, NA,NA),ncol=2,byrow=TRUE), # Kelch links
              cbind(2 + 4 * loop[,1],12 + 1 * loop[,2])) # Kelch oben
Glas <- glas
#plot(glas,type="l",col="orange");polygon(glas,col="yellow")
bottle <- rbind(matrix(c(NA,NA, 2.5,12, 2.5,10, 2.7,9.4, 3.3,8.4,# Hals rechts
                         3.75,7.4, 4,6, 4,1),ncol=2,byrow=TRUE), # Breite rechts
              cbind(2 + 4 * loop[10:27,1],1 + 2 * loop[10:27,2]), # Breite unten
              matrix(c(0,1, 0,6, 0.25,7.4, 0.7,8.4, # Breite links
                       1.3,9.4, 1.5,10, 1.5,12, NA,NA),ncol=2,byrow=TRUE), # Hals rechts
              cbind(2 + 1 * loop[,1],12 + 0.2 * loop[,2])) # Hals oben
Flasche <- bottle
#polygon(bottle,col="yellow");plot(bottle,type="l",col="orange")

paper <- matrix(c(1,0, 11,0, 11,14, 10,14, 10,1, 1,1, 1,0, NA,NA,
                  0,1, 0,15, 10,15, 10,1, 0,1, NA,NA),ncol=2,byrow=TRUE)
Papier <- paper
# plot(paper,type="l",col="orange");polygon(paper,col="yellow")
book <- matrix(c(7,2, 6.6,1.8, 6.4,1.5, 6.35,1, 6.4,0.5, 6.6,0.2, 7,0, 6.95,1.9, NA,NA,
                 7,0, 0.9,0, NA,NA, 0.9,0, 0.5,0.2, NA,NA, 0.5,0.2, 0.3,0.5, NA,NA,
                 0.3,0.5, 0.25,1, NA,NA,
                 0.25,1, 0.3,1.5, 0.5,1.8, 0.9,2, 7,2, 6.8,11, 1,11, 0.6,10.8, 0.4,10.5,
                 0.35,10, 0.25,1, NA,NA, 0.9,2, 1,11, NA,NA),ncol=2,byrow=TRUE)
Buch <- book
# plot(book,type="l",col="orange");polygon(book,col="yellow")
open.book <- matrix(c(6,1, 6,11, NA,NA,
                      .5,11, .5,12.1, NA,NA, .5,12.1, 4,12, NA,NA, 4,12, 5,11.7,
                      NA,NA, 5,11.7, 6,11, NA,NA,  6,11, 7,11.7, NA,NA,
                      7,11.7, 8,12, NA,NA, 8,12, 11.5,12.1, NA,NA, 11.5,12.1,
                      NA,NA, 11.5,12.1, 11.5,11, NA,NA,
                      6,1, 5,1.7, 4,2, 0.5,2.2, 0.5,11,
                      0,11, 0,2, 5,1, 5.5,0.6, 6,0.5, 6.5,0.6, 7,1, 12,2, 12,11,
                      11.5,11, 11.5,2.2, 8,2, 7, 1.7, 6,1,
                      NA,NA),ncol=2,byrow=TRUE)
offenes.Buch <- open.book
# plot(open.book,type="l",col="orange");polygon(open.book,col="yellow")
heart <- matrix(c(0,0, 0.2,0.6, 0.5,1.2, 0.8,1.7, 1.1,2.6, 1.2,3.4, 1.2,4.6, 1,5.6,
                  0.7,6, 0.4,6, 0.2,5.7, 0,5, -.2,5.7, -.4,6, -.7,6, -1,5.6,
                  -1.2,4.6, -1.2,3.4, -1.1,2.6, -.8,1.7, -.5,1.2, -.2,.6, 0,0, NA,NA),ncol=2,byrow=TRUE)
heart <- cbind(2*heart[,1],heart[,2])
Herz <- heart
# plot(heart,type="l",col="darkred");polygon(heart,col="red")
drops <- rbind(loop[6:31,], 0:1, loop[6,], NA)
Tropfen <- drops
# plot(drops,type="l",col="darkred");polygon(drops,col="red")
syring <- matrix(c(0,0, 0,-0.5, 0,0, -0.2,0, -0.2,0.7, 0.2,0.7, 0.2,1, -0.2,1,
                    -0.2,0.7, -0.2,1, 0,1, 0,1.4, -0.2,1.4, 0.2,1.4, 0,1.4, 0,0.7, 0,1,
                    0.2,1, 0.2,0, 0,0, NA,NA),ncol=2,byrow=TRUE)
syringe <- rbind(cbind(0.4*loop[1:36,1],1.4+0.1*-loop[1:36,2]),
                 cbind(0.4*loop[37:1,1],1.4+0.1*-loop[37:1,2]), NA, # oben
                 cbind(0.4*loop[2:36,1],1+0.1*loop[2:36,2]),
                 cbind(0.4*loop[36:2,1],1+0.1*loop[36:2,2]), NA, # oberer Rand
                 matrix(c(-0.2,1, -0.2,0),ncol=2,byrow=TRUE),
                 cbind(0.4*-loop[9:27,1],0.1*loop[9:27,2]), # unterer Rand
                 matrix(c(0.2,0, 0.2,1, 0.2,0.8),ncol=2,byrow=TRUE),
                 cbind(0.4*loop[9:36,1],0.7+0.1*-loop[9:36,2]),
                 cbind(0.4*loop[1:27,1],0.7+0.1*-loop[1:27,2]),
                 matrix(c(NA,NA, -0.03,1.35, -0.03,0.7, # Stempel
                          NA,NA, 0.03,1.35, 0.03,0.7,
                          NA,NA, -0.005,-0.05, -0.005,-0.5, # Nadel
                          0.005,-0.4, 0.005,-0.05, NA,NA),ncol=2,byrow=TRUE))
Spritze <- syringe
Spritz <- syring
# plot(syringe,type="l",col="darkred");polygon(syringe,col="red")
capsule <- rbind(matrix(c(0,1, -1.9,1),ncol=2,byrow=TRUE), # transparent
                 cbind(-1.9-1.8*loop[1:18,1],2*loop[1:18,2]),
                 matrix(c(-1.9,-1, 0,-1, -1.9,-1),ncol=2,byrow=TRUE),
                 cbind(-1.9-1.8*loop[18:1,1],2*loop[18:1,2]),
                 matrix(c(-1.9,1, 0,1),ncol=2,byrow=TRUE),
                 cbind(0.9*-loop[36:19,1],2*loop[36:19,2]),
                 cbind(2.3-1.8*loop[19:37,1],2*loop[19:37,2]),
                 matrix(c(0,1, NA,NA),ncol=2,byrow=TRUE))
Kapsel <- capsule
# plot(capsule,type="l",col="darkred",asp=1);polygon(capsule,col="red")

icons <- list(Stern,Nadelbaum,Busch,Mensch,Mann,Frau,Familie,Familie2,Fahrrad,Schwein,
            Dorsch,Kaninchen,Knochen,Vase,Frucht,Ackerschmalwand,Petrischale,Maus,
            Wurm,Weizen,Zebrabärbling,Seestern,Ei,Blume,Fruchtfliege,Muschel,Hefe,
            E.coli,Glühbirne,Moschee=9*mosque,KKW=9*KKW,Haus,Fabrik,Auto=3*car,
            #alphadach,
            betadach,thetadach,Buch,offenes.Buch,Papier,Glas,Flasche,Herz,
            5*Tropfen,10*Spritze,Kapsel)

iconsD <- c("Stern","Nadelbaum","Busch","Mensch","Mann","Frau","Familie","Familie2","Fahrrad","Schwein",
            "Dorsch","Kaninchen","Knochen","Vase","Frucht","Ackerschmalwand","Petrischale","Maus",
            "Wurm","Weizen","Zebrabärbling","Seestern","Ei","Blume","Fruchtfliege","Muschel","Hefe",
            "E.coli","Glühbirne","Moschee","KKW","Haus","Fabrik","Auto",
            #"alphadach",
            "betadach","thetadach","Buch","offenes.Buch","Papier","Glas","Flasche","Herz",
            "Tropfen","Spritze","Kapsel")
iconsE <- c("star","fir","bush","human","man","woman","family","family2","bike","pig",
               "cod","rabbit","bone","vase","fruit","arabidopsis","dish","mouse",
               "worm","wheat","zebrafish","starfish","egg","flower","drosophila","shell","yeast",
               "germ","bulb","mosque","nuclear.powerplant","house","mill","car",
            #"alphahat",
            "betahat","thetahat","book","open.book","paper","glas","bottle","heart",
            "drops","syringe","capsule")
names(icons) <- iconsE
iconsCL <- c("gold","darkgreen","darkgreen","brown","blue", "pink", "darkgreen", "darkgrey", "blue","pink",
             "blue","brown","grey","grey20","pink","darkgreen","gold","gray",
             "grey","gold","black","pink","gold","lightcyan","brown","white","grey",
             "gray","gold","darkgrey","black","grey80","black","darkblue",
             #"black",
             "black","black","black","black","black","orange","darkgreen","darkred",
             "darkred","black","black")
iconsCF <- c("white","green","green","white","lavender","mistyrose","lightgreen","yellow",NA,"mistyrose",
             "lavender","gold","white","lavender","orange","green","white","white",
             "gray95","cornsilk","white","orange","white","magenta","gold","grey","lavender",
             "lightgray","white","white","lightgrey","red","grey","blue",
             #NA,
             NA,NA,"gold","gold",NA,"yellow","green","red",
             "red","red","red")
iconsCol <- cbind(DE=iconsD,EN=iconsE,lcol=iconsCL,fcol=iconsCF)

##
## show all icons in fitting colors
I <- length(iconsD)
#pp <- function(iconCol){
#  icon <- matrix(eval(parse(text=iconCol[1])),ncol=2)
#  plot(icon,col=iconCol[3],type="l")
#  polygon(icon,col=iconCol[4],border=NA)
#  lines(icon,col=iconCol[3],type="l")
#}
#for(i in 1:I) pp(iconsCol[i,])
show.icons <- function(x=NULL){
  if(!is.null(.GlobalEnv$icons)) icons <- .GlobalEnv$icons
  if(!is.null(.GlobalEnv$iconsD)) iconsD <- .GlobalEnv$iconsD
  if(!is.null(.GlobalEnv$iconsCL)) iconsCL <- .GlobalEnv$iconsCL
  if(!is.null(.GlobalEnv$iconsCF)) iconsCF <- .GlobalEnv$iconsCF
  I <- length(iconsD)
  par(mar = rep(0,4)+0.1)
  rc <- ceiling(sqrt(I))
  plot(c(15,19*rc+20), c(15,19*rc+10),
       pch = "", asp = 1,
       bty = "n", xaxt = "n", yaxt = "n")
  for (i in 1:rc) {
   for (j in 1:rc) {
    k <- rc*(i-1)+j
    if (k>I) {
      break
    }
    polygon(icons[[k]][,1]+19*i,icons[[k]][,2]+19*j,col=iconsCF[k],border=NA)
      lines(icons[[k]][,1]+19*i,icons[[k]][,2]+19*j,col=iconsCL[k])
   }
  }
  par(mar = c(5, 4, 4, 2) + 0.1)
}
#show.icons()

# keep all icons and add one icon
add.icon <- function(x, # matrix of coordinates
                     alias = NULL, # name, if different
                     deutsch = NULL, # name in german
                     line.col = "black", # outline color for show.icons
                     fill.col = "mistyrose" # fill color for show.icons
                     ) {
  if(is.data.frame(x)) x <- as.matrix(x)
  stopifnot(is.numeric(x[,1:2]))
  ni <- length(names(icons))
  assign("iconsCL", c(iconsCL, line.col), envir = .GlobalEnv)
  assign("iconsCF", c(iconsCF, fill.col), envir = .GlobalEnv)
  if(!is.null(deutsch)) {
    stopifnot(is.character(deutsch))
    if(length(deutsch)>1) {
      cat("\n only first element of deutsch used\n")
      deutsch <- deutsch[1]
    } # length of deutsch
    assign("iconsD", c(iconsD, deutsch), envir = .GlobalEnv)
    na <- deutsch
  } # deutsch not supplied
  if(!is.null(alias)) {
    stopifnot(is.character(alias))
    if(length(alias)>1) {
      cat("\n only first element of alias used\n")
      alias <- alias[1]
    } # length of alias
    if(is.null(deutsch)) assign("iconsD",c(iconsD, alias), envir = .GlobalEnv)
    assign("iconsE",c(iconsE, alias), envir = .GlobalEnv)
    na <- alias
  } # alias supplied
  if(is.null(deutsch) & is.null(alias)) {
    na <- deparse(substitute(x))
    assign("iconsE",c(iconsE, deparse(substitute(x))), envir = .GlobalEnv)
  }
  assign("icons", c(icons, eval(na) = list(rbind(x[,1:2],NA))), envir = .GlobalEnv)
} # add.icon

# discard all icons and add just one icon
set.icon <- function(x, # matrix of coordinates
                     alias = NULL, # name, if different
                     deutsch = NULL, # name in german
                     line.col = "black", # outline color for show.icons
                     fill.col = "mistyrose" # fill color for show.icons
) {
  if(is.data.frame(x)) x <- as.matrix(x)
  stopifnot(is.numeric(x[,1:2]))
  ni <- length(names(icons))
  assign("iconsCL", line.col, envir = .GlobalEnv)
  assign("iconsCF", fill.col, envir = .GlobalEnv)
  if(!is.null(deutsch)) {
    stopifnot(is.character(deutsch))
    if(length(deutsch)>1) {
      cat("\n only first element of deutsch used\n")
      deutsch <- deutsch[1]
    } # length of deutsch
    assign("iconsD", deutsch, envir = .GlobalEnv)
    na <- deutsch
  } # deutsch not supplied
  if(!is.null(alias)) {
    stopifnot(is.character(alias))
    if(length(alias)>1) {
      cat("\n only first element of alias used\n")
      alias <- alias[1]
    } # length of alias
    if(is.null(deutsch)) assign("iconsD",alias, envir = .GlobalEnv)
    assign("iconsE", alias, envir = .GlobalEnv)
    na <- alias
  } # alias supplied
  if(is.null(deutsch) & is.null(alias)) {
    na <- deparse(substitute(x))
    assign("iconsE", deparse(substitute(x)), envir = .GlobalEnv)
  }
  assign("icons", eval(na) =list(rbind(x[,1:2],NA)), envir = .GlobalEnv)
} # set.icon
