#############################################################################################
#
# dados matriciais: leitura de dados (GeoTIFF), manipulação de conjuntos de dados
# geográficos matriciais em R, package "raster", sistema de coordenadas, re-projecção
#
#########################################################################################


library(raster)
library(rgdal)
library(sp)
library(rgeos)
#library("gdalUtils") # função remove_file_extension

library(grDevices)
export<-FALSE

# pasta de trabalho
wd<-"Y:\\Aulas\\sigs_com_R\\dados_aulas"
aulas<-"Y:\\Aulas\\sigs_com_R"
setwd(wd)

# LER COM RASTER
# ler geotiff com raster: devolve raster object
# raster atribui CRS ao objecto
fich<-"landsat8pan.tif" # 4.5 Mb
pan<-raster(fich) # cria um objecto RasterLayer com slots @file, @data,... @crs,...
str(pan)
print(pan@crs)
cores<-gray(seq(0,1,length.out=100))
plot(pan,col=cores)
if (export)  png(paste(aulas,"imagem_pan_tons_cinzento_legenda_horizontal.png",sep="\\"), width=800, height=800, res=120)
plot(pan,  xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,horizontal=TRUE)
if (export) graphics.off()

# extrair valores das células para o vector v
v<-values(pan)

if (export)  png(paste(aulas,"val.vs.intensidade.png",sep="\\"), width=800, height=800, res=120)
out<-hist(v,main="valores vs intensidade",ylab="frequências absolutas")
fqs<-out$counts
lines(x=range(v),y=range(fqs),col="blue")
axis(4,at=seq(min(fqs),max(fqs),length.out=5),labels=seq(0,1,length.out=5))
mtext("intensidade", side=4,line=-1.5,col="blue")
if (export) graphics.off()

# restringir valores: saturar
if (export)  png(paste(aulas,"imagem_pan_stretch_legenda_horizontal.png",sep="\\"), width=800, height=800, res=120)
plot(pan,  zlim=c(0,25000), xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,horizontal=TRUE) 
if (export) graphics.off()
plot(pan,  zlim=c(0,quantile(v,.999)), xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,horizontal=TRUE) 

# restringir x e y
plot(pan,  xlim=c(580000,585000), xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,zlim=c(0,25000)) 
plot(pan,  xlim=c(570000,575000), ylim=c(4315000,4325000),zlim=c(0,25000),xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,legend=FALSE) 
image(pan,  xlim=c(570000,575000), ylim=c(4315000,4325000),zlim=c(0,25000))
head(coordinates(pan))
range(coordinates(pan)[,"y"])

# interactividade
plot(pan, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)  
box <- drawExtent() # clicar em 2 cantos
# definir a extensão usando box 
plot(pan, ext=box, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)  
plot(pan, ext=box/2, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)  

# zoom
plot(pan, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)  
zoom(pan, new=FALSE, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)

# click
vals<-click(pan)

# locator
pts <- locator() # devolve uma lista de coordenadas; parar com <ESC>
polygon(cbind(pts$x,pts$y),border="red") # adiciona à imagem o polígono formado pelos pontos digitalizados

# crop
pan.crop <- crop(x=pan,y=pts)
plot(pan.crop)

# re-projectar
igeoe<- "+proj=tmerc +lat_0=39.66666666666666  +towgs84=-304.046,-60.576,103.64,0,0,0,0 +lon_0=1 +k=1 +x_0=200000 +y_0=300000 +ellps=intl  +pm=lisbon +units=m" # Coordenadas ``militares''
m <- projectRaster(pan, crs=igeoe) #demora...

# comparar coordenadas e projecção
if (export)  png(paste(aulas,"imagem_pan_utm_vs_igeoe.png",sep="\\"), width=800, height=500, res=120)
par(mfrow=c(1,2))
# dados originais (coordenadas UTM zona 29) 
plot(pan, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores,zlim=c(0,25000))
xy<-as.vector(pan@extent) 
# xpd para poder escrever fora da imagem
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy),pos=c(1,1,2,2),xpd=TRUE)
text(x=mean(xy[1:2]),y=xy[4],"UTM zona 29",col="yellow",pos=1)

# dados transformados (coordenadas militares) 
plot(m, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores,zlim=c(0,25000))
xy<-as.vector(m@extent) 
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy),pos=c(1,1,2,2),xpd=TRUE)
text(x=mean(xy[1:2]),y=xy[4],"Coord. Militares",col="yellow",pos=1)
if (export) graphics.off()

# nadgrid
#Colocar ptLX_e89.gsb na pasta dada por system.file("proj", package = "rgdal")
igeoe.grid <- CRS("+proj=tmerc +lat_0=39.66666666666666  +nadgrids=ptLX_e89.gsb +lon_0=1 +k=1 +x_0=200000 +y_0=300000 +ellps=intl  +pm=lisbon +units=m")
m.grid <- projectRaster(pan, crs=igeoe.grid)
# dados transformados (coordenadas militares) 3 coefs. 
if (export)  png(paste(aulas,"imagem_pan_bursa_wolf_vs_nadgrid.png",sep="\\"), width=800, height=500, res=120)
par(mfrow=c(1,2))
plot(m, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores,zlim=c(0,25000))
xy<-as.vector(m@extent) 
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy),pos=c(1,1,2,2),xpd=TRUE)
text(x=mean(xy[1:2]),y=xy[4],"3 parâmetros",col="yellow",pos=1)

# dados transformados (coordenadas militares) nadgrid
plot(m.grid, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores,zlim=c(0,25000))
xy<-as.vector(m.grid@extent)
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy),pos=c(1,1,2,2),xpd=TRUE)
text(x=mean(xy[1:2]),y=xy[4],"método das grelhas",col="yellow",pos=1)
if (export) graphics.off()

# exercicio (a) determinar a gama lat/long correspondente a pan
wgs84<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
w <- projectRaster(pan, crs=wgs84)
plot(w, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores,zlim=c(0,25000))
xy<-as.vector(w@extent) 
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy,4),pos=c(1,1,2,2),xpd=TRUE)

# exercicio (b) determinar a localização (lat/long) do parede da barragem de Montargil
zoom(w, new=FALSE, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)
locator()

# Exercício ndvi vs elevação na área protegida Sintra-Cascais
#limite AP Sintra-Cascais
limite.ap<-as.matrix(read.table(file="limite.AP.SintraCascais.ETRS.txt",header=FALSE))

#ler elevações SRTM mde
fich.mde<-"n38_w010_3arc_v2.tif"
mde<-raster(fich.mde)
etrs<-"+proj=tmerc +lat_0=39.6682583 +lon_0=-8.1331083 +k=1 +x_0=0 +y_0=0 +ellps=GRS80 +units=m"
mde.etrs<-projectRaster(mde,crs=etrs)
mde.etrs<-crop(mde.etrs,limite.ap)

#ler imagem ndvi
ndvi<-raster("ndviSintra.tif")
ndvi@crs
vazio<-raster(ext=mde.etrs@extent,crs=mde.etrs@crs,resolution=res(mde.etrs))
ndvi.etrs<-projectRaster(from=ndvi,to=vazio)

# construir imagens para verificar
cores<-gray(seq(0,1,length.out=100))
if (export)  png(paste(aulas,"ap_sintra_cascais_ndvi_mde_transectos.png",sep="\\"), width=800, height=500, res=120)
par(mfrow=c(1,3))
plot(mde.etrs,  xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,legend=FALSE)
polygon(limite.ap,border="red")
xy<-as.vector(mde.etrs@extent) 
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy,0),pos=c(1,1,2,2),xpd=TRUE,cex=0.8)
text(x=mean(xy[1:2]),y=xy[4],"mde.etrs",col="yellow",pos=1)

plot(ndvi.etrs, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE)
polygon(limite.ap,border="black")
xy<-as.vector(ndvi.etrs@extent) 
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy,0),pos=c(1,1,2,2),xpd=TRUE,cex=0.8)
text(x=mean(xy[1:2]),y=xy[4],"ndvi.etrs",col="black",pos=1)

# extrair células num transecto dentro da AP Sintra-Cascais
xy0<-apply(limite.ap,2,mean) # centroide da AP
cp1<-eigen(cov(limite.ap))$vectors[,1] # 1a componente principal
cp2<-eigen(cov(limite.ap))$vectors[,2] # 1a componente principal
xs<-unique(coordinates(mde.etrs)[,1]) # coord x das células
amostra<-cbind(xs, xy0[2]+cp1[2]/cp1[1]*(xs-xy0[1])) # coord x e y 
amostra<-rbind(amostra, cbind(xs, xy0[2]+cp2[2]/cp2[1]*(xs-xy0[1])) )

# verificar localização do transecto:
plot(mde.etrs,  xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,legend=FALSE)
polygon(limite.ap,border="red")
points(amostra,col="yellow",cex=.5)
text(x=mean(xy[1:2]),y=xy[4],"transectos",col="yellow",pos=1)

if (export) graphics.off()

# extrair valores das células sobre o transecto
y<-extract(ndvi.etrs,amostra)
x<-extract(mde.etrs,amostra)


# analisar a relação entre ndvi e elevação
if (export)  png(paste(aulas,"ap_sintra_cascais_ndvi_vs_mde_ajustamento.png",sep="\\"), width=800, height=800, res=120)
plot(y~x,xlab="elevação (m)",ylab="ndvi",main="AP Sintra-Cascais",cex.lab=1.3)
# ajustar curva
xx<-log(x[x>0]) #1/x[x>0]
yy<-log(y[x>0])
ajust<-lm(yy~xx)
curve(exp(ajust$coef[1])*x^ajust$coef[2],add=TRUE,col="red")
text(mean(range(x,na.rm=TRUE)),quantile(range(y,na.rm=TRUE),.25),paste(" y = ", round(exp(ajust$coef[1]),3), " * x^",round(ajust$coef[2],3),sep=""))
#text(0,max(y,na.rm=TRUE),paste("R^2=",round(summary(ajust)[[8]],2)),pos=4,cex=1.3)
if (export) graphics.off()
