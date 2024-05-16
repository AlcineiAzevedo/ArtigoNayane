#Algoritmo do R criado para a correção da distorção:

library(ExpImage)
setwd_script()
Amarelo=read_image("PaletaAmarelo.jpg",plot=T)
Sem_Amarelo=read_image("Fundo.jpg",plot=T)
modelo=segmentation_logit(im=Sem_Amarelo,foreground = Amarelo,background = Sem_Amarelo,return = "model")

nomes=list.files(pattern = "IMG_+")

RESULTref=RESULTobj=NULL
for(j in 1:length(nomes) ){
im=readImage(nomes[j])
im=resize_image(im,p=30,plot=T)

maskRef=predict_logit(im,modelo,plot=F)
im2=crop_image(im,segmentation = erode_image(maskRef,plot=F),plot=T)
maskRef2=predict_logit(im2,modelo)

mmed=measure_image(maskRef2,plot=F,imOut = T)
med=mmed$measures

med2=data.frame(med[order(-med[,3]),][1:8,1:3])
med2=cbind(med2,desvio=1-(med2[,3]/mean(med2[,3])))

m=lm(desvio~x+y+x:y,data=med2)
summary(m)
newData=expand.grid(x=1:info_image(im2)$Length[1],y=1:info_image(im2)$Length[2])
pred=predict(m,newdata=newData)
Corr=matrix(pred,info_image(im2)$Length[1],info_image(im2)$Length[2])
#    plot_image(Corr,col=3)
CorrectRef=(maskRef2+Corr)*maskRef2
#    plot_image(CorrectRef,col=3)

res=NULL
for(i in unique(c(mmed$imOut))[-1]){
  res=rbind(res,c(measure_image(mmed$imOut==i,plot=F)$measures[1:3],sum(CorrectRef[mmed$imOut==i])))
}
ResultadoReferencia=res[order(-res[,3]),][1:8,]


MaskObj=segmentation(gray_scale(im2,method = "SI"),fillHull = TRUE,threshold =0.75,selectHigher = T,plot=T)
CorrectObj=(MaskObj+Corr)*MaskObj
#    plot_image(CorrectObj,col=3)
mmed=measure_image(MaskObj,noise = 100,plot=F,imOut = T)
res=NULL
for(i in unique(c(mmed$imOut))[-1]){
  res=rbind(res,c(measure_image(mmed$imOut==i,plot = F)$measures[1:3],sum(CorrectObj[mmed$imOut==i])))

  }
ResultadoObj=res[order(-res[,3]),][1:16,]

jpeg(paste0("id_",nomes[j]))
plot(im2)
text(ResultadoObj[,1],ResultadoObj[,2],1:16)
dev.off()

RESULTref=rbind(RESULTref,cbind(nomes[j],ResultadoReferencia))
RESULTobj=rbind(RESULTobj,cbind(nomes[j],ResultadoObj))
}

write.table(RESULTref,"CorrecaoReferencia.txt",sep="\t")
write.table(RESULTobj,"CorrecaoObj.txt",sep="\t")

