if(!require("shiny")){
  install.packages("shiny",dependencies = TRUE);
  library("shiny");
}
if(!require("shinydashboard")){
  install.packages("shinydashboard",dependencies = TRUE);
  library("shinydashboard");
}
if(!require("ggplot2")){
  install.packages("ggplot2",dependencies = TRUE);
  library("ggplot2");
}

if(!require("openxlsx")){
  install.packages("openxlsx",dependencies = TRUE);
  library("openxlsx");
}
if(!require("plotly")){
  install.packages("plotly",dependencies = TRUE);
  library("plotly");
}
if(!require("reshape2")){
  install.packages("reshape2",dependencies = TRUE);
  library("reshape2");
}


###############
## FUNCTIONS ##
###############

subst = function(x){ gsub("[ ]",".",x)}

File.Ct = function(Fi.Ct, condition, repTech, repBio){
  Genes<-c()
  CT<-c() 
  cond<-c()
  cpt<-0
  files = c()
  n.files = length(Fi.Ct)
  Gene.prst = c()
  
  for (i in 1:n.files){
    df = read.xlsx (Fi.Ct[i], 
                    sheet = "Results")
    d<-data.frame(tail(df, n=-41L))
    s<-as.matrix(df)
    colnames(d)<-as.vector(subst(s[41,]))
    #     intermed<-data.frame(d$Sample.Name, d$Target.Name, d$CT)
    #     print(intermed)
    SN<-length(setdiff("Sample.Name", colnames(d)))
    TN<-length(setdiff("Target.Name", colnames(d)))
    LCt<-length(setdiff("CT", colnames(d)))
    if( SN == 0 && TN == 0 && LCt == 0 ){
      #       print("Les colonnes Sample Name, Target Name et Ct sont présentes dans un des fichiers")
      files = c(files, "OK")
    }else{
      #       stop("Une des colonnes Sample Name, Target Name et Ct ne sont pas présentes dans un des fichiers")
      files = c(files, "NA")
    }
    data = d[order(d$Sample.Name),]
    data.order = data[order(data$Target.Name),]
    cond = c(cond, unique(as.character(data.order$Sample.Name)))
    Genes = c(Genes, unique(as.character(data.order$Target.Name)))
    Genes.temp = unique(as.character(data.order$Target.Name))
    n.temp = length(Genes.temp)
    n.genes = length(Genes)
    # Genes.inter = intersect(Genes[(n.genes - n.temp):n.temp], Genes.temp)
    # if( length(Genes.inter) < n.temp ){
    #   #       Genes.prst = c(Genes.prst, Genes.temp)
    #   ## cmt afficher les warning sur l'interface ??
    #   warning("Un des gènes est présent plusieurs fois dans les fichiers Ct : ")
    #   Gene.prst = c(Gene.prst, Genes.inter)
    #         # print(Gene.prst)
    # 
    # }
    
    files.OK = files[which(files == "OK")]
    n.OK = length(files.OK)
    n = length(d$Sample.Name)
    CT = c(CT, as.numeric(data.order$CT))
    ct = matrix(CT, nrow = length(Genes), byrow = TRUE)
    cpt = cpt + 1
  }
  
  # ######################################
  # # pour mes fichiers tests je laisse mais
  # # à enlever lors de la livraison
  # # ######################################
  #   G = strsplit(Genes, " ")
  #   j = 1
  #   for(i in G){
  #     Genes[j] = i[1]
  #     j = j + 1
  #   }
  #   Genes[which(!is.na(Genes))]
  #     print(Genes)
  # # #####################################
  
  ### Vérifie le nb de condition
  check.Cond<-NULL
  c.Cond<-NULL
  C = strsplit(cond, "[0-9]")
  j = 1
  for(i in C){
    c.Cond[j] = i[1]
    j = j + 1
  }
  c.Cond<-c.Cond[!is.na(c.Cond)]
  check.Cond<-unique(c.Cond,"[0-9]")
  #   print(condition)
  #   if( length(check.Cond) != condition ){
  #     stop("Le nombre de condition ne correspond à celui présent dans le fichier")
  #   }
  
  ### Vérifie le nb de repTech
  n.rep = n.genes/n.files
  print(n.files)
  print(n.genes)
  Nrep = n.genes - (n.files-1)*round(n.rep)
  rep = as.character(d$Sample.Name[order(d$Sample.Name[1:(length(d$Sample.Name)/Nrep)])])
  rep = rep[!is.na(rep)]
  print(rep)
  print(d$Sample.Name)
  #   cat("rep : ")
  #   print(rep)
  #   r=rep(0, nb)
  #   k = 1
  #   for (i in 1:nb ){
  #     for (j in 1:nb){
  #       if(rep[i] == rep[j]){
  #         r[k] = r[k] + 1
  #       }
  #     }
  #     k=k+1
  #   }
  #   cat("r : ")
  #   print(r)
  #   if( max(r) - min(r) == 0 ){
  #     check.rep = max(r)
  # #     if( check.rep != repTech ){
  # #       stop("Le nombre de réplicats techniques entré ne correspond à celui du ficher")
  # #     }
  #   }else{
  # #     stop("Les conditions n'ont pas le même nombre de réplicats")
  #   }
  print(table(rep))
  check.rep = max(table(rep))
  #   cat("check.rep : ")
  #   print(check.rep)
  
  ### Vérifie le nb de repBio
  Bio<-unlist(strsplit(unique(rep),"[0-9]"))
  #   print(Bio)
  #   print(check.Cond)
  nBio=length(Bio)
  #   print(nBio)
  rBio=rep(0, condition)
  for (i in 1:nBio){
    for (j in 1:condition){
      if( Bio[i] == check.Cond[j] ){
        rBio[j] = rBio[j] + 1
        
      }  
    }
  }
  #   cat("rbio : ")
  #   print(rBio)
  if( max(rBio) - min(rBio) == 0 ){
    check.repB = max(rBio)
    #     if( check.repB != repBio ){
    #       stop("Le nombre de réplicats biologiques entré ne correspond à celui du ficher")
    #     }
  }else{
    #     stop("Les conditions n'ont pas le même nombre de réplicats")
  }
  
  ct = as.numeric(ct)
  #   print(ct)
  ct = matrix(ct, ncol=length(rep))
  #   print(ct)
  ### ORDONNER PAR LE NOM DES GÈNES ###
  # Genes.ord = Genes[order(Genes)]  
  # print(Genes)
  # print(order(Genes))
  # ct.ord = ct[order(Genes),]
  # print(ct.ord)
  # data.CT = data.frame(Genes.ord, ct.ord )
  ### ------------------------------ ####
  data.CT = data.frame(Genes, ct)
  data.CT = data.CT[apply(data.CT,1,function(x) !any(is.na(x))),]
  # cat("rep : ") 
  # print(dim(data.CT))
  NamesCol = c("id",rep)
  colnames(data.CT)<-NamesCol
  # write.table(Genes, "genes.txt", row.names=FALSE, col.names = FALSE, sep="\t")
  
  # print(length(check.Cond))
  print(check.rep)
  # print(check.repB)
  return (list(data.CT = data.CT, Cond = length(check.Cond), repT = Nrep, repB = check.repB, Nfiles = n.files, Nok = n.OK, Gprst = Gene.prst))
  
}

File.E = function(Fi.E){
  #   require(gdata)
  # Récupére les données du fileE
  Q.init<-c(1,1,1/3,1/3,1/9,1/9,1/27,1/27,1/81,1/81,NA,NA)
  Q<-c()
  condi<-c()
  Ct<-c()
  id<-c()
  lid = c()
  Slope = c()
  substi = function(x){ gsub("[,]",".",x)}
  #   setwd("~") #revient dans le répertoire courant
  #   setwd("R_Phaeo/FileE")
  #   chemin<-getwd()
  files = c()
  n.files = length(Fi.E)
  for (i in 1:n.files){
    df = read.xlsx(Fi.E[i], sheet = "Results")
    # avec gdata#     , header = TRUE)
    
    
    d<-data.frame(tail(df, n=-35L))
    s<-as.matrix(df)
    colnames(d)<-as.vector(subst(s[35,]))
    ## ajout 15/11 : 
    data = d[order(d$Sample.Name),]
    data.order = data[order(data$Target.Name),]
    condi = c(condi, as.character(data.order$Sample.Name))
    id = c(id, as.character(data.order$Target.Name))
    lid = c(lid, unique(as.character(data.order$Target.Name)))
    Ct = c(Ct, data.order$CT)
    Slope = c(Slope, data.order$Slope)
    ## pas besoin suite au 4/10
    # m = length(id)
    # Q = c(Q, rep(Q.init, m)
    #######
    SN<-length(setdiff("Sample.Name", colnames(d)))
    TN<-length(setdiff("Target.Name", colnames(d)))
    LCt<-length(setdiff("CT", colnames(d)))
    Sl<-length(setdiff("Slope", colnames(d)))
    if( SN == 0 && TN == 0 && LCt == 0 && Sl == 0){
      #       print("Les colonnes Sample Name, Target Name et Ct sont présentes dans un des fichiers")
      files = c(files, "OK")
    }else{
      #       stop("Une des colonnes Sample Name, Target Name et Ct ne sont pas présentes dans un des fichiers")
      files = c(files, "NA")
    }
    ## suppression 15/11
    # test = data.frame(id=d$Target.Name, condi=d$Sample.Name, Ct=d$CT, Slope=d$Slope)
    # test.order = test[order(test$id),]
    # m = length(unique(test.order$id))
    # lid = c(lid, as.character(unique(test.order$id)))
    # id = c(id, as.character(test.order$id))
    # Ct = c(Ct, as.numeric(substi(test.order$Ct)))
    # condi = c(condi,as.character(test.order$condi))
    # slopeX = c(slopeX,as.numeric(substi(test.order$Slope)))
    # x = rep(Q.init, m)
    # Q = c(Q, x)
  }
  
  ## Enlève le chiffre derrière la condition
  C = strsplit(condi, "[0-9]")
  c.Cond = c()
  j = 1
  for(i in C){
    c.Cond[j] = i[1]
    j = j + 1
  }
  ## mets toutes les conditions en majuscule
  condi = toupper(c.Cond)
  
  
  files.OK = files[which(files == "OK")]
  n.OK = length(files.OK)
  
  ##### chgmt 4/10/16 
  ### plus besoin dû au bon formatage des fichiers ?
  ### Mais laisse pour mes fichiers tests 
  ### à enlever lors de la livraison
  #   id = strsplit(id, " ")
  #   j = 1
  #   for(i in id){
  #     id[j] = i[1]
  #     j = j + 1
  #   }
  #   id = unlist(id)
  # 
  #   lid = strsplit(lid, " ")
  #   j = 1
  #   for(i in lid){
  #     lid[j] = i[1]
  #     j = j + 1
  #   }
  #   lid = unlist(lid)
  # 
  # #######
  C = strsplit(condi, " ")
  j = 1
  for(i in C){
    if ( length(C[[j]]) > 1 ){
      condi[j] = i[2]
      if(is.na(condi[j])){ condi[j] = condi[j-1] }
    }else{
      condi[j] = i[1]
    }
    j = j + 1
  }
  
  ucondi=unique(condi)
  Ef = c()
  slope = c()
  idg = NULL
  icond = NULL
  lid = unique(lid)
  # cat("id : ")
  # print(length(id))
  # cat("Ct : ")
  # print(length(Ct))
  # cat("condi : ")
  # print(length(condi))
  # cat("slope : ")
  # print(length(Slope))
  # cat("Q : ")
  # print(length(Q))
  Eff = data.frame(id=id, Ct=Ct, condi=condi, slope = Slope, stringsAsFactors = FALSE)
  Eff=Eff[!is.na(Eff$Ct),]
  print(head(Eff))
  E1 = 10^(-1/as.numeric(Eff$slope))
  E = data.frame(id = Eff$id, cond = Eff$condi, E = E1)
  E = E[E$cond != "H",]
  E = E[E$cond != "RT-",]
  ## enlève car ciffre très bizarre pour mon fichier test mais à ajouter pour la livraison
  E = E[E$id != "49563",]
  print(head(E))
  ###########################
  ### chgmt 4/10/16
  ### ne sert à rien 
  ### car mtnt utilisation de slope du fichier
  ###########################
  #   Econdi=Eff$condi
  #   for (i in 1:length(lid)){
  #     for (j in 1:length(unique(condi))){
  #       k = which((Eff$id == lid[i]))
  #       c = which(Eff$condi == ucondi[j])
  #       b = which(is.na(Eff$Ct))
  #       inter = intersect(k,c)
  #       inters = intersect(inter[1:10],b)
  #       if(length(inters) > 0){
  #         if(is.na(inter[11])){
  # #             stop("pb NA dans les Ct !")
  #         }else{
  #           inter=inter[11:20]
  #         }
  #       }
  #       
  #       if(length(inter) > 0){
  #         Y = Eff[inter[1]:(inter[10]),2]
  #         X = Eff[inter[1]:(inter[10]),3]
  #         X=log10(X)
  #         
  #         #Méthode de régression
  # #         M0 = lm(Y~X)
  # #         pente = as.numeric(M0$coefficients[2])
  # #         b = as.numeric(M0$coefficients[1])
  # #         Ti=pente*X+b
  # #         SCM=sum((Ti-mean(Y))^2)
  # #         SCT=sum((Y-mean(Y))^2)
  # #         R2=SCM/SCT
  #         pente = slopeX
  # #         print(pente)
  #         eff=10^(-1/pente)
  #         y = rep(eff, 12)
  #         Ef = c(Ef,y)
  #         x = rep(pente, 12) 
  #         slope = c(slope,x)
  #         g=rep(lid[i],12)
  #         idg = c(idg,g)
  #         c=rep(ucondi[j],12)
  #         icond=c(icond,c)
  #       }
  #     }
  #   }
  #   E = data.frame(id=idg, cond=icond, slope=slope, Ef=Ef, stringsAsFactors = FALSE)
  condition = unique(E$cond)
  print(length(condition))
  E=E[order(E$id),]
  #   Eg = data.frame(id=E$id, E=E$Ef, condi=E$cond, stringsAsFactors = FALSE)
  #   Eg = Eg[order(Eg$id),]
  Eg = reshape(E, v.names = "E", idvar = "id", timevar ="cond", direction = "wide")
  # Eg = Eg[!is.na(Eg$E.TRI)&!is.na(Eg$E.FUS)&!is.na(Eg$E.OV),]
  # Eg = Eg[Eg$E.FUS < 3 & Eg$E.OV < 3 & Eg$E.TRI < 3,]
  print(head(Eg))
  # id = Eg$id
  # Eg = Eg[,2:(length(condition)+1)]
  
  # print(order(colnames(Eg)))
  # Eg = Eg[,order(colnames(Eg))]
  # Eg = cbind(id,Eg)
  return (list(dataE = Eg, Nfiles = n.files, Nok = n.OK))
}

#fonction moyenne sur les rep tech pour le gène
moyRepTech = function(condition, repBio, repTech, ct){
  cond = unique(colnames(ct))
  cond = cond[2:(condition*repBio+1)]
  Genes=ct$id
  nbGenes=length(Genes)
  ct=ct[,-1]
  #NamesCol=unique(colnames(ct))
  ct = data.matrix(ct)
  #   n = dim(ct)[2]
  n = condition*repBio*repTech
  #   cat("n : ")
  #   print(n)
  z = matrix(0, nbGenes, condition*repBio)
  # j = 1
  for (j in 1:nbGenes)
  { 
    i = 1
    k = 1
    rep = 0
    while(i < n)
    {
      rep = rep + repTech
      #       cat("repTech")
      #       print(rep)
      #       cat("i")
      #       print(i)
      
      z[j,k] = round(mean(ct[j,i:rep]),3)
      #       cat("z")
      #       print(z[j,k])
      i = i + repTech
      k = k + 1
    }
    # j = j + 1
  }
  z = data.frame(id = Genes,z,stringsAsFactors = FALSE)
  NamesCol=cond
  colnames(z)<-c("id",NamesCol)
  #write.csv(z, "/home/mathilde/StageM2/R_Phaeo/moyRepTech.csv")
  return (z)  
}
#fonction créer E Gènes de référence
E_GenesRef = function(NameGenesRef, E){
  NbGenesRef = length(NameGenesRef)
  Eref <- data.frame(matrix(nrow=NbGenesRef, ncol=dim(E)[2]))
  #   data.frame(id=character(), E=numeric(),stringsAsFactors = FALSE)
  for (i in 1:NbGenesRef) {
    Eref[i,] = E[which(E$id == NameGenesRef[i]),] 
  } 
  Eref[1:NbGenesRef,1]=NameGenesRef
  colnames(Eref) = colnames(E)
  return (Eref)
}
#fonction créer z Gènes de référence
z_GenesRef = function(NameGenesRef, z){
  NbGenesRef = length(NameGenesRef)
  zref <- data.frame(matrix(nrow=NbGenesRef, ncol=dim(z)[2]))
  # #     data.frame(id=character(), X1=numeric(),X2=numeric(),X3=numeric(),
  #                      X4=numeric(),X5=numeric(),X6=numeric(),X7=numeric(),
  #                      X8=numeric(),X9=numeric(),X10=numeric(),X11=numeric(),
  #                      X12=numeric(),stringsAsFactors = FALSE)
  for (i in 1:NbGenesRef) {
    zref[i,] = z[which(z$id == NameGenesRef[i]),] 
  } 
  zref[1:NbGenesRef,1]=NameGenesRef
  colnames(zref) <- colnames(z)
  return (zref)
}

#fonction Normalisation
Norm = function(condition, repBio, E, z, Eref, zref){
  A = merge(z, E, by="id")
  z = A[,1:(condition*repBio+1)]
  E = cbind(A$id, A[,((condition*repBio+1)+1):((condition*repBio+1)+condition)])
  colnames(E)<-c("id",colnames(E[,2:(condition+1)]))
  NbGenes = length(z[,1])
  cond = unique(colnames(z))
  cond = cond[2:(condition*repBio+1)]
  znorm = matrix(0, nrow = NbGenes, ncol = condition*repBio)
  for (j in 1:NbGenes)
  {
    for(i in 1:condition)
    {
      for(k in 1:(condition*repBio))
      {
        #         znorm[j,k] = E[j,i+1]^z[j,k+1] / (Eref[1,i+1]^zref[1,k+1] * Eref[2,i+1]^zref[2,k+1])^(1/2)
        #         print(znorm)
        znorm[j,k] = prod((1+Eref[,i+1])^(-zref[,k+1]))^(1/2)/((1+E[j,i+1])^(-z[j,k+1])) 
        #         print(znorm)
      }
    }
  }
  znorm = round(znorm,3)
  znorm = data.frame(id = z[,1],znorm,stringsAsFactors = FALSE)
  colnames(znorm)<-c("id", cond)
  return (znorm)
}

#fonction moyenne sur les rep bio
moyRepBio = function(condition, repBio, znorm){
  id = znorm$id
  nbGenes = length(id)
  znorm<-data.matrix(znorm[,-1])
  moy = matrix(0, nrow = length(znorm[,1]), ncol = condition)
  j = 1
  for (j in 1:nbGenes)
  { 
    n = condition*repBio
    i = 1
    k = 1
    rep = 0
    while(i < n)
    {
      rep = rep + repBio
      moy[j,k] = mean(znorm[j,i:rep])
      i = i + repBio
      k = k + 1
    }
    #     j = j + 1
  }
  cond = colnames(znorm)
  # cond = unlist(strsplit(cond, split = "[.]"))
  for ( i in 1:length(cond)){
    cond[i]<-substr(cond[i],1,nchar(cond[i])-1)
  }
  ucond=unique(cond)
  moy = data.frame(id = id, moy, stringsAsFactors = FALSE)
  colnames(moy)<-c("id", ucond)
  return (moy)
}

#fonction log-ratio
logRatio = function(condition, moy, comp){
  id = moy$id
  #   cond = colnames(moy[,2:(condition+1)])
  moy<-data.matrix(moy[,-1])
  #####################################"
  #### Ancien code pour logRatio :
  ####################################
  #   print(moy)
  #   colC = NULL
  #   if ( AllComp == TRUE){
  #     CondTemoin = NULL
  #     log = matrix(nrow=length(id), ncol=choose(condition,2))
  #     for (k in 1:length(id)){
  #       c = 1
  #       for (j in 1:(condition-1)){
  #         for (i in 2:condition){
  #           if( j == i ){
  #             next
  #           }else{
  #             log[k,c] = log2(moy[k,j]/moy[k,i])
  #             colC<-c(colC,paste(cond[j],cond[i], sep="/"))
  #             c = c + 1
  #           }
  #         }
  #       }
  #     }
  #   }else{
  #     log = matrix(nrow=length(id),ncol=condition-1)
  #     condT = pmatch(CondTemoin, cond)
  #     for(k in 1:length(id)){
  #       c=1
  #       for (i in 1:condition){
  #         if(i != condT){
  #           log[k,c] = log2(moy[k,condT] / moy[k,i])
  #           colC<-c(colC,paste(CondTemoin,cond[i], sep="/"))
  #           c = c + 1
  #         }
  #       }
  #     }
  #   }
  #   log = data.frame(id, log, stringsAsFactors = FALSE)
  #   colnames(log)<-c("id",unique(colC))
  nComp = length(comp)
  print(comp)
  compNum = c()
  compDen = c()
  for (i in 1:nComp){
    compNum[i] = unlist(strsplit(comp[i], "/"))[1]
    compDen[i] = unlist(strsplit(comp[i], "/"))[2]
  }
  
  log<-matrix(0, nrow = length(id), ncol = nComp)
  for (k in 1:length(id)){
    for (j in 1:nComp){
      log[k,j] = log2(moy[k,compNum[j]]/moy[k,compDen[j]])
    }
  }
  log = data.frame(id, log, stringsAsFactors = FALSE)
  colnames(log)<-c("id",comp)
  return (log)
}

## Fonction permettant de comparer le logRatio de qPCR et de RNA-seq
## FileRNAseq = fichier rna-seq
## cond = cond comparée dans le fichier
## log = le logRatio calculé issu de qPCR
CompLog_RNAseq = function(FileRNAseq, cond, log){
  
  datalog = read.csv(FileRNAseq, header = TRUE)
  datalog = datalog[order(datalog$Name),]
  datalog = data.frame(id = datalog$Name, RNAseq = datalog$log2FoldChange, padj = datalog$padj)
  id = log$id
  log1 = data.frame(id = id, qPCR = log[,which(colnames(log) == cond)])
  logMerge = merge(datalog, log1, by="id")
  #   logRatio = logMerge[which(!is.na(logMerge$RNAseq) & !is.na(logMerge$qPCR)),]
  # print(log)
  # print(cond)
  print(head(logMerge))
  
  return(list(logMerge=logMerge, logRatio = log, logRNAseq = datalog))
}

## Fonction permettant de comparer la moy bio de qPCR et de RNA-seq
## FileRNAseq = fichier rna-seq
## cond = cond comparée dans le fichier
## MoyBio = la moyenne bio calculée qPCR => à enlever ?
## base (TRUE or FALSE) présence des baseMeanA et B dans le fichier
CompMean_RNAseq = function(FileRNAseq, cond, base){
  
  dataBase = read.csv(FileRNAseq, header = TRUE)
  dataBase = dataBase[order(dataBase$Name),]
  condAB = unlist(strsplit(cond, "/"))
  condA = condAB[1]
  condB = condAB[2]
  
  ## si le fichier contient les colonnes basemeanA et basemeanB
  if ( base == TRUE ){
    dataBase = data.frame(id = dataBase$Name, BaseMeanA = dataBase$baseMeanA, BaseMeanB = dataBase$baseMeanB, 
                          Padj = dataBase$padj)
    
    BaseMean = dataBase
    
  }else{
    dataBase = data.frame(id = dataBase$Name, BaseMean = dataBase$baseMean, 
                          logFC = dataBase$log2FoldChange, padj = dataBase$padj)
    BaseMeanB = 2*dataBase$BaseMean / (1 + 2^dataBase$logFC)
    BaseMeanA = (2*dataBase$BaseMean / (1 + 2^dataBase$logFC))*2^dataBase$logFC
    dataBase2 = data.frame(id = dataBase$id, BaseMeanA = BaseMeanA, BaseMeanB = BaseMeanB, Padj = dataBase$padj)
    
    
    BaseMean = dataBase2
    
  }
  colnames(BaseMean) = c("id", condA, condB, "Padj")
  return(BaseMean)
}
#################
## code Server ##
#################

shinyServer(function(input, output, session) {
  observe({  
    y <- input$in1
    if(is.null(y)){
      return(NULL)
    }else{
      ### Récupére les gènes à partir des fichiers CT
      data.ct <- File.Ct(path(),as.numeric(input$in4),
                         as.numeric(input$in5),as.numeric(input$in6))$data.CT 
      Genes <- as.character(data.ct$id)
      #       print(Genes)
      cond <- colnames(data.ct)
      Cond <- unlist(unique(strsplit(cond,"[0-9]")))
      Cond  = Cond[-1]
      Cond  = Cond[!is.na(Cond)]
      Cond2 = rep(Cond, 3)
      #       cat("Cond : ")
      #       print(Cond)
      condLR = c()
      for (i in 1:(length(Cond))){
        for (j in 1:length(Cond)){
          if( Cond[i] != Cond[j] ){
            condPaste = paste(Cond[i], Cond[j], sep = "/")
            condLR =c(condLR, condPaste)
            
          }else{
            next
          }
        }
      }
      CondLR = unique(condLR)
      
      
      # met à jour le choix des gènes pour gènes de réf
      updateSelectInput(session, "in3", choices = Genes)
      # met à jour le choix des gènes pour le graphe 1
      updateSelectInput(session, "in9", choices = Genes)
      # met à jour le choix des conditions pour graphe 2
      updateSelectInput(session, "in10", choices = Cond)
      # met à jour le choix des gènes pour graphe 3
      updateSelectInput(session, "in11", choices = Genes)
      # met à jour les condtions pour le logRatio
      updateSelectInput(session, "lR", choices = CondLR)
      # met à jour pour la comparaison avec RNAseq
      updateSelectInput(session, "inCondRna", choices = CondLR)
      # met à jour le choix des gènes pour le graphe 2 RNA-seq
      updateSelectInput(session, "inGene", choices = Genes)
    }
  })
  
  output$out1  <- reactive({
    File <- input$in1
    if(is.null(File)){
      fichier = "None files are selectized"
      
    }else{
      Ct = File.Ct(path(),as.numeric(input$in4),
                   as.numeric(input$in5),as.numeric(input$in6))
      file = Ct$Nfiles
      Ok = Ct$Nok
      if (file == Ok){
        fichier = "All the files have the three columns"
      }else{
        fichier = "Some files have not the three columns"
      }
    }
    return(fichier)
    
  })
  
  path <- reactive({
    File <- input$in1
    if(is.null(File)){
      return(NULL)
    }
    File$datapath
  })
  #   
  #   Ct = File.Ct(path(),as.numeric(input$in4),
  #              as.numeric(input$in5),as.numeric(input$in6))
  
  #########################
  #### box : à tester avec d'autres fichiers
  ########################
  
  output$boxCond <- renderInfoBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    
    #     print(path())
    if ( is.null(path()) ){
      file = "N"
      cond = 0
    }else{
      Ct = File.Ct(path(),as.numeric(input$in4),
                   as.numeric(input$in5),as.numeric(input$in6))
      cond = Ct$Cond
      file = "O"
      #       print(cond)
    }
    
    
    infoBox( 
      title = "Conditions of the files", 
      icon =  if ( file == "N" || (file == "O" && cond != as.numeric(input$in4)) ) icon("thumbs-o-down") else icon("thumbs-o-up"), 
      color = if (file == "N" || (file == "O" && cond != as.numeric(input$in4))) "red" else "teal",
      fill = TRUE
      
    )
  })
  
  output$boxRepT <- renderInfoBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    if ( is.null(path()) ){
      file = "N"
      repT = 0
    }else{
      Ct = File.Ct(path(),as.numeric(input$in4),
                   as.numeric(input$in5),as.numeric(input$in6))
      #        print(Ct)
      RepT = Ct$repT
      file = "O"
      #       print(RepT)
    } 
    #     print(as.numeric(input$in5))
    infoBox( 
      title = "Technical replicates",
      icon =  if ( file == "N" || (file == "O" && RepT != as.numeric(input$in5)) ) icon("thumbs-o-down") else icon("thumbs-o-up"), 
      color = if (file == "N" || (file == "O" && RepT != as.numeric(input$in5)) ) "red" else "teal",
      fill = TRUE
      
    )
  })
  
  
  output$boxRepB <- renderInfoBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    if ( is.null(path()) ){
      file = "N"
      repB = 0
    }else{
      Ct = File.Ct(path(),as.numeric(input$in4),
                   as.numeric(input$in5),as.numeric(input$in6))
      repB = Ct$repB
      file = "O"
    }
    #print(repT) 
    infoBox( 
      title = "Biological replicates",
      width = 3,
      icon =  if ( file == "N" || (file == "O" && repB != as.numeric(input$in6)) ) icon("thumbs-o-down") else icon("thumbs-o-up"), 
      color = if (file == "N" || (file == "O" && repB != as.numeric(input$in6)) ) "red" else "teal", 
      fill = TRUE
      
    )
  })
  
  output$table1 <- DT::renderDataTable({
    data <- File.Ct(path(),as.numeric(input$in4),
                    as.numeric(input$in5),as.numeric(input$in6))
    datatable(data$data.CT, rownames = FALSE, filter = 'none', options = list(
      pageLength = 50, autoWidth = TRUE, scrollX = TRUE
    ))
  })
  
  output$downloadDataCt <- downloadHandler(
    filename = function() {
      paste('dataCt-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(File.Ct(path(),as.numeric(input$in4),
                        as.numeric(input$in5),as.numeric(input$in6))$data.CT, con)
    }
  )
  
  path2 <- reactive({
    File2 <- input$in2
    if(is.null(File2)){
      return(NULL)
    }
    File2$datapath
  })
  
  output$out2  <- reactive({
    File2 <- input$in2
    if(is.null(File2)){
      fichier = "None files are selectized"
      
    }else{
      E = File.E(File2$datapath)
      file = E$Nfiles
      Ok = E$Nok
      if (file == Ok){
        fichier = "All the files have the four columns"
      }else{
        fichier = "Some files have not the four columns"
      }
    }
    return(fichier)
  })
  
  output$table2 <- renderDataTable({
    data <- File.E(path2())$dataE
    datatable(data, rownames = FALSE, filter = 'none', options = list(
      pageLength = 50, autoWidth = TRUE, scrollX = TRUE
    ))
  })
  
  
  output$downloadDataE <- downloadHandler(
    filename = function() {
      paste('dataE-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(File.E(path2())$dataE, con)
    }
  )
  output$out3 <- renderPrint(input$in3)
  output$outlR<-renderPrint(input$lR)
  
  
  output$out4 <- renderPrint(input$in4)
  
  output$out5 <- renderPrint(input$in5)
  
  output$out6 <- renderPrint(input$in6)
  
  output$out7 <- renderPrint(input$in7)
  
  
  output$out8  <- reactive({
    File <- input$in1
    if(is.null(File)){
      GenePrst = "None files are selectized"
      
    }else{
      Ct = File.Ct(path(),as.numeric(input$in4),
                   as.numeric(input$in5),as.numeric(input$in6))
      GenePrst = Ct$Gprst
    }
    return(GenePrst)
  })
  #   output$out8 <- renderPrint(input$in8)
  #   
  
  #   
  output$table3 <- renderDataTable({
    data <- moyRepTech(as.numeric(input$in4),as.numeric(input$in6), as.numeric(input$in5),
                       File.Ct(path(),as.numeric(input$in4),
                               as.numeric(input$in5),as.numeric(input$in6))$data.CT)
    
    datatable(data, rownames = FALSE, filter = 'none', options = list(
      pageLength = 50, autoWidth = TRUE
    ))
    
  })
  #   
  output$downloadDataRepTech <- downloadHandler(
    filename = function() {
      paste('dataMoyRepTech-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(moyRepTech(as.numeric(input$in4),as.numeric(input$in6), as.numeric(input$in5),
                           File.Ct(path(), as.numeric(input$in4),
                                   as.numeric(input$in5),as.numeric(input$in6))$data.CT), con)
    }
  )
  
  output$table4 <- renderDataTable({
    data <- E_GenesRef(input$in3,File.E(path2())$dataE)
    datatable(data, rownames = FALSE, filter = 'none', options = list(
      pageLength = 50, autoWidth = TRUE
    ))
  })
  
  output$table5 <- renderDataTable({
    data <- z_GenesRef(input$in3, File.Ct(path(), as.numeric(input$in4),
                                          as.numeric(input$in5),as.numeric(input$in6))$data.CT)
    datatable(data, rownames = FALSE, filter = 'none', options = list(
      pageLength = 50, autoWidth = TRUE, scrollX = TRUE
    ))
  })
  #   
  output$table6 <- renderDataTable({
    data <- Norm(as.numeric(input$in4), as.numeric(input$in6), File.E(path2())$dataE,
                 moyRepTech(as.numeric(input$in4),as.numeric(input$in6),as.numeric(input$in5),
                            File.Ct(path(),as.numeric(input$in4), 
                                    as.numeric(input$in5),as.numeric(input$in6))$data.CT),
                 E_GenesRef(input$in3,File.E(path2())$dataE),
                 z_GenesRef(input$in3, File.Ct(path(), as.numeric(input$in4),
                                               as.numeric(input$in5),as.numeric(input$in6))$data.CT))   
    
    datatable(data, rownames = FALSE, filter = 'none', options = list(
      pageLength = 50, autoWidth = TRUE
    ))
  })
  #   
  
  output$downloadDataNorm <- downloadHandler(
    filename = function() {
      paste('dataNorm-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(Norm(as.numeric(input$in4), as.numeric(input$in6), File.E(path2())$dataE,
                     moyRepTech(as.numeric(input$in4),as.numeric(input$in6),as.numeric(input$in5),
                                File.Ct(path(), as.numeric(input$in4), 
                                        as.numeric(input$in5),as.numeric(input$in6))$data.CT),
                     E_GenesRef(input$in3,File.E(path2())$dataE),
                     z_GenesRef(input$in3, File.Ct(path(),as.numeric(input$in4),
                                                   as.numeric(input$in5),as.numeric(input$in6))$data.CT)) , con)
    }
  )
  output$table7 <- renderDataTable({
    data <- moyRepBio(as.numeric(input$in4), as.numeric(input$in6),
                      Norm(as.numeric(input$in4), as.numeric(input$in6), File.E(path2())$dataE,
                           moyRepTech(as.numeric(input$in4),as.numeric(input$in6),as.numeric(input$in5),
                                      File.Ct(path(),as.numeric(input$in4), 
                                              as.numeric(input$in5),as.numeric(input$in6))$data.CT),
                           E_GenesRef(input$in3,File.E(path2())$dataE),
                           z_GenesRef(input$in3, File.Ct(path(),as.numeric(input$in4),
                                                         as.numeric(input$in5),as.numeric(input$in6))$data.CT)))
    datatable(data, rownames = FALSE,  filter = 'none', options = list(
      pageLength = 50, autoWidth = TRUE
    ))
  })
  #   
  output$downloadDataRepBio <- downloadHandler(
    filename = function() {
      paste('dataMoyRepBio-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(moyRepBio(as.numeric(input$in4), as.numeric(input$in6),
                          Norm(as.numeric(input$in4), as.numeric(input$in6), File.E(path2())$dataE,
                               moyRepTech(as.numeric(input$in4),as.numeric(input$in6),as.numeric(input$in5),
                                          File.Ct(path(),as.numeric(input$in4), 
                                                  as.numeric(input$in5),as.numeric(input$in6))$data.CT),
                               E_GenesRef(input$in3,File.E(path2())$dataE),
                               z_GenesRef(input$in3, File.Ct(path(),as.numeric(input$in4),
                                                             as.numeric(input$in5),as.numeric(input$in6))$data.CT))), con)
    }
  )
  #   
  output$table8 <- renderDataTable({
    data <- logRatio(as.numeric(input$in4), 
                     moyRepBio(as.numeric(input$in4), as.numeric(input$in6),
                               Norm(as.numeric(input$in4), as.numeric(input$in6), 
                                    File.E(path2())$dataE, moyRepTech(as.numeric(input$in4), as.numeric(input$in6), as.numeric(input$in5),
                                                                      File.Ct(path(),as.numeric(input$in4), 
                                                                              as.numeric(input$in5),as.numeric(input$in6))$data.CT),
                                    E_GenesRef(input$in3,File.E(path2())$dataE),
                                    z_GenesRef(input$in3, File.Ct(path(),as.numeric(input$in4),
                                                                  as.numeric(input$in5),as.numeric(input$in6))$data.CT))),
                     input$lR)
    datatable(data, rownames = FALSE, filter = 'none', options = list(
      pageLength = 50, autoWidth = TRUE
    ))
  })
  #   
  output$downloadDataLogRatio <- downloadHandler(
    filename = function() {
      paste('dataLogRatio-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(logRatio(as.numeric(input$in4), 
                         moyRepBio(as.numeric(input$in4), as.numeric(input$in6),
                                   Norm(as.numeric(input$in4), as.numeric(input$in6), 
                                        File.E(path2())$dataE, moyRepTech(as.numeric(input$in4), as.numeric(input$in6), as.numeric(input$in5),
                                                                          File.Ct(path(),as.numeric(input$in4), 
                                                                                  as.numeric(input$in5),as.numeric(input$in6))$data.CT),
                                        E_GenesRef(input$in3,File.E(path2())$dataE),
                                        z_GenesRef(input$in3, File.Ct(path(),as.numeric(input$in4),
                                                                      as.numeric(input$in5),as.numeric(input$in6))$data.CT))),
                         input$lR), con)
    }
  )
  #   
  output$plot1 <- renderPlot({
    
    #     # Render a barplot
    #     moy <- moyRepBio(as.numeric(input$in4), as.numeric(input$in6),
    #                      Norm(as.numeric(input$in4), as.numeric(input$in6), File.E(input$in0,input$in2),
    #                           moyRepTech(as.numeric(input$in4),as.numeric(input$in6), as.numeric(input$in5),
    #                                      File.Ct(input$in0,input$in1,as.numeric(input$in4), 
    #                                              as.numeric(input$in5),as.numeric(input$in6))),
    #                           E_GenesRef(input$in3,File.E(input$in0,input$in2)),
    #                           z_GenesRef(input$in3, File.Ct(input$in0,input$in1,as.numeric(input$in4),
    #                                                         as.numeric(input$in5),as.numeric(input$in6)))))
    # #     barplot(as.numeric(moy[moy$id == input$in9, 2:(as.numeric(input$in4)+1)]), 
    # #             main=paste("Moyenne Gène", input$in9, sep=" "), xlab="Condition", ylab="Moyenne", 
    # #             names.arg=colnames(moy[,2:(as.numeric(input$in4)+1)]))
    #     
    #     #### GRAPHE avec ggplot2 #####
    #     colnames(moy)<-c("id","moy.Fus","moy.Ov","moy.Tri")
    # 
    #     moyRe<-reshape(moy, idvar = "gene", ids = row.names(moy), timevar = "condition", direction = "long", varying = 2:4, sep=".")
    #     
    #     # moyRe$id<-as.factor(moyRe$id)
    #     df<-as.data.frame(moyRe[,1:3])
    #     gene<-input$in9
    #     df2<-data.frame(id=NULL,condition=NULL,moy=NULL)
    #     df2<-rbind(df2,df[which(df$id== gene ),])
    #     # Changer les couleurs manuellement
    #     p <- ggplot(data=df2, aes(x=id, y=moy, fill=condition)) +
    #       geom_bar(stat="identity", color="black", position=position_dodge())+
    #       theme_minimal()
    #     
    #     # palettes de couleurs de type brewer
    #     p + scale_fill_brewer(palette="Blues") + ggtitle(paste("barplot du gène",gene,sep=" "))
    plotInput1()
  })
  #   
  plotInput1<-function(){
    moy <- moyRepBio(as.numeric(input$in4), as.numeric(input$in6),
                     Norm(as.numeric(input$in4), as.numeric(input$in6), File.E(path2())$dataE,
                          moyRepTech(as.numeric(input$in4),as.numeric(input$in6), as.numeric(input$in5),
                                     File.Ct(path(),as.numeric(input$in4), 
                                             as.numeric(input$in5),as.numeric(input$in6))$data.CT),
                          E_GenesRef(input$in3,File.E(path2())$dataE),
                          z_GenesRef(input$in3, File.Ct(path(),as.numeric(input$in4),
                                                        as.numeric(input$in5),as.numeric(input$in6))$data.CT)))
    #     barplot(as.numeric(moy[moy$id == input$in9, 2:(as.numeric(input$in4)+1)]), 
    #             main=paste("Moyenne Gène", input$in9, sep=" "), 
    #             xlab="Condition", ylab="Moyenne", 
    #             names.arg=colnames(moy[,2:(as.numeric(input$in4)+1)]))
    
    #### GRAPHE avec ggplot2 #####
    nbCond = dim(moy)[2]-1
    cond = colnames(moy[2:(nbCond+1)])
    nameCol = paste("moy", cond, sep = ".")
    colnames(moy)<-c("id",nameCol)
    
    moyRe<-reshape(moy, idvar = "gene", ids = row.names(moy), timevar = "condition", direction = "long", varying = 2:4, sep=".")
    
    # moyRe$id<-as.factor(moyRe$id)
    df<-as.data.frame(moyRe[,1:3])
    min.df = min(df$moy)
    max.df = max(df$moy) + 10
    
    gene<-input$in9
    df2<-data.frame(id=NULL,condition=NULL,moy=NULL)
    df2<-rbind(df2,df[which(df$id== gene ),])
    max.df2 = max(df2$moy) +  10
    ### mettre à jour le sliderInput avec le max du fichier
    if( input$slide1 < max.df2 ){
      updateSliderInput(session, "slide1", max = max.df, value = max.df2)
    }else{
      updateSliderInput(session, "slide1", max = max.df)      
    }
    # Changer les couleurs manuellement
    p <- ggplot(data=df2, aes(x=id, y=moy, fill=condition)) +
      geom_bar(stat="identity", position=position_dodge()) +
      theme_minimal()
    
    # palettes de couleurs de type brewer
    p + scale_fill_brewer(palette=input$col1) + 
      ggtitle(paste("Gene",gene,sep=" ")) + xlab("") + ylab("") + ylim(min = 0, max = input$slide1)
    
  }
  #   
  output$downloadPlot1 <- downloadHandler(
    filename = function() {
      paste("barplot", paste(input$in9, ".png", sep=""), sep="_")
    },
    content = function(file) {
      ggsave(file,plotInput1())
    }
  )
  
  output$plot2 <- renderPlot({
    #     
    #     # Render a barplot
    #     moy <- moyRepBio(as.numeric(input$in4), as.numeric(input$in6),
    #                      Norm(as.numeric(input$in4), as.numeric(input$in6), File.E(input$in0,input$in2),
    #                           moyRepTech(as.numeric(input$in4),as.numeric(input$in6), as.numeric(input$in5),
    #                                      File.Ct(input$in0,input$in1,as.numeric(input$in4), 
    #                                              as.numeric(input$in5),as.numeric(input$in6))),
    #                           E_GenesRef(input$in3,File.E(input$in0,input$in2)),
    #                           z_GenesRef(input$in3, File.Ct(input$in0,input$in1,as.numeric(input$in4),
    #                                                         as.numeric(input$in5),as.numeric(input$in6)))))
    #     
    #     #       name<-colnames(moy[,1:(as.numeric(input$in4)+1)])
    #     #       cond<-pmatch(input$in10,name)
    #     # main=paste("", input$in10, sep=" "), xlab="Gènes", ylab="Intensité", 
    #     # names.arg=rownames(moy)
    #     name<-colnames(moy[,1:(as.numeric(input$in4)+1)])
    #     cond<-pmatch(input$in10,name)
    #     barplot(as.numeric(moy[, cond]), 
    #             main=paste("Condition", input$in10, sep=" "), 
    #             xlab="Gènes", ylab="Moyenne", 
    #             names.arg=moy$id)
    
    plotInput2()
  })
  
  plotInput2<-function(){
    moy <- moyRepBio(as.numeric(input$in4), as.numeric(input$in6),
                     Norm(as.numeric(input$in4), as.numeric(input$in6), File.E(path2())$dataE,
                          moyRepTech(as.numeric(input$in4),as.numeric(input$in6), as.numeric(input$in5),
                                     File.Ct(path(),as.numeric(input$in4), 
                                             as.numeric(input$in5),as.numeric(input$in6))$data.CT),
                          E_GenesRef(input$in3,File.E(path2())$dataE),
                          z_GenesRef(input$in3, File.Ct(path(),as.numeric(input$in4),
                                                        as.numeric(input$in5),as.numeric(input$in6))$data.CT)))
    
    #### GRAPHE avec ggplot2 #####
    nbCond = dim(moy)[2]-1
    cond = colnames(moy[2:(nbCond+1)])
    nameCol = paste("moy", cond, sep = ".")
    colnames(moy)<-c("id",nameCol)
    
    moyRe<-reshape(moy, idvar = "gene", ids = row.names(moy), timevar = "condition", direction = "long", varying = 2:4, sep=".")
    
    # moyRe$id<-as.factor(moyRe$id)
    df<-as.data.frame(moyRe[,1:3])
    min.df = min(df$moy)
    max.df = max(df$moy) + 10
    cond<-input$in10
    df2<-data.frame(id=NULL,condition=NULL,moy=NULL)
    df2<-rbind(df2,df[which(df$condition == cond ),])
    
    max.df2 = max(df2$moy) + 10
    ### mettre à jour le sliderInput avec le max du fichier
    if( input$slide1 < max.df2 ){
      updateSliderInput(session, "slide2", max = max.df, value = max.df2)
    }else{
      updateSliderInput(session, "slide2", max = max.df)      
    }
    
    # Changer les couleurs manuellement
    p <- ggplot(data=df2, aes(x=id, y=moy)) +
      geom_bar(stat="identity", color="black", fill=input$col2, position=position_dodge())+
      theme_minimal()
    
    # palettes de couleurs de type brewer
    p + scale_fill_brewer(palette=input$col2) + 
      ggtitle(paste("Condition", cond, sep=" ")) + xlab("") + ylab("") + ylim(min = 0, max = input$slide2)
    
  }
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      paste("barplot", paste(input$in10, ".png", sep=""), sep="_")
    },
    content = function(file) {
      ggsave(file,plotInput2(),width = 21)
    }
  )
  
  #   
  output$plot3 <- renderPlot({
    #     
    #     # Render a barplot
    #     moy <- moyRepBio(as.numeric(input$in4), as.numeric(input$in6),
    #                      Norm(as.numeric(input$in4), as.numeric(input$in6), File.E(path2())$dataE,
    #                           moyRepTech(as.numeric(input$in4),as.numeric(input$in6), as.numeric(input$in5),
    #                                      File.Ct(path(),as.numeric(input$in4), 
    #                                              as.numeric(input$in5),as.numeric(input$in6))),
    #                           E_GenesRef(input$in3,File.E(path2())$dataE),
    #                           z_GenesRef(input$in3, File.Ct(path(),as.numeric(input$in4),
    #                                                         as.numeric(input$in5),as.numeric(input$in6)))))
    #     
    #     colnames(moy)<-c("id","moy.Fus","moy.Ov","moy.Tri")
    #     moyRe<-reshape(moy, idvar = "gene", ids = row.names(moy), timevar = "condition", direction = "long", varying = 2:4, sep=".")
    #     df<-as.data.frame(moyRe[,1:3])
    #     gene<-input$in11
    #     
    #     ### graphe avec ggplot2
    #     df2<-data.frame(id=NULL,condition=NULL,moy=NULL)
    #     require(ggplot2)
    #     for (i in 1:length(gene)){
    #       df2<-rbind(df2,df[which(df$id== gene[i] ),])
    #     }
    #     # Changer les couleurs manuellement
    #     p <- ggplot(data=df2, aes(x=id, y=moy, fill=condition)) +
    #       geom_bar(stat="identity", color="black", position=position_dodge())+
    #       theme_minimal()
    #     
    #     # palettes de couleurs de type brewer
    #     p + scale_fill_brewer(palette="Blues")
    plotInput3()
  })
  
  plotInput3<-function(){
    moy <- moyRepBio(as.numeric(input$in4), as.numeric(input$in6),
                     Norm(as.numeric(input$in4), as.numeric(input$in6), File.E(path2())$dataE,
                          moyRepTech(as.numeric(input$in4),as.numeric(input$in6), as.numeric(input$in5),
                                     File.Ct(path(),as.numeric(input$in4), 
                                             as.numeric(input$in5),as.numeric(input$in6))$data.CT),
                          E_GenesRef(input$in3,File.E(path2())$dataE),
                          z_GenesRef(input$in3, File.Ct(path(),as.numeric(input$in4),
                                                        as.numeric(input$in5),as.numeric(input$in6))$data.CT)))
    
    ### Création des graoes avec GGplot2
    nbCond = dim(moy)[2]-1
    cond = colnames(moy[2:(nbCond+1)])
    nameCol = paste("moy", cond, sep = ".")
    colnames(moy)<-c("id",nameCol)
    
    moyRe<-reshape(moy, idvar = "gene", ids = row.names(moy), timevar = "condition", direction = "long", varying = 2:4, sep=".")
    df<-as.data.frame(moyRe[,1:3])
    gene<-input$in11
    min.df = min(df$moy)
    max.df = max(df$moy) + 10
    
    ### graphe avec ggplot2
    df2<-data.frame(id=NULL,condition=NULL,moy=NULL)
    require(ggplot2)
    for (i in 1:length(gene)){
      df2<-rbind(df2,df[which(df$id== gene[i] ),])
    }
    
    max.df2 = max(df2$moy) + 10
    ### mettre à jour le sliderInput avec le max du fichier
    if( input$slide3 < max.df2 ){
      updateSliderInput(session, "slide3", max = max.df, value = max.df2)
    }else{
      updateSliderInput(session, "slide3", max = max.df)      
    }
    
    # Changer les couleurs manuellement
    p <- ggplot(data=df2, aes(x=id, y=moy, fill=condition)) +
      geom_bar(stat="identity", color="black", position=position_dodge())+
      theme_minimal() 
    # +
    #   theme(legend.position="none")
    
    # palettes de couleurs de type brewer
    p + scale_fill_brewer(palette=input$col3) + xlab("") + ylab("") + ylim(min = 0, max = input$slide3) 
  }
  output$downloadPlot3 <- downloadHandler(
    filename = function() {
      paste("barplot", "genes.png", sep="_")
    },
    content = function(file) {
      ggsave(file,plotInput3(), width = 21)
    }
  )
  
  pathRNAseq <- reactive({
    FileRNA <- input$inRnaSeq
    if(is.null(FileRNA)){
      return(NULL)
    }
    FileRNA$datapath
  })
  
  # output$boxRNA <- renderInfoBox({
  #   # The downloadRate is the number of rows in pkgData since
  #   # either startTime or maxAgeSecs ago, whichever is later.
  #   
  #   #     print(path())
  #   if ( is.null(pathRNAseq()) ){
  #     file = "N"
  #   }else{
  #     file = "O"
  #     #       print(cond)
  #   }
  #   
  #   
  #   infoBox( 
  #     title = "Loading of file", 
  #     icon =  if ( file == "N" ) icon("thumbs-o-down") else icon("thumbs-o-up"), 
  #     color = if (file == "N" ) "red" else "teal",
  #     fill = TRUE
  #     
  #   )
  # })
  # 
  output$RNAseq <- renderPlotly({
    plotRNAseq()
  })
  
  # output$tableRNAseq <- renderDataTable({
  #   data = CompLog_RNAseq(FileRNAseq = pathRNAseq(), 
  #                  cond = input$inCondRna, 
  #                  log = logRatio(as.numeric(input$in4), 
  #                                 moyRepBio(as.numeric(input$in4), as.numeric(input$in6),
  #                                           Norm(as.numeric(input$in4), as.numeric(input$in6), 
  #                                                File.E(path2())$dataE, moyRepTech(as.numeric(input$in4), as.numeric(input$in6), as.numeric(input$in5),
  #                                                                                  File.Ct(path(),as.numeric(input$in4), 
  #                                                                                          as.numeric(input$in5),as.numeric(input$in6))$data.CT),
  #                                                E_GenesRef(input$in3,File.E(path2())$dataE),
  #                                                z_GenesRef(input$in3, File.Ct(path(),as.numeric(input$in4),
  #                                                                              as.numeric(input$in5),as.numeric(input$in6))$data.CT))),
  #                                 input$lR))
  #                  
  #   datatable(data$logRNAseq, rownames = FALSE, filter = 'none', options = list(
  #     pageLength = 50, autoWidth = TRUE, scrollX = TRUE
  #   ))
  # })
  
  plotRNAseq<-function(){
    RNAqPCR <- CompLog_RNAseq(FileRNAseq = pathRNAseq(), 
                              cond = input$inCondRna, 
                              log = logRatio(as.numeric(input$in4), 
                                             moyRepBio(as.numeric(input$in4), as.numeric(input$in6),
                                                       Norm(as.numeric(input$in4), as.numeric(input$in6), 
                                                            File.E(path2())$dataE, moyRepTech(as.numeric(input$in4), as.numeric(input$in6), as.numeric(input$in5),
                                                                                              File.Ct(path(),as.numeric(input$in4), 
                                                                                                      as.numeric(input$in5),as.numeric(input$in6))$data.CT),
                                                            E_GenesRef(input$in3,File.E(path2())$dataE),
                                                            z_GenesRef(input$in3, File.Ct(path(),as.numeric(input$in4),
                                                                                          as.numeric(input$in5),as.numeric(input$in6))$data.CT))),
                                             input$lR))$logMerge 
    ## Graphes avec GGplot2
    colnames(RNAqPCR)[2] = "RNAseq"
    
    # m = lm(RNAqPCR$qPCR ~ RNAqPCR$RNAseq)
    # print(summary(m)$r.squared)
    # p = ggplot(data=RNAqPCR, aes(x=RNAseq, y=qPCR)) +
    #   # geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
    #   geom_point() + theme_linedraw()
    # p + stat_smooth(method="lm", se = FALSE, col="red")
    
    
    ggplotRegression <- function (fit, RNA) {
      
      Padj = RNA$padj
      Name = RNA$id
      # aes(colour = Padj), aes(text = paste("Gene:", Name))
      p <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
        geom_point(aes(text = paste("Gene:", Name))) +
        theme_linedraw()  +
        stat_smooth(method = "lm", col = "red", se = FALSE) +
        labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                           "Intercept =",signif(fit$coef[[1]],5 ),
                           " Slope =",signif(fit$coef[[2]], 5),
                           " P =",signif(summary(fit)$coef[2,4], 5)))
      
      
      ggplotly(p)
    }
    fit1 <- lm(qPCR ~ RNAseq, data = RNAqPCR)
    ggplotRegression(fit1, RNAqPCR)
    # print(RNAqPCR)
  }
  
  plotRNA1<-function(){
    moy <- moyRepBio(as.numeric(input$in4), as.numeric(input$in6),
                     Norm(as.numeric(input$in4), as.numeric(input$in6), File.E(path2())$dataE,
                          moyRepTech(as.numeric(input$in4),as.numeric(input$in6), as.numeric(input$in5),
                                     File.Ct(path(),as.numeric(input$in4),
                                             as.numeric(input$in5),as.numeric(input$in6))$data.CT),
                          E_GenesRef(input$in3,File.E(path2())$dataE),
                          z_GenesRef(input$in3, File.Ct(path(),as.numeric(input$in4),
                                                        as.numeric(input$in5),as.numeric(input$in6))$data.CT)))
    base <- CompMean_RNAseq (FileRNAseq= pathRNAseq(),
                             cond = input$inCondRna, base = input$checkbox)
    
    condA = colnames(base)[2]
    # print(condA)
    condB = colnames(base)[3]
    # print(condB)
    # print(colnames(BaseMean))
    moy = moy[,c("id", condA, condB)]
    BaseMean = cbind(base, experience = "RNA-seq")
    MeanPCR = cbind(moy, Padj = NA, experience = "qRT-PCR")
    
    
    colnames(BaseMean) = c("id", condA, condB, "Padj", "experience")
    colnames(MeanPCR) = c("id", condA, condB, "Padj", "experience")
    head(BaseMean)
    head(MeanPCR)
    df = rbind(MeanPCR, BaseMean)
    # head(df)
    df = as.data.frame(df)
    gene = input$inGene
    df2 = df[which(df[,1] == gene),]
    df3 = melt(df2, id.vars = c("id", "experience", "Padj"), variable.name = "condition", value.name = "MoyBio")
    Padj = unique(as.vector(df3$Padj[which(!is.na(df3$Padj))]))
    Padj = as.numeric(Padj)
    if ( Padj <= 0.001){
      lab = "***"
    }else if ( Padj <= 0.01 && Padj > 0.001){
      lab = "**"
    }else if ( Padj <= 0.05 && Padj > 0.01){
      lab="*"
    }else{
      lab=""
    }
    
    
    p <- ggplot(data=df3, aes(x=experience, y=as.numeric(MoyBio), fill=condition)) +
      geom_bar(stat="identity", color="black", position=position_dodge())+
      theme_minimal() + geom_text(aes(x = "RNA-seq",y = max(as.numeric(df3$MoyBio[which(df3$experience == "RNA-seq")])) +
                                        min(as.numeric(df3$MoyBio[which(df3$experience == "RNA-seq")])) + 1/4*max(as.numeric(df3$MoyBio[which(df3$experience == "RNA-seq")])),
                                      label = paste(lab, paste("Padj : ", as.numeric(unique(as.vector(df3$Padj[which(!is.na(df3$Padj))]))), sep=""), sep = "\t")))
    
    # palettes de couleurs de type brewer
    p + scale_fill_brewer(palette=input$colRNA) + 
      ggtitle(paste("Biological mean got by a qRT-PCR and RNA-seq experience in each condition for the gene",gene,sep=" ")) + ylab("Biological Mean")
    
  }
  
  output$RNA1 <- renderPlot({
    plotRNA1()
  })
  
  output$downloadPlotRNA1 <- downloadHandler(
    filename = function() {
      paste("barplot", paste(input$inCondRna, "png", sep = "."), sep="_")
    },
    content = function(file) {
      ggsave(file,plotRNA1())
    }
  )
  
  
})



# à regarder pour faire la barre d'erreur
# http://www.sthda.com/french/wiki/ggplot2-barplots-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees
# colCond=colnames(moy[moy$id=="19761", 2:(condition+1)])
# moyenne=as.numeric(moy[moy$id=="19761", 2:(condition+1)])
# df <- data.frame(condition=colCond, moyenne=moyenne)