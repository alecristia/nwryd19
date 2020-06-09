read.csv("NWR-demo.csv",header=T)->demo
#colnames(demo)

#if family is missing, attribute each child to a different one
demo$familyID=as.character(demo$familyID)
demo$familyID[is.na(demo$familyID)]<-paste("k",demo$ID[is.na(demo$familyID)])

subset(demo,included=="yes" & Age <12 & !is.na(Age))->inc
#if age.exact is missing, use reported age
inc$age.exact[is.na(inc$age.exact)]<-inc$Age[is.na(inc$age.exact)]
read.delim("NWR-transcription.txt", encoding="UTF-8")->trnsc
trnsc$tokid=paste(trnsc$item,trnsc$token)

read.table("final_order.txt",header=T)->crp
crp$tokid=paste(crp$target,crp$nb)
npairs=dim(crp)[1]

trnsc=merge(trnsc,crp,by.x="tokid",by.y="tokid",all=T)

#order so we have all the items by a given child in the right chrono order
trnsc[order(trnsc$file, trnsc$int),]->trnsc

#add variable coding whether it's the child's first or subsequent attempts
trnsc$previous<-c(NA,as.character(trnsc$target[1:(dim(trnsc)[1]-1)]))
trnsc$attempt<-ifelse(trnsc$target!=trnsc$previous,"first","subsequent")
trnsc$attempt[1]<-"first"

#add repetition number
trnsc$rep=gsub(".*_","",gsub(".wav","",trnsc$outfile))

#add coding of correctness online/on the fly of first presentation as a function whether there are subsequent reps of the same target
trnsc$cor.online=1
for(i in 1:npairs) if(trnsc$rep[i]>1) trnsc$cor.online[i-1]<-0  #if the item is repeated, then code the previous one as being wrong
trnsc$cor.online[(npairs+1):dim(trnsc)[1]]<-NA

merge(trnsc,demo,by.x="id",by.y="ID",all.x=T)->trnsc

#add orthographic representations targets & other stims chars
read.delim("stimuli.txt", encoding="UTF-8")->stims



merge(trnsc,stims,by="target",all=T)->trnsc


#DATA REMOVAL *** ATTENTION *** 
#REMOVE ALL TASK 4 (whatever that is)
trnsc[!is.na(trnsc$target),]->trnsc

#REMOVING ALL THE "YI"
trnsc[trnsc$target!="yi",]->trnsc

#remove practice items
trnsc=trnsc[trnsc$type !="practice",]


## PHONOLOGIZE AND SCORE

#log non-fluent speech, then get rid of that marker
trnsc$nonfluent<-NA
trnsc$nonfluent[grep("-",trnsc$mp_uni,fixed=T)]<-1
trnsc$mispronunciation=gsub("-","",trnsc$mispronunciation)

# make all vowels in mispronunciation & target short (so we don't penalize length errors)
trnsc$mp=gsub("([aeiouêâéáóî])\\1+", "\\1", trnsc$mispronunciation)
trnsc$target_ortho=gsub("([aeiouêâéáóî])\\1+", "\\1", trnsc$target_ortho)

# if correct=0 but orthotarget=mispronunciation (probably due to length errors) then change correct to 1 
#sum(trnsc$correct==0 & trnsc$target_ortho == trnsc$mp & !is.na(trnsc$mp)) #this only happens twice
trnsc$correct[trnsc$correct==0 & trnsc$target_ortho == trnsc$mp & !is.na(trnsc$mp)]<-1

# create phonological correspondance matrix
# NOTE!! NON INTUITIVE MAPPING!!!! DO NOT READ THE OUTPUT BELIEVING IT IS PSEUDO IPA!!!
# NOTE2!!! it's better if the chars are not accented & since we don't distinguish long & short, we'll use capitals for nasal & small caps for oral, with no distinction between short and long (see above)
correspondances=matrix( #list correspondances always by pairs, orthography then phonology
  c(
    #nasal  to unichar
    ":i","1",
    ":e","2",
    ":a","3",
    ":ê","4",
    ":â","5",
    ":u","6",
    ":o","7",
    #non-nasal to unaccented
    "î","8",
    "ê","9",
    "â","0",
    "é","E",
    "á","A",
    "ó","O",
    #consonants, veeeery dirty!
    "'n","ń",
    "tp",":",
    "dp","/",
    "kp","P",
    "ngm","G",
    "ńm","M",
    "ng","N",
    "mb","b",
    "nd","D",
    "nt","T",
    "nk","K",
    "dn","?",
    "kn","!",
    "vy","V",
    "mt","C",
    "md","J",
    "mg","X",
    "dm","S",
    "km","Z",
    "gh","H",
    "lv","L",
    "ch","†" #this one doesn't exist in the local orthography but it appears
  ),#last item above should not have a comma
  ncol = 2,byrow = T)

#initialize the unichar phono-like representation
trnsc$mp_uni=trnsc$mp #start from ortho
trnsc$target_uni=trnsc$target_ortho #start from ortho

for(i in 1:dim(correspondances)[1]) { #transform into unicharacter
  trnsc$mp_uni=gsub(correspondances[i,1],correspondances[i,2],trnsc$mp_uni)
  trnsc$target_uni=gsub(correspondances[i,1],correspondances[i,2],trnsc$target_uni)
}


#calculate Levinshtein's distances 
trnsc$ins=trnsc$del=trnsc$sub=trnsc$nchar=trnsc$trafos=trnsc$tarlen=NA
substitution_bank=NULL
deletion_bank=NULL
for(i in grep("0",trnsc$correct)){
  #lev
  lev=adist(trnsc[i,c("mp_uni")],trnsc[i,c("target_uni")],count=T)
  trnsc$ins[i]<-attributes(lev)$counts[,,"ins"]
  trnsc$del[i]<-attributes(lev)$counts[,,"del"]
  trnsc$sub[i]<-attributes(lev)$counts[,,"sub"]
  trnsc$trafos[i]=attributes(lev)$trafos
  trnsc$nchar[i]=nchar(attributes(lev)$trafos)
  trnsc$tarlen[i]=nchar(trnsc$target_uni[i])
  sublist=which(strsplit(attributes(lev)$trafos, "")[[1]]=="S")
  substitution_bank=rbind(substitution_bank,cbind(strsplit(trnsc[i,c("target_uni")],"")[[1]][sublist],strsplit(trnsc[i,c("mp_uni")],"")[[1]][sublist]))
  dellist=which(strsplit(attributes(lev)$trafos, "")[[1]]=="D")
  deletion_bank=rbind(deletion_bank,cbind(strsplit(trnsc[i,c("target_uni")],"")[[1]][dellist]))
}


trnsc$ld=rowSums(trnsc[,c("ins","del","sub")])
trnsc$nld=trnsc$ld/trnsc$nchar
trnsc$nld[is.na(trnsc$nld)]<-0

#proportion correct: remove Substitutions Deletions and Insertions & count only proportion of matches
trnsc$pc=nchar(gsub("[SDI]","",trnsc$trafos))/trnsc$nchar
trnsc$pc[is.na(trnsc$pc)]<-1

write.table(trnsc,"final_data.txt",row.names=F,sep="\t")


#back-transform into orthography
for(i in dim(correspondances)[1]:1) { #
   substitution_bank=gsub(correspondances[i,2],correspondances[i,1],substitution_bank,fixed=T)
   deletion_bank=gsub(correspondances[i,2],correspondances[i,1],deletion_bank,fixed=T)
}

substitution_bank[order(substitution_bank[,1]),]->substitution_bank
write.table(substitution_bank,"substitution_bank.txt",row.names=F,sep="\t")

sort(deletion_bank)->deletion_bank
write.table(deletion_bank,"deletion_bank.txt",row.names=F,sep="\t")