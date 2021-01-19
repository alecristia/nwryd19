library(tidyverse)
library(openxlsx)

read.delim("NWR-transcription.txt", encoding="UTF-8")->trnsc


#DATA REMOVAL *** ATTENTION *** 
#REMOVE ALL TASK 4 (whatever that is)
# M2A: This appears to be a second pass over productions that were deemed to
# be words; the elicitation here is just a translation of what word the
# production was heard to be
trnsc[trnsc$item!="task4",]->trnsc

dim(trnsc) #1488 judgments

trnsc$tokid=paste(trnsc$item,trnsc$token)


# - ? correct treatment of words whose mispronunciation is an English word:
#   mispronunciation meaning
# 361              two     two
# 362              two     two
# --> these two were converted to YD orthography
# 
# The following have not been changed in any way
# 732             loudî      English: loud
# 879          no-pinêt                                          English: no peanut
# 908            nodedi                                           English: no daddy
# 933          no--dadi                                           English: no daddy
# 949           petrumi                                             English: "true"
# 950           petrumi                                             English: "true"
# 951           petrumi                                             English: "true"
# 1373              wee     English: where
# 1374              wee     English: where
# 1375              wee     English: where
# 1379              wee     English: where
# 1380              wee     English: where
# 1388              wee     English: where
# 1394              wee     English: where
# 1396              wee     English: where
# 1398              wee     English: where
# 1399              wee     English: where
# 1402              wat      English: what
# 1405              wat      English: what
# 1411              wee     English: where

trnsc$mispronunciation=as.character(trnsc$mispronunciation)
trnsc$meaning=as.character(trnsc$meaning)
trnsc$mispronunciation[trnsc$mispronunciation=="two"]<-"tuu"
trnsc[grep("Eng",trnsc$meaning),c("mispronunciation","meaning")]

read.table("final_order.txt",header=T)->crp
crp$tokid=paste(crp$target,crp$nb)
npairs=dim(crp)[1] #1488 pairs

trnsc=merge(trnsc,crp,by.x="tokid",by.y="tokid")
dim(trnsc) #haven't lost any judgments

#order so we have all the items by a given child in the right chrono order
trnsc[order(trnsc$file, trnsc$int),]->trnsc

#add repetition number
trnsc$rep=gsub(".*_","",gsub(".wav","",trnsc$outfile))


first.prods <- trnsc %>%
   group_by(id, item) %>%
   summarize(min.int = min(int)) %>%
   mutate(attempt = "first") %>%
   rename("int" = min.int)
trnsc <- trnsc %>%
   left_join(first.prods, by = c("id", "item", "int")) %>%
   replace_na(list(attempt = "subsequent"))


dim(trnsc)
  
# add demographic info
read.csv("NWR-demo.csv",header=T)->demo
#colnames(demo)

#if family is missing, attribute each child to a different one
demo$familyID=as.character(demo$familyID)
demo$familyID[is.na(demo$familyID)]<-paste("k",demo$ID[is.na(demo$familyID)])

#focus on kids who'll be included
subset(demo,included=="yes" & Age <12 & !is.na(Age))->inc
#if age.rounded is missing, use reported age
inc$age.rounded[is.na(inc$age.rounded)]<-inc$Age[is.na(inc$age.rounded)]

merge(trnsc,inc,by.x="id",by.y="ID",all.x=T)->trnsc
dim(trnsc) #still no loss, all good

#add orthographic representations targets & other stims chars
#   
# also, notice that stims and segments are merged using ortho, so as long as ortho is matched
# across the two, the merge should carry over whatever changes you make


#get info on segments
read.xlsx("segments.xlsx")->segments
#add corpus freq to segments
read.delim("segment-counts.txt", encoding="UTF-8",sep="\t",header=T)->phone_counts
merge(segments,phone_counts,by.x="ortho",by.y="ortho",all.x=T)->segments
#add corpus freq TYPES to segments
read.delim("segment-counts-types.txt", encoding="UTF-8",sep="\t",header=T)->phone_counts_types
merge(segments,phone_counts_types,by.x="ortho",by.y="ortho",all.x=T)->segments
write.table(segments,"segments_with_cor_freq.txt",sep="\t",row.names = F)

#read in table with breakdown 
read.xlsx("stimuli.xlsx")->stims
#add average crosslinguistic frequency
stim_seg_freq=matrix(NA,nrow=dim(stims)[1],ncol=8)
for(i in 1:dim(stim_seg_freq)[1]) for(j in 1:dim(stim_seg_freq)[2]) {
  thisfreq=segments$pc[as.character(segments$ortho)==as.character(stims[i,paste0("ortho",j)])]
  stim_seg_freq[i,j]<-ifelse(sum(!is.na(thisfreq))==1,thisfreq,NA)
}
stims$avg_fr=apply(stim_seg_freq,1,mean,na.rm=T)

#add average corpus frequency - LOGGED
stim_seg_freq=matrix(NA,nrow=dim(stims)[1],ncol=8)
for(i in 1:dim(stim_seg_freq)[1]) for(j in 1:dim(stim_seg_freq)[2]) {
  thisfreq=segments$freq_corpus[as.character(segments$ortho)==as.character(stims[i,paste0("ortho",j)])] 
  stim_seg_freq[i,j]<-ifelse(sum(!is.na(thisfreq))==1,thisfreq,NA)
}
stims$avg_fr_cor=apply(log(stim_seg_freq),1,mean,na.rm=T)

#add average corpus frequency based on TYPES - LOGGED
stim_seg_freq=matrix(NA,nrow=dim(stims)[1],ncol=8)
for(i in 1:dim(stim_seg_freq)[1]) for(j in 1:dim(stim_seg_freq)[2]) {
  thisfreq=segments$freq_corpus_types[
    as.character(segments$ortho)==as.character(stims[i,paste0("ortho",j)])] 
  stim_seg_freq[i,j]<-ifelse(sum(!is.na(thisfreq))==1,thisfreq,NA)
}
stims$avg_fr_cor_ty=apply(log(stim_seg_freq),1,mean,na.rm=T)

merge(trnsc,stims,by="target",all.x=T)->trnsc
dim(trnsc)

#REMOVING ALL THE "YI"
trnsc[trnsc$target!="yi",]->trnsc


#REMOVING ALL THE "TPA"
trnsc[trnsc$target!="tpa",]->trnsc
trnsc[trnsc$target!="tp_a",]->trnsc

#remove practice items
trnsc=trnsc[trnsc$type !="practice",]
dim(trnsc) #2021-01-19 1243

## PHONOLOGIZE AND SCORE

#initialize the unichar phono-like representation
trnsc$mp_uni=trnsc$mispronunciation #start from ortho
trnsc$target_uni=trnsc$target_ortho #start from ortho

head(trnsc[,c("mispronunciation","mp_uni","target_uni")])

#log non-fluent speech, then get rid of that marker
trnsc$nonfluent<-NA
trnsc$nonfluent[grep("-",trnsc$mp_uni,fixed=T)]<-1
table(trnsc$nonfluent) #2020-09-16 39
trnsc$mp_uni=gsub("-","",trnsc$mp_uni,fixed=T)

# make all vowels in mispronunciation & target short (so we don't penalize length errors)
trnsc$mp_uni=gsub("([aeiouêâéáóî])\\1+", "\\1", trnsc$mp_uni)
trnsc$target_uni=gsub("([aeiouêâéáóî])\\1+", "\\1", trnsc$target_uni)

# if correct=0 but orthotarget=mispronunciation (probably due to length errors) then change correct to 1 
#sum(trnsc$correct==0 & trnsc$target_ortho == trnsc$mispronunciation & !is.na(trnsc$mispronunciation)) #this only happens twice
trnsc[trnsc$correct==0 & trnsc$target_uni == trnsc$mp_uni & !is.na(trnsc$mp_uni),]
trnsc$correct[trnsc$correct==0 & trnsc$target_uni == trnsc$mp_uni & !is.na(trnsc$mp_uni)]<-1
# M2A: I agree! My question is: Why then are they marked as incorrect in the first place?
# Does this deserve more detective work?
#a2m I looked into in there were disfluencies
# M2A: awesome, thank you! happy with this now :)

# create phonological correspondance matrix
# NOTE!! NON INTUITIVE MAPPING!!!! DO NOT READ THE OUTPUT BELIEVING IT IS PSEUDO IPA!!!
# NOTE2!!! it's better if the chars are not accented & since we don't distinguish long & short, 
# we'll use capitals for nasal & small caps for oral, with no distinction between short and long (see above)
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
    #consonants from rossel ortho replacements, missing previously
    "mbyw","x",
    "mbwy","x",
    "pwy","x",
    "pyw","x",
    "tpy","x",
    "dpy","x",
    "kpy","x",
    "myw","x",
    "mwy","x",
    "ngw","x",
    "nmy","x",
    "ngm","x",
    "mby","x",
    "mbw","x",
    "nty","x",
    "ndy","x",
    "nkw","x",
    "mty","x",
    "mdy","x",
    "mgw","x",
    "dny","x",
    "dmy","x",
    "knw","x",
    "py","x",
    "pw","x",
    "ty","x",
   # "ch"
    "dy","x",
    "ky","x",
    "kw","x",
    #"tp"
    #"dp"
    #"kp"
    "my","x",
    "mw","x",
    "ny","x",
    #"ng"
    "nm","x",
    #"mb"
    #"nt"
    "nj","x",
    #"nd"
    #"nk"
    #"mt"
    #"md"
    #"mg"
    #"dn"
    #"dm"
   # "kn"
    #"km"
    #"vy"
    "ly","x",
    #"lv"
    #"gh"
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
    "ch","t" #this one doesn't exist in the local orthography but it appears -- middy said this is t before e and i
    
  ),#last item above should not have a comma
  ncol = 2,byrow = T)


for(i in 1:dim(correspondances)[1]) { #transform into unicharacter
  trnsc$mp_uni=gsub(correspondances[i,1],correspondances[i,2],trnsc$mp_uni,fixed=T)
  trnsc$target_uni=gsub(correspondances[i,1],correspondances[i,2],trnsc$target_uni,fixed=T)
}

head(trnsc[,c("mispronunciation","mp_uni","target_uni")])


#calculate Levinshtein's distances 
trnsc$ins=trnsc$del=trnsc$sub=trnsc$nchar=trnsc$trafos=trnsc$tarlen=NA
substitution_bank=deletion_bank=match_bank=substitution_bank_try1=deletion_bank_try1=match_bank_try1=NULL

for(i in grep("0",trnsc$correct)){ #this goes through all items with errors (" grep("0",trnsc$correct)")
  #lev
  lev=adist(trnsc[i,c("mp_uni")],trnsc[i,c("target_uni")],count=T)
  trnsc$ins[i]<-attributes(lev)$counts[,,"ins"]
  trnsc$del[i]<-attributes(lev)$counts[,,"del"]
  trnsc$sub[i]<-attributes(lev)$counts[,,"sub"]
  trnsc$trafos[i]=attributes(lev)$trafos #M, I, D and S indicating a match, insertion, deletion and substitution
  trnsc$nchar[i]=nchar(attributes(lev)$trafos)
  trnsc$tarlen[i]=nchar(trnsc$target_uni[i])
  
  #extract substitutions & add them to the bank
  sublist=which(strsplit(attributes(lev)$trafos, "")[[1]]=="S")
  substitution_bank=rbind(substitution_bank,cbind(strsplit(trnsc[i,c("target_uni")],"")[[1]][sublist],strsplit(trnsc[i,c("mp_uni")],"")[[1]][sublist]))
  
  #same for deletions  
  dellist=which(strsplit(attributes(lev)$trafos, "")[[1]]=="D")
  deletion_bank=rbind(deletion_bank,cbind(strsplit(trnsc[i,c("target_uni")],"")[[1]][dellist]))
  
  #end with matches
  matlist=which(strsplit(attributes(lev)$trafos, "")[[1]]=="M")
  match_bank=rbind(match_bank,cbind(strsplit(trnsc[i,c("target_uni")],"")[[1]][matlist]))
  
  #if item is first attempt, add to try1 version of these tables
  if(trnsc[i,"attempt"]=="first") {
    substitution_bank_try1=rbind(substitution_bank_try1,cbind(strsplit(trnsc[i,c("target_uni")],"")[[1]][sublist],strsplit(trnsc[i,c("mp_uni")],"")[[1]][sublist]))
    deletion_bank_try1=rbind(deletion_bank_try1,cbind(strsplit(trnsc[i,c("target_uni")],"")[[1]][dellist]))
    match_bank_try1=rbind(match_bank_try1,cbind(strsplit(trnsc[i,c("target_uni")],"")[[1]][matlist]))
    
    }
}

#create correct bank
# M2A: what's this for if we don't expect any S/D/Is?
#a2m for ease of reading the code, I had left the section that does insertions, deletions, substitutions -- but we know there are only matches here ;)
#but seems that zas confusing
# M2A: ah alright!
for(i in grep("1",trnsc$correct)){ #this goes through all items without errors (" grep("1",trnsc$correct)")
  #lev
  lev=adist(trnsc[i,c("target_uni")],trnsc[i,c("target_uni")],count=T)
  
  trnsc$trafos[i]=attributes(lev)$trafos #M, I, D and S indicating a match, insertion, deletion and substitution
  trnsc$nchar[i]=nchar(attributes(lev)$trafos)
  trnsc$tarlen[i]=nchar(trnsc$target_uni[i])
  
  
  #end with matches
  matlist=which(strsplit(attributes(lev)$trafos, "")[[1]]=="M")
  match_bank=rbind(match_bank,cbind(strsplit(trnsc[i,c("target_uni")],"")[[1]][matlist]))
  
  #if item is first attempt, add to try1
  if(trnsc[i,"attempt"]=="first") {
    match_bank_try1=rbind(match_bank_try1,cbind(strsplit(trnsc[i,c("target_uni")],"")[[1]][matlist]))
    
  }
}

trnsc$ld=rowSums(trnsc[,c("ins","del","sub")])
trnsc$nld=trnsc$ld/trnsc$nchar
trnsc$nld[is.na(trnsc$nld)]<-0

#proportion correct: remove Substitutions Deletions and Insertions & count only proportion of matches
trnsc$phon_score=nchar(gsub("[SDI]","",trnsc$trafos))/trnsc$nchar
trnsc$phon_score[is.na(trnsc$phon_score)]<-1

write.table(trnsc,"final_data.txt",row.names=F,sep="\t")

#finish by writing out the bqnks
#back-transform into orthography
for(i in dim(correspondances)[1]:1) { #
   substitution_bank=gsub(correspondances[i,2],correspondances[i,1],substitution_bank,fixed=T)
   deletion_bank=gsub(correspondances[i,2],correspondances[i,1],deletion_bank,fixed=T)
   match_bank=gsub(correspondances[i,2],correspondances[i,1],match_bank,fixed=T)
   match_bank_try1=gsub(correspondances[i,2],correspondances[i,1],match_bank_try1,fixed=T)
   substitution_bank_try1=gsub(correspondances[i,2],correspondances[i,1],substitution_bank_try1,fixed=T)
   deletion_bank_try1=gsub(correspondances[i,2],correspondances[i,1],deletion_bank_try1,fixed=T)
}

substitution_bank[order(substitution_bank[,1]),]->substitution_bank
write.table(substitution_bank,"substitution_bank.txt",row.names=F,sep="\t")

substitution_bank_try1[order(substitution_bank_try1[,1]),]->substitution_bank_try1  
write.table(substitution_bank_try1,"substitution_bank_try1.txt",row.names=F,sep="\t")

sort(deletion_bank)->deletion_bank
write.table(deletion_bank,"deletion_bank.txt",row.names=F,sep="\t")
sort(deletion_bank_try1)->deletion_bank_try1
write.table(deletion_bank_try1,"deletion_bank_try1.txt",row.names=F,sep="\t")


sort(match_bank)->match_bank
write.table(match_bank,"match_bank.txt",row.names=F,sep="\t")
sort(match_bank_try1)->match_bank_try1
write.table(match_bank_try1,"match_bank_try1.txt",row.names=F,sep="\t")
