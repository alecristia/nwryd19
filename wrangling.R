
read.delim("NWR-transcription.txt", encoding="UTF-8")->trnsc
#DATA REMOVAL *** ATTENTION *** 
#REMOVE ALL TASK 4 (whatever that is)
# M2A: This appears to be a second pass over productions that were deemed to
# be words; the elicitation here is just a translation of what word the
# production was heard to be
trnsc[trnsc$item!="task4",]->trnsc

dim(trnsc) #1488 judgments

trnsc$tokid=paste(trnsc$item,trnsc$token)

read.table("final_order.txt",header=T)->crp
crp$tokid=paste(crp$target,crp$nb)
npairs=dim(crp)[1]

trnsc=merge(trnsc,crp,by.x="tokid",by.y="tokid")
dim(trnsc) #haven't lost any judgments

#order so we have all the items by a given child in the right chrono order
trnsc[order(trnsc$file, trnsc$int),]->trnsc

#add repetition number
trnsc$rep=gsub(".*_","",gsub(".wav","",trnsc$outfile))

# AC's prev code
# trnsc$previous<-c(NA,as.character(trnsc$target[1:(dim(trnsc)[1]-1)]))
# trnsc$attempt<-ifelse(trnsc$target!=trnsc$previous,"first","subsequent")
# trnsc$attempt[1]<-"first"
# table(trnsc$attempt) #1045 first attempts

# MC's new code
# M2A: by my count there are 1029 first attempts, but maybe I'm misunderstanding "token"
# M2A: okay, my solution finds some conflicting cases with your outfile attempt namings,
#      now going off of those for attempt first/subsequent, I still find 1029
# A2m please look at lines 21-22, those are the lines that determine which items are first versus subsequent. 
# int is the interval in praat, so it is a certain measure of time; I don't remember
# what token is, but there is a file called notes_for_supmat.txt which contains the explanation
# for all columns, which may say for this one and others
# M2A: Okay, great! Well that still returns 1029 first cases, but now the tidyverse pipe below
# should guarantee we're getting the cases we want

first.prods <- trnsc %>%
  group_by(id, item) %>%
  summarize(min.int = min(int)) %>%
  mutate(attempt = "first") %>%
  rename("int" = min.int)
trnsc <- trnsc %>%
  left_join(first.prods, by = c("id", "item", "int")) %>%
  replace_na(list(attempt = "subsequent"))
# table(trnsc$attempt)


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
# M2A: Note that some of these stim characters are now updated in stimuli2.txt
# they do not match what is shown here (!!)
# a2m: I'm confused, what do you mean? 
# my pipeline was: stimuli.txt I couldn't read here, so I put these instructions in the rmd:
# <!-- REMEMBER!!!! -->
#   <!-- phonetic characters: -->
#   <!-- - save excel as utf with save as -->
#   <!-- - do $ iconv -f utf-16le -t utf-8 stimuli.txt > stimconv.txt -->
#   <!-- - do $ iconv -f utf-16le -t utf-8 segments.txt > segconv.txt -->
#   
# also, notice that stims and segments are merged using ortho, so as long as ortho is matched
# across the two, the merge should carry over whatever changes you make
# M2A: Some of the segments in stimuli.txt were wrong. I made corrections in stimuli2.txt,
# but now that is mismatched to stimconv and segconv.
read.delim("stimconv.txt", encoding="UTF-8")->stims
read.delim("segconv.txt", encoding="UTF-8")->segments
#add corpus freq to segments
read.delim("segment-counts.txt", encoding="UTF-8",sep="\t",header=T)->phone_counts
merge(segments,phone_counts,by.x="ortho",by.y="ortho",all.x=T)->segments
#add corpus freq TYPES to segments
read.delim("segment-counts-types.txt", encoding="UTF-8",sep="\t",header=T)->phone_counts_types
merge(segments,phone_counts_types,by.x="ortho",by.y="ortho",all.x=T)->segments
write.table(segments,"segments_with_cor_freq.txt",sep="\t",row.names = F)

#add average crosslinguistic frequency
stim_seg_freq=matrix(NA,nrow=dim(stims)[1],ncol=8)
for(i in 1:dim(stim_seg_freq)[1]) for(j in 1:dim(stim_seg_freq)[2]) {
  thisfreq=segments$pc[as.character(segments$phono)==as.character(stims[i,paste0("phono",j)])]
  stim_seg_freq[i,j]<-ifelse(sum(!is.na(thisfreq))==1,thisfreq,NA)
}
# M2A: I'm suspicious of some of the NAs in here (like for 'k', 'w', 'd')--what is segment pc?
# a2m please note I didn't look up all sounds, only those used in our stimuli
# w is in our stimuli for sure, so that may not explain away all the NAs
# pc is percentage of languages in which the sound appears in phoible
# freq is the number of languages in which the sound appears in phoible
# M2A: but the three examples I listed are all in our stimuli (?)
# Thanks for the defs of pc and freq 
stims$avg_fr=apply(stim_seg_freq,1,mean,na.rm=T)

#add average corpus frequency - LOGGED
stim_seg_freq=matrix(NA,nrow=dim(stims)[1],ncol=8)
for(i in 1:dim(stim_seg_freq)[1]) for(j in 1:dim(stim_seg_freq)[2]) {
  thisfreq=segments$freq_corpus[as.character(segments$phono)==as.character(stims[i,paste0("phono",j)])] 
  stim_seg_freq[i,j]<-ifelse(sum(!is.na(thisfreq))==1,thisfreq,NA)
}
stims$avg_fr_cor=apply(log(stim_seg_freq),1,mean,na.rm=T)

#add average corpus frequency based on TYPES - LOGGED
stim_seg_freq=matrix(NA,nrow=dim(stims)[1],ncol=8)
for(i in 1:dim(stim_seg_freq)[1]) for(j in 1:dim(stim_seg_freq)[2]) {
  thisfreq=segments$freq_corpus_types[
    as.character(segments$phono)==as.character(stims[i,paste0("phono",j)])] 
  stim_seg_freq[i,j]<-ifelse(sum(!is.na(thisfreq))==1,thisfreq,NA)
}
stims$avg_fr_cor_ty=apply(log(stim_seg_freq),1,mean,na.rm=T)

merge(trnsc,stims,by="target",all.x=T)->trnsc
dim(trnsc)

#REMOVING ALL THE "YI"
trnsc[trnsc$target!="yi",]->trnsc

#remove practice items
trnsc=trnsc[trnsc$type !="practice",]
dim(trnsc)

## PHONOLOGIZE AND SCORE

#initialize the unichar phono-like representation
trnsc$mp_uni=trnsc$mp #start from ortho
trnsc$target_uni=trnsc$target_ortho #start from ortho


#log non-fluent speech, then get rid of that marker
trnsc$nonfluent<-NA
trnsc$nonfluent[grep("-",trnsc$mp_uni,fixed=T)]<-1
# M2A: there is no mp_uni column in trnsc as far as I can see, so this line
# doesn't do anything
# a2m, my apologies, this is a recent bug
#but indeed, I didn't look at the prevalence of disfluencies
trnsc$mispronunciation=gsub("-","",trnsc$mispronunciation)

# make all vowels in mispronunciation & target short (so we don't penalize length errors)
trnsc$mp=gsub("([aeiouêâéáóî])\\1+", "\\1", trnsc$mispronunciation)
trnsc$target_ortho=gsub("([aeiouêâéáóî])\\1+", "\\1", trnsc$target_ortho)

# if correct=0 but orthotarget=mispronunciation (probably due to length errors) then change correct to 1 
#sum(trnsc$correct==0 & trnsc$target_ortho == trnsc$mp & !is.na(trnsc$mp)) #this only happens twice
trnsc[trnsc$correct==0 & trnsc$target_ortho == trnsc$mp & !is.na(trnsc$mp),]
trnsc$correct[trnsc$correct==0 & trnsc$target_ortho == trnsc$mp & !is.na(trnsc$mp)]<-1
# M2A: This appears to be only two cases and it doesn't seem to be a length error diff
# i.e., mp == mispronunciation on both of these -- should we be cautious about
# counting them as correct?
#a2m I'm not sure I follow:
# target   id       tokid row annotator      date token     item correct mispronunciation
# 647      lvi c108       lvi 6 635        DY 16-Sep-19     6      lvi       0              lvi
# 835 nomiwake c110 nomiwake 21 839        DY 17-Sep-19    21 nomiwake       0         nomiwake
# meaning comment               file int      on     off cor       mp                   outfile
# 647       0         190829_0064S12_ch1  24 138.154 139.049  NA      lvi      pairs/lvi_c108_1.wav
# 835       0         190829_0066S12_ch1  68 256.738 259.013  NA nomiwake pairs/nomiwake_c110_1.wav
# rand nb rep attempt rown included Age Sex birthOrder familyID notes_familyID monolingual
# 647  0,21105252  6   1   first   40      yes   7   B          2       34   old_familyID         yes
# 835 0,671765283 21   1   first   42      yes   5   B          1       33   old_familyID         yes
# matEd Cond     Date        Time           File fastcode fullcode                    Notes
# 647    10  17a 20190829       15:42 190829_0064S12    names                                  
# 835     8  18a 20190829 ended 16:03 190829_0066S12    names          weird weak articulations
# notes_demo                                                                    Where   Village
# 647            on veranda (semi-public, small audience, included other children tested) Village#3
# 835            on veranda (semi-public, small audience, included other children tested) Village#3
# age.rounded type target_ortho    phono phono1 phono2 phono3 phono4 phono5 phono6 phono7 phono8
# 647         7.5    1          lvi     lβʲi    lβʲ      i   <NA>   <NA>   <NA>   <NA>   <NA>   <NA>
#   835         5.2    4     nomiwake ṇɔmiwækɛ      ṇ      ɔ      m      i      w      æ      k      ɛ
# avg_fr avg_fr_cor avg_fr_cor_ty mp_uni target_uni nonfluent
# 647   46.0  -2.646551     -2.682874     NA        lvi        NA
# 835   47.5  -3.076920     -3.030380     NA   nomiwake        NA
# I see: correct=0
# the first target is lvi and it's pronounced as lvi -- so it is indeed not incorrect
# the second target is nomiwake pronounced as nomiwake -- idem
# M2A: I agree! My question is: Why then are they marked as incorrect in the first place?
# Does this deserve more detective work?

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
  trnsc$mp_uni=gsub(correspondances[i,1],correspondances[i,2],trnsc$mp_uni)
  trnsc$target_uni=gsub(correspondances[i,1],correspondances[i,2],trnsc$target_uni)
}


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

trnsc$ld=rowSums(trnsc[,c("ins","del","sub")])
trnsc$nld=trnsc$ld/trnsc$nchar
trnsc$nld[is.na(trnsc$nld)]<-0

#proportion correct: remove Substitutions Deletions and Insertions & count only proportion of matches
# M2A: Isn't 'pc' used as a variable name relating to phone frequency above?
# they don't actually overlap here but I'm wary of potential future confusion
#a2m yikes! fixed 
trnsc$phon_score=nchar(gsub("[SDI]","",trnsc$trafos))/trnsc$nchar
trnsc$phon_score[is.na(trnsc$phon_score)]<-1

write.table(trnsc,"final_data.txt",row.names=F,sep="\t")


#back-transform into orthography
for(i in dim(correspondances)[1]:1) { #
   substitution_bank=gsub(correspondances[i,2],correspondances[i,1],substitution_bank,fixed=T)
   deletion_bank=gsub(correspondances[i,2],correspondances[i,1],deletion_bank,fixed=T)
   substitution_bank_try1=gsub(correspondances[i,2],correspondances[i,1],substitution_bank_try1,fixed=T)
   deletion_bank_try1=gsub(correspondances[i,2],correspondances[i,1],deletion_bank_try1,fixed=T)
}

substitution_bank[order(substitution_bank[,1]),]->substitution_bank
write.table(substitution_bank,"substitution_bank.txt",row.names=F,sep="\t")

substitution_bank_try1[order(substitution_bank_try1[,1]),]->substitution_bank_try1  #this gives an error CHECK **AC**
# M2A: this ^^ doesn't seem to give an error anymore; maybe delete old comment?
write.table(substitution_bank_try1,"substitution_bank_try1.txt",row.names=F,sep="\t")

sort(deletion_bank)->deletion_bank
write.table(deletion_bank,"deletion_bank.txt",row.names=F,sep="\t")
sort(deletion_bank_try1)->deletion_bank_try1
write.table(deletion_bank_try1,"deletion_bank_try1.txt",row.names=F,sep="\t")

sort(match_bank)->match_bank
write.table(match_bank,"match_bank.txt",row.names=F,sep="\t")
sort(match_bank_try1)->match_bank_try1
write.table(match_bank_try1,"match_bank_try1.txt",row.names=F,sep="\t")


#create correct bank
# M2A: what's this for if we don't expect any S/D/Is?
for(i in grep("1",trnsc$correct)){ #this goes through all items without errors (" grep("1",trnsc$correct)")
  #lev
  lev=adist(trnsc[i,c("target_uni")],trnsc[i,c("target_uni")],count=T)
  
  #for ease of reading the code, we leave the section that does insertions, deletions, substitutions -- but we know there are only matches here ;)
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
  
  #if item is first attempt, add to try1
  if(trnsc[i,"attempt"]=="first") {
    substitution_bank_try1=rbind(substitution_bank_try1,cbind(strsplit(trnsc[i,c("target_uni")],"")[[1]][sublist],strsplit(trnsc[i,c("mp_uni")],"")[[1]][sublist]))
    deletion_bank_try1=rbind(deletion_bank_try1,cbind(strsplit(trnsc[i,c("target_uni")],"")[[1]][dellist]))
    match_bank_try1=rbind(match_bank_try1,cbind(strsplit(trnsc[i,c("target_uni")],"")[[1]][matlist]))
    
  }
}
