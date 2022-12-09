

#######################################
###      QC for relative pairs        #
#######################################

## (1) remove 6 rows with empty ("") Relative_ID ----------

## (2) multiple fathers/mothers (908 removed individual-parent pairs are in "dvv_multiple_parents.20220422.tsv") ----------
# 63 index person have more than 1 father, set 126 father id to NA;
# 66 index person have more than 1 mother, set 132 mother id to NA;
# 37 siblings have more than 1 father, set 74 father id to NA; 
# 41 siblings have more than 1 mother, set 82 mother id to NA;
# 30 children have more than 1 father, set 60 fathers to NA;
# 217 children have more than 1 mother, set 434 mothers to NA;
# 30 children have more than 1 father, set 60 fathers to NA;
# 217 children have more than 1 mother, set 434 mothers to NA;

## (3) mismatch of sex for parents (222 removed individual-parent pairs are in "dvv_mismatch_sex.20220422.tsv") ----------
# 207 fathers with sex as female, set 207 father id to NA;
# 15 mothers with sex as male, set 15 mother id to NA;

## (4) mismatch of age at childbearing (19 removed individual-parent pairs are in "dvv_mismatch_birthage.20220422.tsv") ----------
# father with age at childbearing > 10, set 4 father id to NA;
# mother with age at childbearing > 10, set 5 mother id to NA;
# children born 1 year after the death of father, set 4 father id to NA;
# children born 1 month after the death of mother, set 6 mother id to NA;



#######################################
###           output files            #
#######################################

## file 1: pedigree (dvv_pedigree_withfamid.20220501.tsv) ----------
# columns: famid, ID, FATHER_ID, MOTHER_ID, Sex, Birth_Date

## file 2: relative pairs for index person (Index_RelativePair_basic.txt) ----------
# columns: id_1, id_2, type, kinship (not considering inbreeding)
# 4,018,140 index-father pairs; 0.5
# 4,202,659 index-mother pairs; 0.5
# 7,480,444 index-child pairs; 0.5
# 7,779,033 index-fullsib pairs; 0.5
# 808,667 index-halfsibonlyfather pairs; 0.25
# 1,065,498 index-halfsibonlymother pairs; 0.25

## file 3: more relative pairs (e.g. individual-grandparent, individual-cousin) will be added soon ----------
# 

## other tasks -------
# check the sub-structures of the largest pedigree
# check individuals which are not in the largest pedigree (migration? or missing information of relatives such as grandparents or cousins)
# plot the pedigree
# using pydigree to estimate kinship coefficient and h2 for using mixed model equation (can start with psychiatric disorders)




################################################################################################
################################################################################################
################################################################################################

######### Build pedigree for FinRegistry ############## 
setwd("/home/aliu/pedigree/output")
# install.packages('dplyr',repo='file://data/cran/', lib="/home/aliu/library")
library(dplyr, lib="/home/aliu/library")
# install.packages('crayon',repo='file://data/cran/', lib="/home/aliu/library")
library(crayon, lib="/home/aliu/library")
# install.packages('cli',repo='file://data/cran/', lib="/home/aliu/library")
library(cli, lib="/home/aliu/library")
# install.packages('data.table',repo='file://data/cran/', lib="/home/aliu/library")
library(data.table, lib="/home/aliu/library")
# install.packages('VennDiagram', repo='file://data/cran/', lib="/home/aliu/library")
# library(VennDiagram)
# library(feather)
# library(tibble)
# library(tidyr)
# library(lubridate)
# library(stringr)

'%!in%' <- function(x,y)!('%in%'(x,y))




########################################################################
###           Read in DVV relative file and minimum phenotype          #
########################################################################

dvv_relative <- fread("/data/processed_data/dvv/Tulokset_1900-2010_tutkhenk_ja_sukulaiset.txt.finreg_IDsp") %>% as_tibble
dim(dvv_relative)   # 38,475,897       13


miniphe <- fread("/data/processed_data/minimal_phenotype/minimal_phenotype_2022-03-28.csv") %>% as_tibble 
miniphe <- miniphe %>% select(FINREGISTRYID, date_of_birth, death_date, sex) %>% filter(!is.na(sex))
dim(miniphe)   # 7,070,388      4

miniphe_male <- miniphe %>% filter(sex==0)
dim(miniphe_male)  # 3,579,094       4

miniphe_female <- miniphe %>% filter(sex==1)
dim(miniphe_female)  # 3,491,294       4





#######################################################################################
###      Extract info for each type of relatives and check the data structure         #
#######################################################################################

## total N of index person -----------
length(unique(dvv_relative$FINREGISTRYID))   # 5,339,800


# N of index women and index men -----------
dvv_relative %>% filter(Relationship==0 & Sex==1) %>% nrow()   # 2,619,615
dvv_relative %>% filter(Relationship==0 & Sex==2) %>% nrow()   # 2,720,185


# N of females and males (index + relatives) -----------
dvv_relative %>% filter(Sex==1) %>% distinct(Relative_ID) %>% nrow()   # 3,579,094
dvv_relative %>% filter(Sex==2) %>% distinct(Relative_ID) %>% nrow()   # 3,491,294
dvv_relative %>% filter(is.na(Sex)) %>% distinct(Relative_ID) %>% nrow()  # 1


## remove 6 rows Relative_ID=="", all 6 rows with Relationship==2 -----------
dvv_relative <- dvv_relative %>% filter(Relative_ID!="")   # 38,475,891 


## total N of individuals (index + relatives) -----------
id_all <- dvv_relative %>% filter(!is.na(Sex)) %>% distinct(Relative_ID)    # 7,070,388


## N of relatives by types -----------
dvv_relative %>% distinct(Relative_ID, Relationship) %>% group_by(Relationship) %>% count()
#  Relationship       n
#1 0            5,339,800
#2 2            4,272,923
#3 3a           1,910,074
#4 3i           1,808,891
#5 4a           4,056,545
#6 4i           3,891,406


## N of relative pairs by types -----------
dvv_relative %>% group_by(Relationship) %>% count()
#  Relationship       n
#1 0            5,339,800
#2 2            7,480,800
#3 3a           4,202,809
#4 3i           4,018,458
#5 4a           8,845,477
#6 4i           8,588,547


## check whether "How_kinship_has_formed" is empty for indexperson/child/mother/father but available for sibling
dvv_relative %>% filter(Relationship %!in% c("4a","4i")) %>% filter(How_kinship_has_formed!="") %>% nrow()  # 0
dvv_relative %>% filter(Relationship %in% c("4a","4i")) %>% filter(How_kinship_has_formed!="") %>% nrow()   # 17,434,024
dvv_relative %>% filter(Relationship %in% c("4a","4i")) %>% filter(How_kinship_has_formed=="") %>% nrow()   # 0


## extract information by relative types -----------
dvv_index <- dvv_relative %>% filter(Relationship==0)
dim(dvv_index)   # 5,339,800      13
# save(dvv_index, file="dvv_index.Rdata")

dvv_child <- dvv_relative %>% filter(Relationship==2)
dim(dvv_child)   # 7,480,800      13
# save(dvv_child, file="dvv_child.Rdata")

dvv_mother <- dvv_relative %>% filter(Relationship=="3a")
dim(dvv_mother)  # 4,202,809      13
# save(dvv_mother, file="dvv_mother.Rdata")

dvv_father <- dvv_relative %>% filter(Relationship=="3i")
dim(dvv_father)  # 4,018,458      13
# save(dvv_father, file="dvv_father.Rdata")

dvv_maternal_sib <- dvv_relative %>% filter(Relationship=="4a")
dim(dvv_maternal_sib)   # 8,845,477      13
# save(dvv_maternal_sib, file="dvv_maternal_sib.Rdata")

dvv_paternal_sib <- dvv_relative %>% filter(Relationship=="4i")
dim(dvv_paternal_sib)  # 8,588,547      13
# save(dvv_paternal_sib, file="dvv_paternal_sib.Rdata")


## check whether paternal/maternal sibs can be full sibs (before QC)
dvv_maternal_sib_pair <- dvv_maternal_sib %>% distinct(FINREGISTRYID, Relative_ID)   # 8,845,435 maternal sib pairs
dvv_paternal_sib_pair <- dvv_paternal_sib %>% distinct(FINREGISTRYID, Relative_ID)   # 8,588,521 paternal sib pairs
rbind(dvv_maternal_sib_pair, dvv_paternal_sib_pair) %>% distinct() %>% nrow()   # 9,654,338 unique sib pairs
inner_join(dvv_maternal_sib_pair, dvv_paternal_sib_pair, by=c("FINREGISTRYID","Relative_ID")) %>% nrow()   # 7,779,618 full sib pairs
8845435 - 7779618   # 1,065,817 maternal half-sib pairs
8588521 - 7779618   # 808,903 paternal half-sib pairs





#######################################################################################
###                           parents for index person                                #
#######################################################################################

# extract individual-mother and individual-father list 
mother <- dvv_mother %>% rename(MOTHER_ID="Relative_ID", ID="FINREGISTRYID") %>% select(ID, MOTHER_ID) %>% distinct()   # 4,202,809
father <- dvv_father %>% rename(FATHER_ID="Relative_ID", ID="FINREGISTRYID") %>% select(ID, FATHER_ID) %>% distinct()   # 4,018,458


# set MOTHER_ID to NA if an individual has more than 1 mother (remove then it will be considered as NA later)
mother_dup <- mother %>% group_by(ID) %>% count() %>% filter(n>1)
dim(mother_dup)   # 66  2

multiple_mother <- mother %>% filter(ID %in% mother_dup$ID) %>% mutate(type="index-mother")
colnames(multiple_mother) <- c("ID","parent","type")
# write.table(multiple_mother, "dvv_multiple_parents.20220422.tsv", append=F, quote=F, sep="\t", row.names=F, col.names=T)

mother <- mother %>% filter(ID %!in% mother_dup$ID)
dim(mother)   # 4,202,677       2


# set FATHER_ID to NA if an individual has more than 1 father (remove then it will be considered as NA later)
father_dup <- father %>% group_by(ID) %>% count() %>% filter(n>1)
dim(father_dup)   # 63  2

multiple_father <- father %>% filter(ID %in% father_dup$ID) %>% mutate(type="index-father")
colnames(multiple_father) <- c("ID","parent","type")
# write.table(multiple_father, "dvv_multiple_parents.20220422.tsv", append=T, quote=F, sep="\t", row.names=F, col.names=F)

father <- father %>% filter(ID %!in% father_dup$ID)
dim(father)   # 4,018,332       2


# join father and mother
parent <- dvv_index %>% rename(Birth_Date="Relative_DOB", ID="FINREGISTRYID") %>% select(ID, Sex, Birth_Date)
dim(parent)   # 5,339,800       3
parent <- parent %>% left_join(father, by="ID") 
dim(parent)   # 5,339,800       4
parent <- parent %>% left_join(mother, by="ID") 
dim(parent)   # 5,339,800       5

parent %>% filter(is.na(FATHER_ID) & is.na(MOTHER_ID)) %>% nrow()   # 1,103,963
parent %>% filter(!is.na(FATHER_ID) & !is.na(MOTHER_ID)) %>% nrow() # 3,985,172
parent %>% filter(!is.na(FATHER_ID) & is.na(MOTHER_ID)) %>% nrow()  # 33,160
parent %>% filter(is.na(FATHER_ID) & !is.na(MOTHER_ID)) %>% nrow()  # 217,505
parent <- parent %>% select(ID, FATHER_ID, MOTHER_ID, Birth_Date, Sex)
# save(parent, file="dvv_index_parent.Rdata")





#######################################################################################
###                           parents for children                                    #
#######################################################################################

# extract individual-child list by sex of individual
index_female <- dvv_index %>% filter(Sex==2)
index_male <- dvv_index %>% filter(Sex==1)
child_mother <- dvv_child %>% filter(FINREGISTRYID %in% index_female$FINREGISTRYID) %>% 
                      rename(ID="Relative_ID", MOTHER_ID="FINREGISTRYID", Birth_Date="Relative_DOB") %>% select(ID, MOTHER_ID, Sex, Birth_Date) %>% distinct()
dim(child_mother)   # 4,068,849       2

child_father <- dvv_child %>% filter(FINREGISTRYID %in% index_male$FINREGISTRYID) %>% 
                      rename(ID="Relative_ID", FATHER_ID="FINREGISTRYID", Birth_Date="Relative_DOB") %>% select(ID, FATHER_ID, Sex, Birth_Date) %>% distinct()
dim(child_father)   # 3,411,951       2


# set MOTHER_ID to NA if an individual has more than 1 mother (remove then it will be considered as NA later)
child_mother_dup <- child_mother %>% group_by(ID) %>% count() %>% filter(n>1)
dim(child_mother_dup)   # 217   2

multiple_childmother <- child_mother %>% select(ID, MOTHER_ID) %>% filter(ID %in% child_mother_dup$ID) %>% mutate(type="child-mother")
colnames(multiple_childmother) <- c("ID","parent","type")
# write.table(multiple_childmother, "dvv_multiple_parents.20220422.tsv", append=T, quote=F, sep="\t", row.names=F, col.names=F)

child_mother <- child_mother %>% filter(ID %!in% child_mother_dup$ID)
dim(child_mother)   # 4,068,415        4  (3 individuals with ID as "")


# set FATHER_ID to NA if an individual has more than 1 mother (remove then it will be considered as NA later)
child_father_dup <- child_father %>% group_by(ID) %>% count() %>% filter(n>1)
dim(child_father_dup)   # 30  2

multiple_childfather <- child_father %>% select(ID, FATHER_ID) %>% filter(ID %in% child_father_dup$ID) %>% mutate(type="child-father")
colnames(multiple_childfather) <- c("ID","parent","type")
# write.table(multiple_childfather, "dvv_multiple_parents.20220422.tsv", append=T, quote=F, sep="\t", row.names=F, col.names=F)

child_father <- child_father %>% filter(ID %!in% child_father_dup$ID)
dim(child_father)   # 3,411,891       4 


# join father and mother
child_parent <- full_join(child_father, child_mother, by=c("ID","Birth_Date","Sex")) 
child_parent <- child_parent %>% select(ID, FATHER_ID, MOTHER_ID, Birth_Date, Sex)
dim(child_parent)   # 4,272,693       5
# save(child_parent, file="dvv_child_parent.Rdata")

child_parent %>% filter(is.na(FATHER_ID) & is.na(MOTHER_ID)) %>% nrow()   # 0
child_parent %>% filter(!is.na(FATHER_ID) & !is.na(MOTHER_ID)) %>% nrow() # 3,207,613
child_parent %>% filter(!is.na(FATHER_ID) & is.na(MOTHER_ID)) %>% nrow()  # 204,278
child_parent %>% filter(is.na(FATHER_ID) & !is.na(MOTHER_ID)) %>% nrow()  # 860,802
child_parent %>% filter(ID %in% parent$ID) %>% nrow()   # 3,534,501
child_parent %>% filter(ID %!in% parent$ID) %>% nrow()   # 738,192





#######################################################################################
###                           parents for siblings                                    #
#######################################################################################

maternal_sib <- dvv_maternal_sib %>% rename(MOTHER_ID="How_kinship_has_formed", ID="Relative_ID", Birth_Date="Relative_DOB") %>% select(ID, MOTHER_ID, Birth_Date, Sex) %>% distinct()
dim(maternal_sib)   # 4,056,582       3

paternal_sib <- dvv_paternal_sib %>% rename(FATHER_ID="How_kinship_has_formed", ID="Relative_ID", Birth_Date="Relative_DOB") %>% select(ID, FATHER_ID, Birth_Date, Sex) %>% distinct()
dim(paternal_sib)   # 3,891,447       3


# set MOTHER_ID to NA if an individual has more than 1 mother (remove then it will be considered as NA later)
maternal_sib_dup <- maternal_sib %>% group_by(ID) %>% count() %>% filter(n>1)
dim(maternal_sib_dup)   #  37  2

multiple_sibmother <- maternal_sib %>% select(ID, MOTHER_ID) %>% filter(ID %in% maternal_sib_dup$ID) %>% mutate(type="sib-mother")
colnames(multiple_sibmother) <- c("ID","parent","type")
# write.table(multiple_sibmother, "dvv_multiple_parents.20220422.tsv", append=T, quote=F, sep="\t", row.names=F, col.names=F)

maternal_sib <- maternal_sib %>% filter(ID %!in% maternal_sib_dup$ID)
dim(maternal_sib)       # 4,056,508        4  (3 individuals with ID as "")


# set FATHER_ID to NA if an individual has more than 1 mother (remove then it will be considered as NA later)
paternal_sib_dup <- paternal_sib %>% group_by(ID) %>% count() %>% filter(n>1)
dim(paternal_sib_dup)   #  41  2

multiple_sibfather <- paternal_sib %>% select(ID, FATHER_ID) %>% filter(ID %in% paternal_sib_dup$ID) %>% mutate(type="sib-father")
colnames(multiple_sibfather) <- c("ID","parent","type")
# write.table(multiple_sibfather, "dvv_multiple_parents.20220422.tsv", append=T, quote=F, sep="\t", row.names=F, col.names=F)

paternal_sib <- paternal_sib %>% filter(ID %!in% paternal_sib_dup$ID)
dim(paternal_sib)       # 3,891,365        4  (3 individuals with ID as "")


# join father and mother
sib_parent <- full_join(maternal_sib, paternal_sib, by=c("ID","Birth_Date","Sex")) 
sib_parent <- sib_parent %>% select(ID, FATHER_ID, MOTHER_ID, Birth_Date, Sex)
dim(sib_parent)   # 4,203,130       5
# save(sib_parent, file="dvv_sib_parent.Rdata")


sib_parent %>% filter(is.na(FATHER_ID) & is.na(MOTHER_ID)) %>% nrow()   # 0
sib_parent %>% filter(!is.na(FATHER_ID) & !is.na(MOTHER_ID)) %>% nrow() # 3,744,743
sib_parent %>% filter(!is.na(FATHER_ID) & is.na(MOTHER_ID)) %>% nrow()  # 146,622
sib_parent %>% filter(is.na(FATHER_ID) & !is.na(MOTHER_ID)) %>% nrow()  # 311,765
sib_parent %>% filter(ID %in% parent$ID) %>% nrow()   # 3,723,570
sib_parent %>% filter(ID %!in% parent$ID) %>% nrow()  # 479,560





#######################################################################################
###                        Combine all information                                    #
#######################################################################################

parent <- data.frame(get(load("dvv_index_parent.Rdata")))
dim(parent)   # 5,339,800       5
child_parent <- data.frame(get(load("dvv_child_parent.Rdata")))
dim(child_parent)   # 4,272,693       5
sib_parent <- data.frame(get(load("dvv_sib_parent.Rdata")))
dim(sib_parent)   # 4,203,130       5


## people with at least one parent -------------------
parent_all <- rbind(parent, child_parent %>% filter(ID %!in% parent$ID), sib_parent %>% filter(ID %!in% c(parent$ID,child_parent$ID)) ) %>% distinct()
dim(parent_all)   # 6,210,081       5

father_all <- parent_all %>% filter(!is.na(FATHER_ID)) %>% distinct(ID, FATHER_ID, Birth_Date, Sex)
dim(father_all)   # 4748286
mother_all <- parent_all %>% filter(!is.na(MOTHER_ID)) %>% distinct(ID, MOTHER_ID, Birth_Date, Sex)
dim(mother_all)   # 5014334

pedigree <- full_join(father_all, mother_all, by=c("ID","Birth_Date","Sex"))
pedigree <- pedigree %>% select(ID, FATHER_ID, MOTHER_ID, Sex, Birth_Date)
dim(pedigree)  # 5,106,118       5

dim(pedigree)  # 5,106,118       5
pedigree %>% distinct(ID) %>% nrow()   # 5,106,118


## Add people without any parent -------------------
# pedigree_add <- dvv_relative %>% filter(Relative_ID!="" & Relative_ID %!in% pedigree$ID) %>% 
#                    select(Relative_ID, Relative_DOB, Sex) %>% distinct() %>% 
#                    rename(ID="Relative_ID", Birth_Date="Relative_DOB") %>% mutate(FATHER_ID=NA, MOTHER_ID=NA) %>% 
#                    select(ID, FATHER_ID, MOTHER_ID, Sex, Birth_Date)
# nrow(pedigree_add)  # 1,964,270 (10 individuals will have missing ID if directly using this dataset)

pedigree_add <- miniphe %>% filter(FINREGISTRYID %!in% pedigree$ID) %>% 
                   rename(ID="FINREGISTRYID", Birth_Date="date_of_birth", Sex="sex") %>% mutate(FATHER_ID=NA, MOTHER_ID=NA) %>% 
                   select(ID, FATHER_ID, MOTHER_ID, Sex, Birth_Date)
nrow(pedigree_add)  # 1,964,270       1


## combine into one file and order by birth date (from old to young) -------------------
pedigree_full <- rbind(pedigree, pedigree_add) %>% select(ID, FATHER_ID, MOTHER_ID, Sex, Birth_Date)
dim(pedigree_full)   # 7,070,388       5





##################################################
###            QC for the pedigree               #
##################################################

## set FATHER_ID/MOTHER_ID to NA if the registered sex is different -------------------
mismatch_sex <- pedigree_full %>% filter(FATHER_ID %in% miniphe_female$FINREGISTRYID) %>% rename(parent="FATHER_ID") %>% select(ID, parent) %>% mutate(type="father", parent_sex="female") 
dim(mismatch_sex)   # 207
write.table(mismatch_sex, "dvv_mismatch_sex.20220422.tsv", append=F, quote=F, sep="\t", row.names=F, col.names=T)

mismatch_sex <- pedigree_full %>% filter(MOTHER_ID %in% miniphe_male$FINREGISTRYID) %>% rename(parent="MOTHER_ID") %>% select(ID, parent) %>% mutate(type="mother", parent_sex="male") 
dim(mismatch_sex)   # 15
write.table(mismatch_sex, "dvv_mismatch_sex.20220422.tsv", append=T, quote=F, sep="\t", row.names=F, col.names=F)

pedigree_full <- pedigree_full %>% mutate(FATHER_ID=ifelse(FATHER_ID %in% miniphe_female$FINREGISTRYID, NA,FATHER_ID ))
pedigree_full <- pedigree_full %>% mutate(MOTHER_ID=ifelse(MOTHER_ID %in% miniphe_male$FINREGISTRYID, NA,MOTHER_ID ))


## mismatch of age at childbearing  ----------
pedigree_full <- pedigree_full %>% left_join(miniphe_male, by=c("FATHER_ID"="FINREGISTRYID")) %>% select(-sex) %>% 
                    rename(FATHER_BirthDate="date_of_birth", FATHER_DeathDate="death_date")

pedigree_full <- pedigree_full %>% left_join(miniphe_female, by=c("MOTHER_ID"="FINREGISTRYID")) %>% select(-sex) %>% 
                    rename(MOTHER_BirthDate="date_of_birth", MOTHER_DeathDate="death_date")

pedigree_full %>% filter(!is.na(FATHER_ID)) %>% nrow()   # 4,748,079
pedigree_full %>% filter(!is.na(FATHER_ID) | !is.na(FATHER_BirthDate)) %>% nrow()   # 4,748,079
pedigree_full %>% filter(!is.na(FATHER_ID) | !is.na(FATHER_BirthDate) | !is.na(FATHER_DeathDate)) %>% nrow()   # 4,748,079

pedigree_full %>% filter(!is.na(MOTHER_ID)) %>% nrow()   # 5,014,319
pedigree_full %>% filter(!is.na(MOTHER_ID) | !is.na(MOTHER_BirthDate)) %>% nrow()   # 5,014,319
pedigree_full %>% filter(!is.na(MOTHER_ID) | !is.na(MOTHER_BirthDate) | !is.na(MOTHER_DeathDate)) %>% nrow()   # 5,014,319

pedigree_full <- pedigree_full %>% mutate(MOTHER_ChildBirthAge=as.numeric(as.Date(Birth_Date)-as.Date(MOTHER_BirthDate))/365,
                                          FATHER_ChildBirthAge=as.numeric(as.Date(Birth_Date)-as.Date(FATHER_BirthDate))/365,
                                          MOTHER_DeathAge=as.numeric(as.Date(MOTHER_DeathDate)-as.Date(MOTHER_BirthDate))/365,
                                          FATHER_DeathAge=as.numeric(as.Date(FATHER_DeathDate)-as.Date(FATHER_BirthDate))/365) 

pedigree_full <- pedigree_full %>% mutate(MOTHER_ChildBirthAge_max= MOTHER_DeathAge+30/365,
                                          FATHER_ChildBirthAge_max= FATHER_DeathAge+1)


# mother with age at childbearing > 10, set mother id to NA
mismatch_birthage <- pedigree_full %>% filter(MOTHER_ChildBirthAge<10) %>% 
                        mutate(type="mother age at birth <10") %>% 
                        rename(parent="MOTHER_ID", parent_Birth_Date="MOTHER_BirthDate", parent_Death_Date="MOTHER_DeathDate") %>% 
                        select(ID, parent, Birth_Date, parent_Birth_Date, parent_Death_Date, type)
nrow(mismatch_birthage)  # 5 
write.table(mismatch_birthage, "dvv_mismatch_birthage.20220422.tsv", append=F, quote=F, sep="\t", row.names=F, col.names=T)


# father with age at childbearing > 10, set father id to NA
mismatch_birthage <- pedigree_full %>% filter(FATHER_ChildBirthAge<10) %>% 
                        mutate(type="father age at birth <10") %>% 
                        rename(parent="FATHER_ID", parent_Birth_Date="FATHER_BirthDate", parent_Death_Date="FATHER_DeathDate") %>% 
                        select(ID, parent, Birth_Date, parent_Birth_Date, parent_Death_Date, type)
nrow(mismatch_birthage)  # 4
write.table(mismatch_birthage, "dvv_mismatch_birthage.20220422.tsv", append=T, quote=F, sep="\t", row.names=F, col.names=F)


# children born 1 month after the death of mother, set mother id to NA
mismatch_birthage <- pedigree_full %>% filter(MOTHER_ChildBirthAge > MOTHER_ChildBirthAge_max) %>% 
                        mutate(type="child born > 1 month after the death of mother") %>% 
                        rename(parent="MOTHER_ID", parent_Birth_Date="MOTHER_BirthDate", parent_Death_Date="MOTHER_DeathDate") %>% 
                        select(ID, parent, Birth_Date, parent_Birth_Date, parent_Death_Date, type)
nrow(mismatch_birthage)  # 6
write.table(mismatch_birthage, "dvv_mismatch_birthage.20220422.tsv", append=T, quote=F, sep="\t", row.names=F, col.names=F)


# children born 1 year after the death of father, set father id to NA
mismatch_birthage <- pedigree_full %>% filter(FATHER_ChildBirthAge > FATHER_ChildBirthAge_max) %>% 
                        mutate(type="child born > 1 year after the death of father") %>% 
                        rename(parent="FATHER_ID", parent_Birth_Date="FATHER_BirthDate", parent_Death_Date="FATHER_DeathDate") %>% 
                        select(ID, parent, Birth_Date, parent_Birth_Date, parent_Death_Date, type)
nrow(mismatch_birthage)  # 4
write.table(mismatch_birthage, "dvv_mismatch_birthage.20220422.tsv", append=T, quote=F, sep="\t", row.names=F, col.names=F)


pedigree_full %>% filter(!is.na(MOTHER_ID)) %>% nrow()  # 5,014,319
pedigree_full %>% filter(!is.na(FATHER_ID)) %>% nrow()  # 4,748,079

pedigree_full <- pedigree_full %>% mutate(MOTHER_ID=ifelse(MOTHER_ChildBirthAge<10, NA, MOTHER_ID),
                                          FATHER_ID=ifelse(FATHER_ChildBirthAge<10, NA, FATHER_ID),
                                          MOTHER_ID=ifelse(MOTHER_ChildBirthAge<=MOTHER_ChildBirthAge_max | is.na(MOTHER_ChildBirthAge) | is.na(MOTHER_ChildBirthAge_max), MOTHER_ID, NA),
                                          FATHER_ID=ifelse(FATHER_ChildBirthAge<=FATHER_ChildBirthAge_max | is.na(FATHER_ChildBirthAge) | is.na(FATHER_ChildBirthAge_max), FATHER_ID, NA))

pedigree_full %>% filter(!is.na(MOTHER_ID)) %>% nrow()  # 5,014,308
pedigree_full %>% filter(!is.na(FATHER_ID)) %>% nrow()  # 4,748,071


## order by birth date (from old to young) -------------------
pedigree_full <- pedigree_full %>% arrange(-desc(Birth_Date)) %>% select(ID, FATHER_ID, MOTHER_ID, Sex, Birth_Date)
dim(pedigree_full)   # 7,070,388       5

save(pedigree_full, file="dvv_pedigree_full_nofamid.Rdata")
write.table(pedigree_full, "dvv_pedigree_full_nofamid.20220422.tsv", append=F, quote=F, sep="\t", row.names=F, col.names=T)





#####################################################################
#       Prepare input for networkx to add family id                 #
#####################################################################

pedigree_full <- data.frame(get(load("dvv_pedigree_full_nofamid.Rdata")))
dim(pedigree_full)   # 7070388       5


## paternal link 
paternal <- pedigree_full %>% filter(!is.na(FATHER_ID)) %>% rename(ID_1="ID", ID_2="FATHER_ID") %>% select(ID_1, ID_2) 
nrow(paternal)   # 4,748,071


## maternal link 
maternal <- pedigree_full %>% filter(!is.na(MOTHER_ID)) %>% rename(ID_1="ID", ID_2="MOTHER_ID") %>% select(ID_1, ID_2) 
dim(maternal)    # 5,014,308       2


## combine 
relative_link <- rbind(paternal, maternal)
dim(relative_link)   # 9,762,379       2
length(unique(c(relative_link$ID_1, relative_link$ID_2)))   # 6,900,613
write.table(relative_link, "relative_link.20220501.tsv", append=F, quote=F, sep=" ", row.names=F, col.names=F)





#######################################################################################
###              Run networkx to add family id using jupyter notebook                 #
#######################################################################################

## https://github.com/yorogosu/genealogy/blob/main/components.py

# Jupyter & RStudio (SSH tunneling) for FinRegistry -------------------------
# Open the SSH tunnel on your machine (on your local terminal)
# Log in using your Atlas usename as above 
# Go to https://localhost:xxxx/jupyter/ or https://localhost:xxxx/rstudio/ on your browser
# Log into Jupyter or RStudio using the Jupyter/RStudio username 
# Troubleshooting: 
# “No available hosts” → make sure VPN is on
# Jupyter: “Server is starting up” → refresh the browser (it won’t be done automatically even though it says so)
# then type "conda activate shared_env" from the terminal

## /home/aliu/pedigree/script/pedigree_networkx.ipynb

## after login 
# !conda activate shared_env
# !cd /home/aliu/pedigree/script
import networkx as nx
import pandas as pd
import scipy
from matplotlib import pyplot as plt


## read the edge list
data_path = '/home/aliu/pedigree/output/relative_link.20220501.tsv'
G = nx.read_edgelist(data_path)
print(G)    # Graph with 6,900,613 nodes and 9,762,379 edges


## find the components of the graph
components = nx.connected_components(G)

## register id's in order of appearance in the components
indid = []

## register component size
sizes = []
for component in components:
    indid.extend(component)
    sizes.append(len(component))

## register famid's according to component size
famid = []

for i in range(len(sizes)):
    famid.extend([i+1]*sizes[i])

## total number of families
max(famid)   # 149,502

## family_id to id list
ids = pd.DataFrame({'famid':famid, 'indid':indid})
export_csv = ids.to_csv(r'/home/aliu/pedigree/output/famid_link.20220501.csv.gz', compression = 'gzip', sep=' ', index=None, header=True)




#####################################################################
#                       Add family id to pedigree                   #
#####################################################################

famid <- read.table("famid_link.20220501.csv.gz", header=T)
dim(famid)   # 6,900,613       2


## add family id for those with relatives (have famid generated by networkx)
pedigree_full <- pedigree_full %>% left_join(famid, by=c("ID"="indid")) %>% select(famid, ID, FATHER_ID, MOTHER_ID, Sex, Birth_Date)
n_fam <- max(pedigree_full$famid,na.rm=T)   # 149,530


## add family id for those without any relative
pedigree_nofamid <- pedigree_full %>% filter(is.na(famid)) %>% arrange(-desc(Birth_Date)) %>% 
                       mutate(famid=row_number()+n_fam) %>% 
                       select(famid, ID, FATHER_ID, MOTHER_ID, Sex, Birth_Date)
dim(pedigree_nofamid)


## combine
pedigree_withfamid <- rbind(pedigree_full %>% filter(!is.na(famid)), pedigree_nofamid) %>% arrange(-desc(famid), -desc(Birth_Date)) %>% select(famid, ID, FATHER_ID, MOTHER_ID, Sex, Birth_Date)
dim(pedigree_withfamid)  # 7,070,388       6
save(pedigree_withfamid, file="dvv_pedigree_withfamid.Rdata")
write.table(pedigree_withfamid, "dvv_pedigree_withfamid.20220501.tsv", append=F, quote=F, sep="\t", row.names=F, col.names=T)





#######################################################################################
###             From pedigree to relative pairs for index person                      #
#######################################################################################

pedigree_withfamid <- data.frame(get(load("dvv_pedigree_withfamid.Rdata")))
dim(pedigree_withfamid)   # 7,070,388       6

multiple_parents <- read.table("dvv_multiple_parents.20220422.tsv", header=T)
dim(multiple_parents)   # 908   3

mismatch_sex <- read.table("dvv_mismatch_sex.20220422.tsv", header=T)
dim(mismatch_sex)   # 222   4

mismatch_birthage <- read.table("dvv_mismatch_birthage.20220422.tsv", sep="\t", header=T)
dim(mismatch_birthage)  # 17

qc_lst <- unique(c(multiple_parents$ID, mismatch_sex$ID, mismatch_birthage$ID))
length(qc_lst)   # 395


## index (Self) ---------
index_lst <- fread("/data/processed_data/minimal_phenotype/minimal_phenotype_2022-03-28.csv") %>% as_tibble %>% filter(index_person==1) %>% select(FINREGISTRYID)
nrow(index_lst)    # 5,339,804 

pedigree_withfamid <- pedigree_withfamid %>% mutate(is_index=ifelse(ID %in% index_lst$FINREGISTRYID, 1, 0))
pedigree_withfamid %>% group_by(is_index) %>% count()
#   is_index       n
# 1        0 1730584
# 2        1 5339804


## index-parent (PO) ---------
father_lst <- pedigree_withfamid %>% filter(is_index==1 & !is.na(FATHER_ID)) %>% distinct(FATHER_ID)
nrow(father_lst)   # 1,808,755
mother_lst <- pedigree_withfamid %>% filter(is_index==1 & !is.na(MOTHER_ID)) %>% distinct(MOTHER_ID)
nrow(mother_lst)   # 1,910,014

pedigree_withfamid <- pedigree_withfamid %>% mutate(is_parent=ifelse(ID %in% c(father_lst$FATHER_ID, mother_lst$MOTHER_ID), 1, 0))
pedigree_withfamid %>% group_by(is_parent) %>% count()
#   is_parent       n
# 1         0 3351619
# 2         1 3718769


## index-grandparent (1G) ---------
grandfather_lst <- pedigree_withfamid %>% filter(is_parent==1 & !is.na(FATHER_ID)) %>% distinct(FATHER_ID)
nrow(grandfather_lst)   # 960,016
grandmother_lst <- pedigree_withfamid %>% filter(is_parent==1 & !is.na(MOTHER_ID)) %>% distinct(MOTHER_ID)
nrow(grandmother_lst)   # 1,029,297

pedigree_withfamid <- pedigree_withfamid %>% mutate(is_grandparent=ifelse(ID %in% c(grandfather_lst$FATHER_ID, grandmother_lst$MOTHER_ID), 1, 0))
pedigree_withfamid %>% group_by(is_grandparent) %>% count()
#   is_grandparent       n
# 1              0 5081075
# 2              1 1989313


## index-grandgrandparent (2G) ---------
grandgrandfather_lst <- pedigree_withfamid %>% filter(is_grandparent==1 & !is.na(FATHER_ID)) %>% distinct(FATHER_ID)
nrow(grandgrandfather_lst)   # 292,281
grandgrandmother_lst <- pedigree_withfamid %>% filter(is_grandparent==1 & !is.na(MOTHER_ID)) %>% distinct(MOTHER_ID)
nrow(grandgrandmother_lst)   # 332,346

pedigree_withfamid <- pedigree_withfamid %>% mutate(is_greatgrandparent=ifelse(ID %in% c(grandgrandfather_lst$FATHER_ID, grandgrandmother_lst$MOTHER_ID), 1, 0))
pedigree_withfamid %>% group_by(is_greatgrandparent) %>% count()
#   is_greatgrandparent       n
# 1                   0 6445761
# 2                   1  624627


## index-grandgrandgrandparent (3G) ---------
grandgrandgrandfather_lst <- pedigree_withfamid %>% filter(is_greatgrandparent==1 & !is.na(FATHER_ID)) %>% distinct(FATHER_ID)
nrow(grandgrandgrandfather_lst)   # 3,984
grandgrandgrandmother_lst <- pedigree_withfamid %>% filter(is_greatgrandparent==1 & !is.na(MOTHER_ID)) %>% distinct(MOTHER_ID)
nrow(grandgrandgrandmother_lst)   # 5,174

pedigree_withfamid <- pedigree_withfamid %>% mutate(is_greatgreatgrandparent=ifelse(ID %in% c(grandgrandgrandfather_lst$FATHER_ID, grandgrandgrandmother_lst$MOTHER_ID), 1, 0))
pedigree_withfamid %>% group_by(is_greatgreatgrandparent) %>% count()
#   is_greatgreatgrandparent       n
# 1                        0 7061230
# 2                        1    9158


## index-grandgrandgrandparent (4G) ---------
grandgrandgrandgrandfather_lst <- pedigree_withfamid %>% filter(is_greatgreatgrandparent==1 & !is.na(FATHER_ID)) %>% distinct(FATHER_ID)
nrow(grandgrandgrandgrandfather_lst)   # 8
grandgrandgrandgrandmother_lst <- pedigree_withfamid %>% filter(is_greatgreatgrandparent==1 & !is.na(MOTHER_ID)) %>% distinct(MOTHER_ID)
nrow(grandgrandgrandgrandmother_lst)   # 14

pedigree_withfamid <- pedigree_withfamid %>% mutate(is_greatgreatgreatgrandparent=ifelse(ID %in% c(grandgrandgrandgrandfather_lst$FATHER_ID, grandgrandgrandgrandmother_lst$MOTHER_ID), 1, 0))
pedigree_withfamid %>% group_by(is_greatgreatgreatgrandparent) %>% count()
#   is_grandgrandgrandgrandparent       n
# 1                             0 7070366
# 2                             1      22


## index_child ---------
male_child_lst <- pedigree_withfamid %>% filter(!is.na(FATHER_ID) & FATHER_ID %in% index_lst$FINREGISTRYID) %>% select(ID)
nrow(male_child_lst)   # 3,411,887
female_child_lst <- pedigree_withfamid %>% filter(!is.na(MOTHER_ID) & MOTHER_ID %in% index_lst$FINREGISTRYID) %>% select(ID)
nrow(female_child_lst)   # 4,068,598

pedigree_withfamid <- pedigree_withfamid %>% mutate(is_child=ifelse(ID %in% c(male_child_lst$ID, female_child_lst$ID), 1, 0))
pedigree_withfamid %>% group_by(is_child) %>% count()
#   is_child       n
# 1        0 2797453
# 2        1 4272935


## index-grandchild
male_grandchild_lst <- pedigree_withfamid %>% filter(!is.na(FATHER_ID) & FATHER_ID %in% c(male_child_lst$ID, female_child_lst$ID)) %>% select(ID)
nrow(male_grandchild_lst)   # 1,938,726
female_grandchild_lst <- pedigree_withfamid %>% filter(!is.na(MOTHER_ID) & MOTHER_ID %in% c(male_child_lst$ID, female_child_lst$ID)) %>% select(ID)
nrow(female_grandchild_lst)   # 2,112,773

pedigree_withfamid <- pedigree_withfamid %>% mutate(is_grandchild=ifelse(ID %in% c(male_grandchild_lst$ID, female_grandchild_lst$ID), 1, 0))
pedigree_withfamid %>% group_by(is_grandchild) %>% count()
#   is_grandchild       n
# 1             0 4648900
# 2             1 2421488


## siblings ---------
dvv_maternal_sib <- data.frame(get(load("dvv_maternal_sib.Rdata")))
dim(dvv_maternal_sib)   # 8,845,477      13
dvv_maternal_sib <- dvv_maternal_sib %>% filter(FINREGISTRYID %!in% qc_lst) %>% filter(Relative_ID %!in% qc_lst)
dim(dvv_maternal_sib)   # 8,844,531      13 (946 were removed)

dvv_paternal_sib <- data.frame(get(load("dvv_paternal_sib.Rdata")))
dim(dvv_paternal_sib)   #  8,588,547      13
dvv_paternal_sib <- dvv_paternal_sib %>% filter(FINREGISTRYID %!in% qc_lst) %>% filter(Relative_ID %!in% qc_lst)
dim(dvv_paternal_sib)   #  8,587,700      13 (847 were removed)

dvv_maternal_sib_pair <- dvv_maternal_sib %>% distinct(FINREGISTRYID, Relative_ID) %>% mutate(pair=paste0(FINREGISTRYID,"_", Relative_ID))  # 8,844,531 maternal sib pairs
dvv_paternal_sib_pair <- dvv_paternal_sib %>% distinct(FINREGISTRYID, Relative_ID) %>% mutate(pair=paste0(FINREGISTRYID,"_", Relative_ID))  # 8,587,700 paternal sib pairs


fullsib <-  dvv_maternal_sib_pair %>% filter(pair %in% dvv_paternal_sib_pair$pair)
dim(fullsib)   # 7,779,033       3
pedigree_withfamid <- pedigree_withfamid %>% mutate(is_fullsib=ifelse(ID %in% fullsib$Relative_ID, 1, 0))
pedigree_withfamid %>% group_by(is_fullsib) %>% count()
#   is_fullsib       n
# 1          0 3407287
# 2          1 3663101


halfsib_mat <- dvv_maternal_sib_pair %>% filter(pair %!in% dvv_paternal_sib_pair$pair)
nrow(halfsib_mat)   # 1,065,498
pedigree_withfamid <- pedigree_withfamid %>% mutate(is_halfsibmaternal=ifelse(ID %in% halfsib_mat$Relative_ID, 1, 0))
pedigree_withfamid %>% group_by(is_halfsibmaternal) %>% count()
#   is_halfsibmaternal       n
# 1                  0 6440065
# 2                  1  630323


halfsib_pat <- dvv_paternal_sib_pair %>% filter(pair %!in% dvv_maternal_sib_pair$pair)
nrow(halfsib_pat)   # 1,065,498
pedigree_withfamid <- pedigree_withfamid %>% mutate(is_halfsibpaternal=ifelse(ID %in% halfsib_pat$Relative_ID, 1, 0))
pedigree_withfamid %>% group_by(is_halfsibpaternal) %>% count()
#   is_halfsibpaternal       n
# 1                  0 6598212
# 2                  1  472176

save(pedigree_withfamid, file="pedigree_withfamid_subject.Rdata")


pedigree_withfamid %>% filter(is_index==0) %>% filter(is.na(FATHER_ID) | is.na(MOTHER_ID)) %>% group_by(is_parent) %>% count()
#  is_parent      n
# 1         0 163,307
# 2         1 895,965
895965/1059272   # 0.8458309


pedigree_withfamid %>% filter(is_index==0) %>% filter(is.na(FATHER_ID) | is.na(MOTHER_ID)) %>% group_by(is_halfsibmaternal) %>% count()
#  is_halfsibmaternal       n
# 1                  0 1009955
# 2                  1   49317

pedigree_withfamid %>% filter(is_index==0) %>% filter(is.na(FATHER_ID) | is.na(MOTHER_ID)) %>% group_by(is_halfsibpaternal) %>% count()
#  is_halfsibmaternal       n
# 1                  0 1034577
# 2                  1   24695


## save relative pairs ---------
fullsib <- fullsib %>% mutate(relationship="index-fullsib", kinship=0.5) %>% rename(ID="FINREGISTRYID") %>% select(ID, Relative_ID, relationship, kinship)
dim(fullsib)   # 7,779,033       4


halfsib_pat <- halfsib_pat %>% mutate(relationship="index-halfsibonlyfather", kinship=0.25) %>% rename(ID="FINREGISTRYID") %>% select(ID, Relative_ID, relationship, kinship)
dim(halfsib_pat)   # 808,667      4


halfsib_mat <- halfsib_mat %>% mutate(relationship="index-halfsibonlymother", kinship=0.25) %>% rename(ID="FINREGISTRYID") %>% select(ID, Relative_ID, relationship, kinship)
dim(halfsib_mat)   # 1,065,498      4


index_mother <- pedigree_withfamid %>% filter(is_index==1 & !is.na(MOTHER_ID)) %>% distinct(ID, MOTHER_ID) %>% 
                   mutate(relationship="index-mother", kinship=0.5) %>% rename(Relative_ID="MOTHER_ID") %>% select(ID, Relative_ID, relationship, kinship)
dim(index_mother)   # 4,202,659       4


index_father <- pedigree_withfamid %>% filter(is_index==1 & !is.na(FATHER_ID)) %>% distinct(ID, FATHER_ID) %>% 
                   mutate(relationship="index-father", kinship=0.5) %>% rename(Relative_ID="FATHER_ID") %>% select(ID, Relative_ID, relationship, kinship)
dim(index_father)   # 4,018,140       4


index_child <- dvv_child %>% filter(FINREGISTRYID %!in% qc_lst) %>% rename(ID="FINREGISTRYID") %>% 
                  mutate(relationship="index-child", kinship=0.5) %>% 
                  select(ID, Relative_ID, relationship, kinship)
dim(index_child)   #  7,480,444       4

relative_pairs <- rbind(index_mother, index_father, fullsib, halfsib_mat, halfsib_pat, index_child)
dim(relative_pairs)  # 25,354,441        4

relative_pairs %>% group_by(relationship, kinship) %>% count()
#   relationship            kinship       n
# 1 index-child                0.5  7480444
# 2 index-father               0.5  4018140
# 6 index-mother               0.5  4202659
# 3 index-fullsib              0.5  7779033
# 4 index-halfsibonlyfather    0.25  808667
# 5 index-halfsibonlymother    0.25 1065498
save(relative_pairs, file="Index_RelativePair_basic.Rdata")
write.table(relative_pairs, "Index_RelativePair_basic.txt", append=F, quote=F, sep="\t", row.names=F, col.names=T)



# father_child <- pedigree_withfamid %>% filter(is_child==1 & !is.na(FATHER_ID)) %>% distinct(ID, FATHER_ID) %>% 
#                    mutate(relationship="index-child", kinship=0.5) %>% rename(Relative_ID="ID", ID="FATHER_ID") %>% select(ID, Relative_ID, relationship, kinship)
# dim(father_child)   # 4,054,265       4
# 
# mother_child <- pedigree_withfamid %>% filter(is_child==1 & !is.na(MOTHER_ID)) %>% distinct(ID, MOTHER_ID) %>% 
#                    mutate(relationship="index-child", kinship=0.5) %>% rename(Relative_ID="ID", ID="MOTHER_ID") %>% select(ID, Relative_ID, relationship, kinship)
# dim(mother_child)   # 4,218,583       4
#
# relative_pairs <- rbind(index_mother, index_father, fullsib, halfsib_mat, halfsib_pat, mother_child, father_child)
# dim(relative_pairs)  # 26,146,845        4
# 
# relative_pairs %>% group_by(relationship, kinship) %>% count()
# #   relationship            kinship       n
# # 1 index-child                0.5  8272848
# # 2 index-father               0.5  4018140
# # 3 index-fullsib              0.5  7779033
# # 4 index-halfsibonlyfather    0.25  808667
# # 5 index-halfsibonlymother    0.25 1065498
# # 6 index-mother               0.5  4202659


