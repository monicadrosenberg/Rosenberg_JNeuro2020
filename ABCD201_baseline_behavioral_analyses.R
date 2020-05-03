library(dplyr)

######################################################################################
############################## LOAD AND CLEAN VARIABLES ############################## 
######################################################################################
dataDir   <-"ABCDrelease201/"
outputDir <-"Data/"

mysum  <- function(x)sum(x,na.rm = any(!is.na(x)))
mymean <- function(x)mean(x,na.rm = any(!is.na(x)))

######### Read files #########
Demographics <- read.delim(paste(dataDir,"abcddemo01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
Screener     <- read.delim(paste(dataDir,"abcd_screen01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
RAChecklist  <- read.delim(paste(dataDir,"abcd_ra01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
ScannerID    <- read.delim(paste(dataDir,"abcd_mri01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
SiteID       <- read.delim(paste(dataDir,"abcd_lt01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
Family       <- read.delim(paste(dataDir,"acspsw03.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

NIH_toolbox  <- read.delim(paste(dataDir,"abcd_tbss01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
Pearson      <- read.delim(paste(dataDir,"abcd_ps01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
CashChoice   <- read.delim(paste(dataDir,"cct01.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
LittleMan    <- read.delim(paste(dataDir,"lmtp201.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

Nback        <- read.delim(paste(dataDir,"abcd_mrinback02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
RecMem       <- read.delim(paste(dataDir,"mribrec02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
SST          <- read.delim(paste(dataDir,"abcd_sst02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)
MID          <- read.delim(paste(dataDir,"abcd_mid02.txt",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE) %>% slice(-1)

######### Define relevant variables #########
cols.Demographics <- c("interview_age","sex")
cols.Screener     <- c("scrn_asd","scrn_medcond_other","scrn_epls","scrn_seizure","scrn_commondx")
cols.RAChecklist  <- c("ra_scan_check_list_rcom","ra_scan_cl_mid_scan_lap","ra_scan_check_list_vemorc","ra_scan_cl_nbac_scan_lap","ra_scan_check_list_sstrc","ra_scan_cl_sst_scan_lap")
cols.ScannerID    <- c("mri_info_deviceserialnumber")
cols.Family       <- c("rel_relationship","rel_family_id")
cols.SiteID       <- c("site_id_l")
cols.NIH_toolbox  <- c("nihtbx_picvocab_uncorrected","nihtbx_flanker_uncorrected","nihtbx_list_uncorrected","nihtbx_cardsort_uncorrected","nihtbx_pattern_uncorrected","nihtbx_picture_uncorrected","nihtbx_reading_uncorrected","nihtbx_fluidcomp_uncorrected","nihtbx_cryst_uncorrected","nihtbx_totalcomp_uncorrected")
cols.Pearson      <- c("pea_wiscv_tss","pea_ravlt_sd_trial_i_tc","pea_ravlt_sd_trial_ii_tc","pea_ravlt_sd_trial_iii_tc","pea_ravlt_sd_trial_iv_tc","pea_ravlt_sd_trial_v_tc","pea_ravlt_sd_trial_i_tr","pea_ravlt_sd_trial_ii_tr","pea_ravlt_sd_trial_iii_tr","pea_ravlt_sd_trial_iv_tr","pea_ravlt_sd_trial_v_tr","pea_ravlt_sd_trial_i_ti","pea_ravlt_sd_trial_ii_ti","pea_ravlt_sd_trial_iii_ti","pea_ravlt_sd_trial_iv_ti","pea_ravlt_sd_trial_v_ti","pea_ravlt_sd_listb_tc","pea_ravlt_sd_listb_tr","pea_ravlt_sd_listb_ti","pea_ravlt_sd_trial_vi_tc","pea_ravlt_sd_trial_vi_tr","pea_ravlt_sd_trial_vi_ti","pea_ravlt_ld_trial_vii_tc","pea_ravlt_ld_trial_vii_tr","pea_ravlt_ld_trial_vii_ti")
cols.CashChoice   <- c("cash_choice_task")
cols.LittleMan    <- c("lmt_scr_efficiency","lmt_scr_perc_correct","lmt_scr_rt_correct")
cols.Nback        <- c("tfmri_nback_beh_switchflag","tfmri_nback_beh_performflag","tfmri_nb_all_beh_ctotal_mrt","tfmri_nb_all_beh_ctotal_stdrt","tfmri_nb_all_beh_c0b_rate","tfmri_nb_all_beh_c0bnf_rate","tfmri_nb_all_beh_c0bngf_rate","tfmri_nb_all_beh_c0bp_rate","tfmri_nb_all_beh_c0bpf_rate","tfmri_nb_all_beh_c2b_rate","tfmri_nb_all_beh_c2bnf_rate","tfmri_nb_all_beh_c2bngf_rate","tfmri_nb_all_beh_c2bp_rate","tfmri_nb_all_beh_c2bpf_rate","tfmri_nb_all_beh_cnf_rate","tfmri_nb_all_beh_cngf_rate","tfmri_nb_all_beh_cpf_rate","tfmri_nb_all_beh_cplace_rate","tfmri_nb_all_beh_ctotal_rate","tfmri_nb_r1_beh_c0b_rate","tfmri_nb_r2_beh_c0b_rate","tfmri_nb_r1_beh_c2b_rate","tfmri_nb_r2_beh_c2b_rate")
cols.RecMem       <- c("tfmri_rec_beh_switchflag","tfmri_rec_all_beh_posface_br","tfmri_rec_all_beh_posf_dpr","tfmri_rec_all_beh_neutface_br","tfmri_rec_all_beh_neutf_dp","tfmri_rec_all_beh_negface_br","tfmri_rec_all_beh_negf_dp","tfmri_rec_all_beh_place_br","tfmri_rec_all_beh_place_dp")
cols.SST          <- c("tfmri_sst_beh_switchflag","tfmri_sst_beh_performflag","tfmri_sst_all_beh_crgo_rt","tfmri_sst_all_beh_crgo_mrt","tfmri_sst_all_beh_crgo_stdrt","tfmri_sst_all_beh_crlg_rt","tfmri_sst_all_beh_incrgo_rt","tfmri_sst_all_beh_incrlg_rt","tfmri_sst_all_beh_nrgo_rt","tfmri_sst_all_beh_crs_rt","tfmri_sst_all_beh_incrs_rt","tfmri_sst_all_beh_ssds_rt","tfmri_sst_all_beh_tot_mssd","tfmri_sst_all_beh_total_meanrt")
cols.MID          <- c("tfmri_mid_beh_switchflag","tfmri_mid_beh_performflag","tfmri_mid_all_beh_srwpfb_rate","tfmri_mid_all_beh_lrwpfb_rate","tfmri_mid_all_beh_slpfb_rate","tfmri_mid_all_beh_llpfb_rate","tfmri_mid_all_beh_ntpfb_rate","tfmri_mid_r1_beh_t_earnings","tfmri_mid_r2_beh_t_earnings","tfmri_mid_all_beh_t_earnings","tfmri_mid_all_beh_t_nt","tfmri_mid_all_beh_srwpfb_nt","tfmri_mid_all_beh_lrwpfb_nt","tfmri_mid_all_beh_slpfb_nt","tfmri_mid_all_beh_llpfb_nt","tfmri_mid_all_beh_ntpfb_nt","tfmri_mid_all_beh_srwpfb_mrt","tfmri_mid_all_beh_lrwpfb_mrt","tfmri_mid_all_beh_slpfb_mrt","tfmri_mid_all_beh_llpfb_mrt","tfmri_mid_all_beh_ntpfb_mrt")

######### Retain relevant variables #########
Demographics      <- unique(subset(Demographics, select = c("subjectkey", cols.Demographics)))
Screener          <- unique(subset(Screener,     select = c("subjectkey", cols.Screener)))
RAChecklist       <- unique(subset(RAChecklist,  select = c("subjectkey", cols.RAChecklist)))
ScannerID         <- unique(subset(ScannerID,    select = c("subjectkey", cols.ScannerID)))
Family            <- unique(subset(Family,       select = c("subjectkey", cols.Family)))
SiteID            <- SiteID[ which(SiteID$eventname=="baseline_year_1_arm_1"),]
SiteID            <- unique(subset(SiteID,       select = c("subjectkey", cols.SiteID)))
NIH_toolbox       <- unique(subset(NIH_toolbox,  select = c("subjectkey", cols.NIH_toolbox)))
Pearson           <- unique(subset(Pearson,      select = c("subjectkey", cols.Pearson)))
CashChoice        <- unique(subset(CashChoice,   select = c("subjectkey", cols.CashChoice)))
LittleMan         <- unique(subset(LittleMan,    select = c("subjectkey", cols.LittleMan)))
Nback             <- unique(subset(Nback,        select = c("subjectkey", cols.Nback)))
RecMem            <- unique(subset(RecMem,       select = c("subjectkey", cols.RecMem)))
SST               <- unique(subset(SST,          select = c("subjectkey", cols.SST)))
MID               <- unique(subset(MID,          select = c("subjectkey", cols.MID)))

######### Convert variables to numeric #########
Demographics[, "interview_age"] <- lapply("interview_age",  function(x) as.numeric(Demographics[[x]]))
Screener[, cols.Screener]       <- lapply(cols.Screener,    function(x) as.numeric(Screener[[x]]))
Family[, cols.Family]           <- lapply(cols.Family,      function(x) as.numeric(Family[[x]]))
NIH_toolbox[, cols.NIH_toolbox] <- lapply(cols.NIH_toolbox, function(x) as.numeric(NIH_toolbox[[x]]))
Pearson[, cols.Pearson]         <- lapply(cols.Pearson,     function(x) as.numeric(Pearson[[x]]))
CashChoice[, cols.CashChoice]   <- lapply(cols.CashChoice,  function(x) as.numeric(CashChoice[[x]]))
LittleMan[, cols.LittleMan]     <- lapply(cols.LittleMan,   function(x) as.numeric(LittleMan[[x]]))
Nback[, cols.Nback]             <- lapply(cols.Nback,       function(x) as.numeric(Nback[[x]]))
RecMem[, cols.RecMem]           <- lapply(cols.RecMem,      function(x) as.numeric(RecMem[[x]]))
SST[, cols.SST]                 <- lapply(cols.SST,         function(x) as.numeric(SST[[x]]))
MID[, cols.MID]                 <- lapply(cols.MID,         function(x) as.numeric(MID[[x]]))

######### Add performance measure columns #########
RecMem$overall_dprime              <- apply(RecMem[c('tfmri_rec_all_beh_posf_dpr', 'tfmri_rec_all_beh_neutf_dp', 'tfmri_rec_all_beh_negf_dp', 'tfmri_rec_all_beh_place_dp')], 1, mymean)
MID$mean_earnings                  <- apply(MID[c('tfmri_mid_r1_beh_t_earnings', 'tfmri_mid_r2_beh_t_earnings')], 1, mymean)

######### Invert SSRT #########
SST$tfmri_sst_all_beh_total_meanrt_inv <- SST$tfmri_sst_all_beh_total_meanrt*-1

######### Remove cash choice option 3 ("don't know") #########
CashChoice$cash_choice_task_no3 <- CashChoice$cash_choice_task
CashChoice$cash_choice_task_no3[CashChoice$cash_choice_task_no3 == 3] <- NA

######### Merge, clean, crop data #########
data.merge <- Reduce(function(x,y) merge(x = x, y = y, by = "subjectkey", all.x = TRUE, all.y = TRUE), list(Demographics, Screener, RAChecklist, ScannerID, SiteID, Family, NIH_toolbox, Pearson, CashChoice, LittleMan, Nback, RecMem, SST, MID))
data.crop  <- data.merge[ which(data.merge$scrn_asd==0 & (data.merge$scrn_epls!=1 | is.na(data.merge$scrn_epls))), ]
data.crop  <- subset(data.crop, select = c(subjectkey, rel_family_id, site_id_l, ra_scan_cl_mid_scan_lap, ra_scan_cl_nbac_scan_lap, ra_scan_cl_sst_scan_lap, interview_age, sex, tfmri_nback_beh_performflag, tfmri_sst_beh_performflag, tfmri_mid_beh_performflag, nihtbx_list_uncorrected, nihtbx_picvocab_uncorrected, nihtbx_reading_uncorrected, tfmri_nb_all_beh_c2b_rate, pea_wiscv_tss, nihtbx_picture_uncorrected, pea_ravlt_sd_trial_vi_tc, pea_ravlt_ld_trial_vii_tc, tfmri_nb_all_beh_c0b_rate, nihtbx_cardsort_uncorrected, nihtbx_flanker_uncorrected, lmt_scr_efficiency, overall_dprime, nihtbx_pattern_uncorrected, mean_earnings, tfmri_sst_all_beh_total_meanrt_inv, cash_choice_task_no3))

######### Compare releases #########
release1.subs <- read.delim("release1point1_ndars.csv",sep=",", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
release1.subs <- data.frame(intersect(data.crop$subjectkey,release1.subs$x))
release2.subs <- data.frame(setdiff(data.crop$subjectkey,release1.subs$intersect.data.crop.subjectkey..release1.subs.x.))

write.csv(data.crop,file=paste(outputDir,"data.crop.201.csv",sep=""))
start_col <- 12


######################################################################################
############################## GENERATE CONTROL SAMPLES ############################## 
######################################################################################

######### Get complete cases #########
data.comp <- data.crop[complete.cases(data.crop[,start_col:ncol(data.crop)]),]

######### Exclude outliers #########
sd_thresh <- 2.5
data.excl <- data.crop

for (i in start_col:ncol(data.excl)) {
  tmp_mean <- mean(data.excl[,i], na.rm=TRUE)
  tmp_sd   <- sd(data.excl[,i], na.rm=TRUE)
  data.excl[which((data.excl[,i]<=(tmp_mean - sd_thresh*tmp_sd) | data.excl[,i]>=(tmp_mean + sd_thresh*tmp_sd))), i]<-NA
  rm(tmp_mean, tmp_sd)
}

######### Calculate percentage of missing and outlier values #########
data.tmp1 <- data.crop$nihtbx_list_uncorrected
data.tmp2 <- data.excl$nihtbx_list_uncorrected
round(sum(is.na(data.tmp1))/length(data.tmp1)*100,digits=2)
round((sum(is.na(data.tmp2))-sum(is.na(data.tmp1)))/(length(data.tmp1)-sum(is.na(data.tmp1)))*100,digits=2)
rm(data.tmp1, data.tmp2)

######### Exclude family members #########
family_idx <- data.frame()
for (i in data.crop$rel_family_id) {
  tmp_ids <- (which(data.crop$rel_family_id %in% i))
  family_idx <- rbind(tmp_ids[1],family_idx)
  rm(tmp_ids)
}
family_idx  <- unique(family_idx)
data.family <- data.crop[family_idx$X1L, ]

######### Exclude participants who completed neuroimaging tasks outside the scanner #########
data.scan <- data.crop[ which(data.crop$ra_scan_cl_mid_scan_lap %in% 1 & data.crop$ra_scan_cl_sst_scan_lap %in% 1 & data.crop$ra_scan_cl_nbac_scan_lap %in% 1), ]

######### Exclude participants with ABCD performance flags #########
data.flag <- data.crop[ which(data.crop$tfmri_nback_beh_performflag %in% 1 & data.crop$tfmri_sst_beh_performflag %in% 1 & data.crop$tfmri_mid_beh_performflag %in% 1), ]
  
######################################################################################
######################### MEASURE BEHAVIORAL RELATIONSHIPS ########################### 
######################################################################################

######### Correlate NIH Toolbox List Sorting Working Memory Test performance with missing data #########
cor.test(data.crop$nihtbx_list_uncorrected,rowSums(is.na(data.crop[,start_col:ncol(data.crop)])), use="pairwise.complete.obs", method="spearman")

######### Generate behavioral cross-correlation matrices #########
# all data
cormat.crop   <- cor(data.crop[,start_col:ncol(data.crop)], use="pairwise.complete.obs", method="spearman")

# release 1 vs. release 2
cormat.release1 <- cor(data.crop[is.element(data.crop$subjectkey,release1.subs$intersect.data.crop.subjectkey..release1.subs.x.),start_col:ncol(data.family)], use="pairwise.complete.obs", method="spearman")
cormat.release2 <- cor(data.crop[is.element(data.crop$subjectkey,release2.subs$setdiff.data.crop.subjectkey..release1.subs.intersect.data.crop.subjectkey..release1.subs.x..),start_col:ncol(data.family)], use="pairwise.complete.obs", method="spearman")
cor.test(cormat.release1[lower.tri(cormat.release1, diag = FALSE)],cormat.release2[lower.tri(cormat.release2, diag = FALSE)],method = "spearman")

# unrelated subsample
cormat.family <- cor(data.family[,start_col:ncol(data.family)], use="pairwise.complete.obs", method="spearman")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.family[lower.tri(cormat.family, diag = FALSE)],method = "spearman")

# partial correlation controlling for age and sex
library(psych)
data.numeric <- data.crop
data.numeric$sex[data.numeric$sex=="M"]<-1
data.numeric$sex[data.numeric$sex=="F"]<-0
data.numeric <- sapply(data.numeric,as.numeric)
cols.partial_corr_control <- c(7,8) # age and sex column indices
cormat.partial <- partial.r(data.numeric, c(start_col:ncol(data.numeric)), cols.partial_corr_control, use="pairwise", method="spearman")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.partial[lower.tri(cormat.partial, diag = FALSE)],method = "spearman")

# excluding outlier values
cormat.excl   <- cor(data.excl[,start_col:ncol(data.excl)], use="pairwise.complete.obs", method="spearman")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.excl[lower.tri(cormat.excl, diag = FALSE)],method = "spearman")

# complete cases only
cormat.comp   <- cor(data.comp[,start_col:ncol(data.comp)], use="pairwise.complete.obs", method="spearman")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.comp[lower.tri(cormat.comp, diag = FALSE)],method = "spearman")

# in-scanner fMRI tasks only
cormat.scan   <- cor(data.scan[,start_col:ncol(data.scan)], use="pairwise.complete.obs", method="spearman")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.scan[lower.tri(cormat.scan, diag = FALSE)],method = "spearman")

# only children without performance flags
cormat.flag <- cor(data.flag[,start_col:ncol(data.flag)], use="pairwise.complete.obs", method="spearman")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.flag[lower.tri(cormat.flag, diag = FALSE)],method = "spearman")

######### Conservative subsample #########
data.css<-data.excl[complete.cases(data.excl[,start_col:ncol(data.excl)]),]
data.css<-data.css[ which(data.css$ra_scan_cl_mid_scan_lap %in% 1 & data.css$ra_scan_cl_sst_scan_lap %in% 1 & data.css$ra_scan_cl_nbac_scan_lap %in% 1), ]
data.css<-data.css[ which(data.css$tfmri_nback_beh_performflag %in% 1 & data.css$tfmri_sst_beh_performflag %in% 1 & data.css$tfmri_mid_beh_performflag %in% 1), ]
family_idx2 <- data.frame()
for (i in data.css$rel_family_id) {
  tmp_ids <- (which(data.css$rel_family_id %in% i))
  family_idx2 <- rbind(tmp_ids[1],family_idx2)
  rm(tmp_ids)
}
family_idx2  <- unique(family_idx2)
data.css <- data.css[family_idx2$X1L, ]

data.cssnumeric <-data.css
data.cssnumeric$sex[data.cssnumeric$sex=="M"]<-1
data.cssnumeric$sex[data.cssnumeric$sex=="F"]<-0
data.cssnumeric<-sapply(data.cssnumeric,as.numeric)
cormat.csspartial <- partial.r(data.cssnumeric, c(start_col:ncol(data.cssnumeric)), c(7,8), use="pairwise", method="spearman")
cor.test(cormat.crop[lower.tri(cormat.crop, diag = FALSE)],cormat.csspartial[lower.tri(cormat.csspartial, diag = FALSE)],method = "spearman")

######### Site-specific correlation matrices #########
sites     <- unique(data.crop$site_id_l)
sites     <- sites[!is.na(sites)]
site_size <- matrix(0, ncol = 1, nrow = 22)
site_corr <- matrix(0, ncol = 22, nrow = 136)
count     <- 0
for (i in sites) {
  count              <- count+1
  tmp_data           <- data.crop[which(data.crop$site_id_l==i),start_col:ncol(data.crop)]
  site_corrmat       <- cor(tmp_data, use="pairwise.complete.obs", method="spearman")
  site_corr[,count]  <- site_corrmat[lower.tri(site_corrmat, diag = FALSE)]
  site_size[count,1] <- nrow(tmp_data)
  rm(tmp_data, site_corrmat)
}

cols.site <- c(1,2,4:22) # Exclude 22nd site (column 3) with 36 participants
site_xcor        <- cor(site_corr,method = "spearman")
site_xcor        <- site_xcor[cols.site,cols.site] 
diag(site_xcor)  <- NA
site_typicality  <- rowMeans(site_xcor,na.rm=TRUE)
cor.test(site_typicality,site_size[cols.site],method = "spearman")
min(site_xcor[lower.tri(site_xcor, diag = FALSE)])
max(site_xcor[lower.tri(site_xcor, diag = FALSE)])
mean(site_xcor[lower.tri(site_xcor, diag = FALSE)])
sd(site_xcor[lower.tri(site_xcor, diag = FALSE)])

######### Working memory, age, and sex effects #########
min(data.crop[!is.na(data.crop$nihtbx_list_uncorrected),"nihtbx_list_uncorrected"])
max(data.crop[!is.na(data.crop$nihtbx_list_uncorrected),"nihtbx_list_uncorrected"])
mean(data.crop$nihtbx_list_uncorrected,na.rm=TRUE)
sd(data.crop$nihtbx_list_uncorrected,na.rm=TRUE)

cor.test(data.crop$nihtbx_list_uncorrected,data.crop$interview_age,method='pearson')
t.test(data.crop$nihtbx_list_uncorrected~data.crop$sex)
library(effsize)
cohen.d(data.crop$nihtbx_list_uncorrected~data.crop$sex)

######### Steiger's z-test #########
library(cocor)
cocor.dep.groups.overlap(cor(data.crop$nihtbx_list_uncorrected, data.crop$tfmri_nb_all_beh_c0b_rate, use="pairwise.complete.obs", method="spearman"), 
                         cor(data.crop$nihtbx_list_uncorrected, data.crop$tfmri_nb_all_beh_c2b_rate, use="pairwise.complete.obs", method="spearman"), 
                         cor(data.crop$tfmri_nb_all_beh_c0b_rate, data.crop$tfmri_nb_all_beh_c2b_rate, use="pairwise.complete.obs", method="spearman"), 
                         nrow(data.crop), alternative = "two.sided",
                         test = "steiger1980", alpha = 0.05, conf.level = 0.95, null.value = 0,
                         data.name = NULL, var.labels = NULL, return.htest = FALSE)

cocor.dep.groups.overlap(cor(data.crop$nihtbx_list_uncorrected, data.crop$nihtbx_flanker_uncorrected, use="pairwise.complete.obs", method="spearman"), 
                         cor(data.crop$nihtbx_list_uncorrected, data.crop$overall_dprime, use="pairwise.complete.obs", method="spearman"), 
                         cor(data.crop$overall_dprime, data.crop$nihtbx_flanker_uncorrected, use="pairwise.complete.obs", method="spearman"), 
                         nrow(data.crop), alternative = "two.sided",
                         test = "steiger1980", alpha = 0.05, conf.level = 0.95, null.value = 0,
                         data.name = NULL, var.labels = NULL, return.htest = FALSE)

######### Compare effect of excluding relatives to effect of excluding 11537-9750 = 1787 random participants #########
ptm <- proc.time()
set.seed(0)
observed_diff <- abs(cormat.crop-cormat.family)
observed_diff <- observed_diff[lower.tri(observed_diff, diag = FALSE)]
nrand <- 1000
corvector.random <- data.frame(matrix(nrow = length(observed_diff), ncol = nrand))
for (i in 1:nrand) {
  cormat.random <- cor(data.crop[sample(nrow(data.crop), nrow(data.family), replace = FALSE, prob = NULL),start_col:ncol(data.crop)], use="pairwise.complete.obs", method="spearman")
  cormat.random <- abs(cormat.crop-cormat.random)
  corvector.random[,i] <- cormat.random[lower.tri(cormat.random, diag = FALSE)]
}

nonpar_p <- data.frame(matrix(nrow = length(observed_diff), ncol = 1))
for (j in 1:nrow(nonpar_p)) {
  nonpar_p[j,1] <- (1+sum(corvector.random[j,1:nrand]>=observed_diff[j]))/(nrand+1)
}

proc.time() - ptm


######################################################################################
################################### MAKE FIGURES ##################################### 
######################################################################################

######### Figure 1: Behavioral distributions #########
library(ggplot2)
library(gridExtra)
library(tidyr)

data.fig1  <- data.merge[ which(data.merge$scrn_asd==0 & (data.merge$scrn_epls!=1 | is.na(data.merge$scrn_epls))), ]
data.fig1  <- subset(data.fig1, select = c(subjectkey, rel_family_id, site_id_l, ra_scan_cl_mid_scan_lap, ra_scan_cl_nbac_scan_lap, ra_scan_cl_sst_scan_lap, interview_age, sex, tfmri_nback_beh_performflag, tfmri_sst_beh_performflag, tfmri_mid_beh_performflag, nihtbx_list_uncorrected, nihtbx_reading_uncorrected, nihtbx_picvocab_uncorrected, tfmri_nb_all_beh_c2b_rate, pea_ravlt_sd_trial_vi_tc, pea_wiscv_tss, pea_ravlt_ld_trial_vii_tc, nihtbx_picture_uncorrected, tfmri_nb_all_beh_c0b_rate, nihtbx_cardsort_uncorrected, nihtbx_flanker_uncorrected, lmt_scr_efficiency, overall_dprime, nihtbx_pattern_uncorrected, mean_earnings, tfmri_sst_all_beh_total_meanrt, cash_choice_task))

fig1_theme1 <- theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(size = 14), axis.title.y = element_text(colour = "black", size = 14))
fig1_theme2 <- theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(size = 14), axis.title.y = element_text(colour = "white", size = 14))
fig1_theme3 <- theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(colour = "white", size = 14), axis.title.y = element_text(colour = "white", size = 14), axis.text.x = element_text(colour="white"), axis.text.y = element_text(colour="white"))
fig1_theme4 <- theme_minimal() + theme(legend.position=c(.1,.5), legend.title = element_blank(), legend.text = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(size = 14), axis.title.y = element_text(colour = "white", size = 14))
fig1_theme5 <- theme_minimal() + theme(legend.position=c(.1,.5), legend.title = element_blank(), legend.text = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(size = 14), axis.title.y = element_text(colour = "black", size = 14))

color1 <- "#77AAAD"
color2 <- "#6E7783"
color3 <- "#D8E6E7"

p1 <- ggplot(data.fig1, aes(x=nihtbx_list_uncorrected))     + geom_density(alpha = 1, fill = "white", colour = "white") + xlab("List Sorting Working Memory Test") + fig1_theme3

# NIH Toolbox measures
p2 <- ggplot(data.fig1, aes(x=nihtbx_list_uncorrected))     + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("List Sorting Working Memory Test") + fig1_theme1
p3 <- ggplot(data.fig1, aes(x=nihtbx_picvocab_uncorrected)) + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("Picture Vocabulary Test") + fig1_theme2
p4 <- ggplot(data.fig1, aes(x=nihtbx_flanker_uncorrected))  + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("Flanker Test") + fig1_theme2
p5 <- ggplot(data.fig1, aes(x=nihtbx_cardsort_uncorrected)) + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("Dimensional Change Card Sort Test") + fig1_theme1
p6 <- ggplot(data.fig1, aes(x=nihtbx_pattern_uncorrected))  + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("Pattern Comparison Processing Speed Test") + fig1_theme2
p7 <- ggplot(data.fig1, aes(x=nihtbx_picture_uncorrected))  + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("Picture Sequence Memory Test") + fig1_theme2
p8 <- ggplot(data.fig1, aes(x=nihtbx_reading_uncorrected))  + geom_density(alpha = 1, fill = color1, colour = "black") + xlab("Oral Reading Recognition Test") + fig1_theme2

# Other neurocognitive measures
p9 <- ggplot(data.fig1, aes(x=pea_wiscv_tss)) + geom_density(alpha = 1, fill = color2, colour = "black") + xlab("WISC-V: Matrix reasoning scaled score") + fig1_theme1

data.reshape <- gather(data.fig1, "ravlt", "score", c(start_col+4,start_col+6))
p10 <- ggplot(data.reshape, aes(x=score, fill=ravlt, color=ravlt)) + geom_density(alpha = .5) + scale_colour_manual(values=c("black", "black")) + scale_fill_manual(values=c(color2, color2)) + xlab("RAVLT: Total correct") + fig1_theme2

data.reshape <- data.fig1
data.reshape$lmt_scr_efficiency <- data.reshape$lmt_scr_efficiency*10000
p11 <- ggplot(data.reshape, aes(x=lmt_scr_efficiency)) + geom_density(alpha = 1, fill = color2, colour = "black") + xlab("Little Man task: Efficiency ratio") + fig1_theme2

data.reshape <- data.fig1[!is.na(data.fig1$cash_choice_task),]
data.reshape$cash_choice_task[data.reshape$cash_choice_task == 1] <- "smaller-sooner"
data.reshape$cash_choice_task[data.reshape$cash_choice_task == 2] <- "larger-later"
data.reshape$cash_choice_task[data.reshape$cash_choice_task == 3] <- "don't know"

## set the levels in order we want
data.reshape <- within(data.reshape, cash_choice_task <- factor(cash_choice_task, levels=names(sort(table(cash_choice_task)))))
p12 <- ggplot(data.reshape, aes(x=cash_choice_task)) + geom_bar(alpha = 1, fill = color2, colour = "black", label = c("smaller","larger","don't know")) + xlab("Cash choice task") + fig1_theme1

# Neuroimaging task measures
data.reshape <- gather(data.fig1, "enback", "score", c(start_col+8,start_col+3))
data.reshape$score <- data.reshape$score*100
enback_title <- expression(paste("Emotional ", italic("n"), "-back task: ", "Percent accuracy"))
p13 <- ggplot(data.reshape, aes(x=score, fill=enback, color=enback)) + geom_density(alpha = .5) + scale_colour_manual(values=c("black", "black")) + scale_fill_manual(values=c(color3, color3)) + xlab(enback_title) + fig1_theme1

dprime_title <- expression(paste("Emotional ", italic("n"), "-back recognition memory task: ", italic("d'")))
p14 <- ggplot(data.fig1, aes(x=overall_dprime)) + geom_density(alpha = 1, fill = color3, colour = "black") + xlab(dprime_title) + fig1_theme2

p15 <- ggplot(data.fig1, aes(x=tfmri_sst_all_beh_total_meanrt)) + geom_density(alpha = 1, fill = color3, colour = "black") + xlab("Stop-signal task: Stop-signal reaction time") + fig1_theme2

p16 <- ggplot(data.fig1, aes(x=mean_earnings)) + geom_density(alpha = 1, fill = color3, colour = "black") + xlab("Monetary incentive delay task: Earnings") + fig1_theme2

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, nrow = 4)
g<-arrangeGrob(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, nrow = 4)
ggsave(file=paste(outputDir,"Dist_raw_201_test.pdf",sep=""), g, width = 16, height = 8, units = c("in"), dpi = 300)


######### Figure 2: Multidimensional scaling #########
library(MASS)
color1 <- "#77AAAD"
color2 <- "#6E7783"
color3 <- "#D8E6E7"
data.transposed <- scale(data.comp[,start_col:ncol(data.comp)], center = TRUE, scale = TRUE)
data.transposed <- data.frame(t(data.transposed))
d <- dist(data.transposed) # euclidean distances between the rows
mds.cmdscale       <- as.data.frame(cmdscale(as.matrix(d)))
mds.cmdscale$names <- rownames(mds.cmdscale)
mds.cmdscale$types <- c("NIH Toolbox","NIH Toolbox","NIH Toolbox",
                        "Neuroimaging","Neurocognitive","NIH Toolbox",
                        "Neurocognitive","Neurocognitive","Neuroimaging",
                        "NIH Toolbox","NIH Toolbox","Neurocognitive",
                        "Neuroimaging","NIH Toolbox","Neuroimaging",
                        "Neuroimaging","Neurocognitive")

g2 <- ggplot(mds.cmdscale, aes(V1, V2, label=names)) + 
  geom_point(aes(fill=types),colour="black",pch=21, size=10) +
  labs(x="", y="") + theme_minimal() + 
  theme(legend.position="none", panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  scale_colour_manual(values=c("black", "black","black")) + 
  scale_fill_manual(values=c(color2, color3, color1))
ggsave(file=paste(outputDir,"MDS_raw_201.pdf",sep=""), g2, width = 5.5, height = 5, units = c("in"), dpi = 300)


ggplot(mds.cmdscale, aes(V1, V2, label=names)) + 
  geom_point(aes(fill=types),colour="black",pch=21, size=10) +
  labs(x="", y="") + theme_minimal() + 
  theme(legend.position="none", panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  scale_colour_manual(values=c("black", "black","black")) + 
  scale_fill_manual(values=c(color2, color3, color1)) +  geom_text(aes(colour=factor(types)), size=2.2, 
           hjust = "center", vjust = "bottom", nudge_x = 0, nudge_y = 0.005)


######### Figure 3: Behavioral correlation matrix #########
library(corrplot)
cormat.crop.flip <- cormat.crop*-1
corrplot(cormat.crop.flip,method="color",tl.cex=.4,
         tl.col = "black",diag=FALSE,addgrid.col="white",
         tl.pos="n", cl.pos="n")

