#decomposition

rm(list=ls())

source("code/model/clean_data_model.R")
source("code/model/leave_country_out.R")
source("code/model/func_nuclear_and_pgd.R")

grps = c(4,6)
X.list = lapply(grps,function(wave) X[X.all$BB_Wave_num == wave, ])
Z.list = lapply(grps,function(wave) Z[Z.all$BB_Wave_num == wave, ])
all.countries = unique(Z.all$BB_Country)
X.os.list = lapply(grps,function(wave)  X.all$BB_Country[X.all$BB_Wave_num == wave])
Z.os.list = lapply(grps,function(wave)  Z.all$BB_Country[Z.all$BB_Wave_num == wave])
y.list = lapply(grps, function(g) {paste0(X.all[X.all$BB_Wave_num== g,"Y_manifesto_key"],X.all[X.all$BB_Wave_num== g,"BB_Wave_num"])})

Y.list = lapply(1:length(grps), function(g) {
  res = matrix(0, nrow(X.list[[g]]),nrow(Z.list[[g]]))
  colnames(res) = row.names(Z.list[[g]])
  res}
)
for (g in 1:length(grps)){
  for (i in 1:length(y.list[[g]])){
    Y.list[[g]][i,y.list[[g]][i]] = 1
  }
  stopifnot(all(apply(Y.list[[g]],1,sum)==1))
  
}


#load first stage
load.if.exist = function(path){
  if (file.exists(path)){
    readRDS(path)
  }
}

fs.folder = "loo"
mod.list = lapply(all.countries, function(cnt) lapply(grps, function(wave) load.if.exist(paste0(paste0(model_results_path, "/"),fs.folder,"/loo_wave_",wave,"county_",cnt,".Rds"))))
names(mod.list) = all.countries

phi.lists = lapply(mod.list,function(cnt) lapply(cnt, function(w) w$mod$phi))
delta.lists = lapply(mod.list,function(cnt) lapply(cnt, function(w) w$mod$intercept))
#second stage


ssl = lapply(all.countries, function(cnt) readRDS(paste0(paste0(model_results_path, "/loo/loo_second_sage_county_"),cnt,".Rds")))
names(ssl) = all.countries

#xxx - if adding wave 5 should change this to be more similar to the part that was commented out
beta.list = lapply(ssl, function(cnt){as.list(data.frame(cnt$beta)) })
names(beta.list) = names(ssl)

#beta.list = lapply(ssl,function(cnt) lapply(grps, function(i) { if(as.character(i) %in% colnames(cnt)){
#  cnt[,as.character(i)]
#} else {
#  NULL}
#}))

#name disctionaries
key = Z.all[,c("Y_manifesto_key","Y_manifesto_party_id")]
key = key[!duplicated(key),]
rownames(key) = key$Y_manifesto_key
party.id.list = lapply(Z.list, function(z){
  #  v = as.integer(key[rownames(z),"Y_manifesto_party_id"])
  v = as.character(key[substr(rownames(z),1,nchar(rownames(z))-1),
                       "Y_manifesto_party_id"])
  names(v) = rownames(z)
  v})

party.dat = unique(X.all[,c("Y_manifesto_party_id","BB_Country","Y_FR_Pop")])
rownames(party.dat) = party.dat[,1]
party.dat = party.dat[,-1]

#decomposition 
ddal = decomp.do.all.loo(X.list,Z.list,Y.list,phi.lists,beta.list,party.id.list,party.dat,X.os.list,Z.os.list,1,2,dec.order = c("resid", "Z","X","param"))
ddal[nrow(ddal),]/sum(ddal[nrow(ddal),])
print(ddal)
saveRDS(ddal,paste0(decomposition_path, "/loo/decomposition.Rds"))

dec4_6 = readRDS(paste0(decomposition_path, "/dec_4_6.Rds"))
orig = dec4_6[-nrow(dec4_6),ncol(dec4_6)]
lco = ddal[-nrow(ddal),ncol(ddal)]
orig.lco = c(orig,lco)
mn = min(orig.lco)
mx = max(orig.lco)
plot(orig,lco,pch = 19, xlim = c(mn,mx),ylim = c(mn,mx), xlab = "Main Specification", ylab = "LCO speciication")
abline(0,1)
grid()