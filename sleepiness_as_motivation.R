library("MASS")
library("lmtest")

#### Read data and correct errors ####

# # anonymise data
#data=read.csv("data/data_slesi_motivation.txt", sep="\t")
#data = data[order(runif(dim(data)[1])), ]
#data$id=seq(1,dim(data)[1])
#write.csv(data, file="data/sleepiness_as_motivation.csv",  row.names=F)

data=read.csv("data/sleepiness_as_motivation.csv")
data$kss=data$MOS_24

dv=names(data[!names(data) %in% c("id", "sd", "kss", "MOS_22_text", "MOS_23_text")])
data[data$MOS_23 %in% c(6), "MOS_23"] <- NA # this is the freely rated category (see below)
data[data$MOS_22_text %in% c("20", "24"), "MOS_23"] <- 6 # Note: MOS_22_text is actually MOS_23_text
data[data$MOS_19 %in% c(0), "MOS_19"] <- NA # remove respondents with no partner

#### functions ####

star <- function(p){
  if (p<.001) return("***")
  if (p<.01) return("**")
  if (p<.05) return("*")
  return("")
}

resultTable <- function(models, dv, iv, method=NA) {
  r=data.frame(dv=NA, method=NA, OR=NA, lower=NA, upper=NA, p=NA, star=NA, est=NA, se=NA)
  m=method
  for (i in 1:length(dv)) {
    mo=models[[dv[i]]]
    if (is.na(method)) {me = best(mo)}
    else {me = method}
    m=mo[[me]]
    if (is.null(m[[iv]]$coefficients[[iv]]) | is.null(m$p[[iv]])) {next}
    r[i, "dv"] = dv[i]
    r[i, "method"] = me
    r[i, "est"] = m[[iv]]$coefficients[[iv]]
    r[i, "se"] = sqrt(vcov(m[[iv]])[[iv,iv]])
    r[i, "OR"] = exp(r[i, "est"])
    r[i, "lower"] = exp(r[i, "est"]-qnorm(.975)*r[i, "se"])
    r[i, "upper"] = exp(r[i, "est"]+qnorm(.975)*r[i, "se"])
    r[i, "p"] = m$p[[iv]]
    r[i, "star"] = star(r[i, "p"])
    for (j in 1:length(m[[iv]]$zeta)) {
      r[i, paste0("zeta", j)] = m[[iv]]$zeta[[j]]
      c = names(m[[iv]]$zeta)[j]
      print(c)
      r[i, paste0("zeta", j, "_se")] = sqrt(vcov(m[[iv]])[[c,c]])
    }
  }
  r
}

#### fit models ####

models=list()
method = c("logistic")

for (v in dv) {
  d=data
  d = d[!is.na(d$kss) & !is.na(d$sd) & !is.na(d[[v]]),]
  d[[v]] <- factor(d[[v]] , ordered=TRUE)

  for (i in 1:length(method)) {
    m=list()
    m$base = polr(paste0(v,"~ 1" ), data=d, Hess=TRUE, method=method[i])
    m$kss = tryCatch({polr(paste0(v, "~ kss"), data=d, Hess=TRUE, method=method[i])},
             error = function(e) NULL) # catch one error when kss (MOS_24) is both IV and DV
    m$sd = polr(paste0(v, "~ sd"), data=d, Hess=TRUE, method=method[i])
    m$p$kss = tryCatch({lrtest(m$kss, m$base)$`Pr(>Chisq)`[[2]]},  error = function(e) NULL)
    m$p$sd = lrtest(m$sd, m$base)$`Pr(>Chisq)`[[2]]
    models[[v]][[method[i]]] = m
  }
  models$data=data
}

save(models, file="data/sleepiness_as_motivation_models.RDta")

#### save data for tables ####

kss_table = resultTable(models, dv, iv="kss", method="logistic")
sd_table = resultTable(models, dv, iv="sd", method="logistic")

write.csv(kss_table, "tables/kss_table.csv")
write.csv(sd_table, "tables/sd_table.csv")

