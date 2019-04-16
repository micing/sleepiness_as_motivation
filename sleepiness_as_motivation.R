library("MASS")
library("lmtest")
library("tidyverse")
library("readxl")

#### Read data, correct errors and anonymise ####

# d=read_excel("data/MOS_clean-2.xlsx") %>%
#   left_join(read_excel("data/SD gender age MOS.xlsx") %>% rename_all(tolower)) %>%
#   rename(MOS_23_text = MOS_2_text) %>%
#   mutate(
#     MOS_23 = replace(MOS_23, MOS_23 == 6, NA), # this is the freely rated category (see below)
#     MOS_23 = replace(MOS_23, MOS_23_text %in%c(20, 24), 6), # se above
#     MOS_19 = replace(MOS_19, MOS_19 == 0, NA), # remove respondents with no partner
#     kss = MOS_24,
#     rand=rnorm(123)) %>%
#     arrange(rand) %>%
#     mutate(id=seq(1,123)) %>% select(id, sd, kss, starts_with("MOS"))
# write_tsv(d, path="data/sleepiness_as_motivation.tsv")
  
data=read_tsv("data/sleepiness_as_motivation.tsv")
dv=names(data[!names(data) %in% c("id", "sd", "kss", "MOS_23_text")])

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
    print(v)
    m=list()
    m$base = polr(paste0(v,"~ 1" ), data=d, Hess=TRUE, method=method[i])
    if (v != "MOS_24") { # don't fit when kss (MOS_24) is both IV and DV
      m$kss = polr(paste0(v, "~ kss"), data=d, Hess=TRUE, method=method[i])
      m$p$kss =lrtest(m$kss, m$base)$`Pr(>Chisq)`[[2]]
    }
    m$sd = polr(paste0(v, "~ sd"), data=d, Hess=TRUE, method=method[i])
    m$p$sd = lrtest(m$sd, m$base)$`Pr(>Chisq)`[[2]]
    models[[v]][[method[i]]] = m
  }
  models$data=data
}

save(models, file="data/sleepiness_as_motivation_models.RDta")

#### save data for tables ####

kss_table = resultTable(models, dv, iv="kss", method="logistic")
sd_table = resultTable(models, dv, iv="sd", method="logistic")

write_tsv(kss_table, "tables/kss_table.tsv")
write_tsv(sd_table, "tables/sd_table.tsv")

