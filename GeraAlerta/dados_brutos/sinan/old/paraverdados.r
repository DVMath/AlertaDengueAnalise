library(foreign)
d <- read.dbf("Dengue_29-12-2014.dbf")
sort(table(d$NM_BAIRRO[d$]))
d <- read.dbf("../DENGON2015_03_02_2015.dbf")
sort(table(d$NM_BAIRRO))
d <- read.dbf("Dengue2010_BancoSINAN16_04_2012_v09012014.dbf")
names(d)
plot(d$X,d$Y)
