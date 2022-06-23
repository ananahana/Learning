library(data.table)
library(magrittr)
library(readxl)
library(dtplyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(ggthemes)
library(GGally)


path <- paste0("extdata/eqtl/")
expression <- fread(paste0(path, "expression.txt"))
gene <- fread("extdata/eqtl/gene.txt")
genotype <- fread("extdata/eqtl/genotype.txt")
growth <- fread("extdata/eqtl/growth.txt")
marker <- fread("extdata/eqtl/marker.txt")

