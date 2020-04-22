library(brazilmaps)
library(sf)
library(data.table)
library(ggplot2)
library(ggpubr)
library(latex2exp)
library(colorspace)

BR <- get_brmap("State")

# Data from DATASUS
DENV14_17 <- fread("UF	DEN1	DEN2	DEN3	DEN4
Rondônia   	96	0	0	4
Acre   	9	20	0	0
Amazonas   	44	1	3	55
Roraima   	8	3	3	12
Pará   	114	6	0	40
Amapá   	244	1	0	0
Tocantins   	38	5	1	9
Maranhão   	61	3	2	11
Piauí   	57	3	2	1
Ceará   	348	0	5	26
Rio Grande do Norte	55	1	0	9
Paraíba   	82	3	0	1
Pernambuco   	285	8	14	11
Alagoas   	15	0	0	11
Sergipe   	15	0	0	5
Bahia   	139	10	1	301
Minas Gerais  	1424	13	4	21
Espírito Santo  	200	0	1	21
Rio de Janeiro 	504	6	3	87
São Paulo  	2882	97	10	230
Paraná   	2436	9	27	29
Santa Catarina  	7	0	0	0
Rio Grande do Sul	214	2	0	3
Mato Grosso do Sul	650	26	0	45
Mato Grosso  	197	4	0	3
Goiás   	1244	180	5	181
Distrito Federal  	243	38	5	4
")

DENV14_17 <- data.frame(DENV14_17)
DENV14_17$UF <- toupper(DENV14_17$UF)
DENV14_17$UF <- as.factor(DENV14_17$UF)
DENV14_17$Total <- DENV14_17$DEN1 + DENV14_17$DEN2 + DENV14_17$DEN3 + DENV14_17$DEN4
DENV14_17$chanceDENV4 <- DENV14_17$DEN4/DENV14_17$Total
DENV14_17$chanceDENV3 <- DENV14_17$DEN3/DENV14_17$Total
DENV14_17$chanceDENV2 <- DENV14_17$DEN2/DENV14_17$Total
DENV14_17$chanceDENV1 <- DENV14_17$DEN1/DENV14_17$Total

# Get Lat Lon
DENV14_17$Lat <- c(NA)
DENV14_17$Lon <- c(NA)
for(UF in (DENV14_17$UF)){
  print(UF)
  DENV14_17[which(DENV14_17$UF==UF),]$Lon <- (max(st_coordinates(BR[which(BR$nome==UF),])[,1]) +
                      min(st_coordinates(BR[which(BR$nome==UF),])[,1]))/2
  DENV14_17[which(DENV14_17$UF==UF),]$Lat <- (max(st_coordinates(BR[which(BR$nome==UF),])[,2]) +
                                                min(st_coordinates(BR[which(BR$nome==UF),])[,2]))/2
}

pal <- choose_palette()
# Plots
p1 <- plot_brmap(BR, data_to_join = DENV14_17, 
                    join_by = c("nome" = "UF"), 
                    var = c("chanceDENV1")) +
  geom_point(data = DENV14_17[which(DENV14_17$DEN4>0),], 
             aes(x = Lon, y = Lat, size = DEN1), 
             shape = 21, colour = "white", fill = "red", stroke = 2,
             alpha = 0.5) +
  scale_size_continuous(range = c(0, 20), guide = 'none') +
  scale_fill_stepsn(colours = pal(10),
                    breaks=seq(0.1,0.9, 0.1),
                    limits = c(0, 1),
                    name = TeX("$Proporção \\, = \\, \\frac{Sorotipo}{Total}$"))

p2 <- plot_brmap(BR, data_to_join = DENV14_17, 
                 join_by = c("nome" = "UF"), 
                 var = c("chanceDENV2")) +
  geom_point(data = DENV14_17[which(DENV14_17$DEN4>0),], 
             aes(x = Lon, y = Lat, size = DEN2), 
             shape = 21, colour = "white", fill = "red", stroke = 2,
             alpha = 0.5) +
  scale_size_continuous(range = c(0, 20), guide = 'none') +
  scale_fill_stepsn(colours = pal(10),
                    breaks=seq(0.1,0.9, 0.1),
                    limits = c(0, 1),
                    name = TeX("$Proporção \\, \\frac{Sorotipo}{Total}$"))

p3 <- plot_brmap(BR, data_to_join = DENV14_17, 
                 join_by = c("nome" = "UF"), 
                 var = c("chanceDENV3")) +
  geom_point(data = DENV14_17[which(DENV14_17$DEN4>0),], 
             aes(x = Lon, y = Lat, size = DEN3), 
             shape = 21, colour = "white", fill = "red", stroke = 2,
             alpha = 0.5) +
  scale_size_continuous(range = c(0, 20), guide = 'none') +
  scale_fill_stepsn(colours = pal(10),
                    breaks=seq(0.1,0.9, 0.1),
                    limits = c(0, 1),
                    name = TeX("$Proporção \\, \\frac{Sorotipo}{Total}$"))

p4 <- plot_brmap(BR, data_to_join = DENV14_17, 
                 join_by = c("nome" = "UF"), 
                 var = c("chanceDENV4")) +
  geom_point(data = DENV14_17[which(DENV14_17$DEN4>0),], 
             aes(x = Lon, y = Lat, size = DEN4), 
             shape = 21, colour = "white", fill = "red", stroke = 2,
             alpha = 0.5) +
  scale_size_continuous(range = c(0, 20), guide = 'none') +
  scale_fill_stepsn(colours = pal(10),
                    breaks=seq(0.1,0.9, 0.1),
                    limits = c(0, 1),
                    name = TeX("$Proporção \\, \\frac{Sorotipo}{Total}$"))

ggexport(annotate_figure(ggarrange(p1, p2, p3, p4, 
                                   nrow = 2, 
                                   ncol = 2,
                                   labels = c("DENV1", "DENV2", "DENV3", "DENV4"),
                                   common.legend = TRUE,
                                   legend = "right"),
                         top = text_grob("Dengue no Brasil entre 2014 e 2017 (DATASUS)\n", 
                                         color = "black", 
                                         face = "bold", 
                                         size = 20),
                         bottom = text_grob("*Círculos maiores indicam onde o sorotipo é mais encontrado, em números absolutos.\n**Cores mais escuras indicam onde o sorotipo é mais comum, entre os diagnosticados com dengue.", 
                                         color = "black", 
                                         face = "italic", 
                                         size = 10,
                                         hjust = 0,x = 0),
                         right = text_grob("Feito por: rnahumaf@gmail.com (GitHub: rnahumaf)",
                                           size = 10,
                                           rot = -90)
                         ), 
               width = 2500, height = 2500, res = 300, 
               filename = "DENV.png")
