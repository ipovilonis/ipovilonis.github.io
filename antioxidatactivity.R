library(readxl)
data_aaoF<-read_excel("database.xlsx", sheet="aaoF")
data_aaoF$year<-as.factor(data_aaoF$year)
data_aaoF$phenotype<-as.factor(data_aaoF$phenotype)
data_aaoF$conc<-factor(data_aaoF$conc,levels=c("0.025", "0.05","0.1","0.25"))

library(tidyr)

data_aaoF_2 <- data_aaoF %>%
  pivot_wider(
    names_from = conc,
    values_from = c("rep_1n","rep_2s","rep_3e","rep_4o")
  )

data_rep1n <- data_aaoF_2[,1:6]
data_rep2e <- data_aaoF_2[,c(1:2,7:10)]
data_rep3e <- data_aaoF_2[,c(1:2,11:14)]
data_rep4o <- data_aaoF_2[,c(1:2,15:18)]

colnames(data_rep2e)<-colnames(data_rep1n)
colnames(data_rep3e)<-colnames(data_rep1n)
colnames(data_rep4o)<-colnames(data_rep1n)

data_aao <- rbind(data_rep1n,data_rep2e,data_rep3e,data_rep4o)

colnames(data_aao) <- c("year","phenotype","c_0.025","c_0.05","c_0.1","c_0.25")

data_aao$repet<-as.factor(c(rep(1,110),rep(2,110),rep(3,110),rep(4,110)))

data_aao<-as.data.frame(data_aao)

data_aao <- dplyr::arrange(data_aao, desc(repet))
data_aao <- dplyr::arrange(data_aao, desc(phenotype))

table_aao_0.025 <- data_aao %>% 
  dplyr::group_by(year, phenotype) %>% 
  dplyr::summarise(n=n(),
                   Mean=mean(c_0.025),
                   sd=sd(c_0.025))

table_aao_0.05 <- data_aao %>% 
  dplyr::group_by(year, phenotype) %>% 
  dplyr::summarise(n=n(),
                   Mean=mean(c_0.05),
                   sd=sd(c_0.05))

table_aao_0.1 <- data_aao %>% 
  dplyr::group_by(year, phenotype) %>% 
  dplyr::summarise(n=n(),
                   Mean=mean(c_0.1),
                   sd=sd(c_0.1))

table_aao_0.25 <- data_aao %>% 
  dplyr::group_by(year, phenotype) %>% 
  dplyr::summarise(n=n(),
                   Mean=mean(c_0.25),
                   sd=sd(c_0.25))

aao_resume <- rbind(table_aao_0.025,table_aao_0.05,table_aao_0.1,table_aao_0.25)
aao_resume$conc_f <- c(rep("0.025",110),rep("0.05",110),rep("0.1",110),rep("0.25",110))

aao_resume$site <- rep(c(rep("concordia",13),rep("palmar",16),rep("gualeguaychu",9),
                     rep("concordia",11),rep("palmar",17),rep("gualeguaychu",16),
                     rep("concordia",17),rep("palmar",1),rep("gualeguaychu",10)
                     ),4)

aao_resume$site <- factor(aao_resume$site, levels=c("concordia", "palmar","gualeguaychu"))

aao_resume<-na.omit(aao_resume)

table_aao_sites <- aao_resume %>%
dplyr::group_by(site, year, conc_f) %>%
dplyr::summarise(mean = mean(Mean),
                 sd = sd (Mean)
                 )

library(ggplot2)
# library(forcats) x = fct_reorder2(species, site, n)

ggaao_f <- ggplot(table_aao_sites, aes(x = year, y = mean, fill = conc_f)) +
  geom_bar(stat = "identity", size = 1, position = position_dodge(0.8)) +
  labs(y = " % AOA (fresh weight)", x = "") +
  facet_grid(. ~ year) +
  facet_wrap(~site, ncol = 3, scales = "free") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd),width=.05, color="black", position = position_dodge(0.8))+
  scale_fill_manual(values = c("lightblue","grey", "lightblue4","#358999")) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    panel.grid.major.y = element_line(size = 0.05, color = 'black'),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 20, color = 'black'),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.border = element_rect(fill = "transparent"),
    axis.text.x = element_text(angle = 60, hjust = 1, color = 'black'),
    axis.text.y = element_text(angle = 0, hjust = 1, color = 'black')
  ) +
  scale_y_continuous(breaks = seq(10, 100, 10), limits = c(0, 100))

ggaao_f + coord_cartesian(ylim = c(20, 100))


