
#######################################
##### curve of learning script
##### version June, 28 2022

#######################################
####pred_num <- lmer(rate~math_level*pos_num*score+(pos_num|participant), data=g1, contrasts=list(score="contr.sum"))

#########################################
library(readxl)
library(afex)
library(emmeans)
library(ggplot2)


########### FUNCTIONS ############################################################

  
helmert.emmc <- function(levs, ...) {
     M <- as.data.frame(contr.helmert(levs))
     names(M) <- paste(levs[-1],"vs earlier")
     attr(M, "desc") <- "Helmert contrasts"
     M
 }

###########PARTICIPANTS ##########################################################
##################################################################################

########### EXPERIMENT 1 #########################################################
####### group 

parts1 <- read.csv("participants_xp2.csv", sep=";", header=T)
summary(parts1)
####### rates
g1 <- read.csv("scores_xp1.csv", sep=";", dec=",", header=T)

########## REPLICATION ###########################################################
###### group
parts2 <- read.csv("participants_xp2.csv", sep=";", header=T)
summary(parts2)
###### rates 
g2 <- read.csv("scores_xp2.csv", sep=";", dec=",", header=T)

##################################################################################
################# EXPERIMENT 1 ANALYSES ##########################################

############ effect of the position as a numerical factor
#### contrast pour la moyenne du score, et pas les scores un par un
pred_num <- lmer(score~math_level*pos_num*score_number+(1|participant), data=g1, contrasts=list(score_number="contr.sum")) 
anova(pred_num)
########## direction of the trend 
summary(pred_num)

############# effect of the position as a categorical factor 
pred_cat <- lmer(score~math_level*position*score_number+(1|participant), data=g1, contrasts=list(score_number="contr.sum"))
anova(pred_cat)

############ exploration of the main effect of position: lessons rated higher or lower
trend_pos <- contrast(emmeans(pred_cat, specs="position"), "helmert")
trend_pos

########## interaction math level position, exploration
trend_pos_niv <-summary(emtrends(pred_cat,var="math_level", specs="position"), infer=TRUE, null=0)
trend_pos_niv


###################################### POWER ANALYSES FOR LINEAR EFFECT ##########################################
##################################################################################################################

f_boot_num=function(d1, d2, form, n) {
		    formy <- lmer(form_num, d1)
		    simy=simulate(formy, nsim=n, newdata=d2, allow.new.level=T, seed=3)
                    #### d2_sim stores simulations according to parameters of second group	          
		    d2_sim=cbind(d2, simy)
		    #### setting the dfs, one df to store each p value mod_categ + estimates for positions expl effects
		    df_mod=data.frame()
		    df_estim=data.frame()
		    df_mod_int=data.frame()
                    ###### simulations for rates are put in the columns of d2_sim from 7th to last col of df
                    for (i in 8:length(d2_sim)){
		    	print(names(d2_sim)[i])
			
			#### main effect for position, global model
	                modz=lmer(d2_sim[,i]~math_level*pos_num*score_number+(1|participant), d2_sim)
			df_mod=rbind(df_mod, anova(modz)[2,6])
			names(df_mod)[1]="pos_main"

			#### estimate
			df_estim=rbind(df_estim, coef(summary(modz))[3,1])
			names(df_estim)[1]="estimate"

			#### interaction between position and math level
			df_mod_int=rbind(df_mod_int, anova(modz)[4,6])
			names(df_mod_int)[1]="interaction"
			
			}

		   df_mod_num=cbind( df_mod, df_estim, df_mod_int)
		   mod_num <<- df_mod_num
		   sim_num <<- d2_sim
}

######################### SIMULATED DF	###########################################
form_num=(score~math_level*pos_num*score_number+(1|participant))
f_boot_num(g1, g2, form, 1000)
summary(mod_num)

########################## POWER ANALYSES GLOBAL INCREASE #########################
###################################################################################

###### main position effect, only when trend is positive
mod_num$seuil_posmain=ifelse(mod_num$pos_main<0.05 & mod_num$estimate>0, 1, 0)
mean(mod_num$seuil_posmain)

###### interaction math level position
mod_num$seuil_int=ifelse(mod_num$interaction<0.05, 1, 0)
mean(mod_num$seuil_int)


########################### POWER ANALYSES CAT POSITION EFFECT ####################
###################################################################################

#### effect of position as categorial variable
#### function(d1=df that serves as a  basis for estimation, d2=df that is used for power analysis, form=stat model, n=number of iterations for simulation)
f_boot=function(d1, d2, form, n) {
		    formy <- lmer(form, d1)
		    simy=simulate(formy, nsim=n, newdata=d2, allow.new.level=T, seed=3)
                    #### d2_sim stores simulations according to parameters of second group	          
		    d2_sim=cbind(d2, simy)
		    #### setting the dfs, one df to store each p value output + estimates for positions exploration effects
		    df_mod=data.frame()
		    df_trend2=data.frame()
		    df_trend3=data.frame()
		    df_trend4=data.frame()
		    df_trend5=data.frame()
		    df_trend6=data.frame()
		    df_trend7=data.frame()
		    df_estim3=data.frame()
		    df_estim6=data.frame()
		    df_trend_niv=data.frame()
		    df_mod_int=data.frame()
		    df_trend_niv1=data.frame()
		    df_trend_niv2=data.frame()
		    df_trend_niv3=data.frame()
		    df_trend_niv4=data.frame()
		    df_trend_niv5=data.frame()
		    df_trend_niv6=data.frame()
		    df_trend_niv7=data.frame()
		    df_estim_niv5=data.frame()

                    ###### simulations for scores are put in the columns of d2_sim from 7th to last col of df
                    for (i in 8:length(d2_sim)){
		    	print(names(d2_sim)[i])
			
			#### main effect for position, global model
	                modz=lmer(d2_sim[,i]~math_level*position*score_number+(1|participant), d2_sim)
			df_mod=rbind(df_mod, anova(modz)[2,6])
			names(df_mod)[1]="pos_main"

			#### interaction between position and math level
			df_mod_int=rbind(df_mod_int, anova(modz)[4,6])
			names(df_mod_int)[1]="interaction"

			#### exploration of the position effect for each pos
			trend_pos <- data.frame(contrast(emmeans(modz, specs="position"), "helmert"))

			df_trend2=rbind(df_trend2, trend_pos[1,6])
			df_trend3=rbind(df_trend3, trend_pos[2,6])
			df_trend4=rbind(df_trend4, trend_pos[3,6])
			df_trend5=rbind(df_trend5, trend_pos[4,6])
			df_trend6=rbind(df_trend6, trend_pos[5,6])
			df_trend7=rbind(df_trend7, trend_pos[6,6])
			names(df_trend2)[1]="pos_expl2"
			names(df_trend3)[1]="pos_expl3"
			names(df_trend4)[1]="pos_expl4"
			names(df_trend5)[1]="pos_expl5"
			names(df_trend6)[1]="pos_expl6"
			names(df_trend7)[1]="pos_expl7"
			df_estim3=rbind(df_estim3, trend_pos[2,2])
			names(df_estim3)[1]="pos3_estim"
			df_estim6=rbind(df_estim6, trend_pos[5,2])
			names(df_estim6)[1]="pos6_estim"	
			
			### exploration of the interaction math_lev position for each pos
			trend_pos_lev <-data.frame(summary(emtrends(modz,var="math_level",specs="position"), infer=TRUE, null=0))
			#### collecting p value for each position
			df_trend_niv1=rbind(df_trend_niv1, trend_pos_lev[1,8])
			names(df_trend_niv1)[1]="lev_eff1"
			df_trend_niv2=rbind(df_trend_niv2, trend_pos_lev[2,8])
			names(df_trend_niv2)[1]="lev_eff2"
			df_trend_niv3=rbind(df_trend_niv3, trend_pos_lev[3,8])
			names(df_trend_niv3)[1]="lev_eff3"
			df_trend_niv4=rbind(df_trend_niv4, trend_pos_lev[4,8])
			names(df_trend_niv4)[1]="lev_eff4"
			df_trend_niv5=rbind(df_trend_niv5, trend_pos_lev[5,8])
			names(df_trend_niv5)[1]="lev_eff5"
			df_trend_niv6=rbind(df_trend_niv6, trend_pos_lev[6,8])
			names(df_trend_niv6)[1]="lev_eff6"
			df_trend_niv7=rbind(df_trend_niv7, trend_pos_lev[7,8])
			names(df_trend_niv7)[1]="lev_eff7"
						### enlever le estimate trend_pos5???
			### collecting the estimate because we look at a negative estimate, significant
			df_estim_niv5=rbind(df_estim_niv5, trend_pos_lev[5,2])
			names(df_estim_niv5)[1]="lev_estim_niv5"

			}
		   df_trend=cbind(df_trend2, df_trend3, df_estim3, df_trend4, df_trend5, df_trend6, df_estim6, df_trend7)
		   df_niv=cbind(df_trend_niv1, df_trend_niv2, df_trend_niv3, df_trend_niv4, df_trend_niv5, df_trend_niv6, df_trend_niv7)
		   df_mod=cbind( df_mod, df_mod_int, df_trend, df_niv, df_estim_niv5)
		   ##### saving the analyses 
		   mod_categ <<- df_mod
		   sim_categ <<- d2_sim
}

########## stimulated data ###################################################
##### 

form=(score~math_level*position*score_number+(1|participant))
f_boot(g1, g2, form, 1000)
summary(mod_categ)
 
############ POWER ANALYSES FOR POSITION EFFECT CATEGORIAL ########################
###################################################################################

###### Position effect
mod_categ$seuil_posmain=ifelse(mod_categ$pos_main<0.05, 1, 0)
mean(mod_categ$seuil_posmain)


##### pos2 exploration
mod_categ$seuil_pos2=ifelse(mod_categ$pos_expl2<0.05, 1, 0)
mean(mod_categ$seuil_pos2)

##### pos3 exploration
mod_categ$seuil_pos3=ifelse(mod_categ$pos3_estim<0 & mod_categ$pos_expl3<0.05, 1, 0)
mean(mod_categ$seuil_pos3)

##### pos4 exploration
mod_categ$seuil_pos4=ifelse(mod_categ$pos_expl4<0.05, 1, 0)
mean(mod_categ$seuil_pos4)

##### pos5 exploration
mod_categ$seuil_pos5=ifelse(mod_categ$pos_expl5<0.05, 1, 0)
mean(mod_categ$seuil_pos5)

##### pos6 exploration
mod_categ$seuil_pos6=ifelse(mod_categ$pos6_estim>0 & mod_categ$pos_expl6<0.05, 1, 0)
mean(mod_categ$seuil_pos6)

##### pos7 exploration
mod_categ$seuil_pos7=ifelse(mod_categ$pos_expl7<0.05, 1, 0)
mean(mod_categ$seuil_pos7)

##########interaction
###### interaction math level position
mod_categ$seuil_int=ifelse(mod_categ$interaction<0.05, 1, 0)
mean(mod_categ$seuil_int)


#####pos1
mod_categ$seuil_lev1=ifelse(mod_categ$lev_eff1<0.05, 1, 0)
mean(mod_categ$seuil_lev1)

### pos2
mod_categ$seuil_lev2=ifelse(mod_categ$lev_eff2<0.05, 1, 0)
mean(mod_categ$seuil_lev2)

### pos3
mod_categ$seuil_lev3=ifelse(mod_categ$lev_eff3<0.05, 1, 0)
mean(mod_categ$seuil_lev3)


### pos4
mod_categ$seuil_lev4=ifelse(mod_categ$lev_eff4<0.05, 1, 0)
mean(mod_categ$seuil_lev4)

### pos5 exploration from interaction math level position
mod_categ$seuil_lev5=ifelse(mod_categ$lev_estim_niv5<0 & mod_categ$lev_eff5<0.05, 1, 0)
mean(mod_categ$seuil_lev5)

### pos6
mod_categ$seuil_lev6=ifelse(mod_categ$lev_eff6<0.05, 1, 0)
mean(mod_categ$seuil_lev6)

##pos7
mod_categ$seuil_lev7=ifelse(mod_categ$lev_eff7<0.05, 1, 0)
mean(mod_categ$seuil_lev7)



######################## EXPERIMENT2 ANALYSES #######################################################################
#####################################################################################################################

########### position as  numerical factor #######################################
pred2_num <- lmer(score~math_level*pos_num*score_number+(1|participant), data=g2, contrasts=list(score="contr.sum"))
anova(pred2_num)


######### effect of position as categorical factor ##################################################################
position2 <- lmer(score~math_level*position*score_number+(1|participant), data=g2) 
anova(position2)


############ effect of 3rd lesson ##############################################
trend_pos_g2 <- contrast(emmeans(position2, specs=c("position")), "eff")
trend_pos_g2

########## negative estimate on 5th lesson #######################################
trend_pos_niv2 <-summary(emtrends(position2,var="math_level",specs=c("position")), infer=TRUE, null=0)
trend_pos_niv2


#################### FIGURES ####################################################
#################################################################################

####### esthetic functions
mycolors=function(ggobject){
ggobject+
scale_color_manual(values = c("#00AFBB", "#E7B800", "#CC0066", "#FF6666", "#9999FF", "#D16103","#FFDB6D" ))
}

myblues=function(ggobject){
ggobject+
scale_color_manual(values = c("#CCFFFF", "#99FFFF", "#99CCFF","#6699CC", "#336699", "#003399", "#003366" ))
}

mygreens=function(ggobject){
ggobject+
scale_color_manual(values = c("#ABFAAB", "#A7E4A7", "#8DC08D", "#33AE33", "#1D811D", "#456E45", "#1F3A1F"))
}


themetiny=function(ggobject){
ggobject+
theme(plot.title = element_text(face="bold",size=20,hjust = 0.5))+
theme(axis.text=element_text(size=15,face="bold"),                                                                                                                                                 
axis.title=element_text(size=15,face="bold"),
legend.text=element_text(size=15))
}

################### EXPERIENCE 1 PLOT  ###############################
######################################################################

### pred of model + raw data, mean of two scores
ptrends <- data.frame(summary(emmeans(pred,~position)))
raw <- aggregate(rate~position+participant+math_level, data=g1, FUN=mean)

pos_g1 <- ggplot(aes(x=position,y=emmean, group=7),data=ptrends)+
	    geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),size=0.8,width=0.4)+
            geom_point(data=raw,aes(y=rate), shape=17, color=  "#C67A51", alpha=1/3, position=position_jitter(w=0.1,h=0), size=3)+
 	    geom_point(size=4)+
 	    geom_line()+
	    coord_cartesian( ylim = c(-0.5,12), expand = FALSE, default = TRUE, clip = "on")+
	    scale_y_continuous(breaks = c(0,2,4,6,8,10))+
 	    theme_classic()+
 	    ggtitle("Effect of the order of the lessons on average score") + xlab("Order of the lessons")+ylab("Score")
  pos_g1 <-themetiny(pos_g1)
  pos_g1 <- mycolors(pos_g1)
  dev.new(width=10,height=10)
  pos_g1

##### effect of position: linked lines by participant
gg <- ggplot(aes(x=position,y=rate, group=participant, color=factor(math_level)),data=raw)+
  geom_point()+
  geom_line()+
  theme_classic()
  gg <- gg + ggtitle("Effect of the order of the lesson on rate") + xlab("Position of the lesson")+ylab("Rate")
  gg <- gg + guides(color=guide_legend("Math level"))
  gg <-themetiny(gg)
  dev.new(width=7,height=4)
  gg


################ INTERACTION LEVEL MATH POSITION ##########################

###########################################################################
#### faire apparaître sur le graphique les prédictions de tous les niveaux jusqu'à 7 ? OU juste les niveaux instanciés ???
#### pour l'instant juste les niveaux instanciés, sinon faire :
############ interaction math level and position (categ) 
# math_pr <- data.frame(summary(emmeans(pred, specs="position", by="math_level", at=list(math_level=c(0,1,3,4,5,6,7)), cov.reduce=range)))
raw$pos_num=raw$position

math_pr <- data.frame(summary(emmeans(pred_cat, specs="position", by="math_level", cov.reduce=F)))
math_pr

inter_g1 =ggplot(aes(x=position, y=emmean, color=factor(math_level), group=7),data=math_pr)+
	   geom_point(data=raw,aes(y=rate,color=factor(math_level), group=factor(math_level)), shape=17, size=4,alpha=1/3, 
	   position=position_jitterdodge(jitter.width = 0.2, jitter.height = 0, dodge.width =0.5))+
	   geom_line(aes(group=math_level), position=position_jitterdodge(jitter.width = 0, jitter.height = 0, dodge.width =0.5))+
	   geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL, group=factor(math_level)),size=0.8, position=position_dodge(0.5))+
	   geom_point(aes(group=factor(math_level), fill=factor(math_level)), size=4, colour="black", shape=21,  position=position_jitterdodge(jitter.width = 0, jitter.height = 0, dodge.width =0.5))+
	   theme_classic()+
	   coord_cartesian( ylim = c(-0.5,12), expand = FALSE, default = TRUE, clip = "on")+
	   scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  	   ggtitle("Effect of the order of the lessons on average score according to education in mathematics") + xlab("Order of the lessons")+ylab("Score")+
  	   guides(color=guide_legend("math_level"))+
	   scale_fill_manual(values=c("#ABFAAB", "#A7E4A7", "#8DC08D", "#33AE33", "#1D811D", "#456E45", "#1F3A1F"))
  inter_g1 <- mygreens(inter_g1)
  inter_g1  <-themetiny(inter_g1)
  dev.new(width=10,height=10)
  inter_g1


###### SIMULATION: POSITION EFFECT ON RATES ##############################

sim_plot=reshape2::melt(sim_categ, id.var=c("participant", "math_level", "position", "pos_num", "score", "rate", "number_lesson"))
sim_plot=aggregate(value~participant+math_level+position+score+pos_num+rate+number_lesson, data=sim_plot, FUN=mean)
sim_plot=subset(sim_plot, select=-c(rate, number_lesson))
sim_plot1=aggregate(value~participant+math_level+position, data=sim_plot, FUN=mean)
sim_plot2=aggregate(value~position, data=sim_plot, FUN=mean)
sim_plot3=aggregate(value~position+math_level, data=sim_plot, FUN=mean)

posim <- ggplot(aes(x=position,y=value, group=7),data=sim_plot2)+
  	  geom_point(aes(y=value), data=sim_plot1, alpha=1/3, shape=17, size=3, color="#C67A51", position=position_jitter(h=0, w=0.1))+
       	  geom_point(size=4)+
  	  geom_line()+
  	  theme_classic()+
	  coord_cartesian( ylim = c(-0.5,12), expand = FALSE, default = TRUE, clip = "on")+
	  scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  	  ggtitle("Effect of the order of the lessons on simulated average score") + xlab("Order of the lessons")+ylab("Score")+
  	  guides(color=guide_legend("math_level"))
  posim <-themetiny(posim)
  posim <- mycolors(posim)
  dev.new(width=10,height=10)
  posim

#######	INTERACTION POSITION MATH LEVEL ###########################################

inter_sim =ggplot(aes(x=position, y=value, color=factor(math_level), group=7),data=sim_plot3)+
	   geom_point(data=sim_plot1,aes(y=value,color=factor(math_level), group=factor(math_level)),size=4, shape=17, alpha=1/3, position=position_jitterdodge(jitter.width = 0.2, jitter.height = 0, 
	   dodge.width =0.5))+
	   geom_line(aes(group=math_level))+
	   geom_point(aes(group=factor(math_level), fill=factor(math_level)), size=4, shape=21, colour="black")+
	   theme_classic()+
	   coord_cartesian( ylim = c(-0.5,12), expand = FALSE, default = TRUE, clip = "on")+
	   scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  	   ggtitle("Effect of the position of the lessons on average score according to education in mathematics") + xlab("Order of the lessons")+ylab("Score")+
  	   guides(color=guide_legend("math_level"))+
	   scale_fill_manual(values=c("#ABFAAB", "#A7E4A7", "#8DC08D", "#33AE33", "#1D811D", "#456E45", "#1F3A1F"))
  inter_sim <- mygreens(inter_sim)
  inter_sim  <-themetiny(inter_sim)
  dev.new(width=10,height=10)
  inter_sim


################ G2 PLOTS #########################################################

trends_g2 <- data.frame(summary(emmeans(pred2,~position)))
raw_g2 <- aggregate(rate~position+participant+math_level, data=g2, FUN=mean)


pos_g2 <- ggplot(aes(x=position,y=emmean, group=7),data=trends_g2)+
  	  geom_point(aes(y=rate), data=raw_g2, alpha=1/3, size=3, color="#C67A51", shape=17, position=position_jitter(h=0, w=0.1))+
	  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),size=0.8,width=0.4)+
       	  geom_point(size=4)+
  	  geom_line()+
  	  theme_classic()+
	  coord_cartesian( ylim = c(-0.5,12), expand = FALSE, default = TRUE, clip = "on")+
	  scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  	  ggtitle("Effect of the order of the lessons on average score") + xlab("Order of the lessons")+ylab("Score")+
  	  guides(color=guide_legend("math_level"))
  pos_g2 <-themetiny(pos_g2)
  pos_g2 <- mycolors(pos_g2)
  dev.new(width=10,height=10)
  pos_g2


######### INTERACTION MATH LEVEL POSITION ############################################

#### idem ici, pour mettre tous les niveaux jusqu'à 10
ptrends_g22 <- data.frame(summary(emmeans(pred2, specs="position",  by="math_level", cov.reduce=F)))

inter_g2 =ggplot(aes(x=position, y=emmean, color=factor(math_level), group=7),data=ptrends_g22)+
	   geom_point(data=raw_g2,aes(y=rate,color=factor(math_level), group=factor(math_level)), shape=17,  size=4,alpha=1/3, position=position_jitterdodge(jitter.width = 0.2, jitter.height = 0,
	    dodge.width =0.5))+
	   geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL, group=factor(math_level)),size=0.8, position=position_dodge(0.5))+
	   geom_line(aes(group=math_level), position=position_jitterdodge(jitter.width = 0, jitter.height = 0, dodge.width =0.5))+
	   geom_point(aes(group=factor(math_level), fill=factor(math_level)), size=4,  shape=21, colour="black", position=position_jitterdodge(jitter.width = 0, jitter.height = 0, dodge.width =0.5))+
	   theme_classic()+
	   scale_y_continuous(breaks = c(0,2,4,6,8,10))+
           coord_cartesian( ylim = c(-0.5,12), expand = FALSE, default = TRUE, clip = "on")+
  	   ggtitle("Effect of the order of the lessons on average score according to education in mathematics") + xlab("Order of the lessons")+ylab("Score")+
  	   guides(color=guide_legend("math_level"))+
	   scale_fill_manual(values=c("#ABFAAB", "#A7E4A7", "#8DC08D", "#33AE33", "#1D811D", "#456E45", "#1F3A1F"))
  inter_g2 <- mygreens(inter_g2)
  inter_g2  <-themetiny(inter_g2)
  dev.new(width=10,height=10)
  inter_g2


################# correlation last rate performance ######################################################################
############ does last rate and max rate correlate with performance ? #########################################
##### performances 
perf=read.table("tests.csv", sep=";", header=TRUE, dec=",")
g1$participant=str_replace(g1$participant, "Sujet", "part")


#### selecting max rate among all rates (pos and score)
max_rate=aggregate(rate~participant+number_lessons+math_level, data=g1, FUN=max)
names(max_rate)[4]="max_rate"
g1_perf_max=merge(max_rate, tests)
#### selecting the last rate attributed (first score)

#### selecting the first iteration of rate
g1_f=subset(g1, score=="score1")

#### selecting the last rate of first scores
g1_f=subset(g1_f, substr(g1_f$position, 4,4)==g1_f$number_lessons)
names(g1_f)[4]="last_rate"
g1_perf_last=merge(g1_f, tests)

####  

