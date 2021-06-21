# Shiny app for CFRD
library(shiny)
library(ggplot2)
library(survival)
library(tidyverse)
library(timeROC)
##########################################################################################

# theme_Publication <- function(base_size=14, base_family="helvetica") {
#   library(grid)
#   library(ggthemes)
#   (theme_foundation(base_size=base_size, base_family=base_family)
#     + theme(plot.title = element_text(face = "bold",
#                                       size = rel(1.2), hjust = 0.5),
#             text = element_text(),
#             panel.background = element_rect(colour = NA),
#             plot.background = element_rect(colour = NA),
#             panel.border = element_rect(colour = NA),
#             axis.title = element_text(face = "bold",size = rel(1)),
#             axis.title.y = element_text(angle=90,vjust =2),
#             axis.title.x = element_text(vjust = -0.2),
#             axis.text = element_text(), 
#             axis.line = element_line(colour="black"),
#             axis.ticks = element_line(),
#             panel.grid.major = element_line(colour="#f0f0f0"),
#             panel.grid.minor = element_blank(),
#             legend.key = element_rect(colour = NA),
#             legend.position = "bottom",
#             legend.direction = "horizontal",
#             legend.key.size= unit(0.2, "cm"),
#             legend.margin = unit(0, "cm"),
#             legend.title = element_text(face="italic"),
#             plot.margin=unit(c(10,5,5,5),"mm"),
#             strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
#             strip.text = element_text(face="bold")
#     ))
#   
# }


##Assign CFTR Severity Scores 

list4.1=c("621+1G>T", "G542X", "1717-1G>A", "R553X", "W1282X", "711+1G>T", "1898+1G>A", "1154insTC", "2184insA", 
          "S489X","3659delC", "Q493X", "E60X", "R1162X", "Y1092X", "2183AA->G", "3905insT", "394delTT", "2184delA", "4016insT", 
          "Q1313X", "E585X", "2183AA>G", "2347delG", "3007delG", "R1158X", "1078delT", "CFTRdele2,3", "S4X", "2622+1G>A", 
          "3120+1G>A", "E831X", "R75X", "W846X", "1161delC", "1213delT", "1248+1G>A", "1259insA", "2143delT", "2189CT->A", 
          "2622+1G>T", "2787del16bps", "365-366insT", "406-1G>A", "4209TGTT>AA", "557delT", "712-1G->T", "CFTRdele2-3", 
          "CFTRdele2-4", "E193X", "K710X", "L1254X", "L732X", "Q220X", "Q39X", "Q414X", "Q814X", "S1196X", "S912X", "1138insG",
          "1249-27delTA", "1249-29delAT", "1309delG", "1342-1delG", "185+1G>T", "2002del8(GCTATTTT)", "2118del4", "2184DELA",
          "3028delA", "3271+1G>T", "3396delC", "3447delG", "3617delGA", "3659DELC", "3662delA", "3667ins4(TCAA)", "4089delA",
          "4326delTC", "4374+1G>T", "441delA", "C524X", "CFTRdele10", "CFTRdele2", "CFTRdele4-8", "E1104X", "G27X", "K553X", 
          "Q1382X", "Q890X", "R709X", "R785X", "S466X(TAG)", "W1274X", "Y1092X1001+11CORdelTAF508", "3120G>A", "3876delA", "457TATdelinsG", "875+1G>C", "C276X", "CFTRdele17A-18", "L88X(T>G)", "del exons 2-3", 
          "681delC", "c.4021dupT", "(p.PHE342fs)", "L218X", "474del13BP,490C>G,491G->",	
          "1288INTA", "1288insTA", "1366delG", "1461insAGAT", "1524+1G>A", "1811+1G>C", "1874insT", "2105-2117 del13insAGAAA",
          "2142delT", "2183delT", "2951insA", "296+1G>T", "306insA", "3154delG", "3171insC", "360-365insT", "3600+1G>T", 
          "360insT", "3791delC", "4010del4(TATT)", "4022insT", "4374+1G>A", "650delATAAA", "G330X", "K793X", "Q1035X", 
          "Q1313X and I1027T", "Q552X", "W1089X", "W679X", "Y1307X", "Y849X", "W1282X", "G542X", 
          "R553X","E60X", "2184insA", "712-1G->T", "2183AA-G", "621+1G->T", "1154insTC", "1461ins4", "1898+1G->A", 
          "2183delAA>G", "3120+1G->A", "3349insT", "1154InsTC", "ex14a",  "E92X", "Dup ex6b-10", 
          "654del5", "237insA", "W1204X", "Y122X", "CFTRdele2(ins186)", "1717-1G->A", "R851X", "406-1G->A", "Q2X", "2585delT", "del X22-24") 

list4.2=c("621+1G>T", "W1282X", "G542X", "1717-1G>A", "3659delC", "R553X", "2184delA", "1154insTC", "711+1G>T", 
          "R1158X", "R1162X", "R75X", "W57X", "Y1092X", "1898+1G>A", "2183AA>G", "2184DELA", "3905insT", "CFTRdele10", 
          "CFTRdele2,3", "E585X", "E60X", "S489X", "S4X",  "11531154dup", 
          "1461ins4(AGAT)", "1716-1G>A", "3120+1G>T", "4016insT", "4016onsT", "L218X", "Q1042X", "Q552X", "R709X", "W1204X", 
          "1138insG", "1161delC", "1525-1G>A", "2622+1G>A", "3171delC", 
          "3786delA", "712-1G>T", "CFTRdele2-3", "Q493X",  "621+1G->T", "W1089X", "712-1G->T",  "1717-1G->A" ,"Q1412X","R792X")

list3.1=c("DF508", "[delta]F508", "delF508", "[delta]I507", "[delta]F508/I507", "[delta]F508;I1027T", "F", "F(_I507)", "I507", "_I507", "F508del", "F508", "I507")

list3.2=c("DF508", "[delta]F508", "delF508", "[delta]I507", "delI507","F", "I507", 
          "S1251N(F508C)", "F(_I507)","F508del", "F508", "DI507","I507del")


list2.1=c("M1101K", "N1303K", "V520F", "R1066C","L1077P", "R560T", "S549N", "A561E" )

list2.2=c("G551D", "N1303K", "M1101K", "R560T", "L1077P", "V520F", "S549N", "R1066C;IVS8(5T)", "I148T;3199del6", "3199del6", "R1066C")

list1.1=c("A455E", "2789+5G>A", "L1335P", "L145H", "R347P", "S341P", "1716G>A", "G1244E", "G85E", "L206W", "R117H", 
          "Y503N","(3499+37G>A)", "1868A>G", "3849+10kbC>T", "7T", "D1152H", "G239R",
          "G480S", "M1137V", "R117H(5T)", "R31L", "R334L", "R334W", "R347H", "V392G", "Y569D", 
          "-743delT","4005+2T>C", "A155P", "D579G", "F508I (c.1663T>A)", "G458V", "G576A", "I148T", "I506T", "IVS8(5T)", 
          "L138ins", "L558S", "L997F", "P67L", "R555G", "S492F", "W1098R", "Y109C" )
list1.2=c("G85E", "3849+10kbC>T", "A455E", "IVS8(5T)", "3272-26A>G", "3600+2insT", "I1027T", "S1251N", 
          "S1251N", "2789+5G>A", "G1247R", "I148T", "P574H", "R347P", "2789+2insA", "3121-2A>G", "3600G>A", "4005+2T>C", 
          "711+5G>A", "A1006E+V562I_in_cis", "G1244E", "H1079P", "L206W", "P67L", "Q1291H", "Q1411X", "R1070Q", "S341P", 
          "S492F","1525-2A>G", "2752-2A>G", "3273-26A>G", "3850-3T>G;R75Q", "A559T", "D1152H", "D579G", "H199R", "I1234V", 
          "I336K", "I444S", "I506T","L468P", "L558S", "Q1071K", "R1070W", "R117H", "R334W", "R75Q", "S1118C", 
          "S954L", "V1240G; R75Q", "V562I; IVS8(5T)","1868A>G", "2789+5G>A", "3272-93", 
          "3849+10kbc>T", "5T", "7T", "A1076V", "A455E", "A559T", "D110H", "D1152H", "G85E", "L206W", "P67L", "Q1291H", "R117H", 
          "R334W", "R352Q", "Y569D", "t1299i", "1898+3G>A", "711+3A>G", "A1067V", "C332T", "D614G", "G178R", "G628R(5T)", "L1335P", 
          "N396Y", "R1117H", "R117C", "R117H(5T)", "R347H", "R347P/R347H", "S1235R", "S549R", "T1299I", "T338I", "V1153E", "V232D", 
          "V456A", "W1098R", "Y914C","-589G>A", "1002-2A>G", "2789+5C>T", "2789+5G>A;I102(7T)", 
          "3849+10kbC>T; R668C", "3849G>A", "546insCTA", "621+2G>T", "875+40A>G", "A1006E+V562IINCIS", "A613T", "D1270N; R74W", 
          "D443Y", "E116K", "F508C", "G1003E", "G458V", "L165S", "M150K", "R117H(7T)", "R117P", "R560K", "R668C", "S1159F", 
          "Q1209P", "2789+5G->A", "D1270N" )

m1=c("NEGATIVE", "MISSINGDATA", "WAITINGRESU"," ", "________", "c.4028C>T", "NOTTYPED", "UNKNOWN", "-999","unknown" ,"Not ID", "UNK", "**No Data Available","Missing","**Not Identified","unk","Unknown")

mutation <- sort(c(list4.1,list4.2,list3.1,list3.2,list2.1,list2.2,list1.1,list1.2))

assign_cftr <- function(input_data){
  output <- NA
  if(input_data %in% c(list4.1, list4.2)){
    output <- 4
  } 
  else if(input_data %in% c(list3.1, list3.2)){
    output <- 3
  }
  else if(input_data %in% c(list2.1, list2.2)){
    output <- 2
  }
  else if(input_data %in% c(list1.1, list1.2)){
    output <- 1
  }
  else if(input_data %in% m1){
    output <- 0
  }
  
  output
}

assign_score <- function(score1, score2){
  if(score1==4 & score2==4){
    cftr_score=5
  }
  else if((score1==3 & score2==4) | (score1==4 & score2==3)){
    cftr_score=4
  }
  else if(score1==3 & score2==3){
    cftr_score=3
  }
  else if((score1==2 & score2>=2) | (score1>=2 & score2==2)){
    cftr_score=2
  }
  else if((score1==1 & score2>=1) | (score1>=1 & score2==1)){
    cftr_score=1
  }
  else if(score1==0 | score2==0){
    cftr_score=0
  }
  
  
  cftr_score
}

compute_risk <- function(model,year,cftr_score,sex,mi,rs1964986,rs4077468,rs7903146,rs959173,rs7822917,rs12318809){
  score_factor <- ifelse(cftr_score>1, 1, 0)
  when_birth <- case_when(
      year >= 2000 ~ "1_young",
      year >= 1990 ~ "2_secondyoung",
      year >= 1980 ~ "3_med",
      year >= 1970 ~ "4_secondmed")
  sex <- ifelse(sex=="F", 0, 1)
  ind <- data.frame(when_birth=when_birth,score_factor=score_factor,gender=sex,mi=mi,rs1964986=2-rs1964986,
                    rs4077469=2-rs4077468,rs7903146=rs7903146,rs959173=2-rs959173,rs7822917=rs7822917,rs12318809=rs12318809)
  select_coef <- summary(model)$coef
  select <- c("score_factor","gender","rs12318809","rs959173",
              "rs1964986","rs4077469","rs7822917","mi","rs7903146")
  risk_geno <- as.matrix(ind[,select]) %*% as.matrix(select_coef[c(1,2,6,7,8,9,10,11,12),1]) 
  risk_when_birth <- with(ind, ifelse(when_birth=="4_secondmed", select_coef[5,1], 
                                      ifelse(when_birth=="3_med", select_coef[4,1], 
                                             ifelse(when_birth=='2_secondyoung', select_coef[3,1], 0))))
  risk_score <- as.numeric(risk_geno + risk_when_birth)
  #risk_score <- predict(model, ind, type='lp')
  
  print(risk_score)
  
  risk_score
}
###########################################################################################
# risk <- readRDS('C:/Users/jerry/OneDrive/2020_CFRD/app_data/risk.rds')
# cox_cftrscore <- readRDS('C:/Users/jerry/OneDrive/2020_CFRD/app_data/model.rds')
# cfrd_new <- readRDS('C:/Users/jerry/OneDrive/2020_CFRD/app_data/cfrd_new.rds')
# age_event <- readRDS('C:/Users/jerry/OneDrive/2020_CFRD/app_data/age_event.rds')

#risk <- readRDS('app_data/risk.rds')
#cox_cftrscore <- readRDS('app_data/model.rds')
risk <- readRDS('app_data/risk_new.rds')
cox_cftrscore <- readRDS('app_data/model_new.rds')
#cfrd_new <- readRDS('app_data/cfrd_new.rds')
#age_event <- readRDS('app_data/age_event.rds')

ui <- fluidPage(
  headerPanel("Personalized CFRD Risk Based on Genetic and Clinical Measures at Birth"),
  sidebarPanel(
    textInput('birth', 'Year of Birth'),
    selectInput('cftr1', 'CFTR Genotype Mutation 1', choices=mutation),
    selectInput('cftr2', 'CFTR Genotype Mutation 2', choices=mutation),
    selectInput('sex', 'Sex', choices = c("Male"='M', "Female"='F')),
    selectInput('mi', 'Meconium Ileus (Y/N)', choices = c('Y'=1, 'N'=0)),
    selectInput('rs1964986', 'PRSS1: rs1964986 (Number of C alleles)', choices = c(0,1,2)),
    selectInput('rs4077468', 'SLC26A9: rs4077468 (Number of A alleles)', choices = c(0,1,2)),
    selectInput('rs7903146', 'TCF7L2: rs7903146 (Number of T alleles)', choices = c(0,1,2)),
    selectInput('rs959173', 'CAV1: rs959173 (Number of C alleles)', choices = c(0,1,2)),
    selectInput('rs7822917', 'NRG1: rs7822917 (Number of T alleles)', choices = c(0,1,2)),
    selectInput('rs12318809', 'SLC5A8: rs12318809 (Number of G alleles)', choices = c(0,1,2)),
    actionButton('update', 'Update')
  ),
  mainPanel(plotOutput('plot1')),
  mainPanel(plotOutput('plot2'))
)

server <- function(input, output){
  risk_score <- eventReactive(input$update, {
    year <- as.numeric(input$birth)
    cftr_score <- assign_score(assign_cftr(input$cftr1), assign_cftr(input$cftr2))
    sex <- input$sex
    mi <- as.numeric(input$mi)
    rs1964986 <- as.numeric(input$rs1964986)
    rs4077468 <- as.numeric(input$rs4077468)
    rs7903146 <- as.numeric(input$rs7903146)
    rs7822917 <- as.numeric(input$rs7822917)
    rs959173 <- as.numeric(input$rs959173)
    rs12318809 <- as.numeric(input$rs12318809)
    
    compute_risk(cox_cftrscore,year,cftr_score,sex,
                 mi,rs1964986,rs4077468,rs7903146,
                 rs959173,rs7822917,rs12318809)
  })
  
  # year <- eventReactive(input$update,{
  #   as.numeric(input$birth)
  # })
  # cftr_score <- eventReactive(input$update, {
  #   assign_score(assign_cftr(input$cftr1), assign_cftr(input$cftr2))
  # })
  # sex <- eventReactive(input$update, {
  #   input$sex
  # })
  # mi <- eventReactive(input$update, {
  #   as.numeric(input$mi)
  # })
  # rs1964986 <- eventReactive(input$update, {
  #   as.numeric(input$rs1964986)
  # })
  # rs4077468 <- eventReactive(input$update, {
  #   as.numeric(input$rs4077468)
  # })
  # rs7901695 <- eventReactive(input$update, {
  #   as.numeric(input$rs7901695)
  # })
  # rs16879142 <- eventReactive(input$update, {
  #   as.numeric(input$rs16879142)
  # })
  # rs1729533 <- eventReactive(input$update, {
  #   as.numeric(input$rs1729533)
  # })
  # rs12318809 <- eventReactive(input$update, {
  #   as.numeric(input$rs12318809)
  # })
  # 
 
  
  output$plot1 <- renderPlot({
    # risk_score <- compute_risk(cox_cftrscore,year(),cftr_score(),sex(),
    #                            mi(),rs1964986(),rs4077468(),rs7903146(),
    #                            rs16879142(),rs1729533(),rs12318809())
    print(dim(risk))
    percentile <- round(100*sum(risk < risk_score())/dim(risk)[1])
    print(percentile)
    ggplot(risk) + 
      geom_density(fill='steelblue1', col='steelblue1', alpha=0.3, aes(x=CFRD)) + 
      labs(y='Density', x='CFRD Risk', title="CFRD Risk Distribution in Canadian Gene Modifier Study") + 
      annotate('text',x=risk_score(),y=0.4,label=paste('CFRD Risk: ',percentile,'th percentile',sep=''), size=7, col='red') +
      geom_point(x=risk_score(),y=0.3,size=5,col='red') +
      geom_segment(aes(x=risk_score(),y=0,xend=risk_score(),yend=0.3), col='red') +
      theme(plot.title=element_text(size=16, face='bold'),
            axis.title=element_text(size=14,face="bold"))
  })
  
  output$plot2 <- renderPlot({
    bh=basehaz(cox_cftrscore)
    cumu_risk = (1-exp(-bh[,1])^(exp(risk_score())))[c(15,18,20,22,25,28,30,32,34)]
    
    canada <- data.frame(age=c(15,18,20,22,25,28,30,32,34),
                         risk=cumu_risk)

    
    ggplot(canada, aes(x=age,y=risk))+
      geom_line(size=1.5, color='red') + 
      geom_point(color='red', size=4)+
      ylim(0,1) +
      xlab("Age") + 
      ylab("Observed CFRD Prevalence") + 
      ggtitle("Observed CFRD Prevalence Rates Across Ages") +
      theme(plot.title=element_text(size=16, face='bold'),
            axis.title=element_text(size=14,face="bold"))
  })
  
}



shinyApp(ui = ui, server = server)