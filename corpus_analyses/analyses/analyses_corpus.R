######################################################################
#This script contains codes for the statistical analyses and plots in
#Section 4: Observational examination of Strong Bayes: corpus analyses 
######################################################################

# library 
library(rstudioapi)
library(ggplot2)
library(ggh4x)
library(viridis)
library(ggthemes)
library(scales)
library(dplyr)
library(reshape2)
library(gridExtra) 
library(lme4)
library(emmeans)

# set the source file direction as working directory
setwd(dirname(getSourceEditorContext()$path))


# add names to different levels of each covariate

named.contr.sum<-function(x, ...) {
  if (is.factor(x)) {
    x <- levels(x)
  } else if (is.numeric(x) & length(x)==1L) {
    stop("cannot create names with integer value. Pass factor levels")
  }
  x<-contr.sum(x, ...)
  colnames(x) <- apply(x,2,function(x) 
    names(x[x>0])
  )
  x
}

#### Analysis of the extracted samples from OntoNotes ####

##### read dataframe #####
df <- read.csv(file = '../extracted_passages/ontonotes_extracted_passages.csv', header = TRUE)
df_narration <- subset(df, df$coherence_relation == 'narration') #985
df_result <- subset(df, df$coherence_relation == 'result') #1330
df_contrast <- subset(df, df$coherence_relation == 'contrast')

# --- next mention frequency/coreference type ---
# counts: subject coref, non-subject coref 
subject_narration  <- sum(df_narration$coreference_type == 'subject')
non_subject_narration <- sum(df_narration$coreference_type == 'non_subject') 
total_narration <- nrow(df_narration)
other_narration <- sum(df_narration$coreference_type == 'other') 

subject_result  <- sum(df_result$coreference_type == 'subject')
non_subject_result <- sum(df_result$coreference_type == 'non_subject')
total_result <- nrow(df_result)
other_result <- sum(df_result$coreference_type == 'other') 

subject_contrast  <- sum(df_contrast$coreference_type == 'subject') 
non_subject_contrast <- sum(df_contrast$coreference_type == 'non_subject')  
total_contrast = nrow(df_contrast)
other_contrast <- sum(df_contrast$coreference_type == 'other')   


# --- pronoun production ---
pronominal_next_mention_subjCoref_narration <- sum(df_narration$coreference_type == 'subject' & df_narration$pronominalization == 'pronoun')
pronominal_next_mention_nonsubjCoref_narration <- sum(df_narration$coreference_type == 'non_subject' & df_narration$pronominalization == 'pronoun')

pronominal_next_mention_subjCoref_result <- sum(df_result$coreference_type == 'subject' & df_result$pronominalization == 'pronoun')
pronominal_next_mention_nonsubjCoref_result <- sum(df_result$coreference_type == 'non_subject' & df_result$pronominalization == 'pronoun')

pronominal_next_mention_subjCoref_contrast <- sum(df_contrast$coreference_type == 'subject' & df_contrast$pronominalization == 'pronoun')
pronominal_next_mention_nonsubjCoref_contrast <- sum(df_contrast$coreference_type == 'non_subject' & df_contrast$pronominalization == 'pronoun')


##### Figure 2a: Coreference type by discourse relation in OntoNotes #####
# --- create a dataset ---
relation <- c(rep("Narration(N=985)" , 3) , rep("Result(N=1330)" , 3) , 
              rep("Contrast(N=2572)" , 3)  )
cor_type <- rep(c("Other" , "Non-subject" , "Subject") , 3)
percentage <- c(other_narration,non_subject_narration, subject_narration,
                other_result,non_subject_result, subject_result,
                other_contrast,non_subject_contrast, subject_contrast)
data <- data.frame(relation,cor_type,percentage)

data$cor_type <- factor(data$cor_type, 
                        levels=c("Other" , "Non-subject" , "Subject")) 
data$relation <- factor(data$relation, 
                        levels=c("Narration(N=985)" , "Result(N=1330)" , "Contrast(N=2572)")) 

# ---  Stacked + percent ---
data <- data %>%
  group_by(relation) %>%
  mutate(count= sum(percentage)) %>%
  group_by(cor_type, add=TRUE) %>%
  mutate(per=paste0(round(100*percentage/count,1),'%'))

data_subject <- data[cor_type == "Subject",]

# ---  plot ---
ggplot(data, aes(fill=cor_type, y=percentage, x=relation)) + 
  geom_bar(position="fill", stat="identity",width=0.4) +
  scale_fill_manual(values = c("Other" = "skyblue4",
                               "Non-subject" = "grey78",
                               "Subject" = "slategray3")) +
  labs(x= "", y = "Percentage of coreference types", 
       fill = "Coreference type") +
  theme(axis.text.x=element_text(size=25),
        axis.text.y=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        legend.position="top"
  ) +
  geom_text(size = 10,aes(label=paste0(per)),
            position=position_fill(vjust=0.5), colour="white") +
  scale_y_continuous(labels = scales::percent) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )


##### Figure 3a: Pronominalization rate of next mention by relation in OntoNotes #####

# --- dataframe for pronominalization rate ---
relation <- c("Narration" , "Result" , "Contrast")

percentage <- c(pronominal_next_mention_subjCoref_narration/subject_narration,
                pronominal_next_mention_subjCoref_result/subject_result,
                pronominal_next_mention_subjCoref_contrast/subject_contrast,
                pronominal_next_mention_nonsubjCoref_narration/non_subject_narration,
                pronominal_next_mention_nonsubjCoref_result/non_subject_result,
                pronominal_next_mention_nonsubjCoref_contrast/non_subject_contrast)

count <- c(pronominal_next_mention_subjCoref_narration,
           pronominal_next_mention_subjCoref_result,
           pronominal_next_mention_subjCoref_contrast,
           pronominal_next_mention_nonsubjCoref_narration,
           pronominal_next_mention_nonsubjCoref_result,
           pronominal_next_mention_nonsubjCoref_contrast)

coreference_type <- c(rep('subject', 3), rep('non subject', 3))

data_pro <- data.frame(relation, coreference_type, count,percentage)

data_pro$relation <- factor(data_pro$relation, 
                            levels=c("Narration" , "Result" , "Contrast")) 

# --- plot ---
ggplot(data_pro, aes(x = coreference_type, y = percentage, fill = relation)) +
  geom_bar(width = 0.95, position = position_dodge(0.95), stat = "identity") +
  geom_text(size = 10, 
            aes(
              label = paste0(
                format(count, big.mark = ","), "\n",
                "(", scales::percent(percentage, accuracy = 0.1L), ")"
              )
            ),
            position = position_dodge(width = 0.95), vjust = 0.5) +
  labs(x = "", y = "Pronominalization rate", fill = "Discourse relation") +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    plot.margin = margin(t = 10, r = 10,  l = 10),
    legend.position = "top"
  ) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_manual(values=c("#F0B6DA", "#C5E4E7", "#FFD700"))+
  #scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  guides(x = "axis_nested", label.position = "top") +
  scale_x_discrete(limits = c("subject", "non subject"))


##### Table 7: logistic regression on OntoNotes next-mention frequency ###### 

# View the dataset
head(df)

df  = df  %>%
  mutate(
    verb = as.factor(verb),
    subject_or_not = as.factor(subject_or_not),
    relation = as.factor(coherence_relation), 
    pronominalization = as.factor(pronominalization),
    coreference_type = as.factor(coreference_type),
    X12person = as.factor(X12person),
    proAntecedent = as.factor(proAntecedent),
    antecedentType = as.factor(subj_antecedent_type)
  )
str(df)

### sum coding ###
# contrasts(df$relation)=named.contr.sum(levels(df$relation))

# Build the mixed-effects logistic regression model

model_verb <- glmer(subject_or_not ~ relation  + (1 + relation | verb) , 
                    data = df, family = binomial,
                    control = glmerControl(optimizer = "bobyqa") )
summary(model_verb)

# --- Conduct post-hoc comparisons ---
emmeans_model <- emmeans(model_verb, ~ relation)

# Obtain pairwise comparisons
pairwise_comparisons <- pairs(emmeans_model)

# Summarize the results
summary(pairwise_comparisons)



##### Table 9: logistic regression on OntoNotes pronominalization rates ###### 

# subsets of the original dataframe
df_subj <- subset(df, df$coreference_type == 'subject')
df_nonSubj <- subset(df, df$coreference_type == 'non_subject')
df_noOther <- subset(df, df$coreference_type %in% c('subject', 'non_subject'))

# drop missing levels from factorized variables
df_noOther$coreference_type <- droplevels(df_noOther$coreference_type)
df_subj <- df_subj %>%
  mutate(
    X12person = droplevels(X12person),
    proAntecedent = droplevels(proAntecedent),
    antecedentType = droplevels(antecedentType)
  )

# Relevel the coreference_type variable
# df_noOther$coreference_type <- relevel(df_noOther$coreference_type, ref = "subject")
#df_subj$X12person <- relevel(df_subj$X12person, ref = "True")
#df_subj$proAntecedent <- relevel(df_subj$proAntecedent, ref = "pronoun")
df_subj$antecedentType <- relevel(df_subj$antecedentType, ref="non_pronoun")


### sum coding ###
#contrasts(df_noOther$relation)=named.contr.sum(levels(df_noOther$relation))
#contrasts(df_noOther$coreference_type)=named.contr.sum(levels(df_noOther$coreference_type))
#contrasts(df_subj$X12person)=named.contr.sum(levels(df_subj$X12person))
#contrasts(df_subj$proAntecedent)=named.contr.sum(levels(df_subj$proAntecedent))
#contrasts(df_subj$antecedentType)=named.contr.sum(levels(df_subj$antecedentType))

model_pro <- glmer(pronominalization ~ relation*coreference_type +  (1 | document_id), 
                   data = df_noOther , family = binomial)
summary(model_pro)
 

# Conduct post-hoc comparisons
emmeans_model <- emmeans(model_pro, ~ relation|coreference_type)

# Obtain pairwise comparisons
pairwise_comparisons <- pairs(emmeans_model)

# Summarize the results
summary(pairwise_comparisons)


##### Appendix B.2: Robustness test of pronoun production analysis #####
model_proSubj <- glmer(pronominalization ~ relation + antecedentType + (1 | document_id), 
                   data = df_subj , family = binomial)
#--- Table 21: Pronominalization of subject re-mentions in OntoNotes ---
summary(model_proSubj)
#                                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          1.5465     0.1796   8.610  < 2e-16 ***
# relationnarration                   -0.1228     0.2053  -0.598    0.550    
# relationresult                      -0.1501     0.2005  -0.749    0.454    
# antecedentTypefirst_second_pronoun   1.3107     0.2182   6.007 1.89e-09 ***
# antecedentTypeother_pronoun          0.3856     0.1896   2.033    0.042 * 

##### Figure 4 pie chart Narration distribution (by genre) in OntoNotes #####

  # library
library(ggrepel)
library(tidyverse)

#####Figure 4a: distribution of Narration coreference samples by genre in OntoNotes  #####
# Create a new column "genre" in the dataset
df$genre <- sub("/.*", "", df$document_id)

narration_total <- sum(df$relation == 'narration')
# Discription of genres in OntoNotes
  # bc: broadcast conversation
  # bn: broadcast news
  # mz: magazine genre (Sinorama magazine)
  # nw: newswire genre
  # pt: pivot text (250K English translation of the New Testament annotated with parse, proposition, name and coreference; and about 100K parses for a portion of the Old Testament)
  # tc: telephone conversation (CallHome corpus)
  # wb: web data (85K of single sentences selected to improve sense coverage)
bc <- sum(df$relation == 'narration' & df$genre == 'bc' ) # 56
mz <- sum(df$relation == 'narration' & df$genre == 'mz' ) # 30
nw <- sum(df$relation == 'narration' & df$genre == 'nw' ) # 40

bn <- sum(df$relation == 'narration' & df$genre == 'bn' ) # 30
pt <- sum(df$relation == 'narration' & df$genre == 'pt' ) # 688
wb <- sum(df$relation == 'narration' & df$genre == 'wb' ) # 37
tc <- sum(df$relation == 'narration' & df$genre == 'tc' ) # 104



# merge magazine and newswire
data <- data.frame(
  Genre=c('Broadcast news', 'Web data', 'Broadcast conversation', 
          'Newswire', 'Telephone conversation',
          'New Testament and Old Testament'),
  value=c(bn, wb, bc, mz+nw, tc, pt)
)



data$Genre <- factor(data$Genre,
                     levels=c('Broadcast news', 'Web data', 'Broadcast conversation',
                              'Newswire', 'Telephone conversation',
                              'New Testament and Old Testament'))


# Get the positions
data2 <- data %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))


ggplot(data, aes(x = "" , y = value, fill = Genre)) +
  geom_bar(stat="identity", width=1)  +
  coord_polar(theta = "y") +
#  scale_fill_brewer(palette = "Pastel1") +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() +
  scale_fill_brewer() +
  geom_label_repel(data = subset(data2, value > 600),
            aes(y = pos, label = scales::percent(value / sum(data$value), accuracy = 0.1)),
            size = 8,  show.legend = FALSE, colour = 'white') +
  guides(fill = guide_legend(byrow = TRUE, override.aes = aes(label = ""))) +
  theme(legend.title = element_text (size =25))
        #,
        #legend.text=element_text(size=25),
        #legend.spacing.y = unit(0.3, 'cm')) 
  
##### Figure 4b: genre distribution in OntoNotes #####
token_total = 1745
data <- data.frame(
  Genre=c('Broadcast news', 'Web data', 'Newswire',
          'Broadcast conversation', 'Telephone conversation',
          'New Testament and Old Testament'),
  value=c(200, 300, 625, 200, 120, 300)
)

data$Genre <- factor(data$Genre,
                     levels=c('Broadcast news', 'Web data', 'Newswire',
                              'Broadcast conversation', 'Telephone conversation',
                              'New Testament and Old Testament'))

# Get the positions
data2 <- data %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))


ggplot(data, aes(x = "" , y = value, fill = Genre)) +
  geom_bar(stat="identity", width=1)  +
  coord_polar(theta = "y") +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() +
  scale_fill_brewer() +
#  scale_fill_brewer(palette = "Reds") +
  geom_label_repel(data = subset(data2, Genre == 'New Testament and Old Testament' ),
                   aes(y = pos, label = scales::percent(value / sum(data$value), accuracy = 0.1)),
                   size = 8,  show.legend = FALSE, colour = 'white') +
  guides(fill = guide_legend(byrow = TRUE, override.aes = aes(label = ""))) +
  theme(legend.title = element_text (size =25))
        #,
        #legend.text=element_text(size=25),
        #legend.spacing.y = unit(0.3, 'cm')) 


##### two pie charts side by side #####

df <- data.frame(chart = c('(a)','(a)','(a)',
                           '(a)','(a)','(a)',
                           '(b)','(b)','(b)',
                           '(b)','(b)','(b)'),
                Genre = c('Broadcast news', 'Web data', 'Broadcast conversation', 'Newswire', 'Telephone conversation','New Testament and Old Testament',
                           'Broadcast news', 'Web data', 'Broadcast conversation', 'Newswire', 'Telephone conversation','New Testament and Old Testament'),
                value = c(30,37,56,70,104,688,200,300,200,625,120,300))


df$chart <- factor(df$chart,
                   levels = c ('(a)','(b)'))
df$Genre <- factor(df$Genre,
                     levels=c('Broadcast news', 'Web data', 'Broadcast conversation', 'Newswire', 'Telephone conversation','New Testament and Old Testament'))

df <- df%>%group_by(chart)%>%mutate(Percentage=paste0(round(value/sum(value)*100,1)))
df$Percentage <- as.numeric(df$Percentage)
df <- df%>%group_by(chart)%>%  
  arrange(-Percentage)%>% 
  mutate(Percentage_cumsum=cumsum(Percentage))


ggplot(data=df, aes(x=" ", y=Percentage, group = Genre, fill=Genre,label = paste(Percentage,'%'))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start=0) + 
  facet_grid(.~ chart) +
  theme_void() +
  scale_fill_brewer() + 
  geom_text(
    data = subset(df, Genre == 'New Testament and Old Testament' ),
    position = position_stack(vjust = 0.6),
    colour = 'White',
    size = 8.7) +
  theme(strip.text = element_text(size = 25),
    legend.title = element_text (size =25),
        legend.text=element_text(size=25),
        legend.spacing = unit(0.3, 'cm'),
        #strip.text.x = element_text(size = 19),
        panel.spacing = unit(1, "lines"))


#### Analysis of the extracted samples from RST-DT ####

##### read dataframe #####
df2 <- read.csv(file = '../extracted_passages/rstdt_extracted_passages.csv', header = TRUE)
df2_narration <- subset(df2, df2$coherence_relation == 'narration')
df2_result <- subset(df2, df2$coherence_relation == 'result')
df2_contrast <- subset(df2, df2$coherence_relation == 'contrast')

# --- next mention frequency/coreference type ---
### counts: subject coref, non-subject coref 
subject_narration  <- sum(df2_narration$coreference_type == 'subject')
non_subject_narration <- sum(df2_narration$coreference_type == 'non_subject') 
total_narration <- nrow(df2_narration)
other_narration <- sum(df2_narration$coreference_type == 'other') 

subject_result  <- sum(df2_result$coreference_type == 'subject')
non_subject_result <- sum(df2_result$coreference_type == 'non_subject')
total_result <- nrow(df2_result)
other_result <- sum(df2_result$coreference_type == 'other') 

subject_contrast  <- sum(df2_contrast$coreference_type == 'subject') 
non_subject_contrast <- sum(df2_contrast$coreference_type == 'non_subject')  
total_contrast = nrow(df2_contrast)
other_contrast <- sum(df2_contrast$coreference_type == 'other')   

# --- pronoun production ---
pronominal_next_mention_subjCoref_narration <- sum(df2_narration$coreference_type == 'subject' & df2_narration$pronominalization == 'pronoun')
pronominal_next_mention_nonsubjCoref_narration <- sum(df2_narration$coreference_type == 'non_subject' & df2_narration$pronominalization == 'pronoun')

pronominal_next_mention_subjCoref_result <- sum(df2_result$coreference_type == 'subject' & df2_result$pronominalization == 'pronoun')
pronominal_next_mention_nonsubjCoref_result <- sum(df2_result$coreference_type == 'non_subject' & df2_result$pronominalization == 'pronoun')

pronominal_next_mention_subjCoref_contrast <- sum(df2_contrast$coreference_type == 'subject' & df2_contrast$pronominalization == 'pronoun')
pronominal_next_mention_nonsubjCoref_contrast <- sum(df2_contrast$coreference_type == 'non_subject' & df2_contrast$pronominalization == 'pronoun')

##### Figure 2b: Coreference type by discourse relation in RST-DT #####
# --- create a dataset ----
relation <- c(rep("Narration(N=71)" , 3) , rep("Result(N=148)" , 3) , 
              rep("Contrast(N=349)" , 3)  )
cor_type <- rep(c("Other" , "Non-subject" , "Subject") , 3)
percentage <- c(other_narration,non_subject_narration, subject_narration,
                other_result,non_subject_result, subject_result,
                other_contrast,non_subject_contrast, subject_contrast)
data <- data.frame(relation,cor_type,percentage)

data$cor_type <- factor(data$cor_type, 
                        levels=c("Other" , "Non-subject" , "Subject")) 
data$relation <- factor(data$relation, 
                        levels=c("Narration(N=71)" , "Result(N=148)" , "Contrast(N=349)")) 

# --- Stacked + percent ---
data <- data %>%
  group_by(relation) %>%
  mutate(count= sum(percentage)) %>%
  group_by(cor_type, add=TRUE) %>%
  mutate(per=paste0(round(100*percentage/count,1),'%'))

data_subject <- data[cor_type == "Subject",]

data$relation_b  <- c(rep("Narration(N=71)" , 3) , rep("Result(N=148)" , 3) , 
                      rep("Contrast(N=349)" , 3)  )
data$relation_b <- factor(data$relation_b, 
                          levels=c("Narration(N=71)" , "Result(N=148)" , "Contrast(N=349)")) 

# ---  plot ---
ggplot(data, aes(fill=cor_type, y=percentage, x=relation_b)) + 
  geom_bar(position="fill", stat="identity",width=0.4) +
  scale_fill_manual(values = c("Other" = "skyblue4",
                               "Non-subject" = "grey78",
                               "Subject" = "slategray3")) +
  labs(x= "", y = "Percentage of coreference types", 
       fill = "Coreference type") +
  theme(axis.text.x=element_text(size=25),
        axis.text.y=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text=element_text(size=25),
        legend.title=element_text(size=25),
        legend.position="top"
  ) +
  geom_text(size = 10,aes(label=paste0(per)),
            position=position_fill(vjust=0.5), colour="white") +
  scale_y_continuous(labels = scales::percent) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )


##### Figure 3b: Pronominalization rate of next mention by relation in RST-DT #####
# --- dataframe for pronominalization rate ---
relation <- c("Narration" , "Result" , "Contrast")

percentage <- c(pronominal_next_mention_subjCoref_narration/subject_narration,
                pronominal_next_mention_subjCoref_result/subject_result,
                pronominal_next_mention_subjCoref_contrast/subject_contrast,
                pronominal_next_mention_nonsubjCoref_narration/non_subject_narration,
                pronominal_next_mention_nonsubjCoref_result/non_subject_result,
                pronominal_next_mention_nonsubjCoref_contrast/non_subject_contrast)
percentage

count <- c(pronominal_next_mention_subjCoref_narration,
           pronominal_next_mention_subjCoref_result,
           pronominal_next_mention_subjCoref_contrast,
           pronominal_next_mention_nonsubjCoref_narration,
           pronominal_next_mention_nonsubjCoref_result,
           pronominal_next_mention_nonsubjCoref_contrast)

coreference_type <- c(rep('subject', 3), rep('non subject', 3))

data_pro <- data.frame(relation, coreference_type, count,percentage)

data_pro$relation <- factor(data_pro$relation, 
                            levels=c("Narration" , "Result" , "Contrast")) 

# --- plot ---
ggplot(data_pro, aes(x = coreference_type, y = percentage, fill = relation)) +
  geom_bar(width = 0.95, position = position_dodge(0.95), stat = "identity") +
  geom_text(size = 10, 
            aes(
              label = paste0(
                format(count, big.mark = ","), "\n",
                "(", scales::percent(percentage, accuracy = 0.1L), ")"
              )
            ),
            position = position_dodge(width = 0.95), vjust = 0.5) +
  labs(x = "", y = "Pronominalization rate", fill = "Discourse relation") +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    plot.margin = margin(t = 10, r = 10,  l = 10),
    legend.position = "top" 
  ) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  #scale_fill_viridis(discrete = TRUE) +
  scale_fill_manual(values=c("#F0B6DA", "#C5E4E7", "#FFD700")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  guides(x = "axis_nested", label.position = "top") +
  scale_x_discrete(limits = c("subject", "non subject"))


##### Table 8: logistic regression on RST-DT next-mention frequency ###### 

df2  = df2  %>%
  mutate(
    verb = as.factor(verb),
    subject_or_not = as.factor(subject_or_not),
    relation = as.factor(coherence_relation), 
    pronominalization = as.factor(pronominalization),
    coreference_type = as.factor(coreference_type),
    X12person = as.factor(X12person),
    proAntecedent = as.factor(proAntecedent)
  )
str(df2)

### sum coding ###
# contrasts(df2$relation)=named.contr.sum(levels(df2$relation))


# Build the mixed-effects logistic regression model
# too few data for the random structure
model <- glmer(subject_or_not ~ relation  + (1 + relation | verb) , 
               data = df2, family = binomial,
               control = glmerControl(optimizer = "bobyqa") )

# final model
model_verb <- glmer(subject_or_not ~ relation  + (1 | verb) , 
                    data = df2, family = binomial,
                    control = glmerControl(optimizer = "bobyqa") )
summary(model_verb)

# --- Conduct post-hoc comparisons ---
emmeans_model <- emmeans(model_verb, ~ relation)

# Obtain pairwise comparisons
pairwise_comparisons <- pairs(emmeans_model)

# Summarize the results
summary(pairwise_comparisons)



##### Table 10: logistic regression on RST-DT pronominalization rates ###### 

# subsets of the original dataframe
df2_subj <- subset(df2, df2$coreference_type == 'subject')
df2_nonSubj <- subset(df2, df2$coreference_type == 'non_subject')
df2_noOther <- subset(df2, df2$coreference_type %in% c('subject', 'non_subject'))

# drop missing levels from the factor variable
df2_noOther$coreference_type <- droplevels(df2_noOther$coreference_type)
df2_subj$X12person <- droplevels(df2_subj$X12person)
df2_subj$proAntecedent <- droplevels(df2_subj$proAntecedent)

# Relevel the coreference_type variable
#df2_noOther$coreference_type <- relevel(df2_noOther$coreference_type, ref = "non_subject")
#df2_subj$X12person <- relevel(df2_subj$X12person, ref = "FALSE")
#df2_subj$proAntecedent <- relevel(df2_subj$proAntecedent, ref = "TRUE")

# sum coding #
#contrasts(df2_noOther$relation)=named.contr.sum(levels(df2_noOther$relation))
#contrasts(df2_noOther$coreference_type)=named.contr.sum(levels(df2_noOther$coreference_type))
#contrasts(df2_subj$X12person)=named.contr.sum(levels(df2_subj$X12person))
#contrasts(df2_subj$proAntecedent)=named.contr.sum(levels(df2_subj$proAntecedent))


model_pro_rstdt <- glmer(pronominalization ~ relation*coreference_type +  (1 | document_id), 
                         data = df2_noOther , family = binomial)
summary(model_pro_rstdt)


# --- Conduct post-hoc comparisons ---
emmeans_model <- emmeans(model_pro_rstdt, ~ relation|coreference_type)

# Obtain pairwise comparisons
pairwise_comparisons <- pairs(emmeans_model)

# Summarize the results
summary(pairwise_comparisons)

##### limited number of samples with pronominal antecedents in RST-DT #####
# See Appendix B.2 Robustness test of pronoun production analysis
sum(df2$proAntecedent == 'TRUE') # 25
sum(df2$X12person == 'TRUE') # 5
sum(df2$proAntecedent == 'TRUE' & df2$relation == 'narration') # 5
sum(df2$proAntecedent == 'TRUE' & df2$relation == 'contrast') # 14
sum(df2$proAntecedent == 'TRUE' & df2$relation == 'result') # 6









