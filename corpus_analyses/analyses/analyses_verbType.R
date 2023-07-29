# --- Analyses on passages of transfer-of-possession verbs (TPVs) 
# --- and implicit causality verbs (ICVs) ---

# TPV analyses ----
# library
library(readr)
library(ggplot2)
library(rstudioapi)
library(scales)

# set the source file direction as working directory
setwd(dirname(getSourceEditorContext()$path))

#### read the dataframe of all responses ####
df_tpv <- read_csv('../extracted_passages/tpv_extracted_passages.csv')  
str(df_tpv)
sum(is.nan(as.matrix(df_tpv))) # 0


#### raw counts of next mention ####
source_in_source_goal <- sum(df_tpv$verb_type == 'source_goal' & df_tpv$coreference_type == 'source') # 98
goal_in_source_goal <- sum(df_tpv$verb_type == 'source_goal' & df_tpv$coreference_type == 'goal') # 27
source_goal <- sum(df_tpv$verb_type == 'source_goal') # 343


goal_in_goal_source <- sum(df_tpv$verb_type == 'goal_source' & df_tpv$coreference_type == 'goal') # 38
source_in_goal_source <- sum(df_tpv$verb_type == 'goal_source' & df_tpv$coreference_type == 'source') # 9
goal_source <- sum(df_tpv$verb_type == 'goal_source') # 167

percentage_goal_in_goal_source = goal_in_goal_source/goal_source
percentage_source_in_source_goal = source_in_source_goal/source_goal
percentage_goal_in_source_goal = goal_in_source_goal/source_goal
percentage_source_in_goal_source = source_in_goal_source/goal_source


#### Figure 11: Percentage of goal and source continuations in two transfer-of-possession contexts. ####

#--- create a dataset ---
grammatical_roles <- c(rep("subject antecedent", 2), rep("object antecedent" , 2) )
thematic_roles <- rep(c("goal antecedent" , "source antecedent" ) , 2)
percentage <- c(percentage_goal_in_goal_source, percentage_source_in_source_goal,
                percentage_goal_in_source_goal, percentage_source_in_goal_source)
data <- data.frame(grammatical_roles,thematic_roles,percentage)

data$grammatical_roles <- factor(data$grammatical_roles, levels=c("subject antecedent", "object antecedent") 
)

#--- plot ---
ggplot(data, aes(fill=thematic_roles, y=percentage, x=grammatical_roles)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_manual(values = c("goal antecedent" = "orange",
                               "source antecedent" = "#999999")) +
  labs(x= "\nGrammatical role", y = "Percentage of coreference samples", 
       fill = "Thematic role") +
  theme(axis.text.x=element_text(size=22),
        axis.text.y=element_text(size=22),
        axis.title=element_text(size=22),
        legend.text=element_text(size=22),
        legend.title=element_text(size=22),
        legend.position="top",
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = margin(l=10, r = 20) 
        ) +
  geom_text(size=6,aes(label=scales::percent(percentage)), position=position_dodge(width=0.9), vjust=-0.16) +
  scale_y_continuous(labels = scales::percent) 



#### chi-square contingency: next mention biases  ####

# --- subject antecedent ---
subject_observed_table <- matrix(c(source_in_source_goal, goal_in_goal_source,
                                   source_goal - source_in_source_goal,
                                   goal_source - goal_in_goal_source), nrow = 2, ncol = 2, byrow = T)
rownames(subject_observed_table) <- c('subject', 'non-subject')
colnames(subject_observed_table) <- c('source-goal', 'goal-source')
subject_observed_table
X <- chisq.test(subject_observed_table)
X
X$expected


# --- object antecedent ---
object_observed_table <- matrix(c(goal_in_source_goal, source_in_goal_source,
                                  source_goal - goal_in_source_goal,
                                  goal_source - source_in_goal_source), nrow = 2, ncol = 2, byrow = T)
rownames(object_observed_table) <- c('object', 'non-object')
colnames(object_observed_table) <- c('source-goal', 'goal-source')
object_observed_table
X <- chisq.test(object_observed_table)
X <-fisher.test(object_observed_table)
X
X$expected

  
#### raw counts of produced pronouns ####

number_pronominal_source_in_source_goal <- sum(df_tpv$verb_type == 'source_goal' & df_tpv$coreference_type == 'source' & df_tpv$pronominalization == 'pronoun') # 69
number_pronominal_goal_in_source_goal <- sum(df_tpv$verb_type == 'source_goal' & df_tpv$coreference_type == 'goal' & df_tpv$pronominalization == 'pronoun') # 16
number_pronominal_source_in_goal_source <- sum(df_tpv$verb_type == 'goal_source' & df_tpv$coreference_type == 'source' & df_tpv$pronominalization == 'pronoun')   # 4
number_pronominal_goal_in_goal_source <- sum(df_tpv$verb_type == 'goal_source' & df_tpv$coreference_type == 'goal' & df_tpv$pronominalization == 'pronoun')  # 31 

percentage_pronominal_goal_in_goal_source = number_pronominal_goal_in_goal_source/goal_in_goal_source
percentage_pronominal_source_in_source_goal = number_pronominal_source_in_source_goal/source_in_source_goal
percentage_pronominal_goal_in_source_goal = number_pronominal_goal_in_source_goal/goal_in_source_goal
percentage_pronominal_source_in_goal_source = number_pronominal_source_in_goal_source/source_in_goal_source


#### Figure 12: Pronominalization rate in each category for transfer-of-possession verbs. ####

#--- create a dataset ---
grammatical_roles <- c(rep("subject antecedent", 2), rep("object antecedent" , 2) )
thematic_roles <- rep(c("goal antecedent" , "source antecedent" ) , 2)
percentage <- c(percentage_pronominal_goal_in_goal_source, percentage_pronominal_source_in_source_goal,
                percentage_pronominal_goal_in_source_goal, percentage_pronominal_source_in_goal_source)
data <- data.frame(grammatical_roles,thematic_roles,percentage)

data$grammatical_roles <- factor(data$grammatical_roles, levels=c("subject antecedent", "object antecedent") 
)


# --- plot ---
ggplot(data, aes(fill=thematic_roles, y=percentage, x=grammatical_roles)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_manual(values = c("goal antecedent" = "#FFDB6D",
                               "source antecedent" = "#C3D7A4")) +
  labs(x= "\nGrammatical role", y = "Pronominalization rate", 
       fill = "Thematic role") +
  theme(axis.text.x=element_text(size=22),
        axis.text.y=element_text(size=22),
        axis.title=element_text(size=22),
        legend.text=element_text(size=22),
        legend.title=element_text(size=22),
        legend.position="top",
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_text(size = 6, aes(label=scales::percent(percentage,.1)),position=position_dodge(width=0.9), vjust=-0.25) +
  scale_y_continuous(labels = scales::percent)





#### chi-square contingency: pronominalization biases ####

# --- subject antecedent ---
subject_observed_table <- matrix(c(number_pronominal_source_in_source_goal, number_pronominal_goal_in_goal_source,
                           source_in_source_goal-number_pronominal_source_in_source_goal,
                           goal_in_goal_source-number_pronominal_goal_in_goal_source), nrow = 2, ncol = 2, byrow = T)
rownames(subject_observed_table) <- c('pronoun', 'non-pronoun')
colnames(subject_observed_table) <- c('source in source-goal', 'goal in goal-source')
subject_observed_table
X <- chisq.test(subject_observed_table)
X
X$expected

# --- object antecedent ---
object_observed_table <- matrix(c(number_pronominal_goal_in_source_goal, number_pronominal_source_in_goal_source,
                                   goal_in_source_goal-number_pronominal_goal_in_source_goal,
                                   source_in_goal_source-number_pronominal_source_in_goal_source), nrow = 2, ncol = 2, byrow = T)
rownames(object_observed_table) <- c('pronoun', 'non-pronoun')
colnames(object_observed_table) <- c('goal in source-goal', 'source in goal-source')
object_observed_table
X <- chisq.test(object_observed_table)
X <-fisher.test(object_observed_table)
X
X$expected



# ICV analyses ----

#### read the dataframe of all responses ####
df_icv <- read_csv('../extracted_passages/icv_extracted_passages.csv')  
str(df_icv)
sum(is.nan(as.matrix(df_icv))) # 0


#### raw counts of next mention ####
subject_in_subject_biased <- sum(df_icv$verb_type == 'subject_biased' & df_icv$coreference_type == 'subject') # 49
object_in_subject_biased <- sum(df_icv$verb_type == 'subject_biased' & df_icv$coreference_type == 'object') # 24
subject_biased <- sum(df_icv$verb_type == 'subject_biased') #148

subject_in_object_biased <- sum(df_icv$verb_type == 'object_biased' & df_icv$coreference_type == 'subject') # 104
object_in_object_biased <- sum(df_icv$verb_type == 'object_biased' & df_icv$coreference_type == 'object') # 41
object_biased <- sum(df_icv$verb_type == 'object_biased') #339

percentage_subject_in_subject_biased = subject_in_subject_biased/subject_biased
percentage_object_in_subject_biased = object_in_subject_biased/subject_biased
percentage_subject_in_object_biased = subject_in_object_biased/object_biased
percentage_object_in_object_biased = object_in_object_biased/object_biased


#### Figure 13: Percentage of continuations in subject-biased and object-biased implicit causality verb contexts. ####

# --- create a dataset ---
icv_contexts <- c(rep("subject biased", 2), rep("object biased" , 2) )
grammatical_roles <- rep(c("subject" , "object" ) , 2)
percentage <- c(percentage_subject_in_subject_biased, percentage_object_in_subject_biased,
                percentage_subject_in_object_biased,percentage_object_in_object_biased)
data <- data.frame(grammatical_roles,icv_contexts,percentage)

data$grammatical_roles <- factor(data$grammatical_roles, levels=c("subject", "object") 
)

data$icv_contexts <- factor(data$icv_contexts, levels=c("subject biased", "object biased") 
)

# --- plot ---
ggplot(data, aes(fill=grammatical_roles, y=percentage, x=icv_contexts)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_manual(values = c("subject" = "orange",
                               "object" = "#999999")) +
  labs(x= "\nICV context", y = "Percentage of coreference samples", 
       fill = "Grammatical role") +
  theme(axis.text.x=element_text(size=22),
        axis.text.y=element_text(size=22),
        axis.title=element_text(size=22),
        legend.text=element_text(size=22),
        legend.title=element_text(size=22),
        legend.position="top"
  ) +
  geom_text(size=6,aes(label=scales::percent(percentage)), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )


#### chi-square contingency: next-mention biases ####

#subject antecedent
subject_observed_table <- matrix(c(subject_in_subject_biased, subject_in_object_biased,
                                   subject_biased - subject_in_subject_biased,
                                   object_biased - subject_in_object_biased
                                    ), nrow = 2, ncol = 2, byrow = T)
rownames(subject_observed_table) <- c('subject', 'non-subject')
colnames(subject_observed_table) <- c('subject-biased', 'object-biased')
subject_observed_table
X <- chisq.test(subject_observed_table)
X
X$expected


#object antecedent
object_observed_table <- matrix(c(object_in_subject_biased, object_in_object_biased,
                                  subject_biased - object_in_subject_biased,
                                  object_biased - object_in_object_biased), nrow = 2, ncol = 2, byrow = T)
rownames(object_observed_table) <- c('object', 'non-object')
colnames(object_observed_table) <- c('subject-biased', 'object-biased')
object_observed_table
X <- chisq.test(object_observed_table)
X
X$expected


#### raw counts of produced pronouns ####
number_pronominal_subject_in_subject_biased <- sum(df_icv$verb_type == 'subject_biased' & df_icv$coreference_type == 'subject' & df_icv$pronominalization == 'pronoun')  # 44  
number_pronominal_object_in_subject_biased <- sum(df_icv$verb_type == 'subject_biased' & df_icv$coreference_type == 'object' & df_icv$pronominalization == 'pronoun') # 18
number_pronominal_subject_in_object_biased <- sum(df_icv$verb_type == 'object_biased' & df_icv$coreference_type == 'subject' & df_icv$pronominalization == 'pronoun') # 93  
number_pronominal_object_in_object_biased <- sum(df_icv$verb_type == 'object_biased' & df_icv$coreference_type == 'object' & df_icv$pronominalization == 'pronoun') # 28

percentage_pronominal_subject_in_subject_biased = number_pronominal_subject_in_subject_biased/subject_in_subject_biased
percentage_pronominal_object_in_subject_biased = number_pronominal_object_in_subject_biased/object_in_subject_biased
percentage_pronominal_subject_in_object_biased = number_pronominal_subject_in_object_biased/subject_in_object_biased
percentage_pronominal_object_in_object_biased = number_pronominal_object_in_object_biased/object_in_object_biased


#### Figure 14: Pronominalization rate in each category for subject-biased and object-biased implicit causality verb contexts.####
# --- create a dataset ---
grammatical_roles <- c(rep("subject antecedent", 2), rep("object antecedent" , 2) )
expected_biased_roles <- rep(c("implicit cause" , "non implicit cause" ) , 2)
percentage <- c(percentage_pronominal_subject_in_subject_biased, percentage_pronominal_subject_in_object_biased,
                percentage_pronominal_object_in_object_biased, percentage_pronominal_object_in_subject_biased)
data <- data.frame(grammatical_roles,expected_biased_roles,percentage)

data$grammatical_roles <- factor(data$grammatical_roles, levels=c("subject antecedent", "object antecedent") 
)


# --- plot ---
ggplot(data, aes(fill=expected_biased_roles, y=percentage, x=grammatical_roles)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_manual(values = c("implicit cause" = "#FFDB6D",
                               "non implicit cause" = "#C3D7A4")) +
  labs(x= "\nGrammatical role", y = "Pronominalization rate", 
       fill = "") +
  theme(axis.text.x=element_text(size=22),
        axis.text.y=element_text(size=22),
        axis.title=element_text(size=22),
        legend.text=element_text(size=22),
        legend.title=element_text(size=22),
        legend.position="top"
  ) +
  geom_text(size= 6, aes(label=percent(percentage,.1)), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )




#### chi-square contingency: pronominalization biases ####

#subject antecedent
subject_observed_table <- matrix(c(number_pronominal_subject_in_subject_biased, number_pronominal_subject_in_object_biased,
                                   subject_in_subject_biased-number_pronominal_subject_in_subject_biased,
                                   subject_in_object_biased-number_pronominal_subject_in_object_biased), nrow = 2, ncol = 2, byrow = T)
rownames(subject_observed_table) <- c('pronoun', 'non-pronoun')
colnames(subject_observed_table) <- c('subject in subject-biased ICV', 'subject in object_object_biased')
subject_observed_table
X <- chisq.test(subject_observed_table)
X
X$expected

#object antecedent
object_observed_table <- matrix(c(number_pronominal_object_in_subject_biased, number_pronominal_object_in_object_biased,
                                  object_in_subject_biased-number_pronominal_object_in_subject_biased,
                                  object_in_object_biased-number_pronominal_object_in_object_biased), nrow = 2, ncol = 2, byrow = T)
rownames(object_observed_table) <- c('pronoun', 'non-pronoun')
colnames(object_observed_table) <- c('object in subject-biased ICV', 'object in object_object_biased')
object_observed_table
X <- chisq.test(object_observed_table)
X
X$expected
# X-squared = 0.08482, df = 1, p-value = 0.7709



  