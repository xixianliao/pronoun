#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This script contains codes for the automatic annotation of referential form;
codes for estimating accuracy of this annotation;
codes for estimating accuracy of participants' own next-mention annotations;
codes for generating a dataframe of all the model predictions for model comparisons;
"""

import pandas as pd
import re
import numpy as np
import collections
from nltk import word_tokenize, pos_tag_sents 
import collections
import spacy
from scipy.stats import chi2_contingency
from scipy import stats
from sklearn.metrics import cohen_kappa_score



# automatically annotate pronominalization: pro/non-pro                       
def annotate_pronominalization (df):       
    df['pronominalization'] = df['response'].str.startswith(('He ','She ','he ','she '))
    df.loc[df["prompt"] == 'pronoun', "pronominalization"] = True
    df['pronominalization'] = df['pronominalization'].map({True: 'pro', False: 'non_pro'}) 
        
    responses = df['response'].tolist()
    stimuli = df['text'].tolist()
    full_passage = [m+' '+n for m,n in zip(stimuli,responses)]

    tokenized_responses = [word_tokenize(response) for response in responses]
    length_responses = [len(response) for response in tokenized_responses]    
    tagged_texts = pos_tag_sents(map(word_tokenize, full_passage))         
    first_tagged_token = [m[-n][1] for m,n in zip(tagged_texts, length_responses)] 
    
    nlp = spacy.load("en_core_web_sm")   
#    processed_texts = [nlp(p) for p in full_passage]     
#    first_tagged_token = [m[-n].pos_ for m,n in zip(processed_texts, length_responses)] 
    
    df['POS'] = first_tagged_token  
    df.loc[(df["prompt"] == 'bare') & (df['POS'].str.startswith('PRP')), 'pronominalization'] = 'pro'
    df.loc[(df["prompt"] == 'bare') & (df['POS'].str.startswith('PRP$')), 'pronominalization'] = 'possessive_pro'


    subj_pronounOrNot = []
    
        
    def flatten_tree(tree):
        return ''.join([token.text_with_ws for token in list(tree)]).strip()  

    for i,p in enumerate(full_passage):  
        subj = []
        doc = nlp(p)
        sent = list(doc.sents)[1]
        for word in sent:
            if word.dep_ in ('nsubj', 'nsubjpass'):
                subj.append(flatten_tree(word.subtree))
                
        if len(subj) > 0 and len(subj[0].split()) == 1 and nlp(subj[0])[0].pos_ == 'PRON':
            subj_pronounOrNot.append(True)
        elif nlp(responses[i])[0].pos_ == 'PART':
            subj_pronounOrNot.append('PART')
        elif len(subj) > 0 and nlp(subj[0])[0].pos_ == 'PROPN':
            subj_pronounOrNot.append('name')                      
        elif len(subj) == 0:
            subj_pronounOrNot.append('noSubj')
        else:
            subj_pronounOrNot.append(False)
    
    df['subj_proOrNot'] = subj_pronounOrNot
    
    df.loc[(df["prompt"] == 'bare') & (df['subj_proOrNot']== True), 'pronominalization'] = 'pro'
    df.loc[(df["prompt"] == 'bare') & (df['POS'].str.startswith('V')), 'pronominalization'] = 'zero'
    df.loc[(df["prompt"] == 'bare') & (df['subj_proOrNot']== 'noSubj'), 'pronominalization'] = 'zero'
    # could, should, might
    df.loc[(df["prompt"] == 'bare') & (df['POS'].str.startswith('MD')), 'pronominalization'] = 'zero'   
    df.loc[(df["prompt"] == 'bare') & (df['subj_proOrNot']== 'name'), 'pronominalization'] = 'non_pro'
    # not too closely
    df.loc[(df["prompt"] == 'bare') & (df['subj_proOrNot']== 'PART'), 'pronominalization'] = 'zero'
    df.loc[(df["prompt"] == 'bare') & (df['response'].str.lower().str.startswith(('her daughter', ' her daughter'))), 'pronominalization'] = 'non_pro'    
    
        



# check inter-annotator agreement of the manual examination on participants' own next-mention annotation
def inter_annotator_agreement (df_manual_examination_file):
    
    # Extract the judgments from the two judgers
    annotations1 = df_manual_examination_file['Annotator1'].tolist()
    annotations2 = df_manual_examination_file['Annotator2'].tolist()
    
    # Use a list comprehension and the .replace() method to remove spaces after letters
    annotations1 = [item.replace(' ', '') for item in annotations1]
    annotations2 = [item.replace(' ', '') for item in annotations2]
    
    # Calculate Cohen's kappa
    kappa = cohen_kappa_score(annotations1, annotations2,weights=None)
    
    # Calculate the number of observations
    n = len(annotations1)
    
    # Calculate the observed agreement
    p_o = np.sum(np.array(annotations1) == np.array(annotations2)) / n
    
    # Calculate the probability of random agreement
    p_e = np.sum([(np.sum(np.array(annotations1) == i) / n) * (np.sum(np.array(annotations2) == i) / n) for i in set(annotations1)])
    
    # Calculate the z-score
    z_score = (p_o - p_e) / np.sqrt((p_o * (1 - p_o) + p_e * (1 - p_e)) / n)
    
    # Calculate the p-value
    p_value = 2 * (1 - stats.norm.cdf(abs(z_score)))
    
    # Print the kappa score, z-score, and p-value
    print("Inter-annotator agreement of the manual examination on participants' own next-mention annotation")
    print("Cohen's Kappa:", kappa)
    print("Z-score:", z_score)
    print("P-value:", p_value)
    print()

 



if __name__ == '__main__':

    # read dataframe
    df = pd.read_csv('allResponses.csv', header=0)
    # automatically annotate the pronominalization of the subject in responses    
    annotate_pronominalization (df)
    df = df.drop(columns = ['POS', 'subj_proOrNot'])
    # save new dataframe
    df.to_csv('/Users/passage_continuation_experiment/data/allResponses.csv',index=False)

    #check percentage of zero subject 
    per_zero = len(df[(df['prompt'] ==  'bare') & (df['pronominalization'] ==  'zero')])/len(df[(df['prompt'] ==  'bare')])
    # print percentage of zero subject in all the responses from the bare prompt condition: 0.091
    print('percentage of zero/null subject:',per_zero)
    print()
    
    # check percentage of re-mentions of both subject and non-subject referents  (excluded from the main analysis)
    per_both = len(df[(df['prompt'] ==  'bare') & (df['annotated_nm'] == 2)])/len(df[(df['prompt'] ==  'bare')])
    # print percentage of re-mentions of both: 0.1523
    print('percentage of re-mentions of both:',per_both)
    print()
    
    # check percentage of possessive NPs as next mention (excluded from the main analysis)
    per_possessNP = len(df[(df['prompt'] ==  'bare') & (df['pronominalization'] ==  'possessive_pro')])/len(df[(df['prompt'] ==  'bare')])
    # print percentage of possessive NPs as next mention: 0.01267
    print('percentage of possessive NPs:', per_possessNP)
    print()


    # randomly sample 10% for manual examination of automatically-annotated pronominalization labels
    df_pro_manualExamination = df[(df['annotated_nm'] < 2) & (df['prompt'] ==  'bare') & (df['pronominalization'] != 'zero') & (df['pronominalization'] != 'possessive_pro')]
    pronominalization_check = df_pro_manualExamination.sample(int(len(df_pro_manualExamination)*0.1), random_state=4)
    pronominalization_check.to_csv('/Users/passage_continuation_experiment/data/pronominalization_check.csv',index=True)
    
    ## sample 10% for maunal examination of next mention annotation    
    df_nextMention_manualExamination = df[(df['annotated_nm'] < 2) & (df['pronominalization'] != 'zero') & (df['pronominalization'] != 'possessive_pro')]          
    nextMention_check = df_nextMention_manualExamination.sample(int(len(df_nextMention_manualExamination)*0.1), random_state=4)
    nextMention_check.to_csv('/Users/passage_continuation_experiment/data/nextMention_check.csv',index=True)


    # check inter-annotator agreement of the manual examination on participants' own next-mention annotation
    df_manual_examination_file = pd.read_csv('/Users/passage_continuation_experiment/data/nextMention_check_result.csv',  header=0)
    inter_annotator_agreement (df_manual_examination_file)
    
    # estimate the accuracy of participants' own next-mention annotation
    total_count = len(df_manual_examination_file)
    t_count = df_manual_examination_file['Final'].value_counts().get('T', 0)
    percentage_t = (t_count / total_count) * 100    
    print(f"The estimated accuracy of participants' own next-mention annotation is: {percentage_t:.1f}%")
    print()
    
    # estimate the accuracy of automatic annotation of pronominalization
    df_pro_manual_examination_file = pd.read_csv('/Users/passage_continuation_experiment/data/pronominalization_check_result.csv',  header=0)
    total_count2 = len(df_pro_manual_examination_file)
    t_count2 = df_pro_manual_examination_file['Annotator1'].value_counts().get('T', 0)
    percentage_t2 = (t_count2 / total_count2) * 100    
    print(f"The estimated accuracy of automatic annotation on pronominalization is: {percentage_t2:.1f}%")
    print()

    
    ## the rest of the codes generate a dataframe with model predictions for model comparisons##   
    # dataframe excluding "zero" and "possessive NPs"
    df_noZero = df[(df['pronominalization'].isin(['pro','non_pro'])) & (df['annotated_nm'].isin([0,1]))]
    
    relations = []
    itemID = []
    countNP1 = []
    countNP2 = []
    countTotalNP12Remention = []    
    countOvertProNP1 = []
    countOvertProNP2 = []    
    countPromptPro = []
    countNP12GivenPro = []
    countNP1GivenPro = []
    countNP2GivenPro = []


    for i in range(1,31): 
        for r in ['Narration','Contrast','Result']:
            c_NP1 = len(df_noZero[(df_noZero.itemID == i) & (df_noZero.relation == r) &
                           (df_noZero.prompt == 'bare') & (df_noZero.annotated_nm == 0)])
            c_NP2 = len(df_noZero[(df_noZero.itemID == i) & (df_noZero.relation == r) &
                           (df_noZero.prompt == 'bare') & (df_noZero.annotated_nm == 1)])
            c_totalNP12Remention = c_NP1 + c_NP2
    
        
            c_overtProNP1 = len(df_noZero[(df_noZero.itemID == i) & (df_noZero.relation == r) &
                           (df_noZero.prompt == 'bare') & (df_noZero.annotated_nm == 0) &
                           (df_noZero.pronominalization == 'pro')])
            c_overtProNP2 = len(df_noZero[(df_noZero.itemID == i) & (df_noZero.relation == r) &
                           (df_noZero.prompt == 'bare') & (df_noZero.annotated_nm == 1) &
                           (df_noZero.pronominalization == 'pro')])
        
            # interpretation: RefGivenPro
            c_promptPronoun = len(df[(df.itemID == i) & (df.relation == r) &
                           (df.prompt == 'pronoun') ])    
            c_NP1GivenPro = len(df_noZero[(df_noZero.itemID == i) & (df_noZero.relation == r) &
                           (df_noZero.prompt == 'pronoun') & (df_noZero.annotated_nm == 0)])
            c_NP2GivenPro = len(df_noZero[(df_noZero.itemID == i) & (df_noZero.relation == r) &
                           (df_noZero.prompt == 'pronoun') & (df_noZero.annotated_nm == 1)])
            
            c_NP12GivenPro = c_NP1GivenPro + c_NP2GivenPro
    
        
            relations.append(r)
            itemID.append(i)
            countNP1.append(c_NP1)
            countNP2.append(c_NP2)
            countTotalNP12Remention.append(c_totalNP12Remention)
            
            countOvertProNP1.append(c_overtProNP1)  
            countOvertProNP2.append(c_overtProNP2) 
            
            countPromptPro.append(c_promptPronoun)
            countNP12GivenPro.append(c_NP12GivenPro)
            countNP1GivenPro.append(c_NP1GivenPro)
            countNP2GivenPro.append(c_NP2GivenPro)

    # smoothing: countNP1/2 + 2, countOvertProNP1/2 + 1
            
    countNP1_smoothed = [x+2 for x in countNP1]
    countNP2_smoothed = [x+2 for x in countNP2]
    countTotalNP12Remention_smoothed = [x+4 for x in countTotalNP12Remention]
    
    countOvertProNP1_smoothed = [x+1 for x in countOvertProNP1]
    countOvertProNP2_smoothed = [x+1 for x in countOvertProNP2]
    
            
    # creating a nested list
    cols = [itemID, relations, countNP1, countNP2, countTotalNP12Remention, 
            countNP1_smoothed, countNP2_smoothed, countTotalNP12Remention_smoothed,
            countOvertProNP1, countOvertProNP2, 
            countOvertProNP1_smoothed, countOvertProNP2_smoothed,
            countPromptPro, countNP12GivenPro, countNP1GivenPro, countNP2GivenPro]
    
    # creating DataFrame
    df_onlyOvert = pd.DataFrame(cols, index= ['itemID','relation',
                                              'countNP1', 'countNP2', 'countTotalNP12Remention', 
                                              'countNP1_smoothed', 'countNP2_smoothed', 'countTotalNP12Remention_smoothed',
                                              'countOvertProNP1','countOvertProNP2', 
                                              'countOvertProNP1_smoothed', 'countOvertProNP2_smoothed',
                                              'countPromptPro', 'countNP12GivenPro',
                                              'countNP1GivenPro', 'countNP2GivenPro']).T
        
    
    df_overtOnly = pd.DataFrame(np.repeat(df_onlyOvert.values, 2, axis=0))
    df_overtOnly.columns = df_onlyOvert.columns
 
    
    df_overtOnly['ref'] = ['subject', 'nonsubject'] * 90
    
    # change the location of the column ref
    column_to_move = df_overtOnly.pop("ref")
    # insert column with insert(location, column_name, column_value)
    df_overtOnly.insert(0, "ref", column_to_move)
    
    
    # add posterior column: refGivenPro
    df_overtOnly['countNP1GivenPro'] = df_overtOnly['countNP1GivenPro'].astype(np.float) 
    df_overtOnly['countNP2GivenPro'] = df_overtOnly['countNP2GivenPro'].astype(np.float) 
    df_overtOnly['countNP12GivenPro'] = df_overtOnly['countNP12GivenPro'].astype(np.float)
    df_overtOnly['NP1GivenPro'] = df_overtOnly['countNP1GivenPro'].divide(df_overtOnly['countNP12GivenPro']) 
    df_overtOnly['NP2GivenPro'] = df_overtOnly['countNP2GivenPro'].divide(df_overtOnly['countNP12GivenPro']) 
    df_overtOnly['NP1GivenPro'] = df_overtOnly['NP1GivenPro'].replace(np.nan, 0)
    df_overtOnly['NP2GivenPro'] = df_overtOnly['NP2GivenPro'].replace(np.nan, 0)
    
    # add prior column: refPrior
    df_overtOnly['countNP1'] = df_overtOnly['countNP1'].astype(np.float) 
    df_overtOnly['countNP2'] = df_overtOnly['countNP2'].astype(np.float) 
    df_overtOnly['countTotalNP12Remention'] = df_overtOnly['countTotalNP12Remention'].astype(np.float)
    df_overtOnly['NP1Prior'] = df_overtOnly['countNP1'].divide(df_overtOnly['countTotalNP12Remention']) 
    df_overtOnly['NP2Prior'] = df_overtOnly['countNP2'].divide(df_overtOnly['countTotalNP12Remention']) 
    df_overtOnly['NP1Prior'] = df_overtOnly['NP1Prior'].replace(np.nan, 0)
    df_overtOnly['NP2Prior'] = df_overtOnly['NP2Prior'].replace(np.nan, 0)
    
    
    # add smoothed prior: refPriorSmoothed
    df_overtOnly['NP1PriorSmoothed'] = df_overtOnly['countNP1_smoothed'].divide(df_overtOnly['countTotalNP12Remention_smoothed']) 
    df_overtOnly['NP2PriorSmoothed'] = df_overtOnly['countNP2_smoothed'].divide(df_overtOnly['countTotalNP12Remention_smoothed']) 
        
    
    # add likelihood column: proGivenRef
    df_overtOnly['countOvertProNP1'] = df_overtOnly['countOvertProNP1'].astype(np.float) 
    df_overtOnly['countOvertProNP2'] = df_overtOnly['countOvertProNP2'].astype(np.float) 
    df_overtOnly['proGivenNP1'] = df_overtOnly['countOvertProNP1'].divide(df_overtOnly['countNP1']) 
    df_overtOnly['proGivenNP2'] = df_overtOnly['countOvertProNP2'].divide(df_overtOnly['countNP2']) 
    df_overtOnly['proGivenNP1'] = df_overtOnly['proGivenNP1'].replace(np.nan, 0)
    df_overtOnly['proGivenNP2'] = df_overtOnly['proGivenNP2'].replace(np.nan, 0)
    
    # add smoothed likelihood: proGivenRefSmoothed
    df_overtOnly['proGivenNP1Smoothed'] = df_overtOnly['countOvertProNP1_smoothed'].divide(df_overtOnly['countNP1_smoothed']) 
    df_overtOnly['proGivenNP2Smoothed'] = df_overtOnly['countOvertProNP2_smoothed'].divide(df_overtOnly['countNP2_smoothed']) 
    
    
    
    # Expectancy Model 
    # P(referent|pronoun) = P(referent)
    df_overtOnly.loc[(df_overtOnly["ref"] == 'subject'), 'EXPECTANCY'] = df_overtOnly['NP1Prior']
    df_overtOnly.loc[(df_overtOnly["ref"] == 'nonsubject'), 'EXPECTANCY'] = df_overtOnly['NP2Prior']
    
    # Bayesian 
    #  P(pronoun|referent)P(referent)/ SUM(P(pronoun|referent)P(referent))
    df_overtOnly.loc[(df_overtOnly["ref"] == 'subject'), 'BAYESIAN'] = (df_overtOnly['proGivenNP1'] * df_overtOnly['NP1Prior']) / ((df_overtOnly['proGivenNP1'] * df_overtOnly['NP1Prior']) + (df_overtOnly['proGivenNP2'] * df_overtOnly['NP2Prior']))
    df_overtOnly.loc[(df_overtOnly["ref"] == 'nonsubject'), 'BAYESIAN'] = (df_overtOnly['proGivenNP2'] * df_overtOnly['NP2Prior']) / ((df_overtOnly['proGivenNP1'] * df_overtOnly['NP1Prior']) + (df_overtOnly['proGivenNP2'] * df_overtOnly['NP2Prior']))
        
    # Mirror model
    #  P(pronoun|referent)/ SUM (P(pronoun|referent))
    df_overtOnly.loc[df_overtOnly["ref"] == 'subject', 'MIRROR'] = df_overtOnly['proGivenNP1'] / (df_overtOnly['proGivenNP1'] + df_overtOnly['proGivenNP2'])
    df_overtOnly.loc[df_overtOnly["ref"] == 'nonsubject', 'MIRROR'] = df_overtOnly['proGivenNP2'] / (df_overtOnly['proGivenNP1'] + df_overtOnly['proGivenNP2'])
       
    
    # Smoothed prediction: Expectancy Model 
    # P(referent|pronoun) = P(referent)
    df_overtOnly.loc[(df_overtOnly["ref"] == 'subject'), 'EXPECTANCY_smoothed'] = df_overtOnly['NP1PriorSmoothed']
    df_overtOnly.loc[(df_overtOnly["ref"] == 'nonsubject'), 'EXPECTANCY_smoothed'] = df_overtOnly['NP2PriorSmoothed']
    
    
    # Smoothed prediction: Bayesian
    #  P(pronoun|referent)P(referent)/ SUM(P(pronoun|referent)P(referent))
    df_overtOnly.loc[(df_overtOnly["ref"] == 'subject'), 'BAYESIAN_smoothed'] = (df_overtOnly['proGivenNP1Smoothed'] * df_overtOnly['NP1PriorSmoothed']) / ((df_overtOnly['proGivenNP1Smoothed'] * df_overtOnly['NP1PriorSmoothed']) + (df_overtOnly['proGivenNP2Smoothed'] * df_overtOnly['NP2PriorSmoothed']))
    df_overtOnly.loc[(df_overtOnly["ref"] == 'nonsubject'), 'BAYESIAN_smoothed'] = (df_overtOnly['proGivenNP2Smoothed'] * df_overtOnly['NP2PriorSmoothed']) / ((df_overtOnly['proGivenNP1Smoothed'] * df_overtOnly['NP1PriorSmoothed']) + (df_overtOnly['proGivenNP2Smoothed'] * df_overtOnly['NP2PriorSmoothed']))
    
    
    # Smoothed prediction: Mirror model
    #  P(pronoun|referent)/ SUM (P(pronoun|referent))
    df_overtOnly.loc[df_overtOnly["ref"] == 'subject', 'MIRROR_smoothed'] = df_overtOnly['proGivenNP1Smoothed'] / (df_overtOnly['proGivenNP1Smoothed'] + df_overtOnly['proGivenNP2Smoothed'])
    df_overtOnly.loc[df_overtOnly["ref"] == 'nonsubject', 'MIRROR_smoothed'] = df_overtOnly['proGivenNP2Smoothed'] / (df_overtOnly['proGivenNP1Smoothed'] + df_overtOnly['proGivenNP2Smoothed'])
    
    
    # add column: observed posterior
    df_overtOnly.loc[(df_overtOnly["ref"] == 'subject'), 'observedPosterior'] = df_overtOnly['NP1GivenPro']
    df_overtOnly.loc[(df_overtOnly["ref"] == 'nonsubject'), 'observedPosterior'] = df_overtOnly['NP2GivenPro']    
    
    df_overtOnly.to_csv('model_predictions.csv')
    
