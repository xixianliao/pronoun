#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import OntoNotes_AllenNLP as onto
from nltk.stem import WordNetLemmatizer 



# single S or S CC S    
def count_subsents(tree):
    #None -> only one sentence, 1-> more than one subsentence
    number_subsents=0
    for ch in range(len(tree[0])):
        if tree[0][ch].label() == 'S':
            number_subsents = number_subsents+1
            return number_subsents

def multiplesubsents(tree):
    last_subsent =''
    for ch in range(len(tree[0])):
        if tree[0][ch].label() == 'S':
            last_subsent = tree[0][ch]
    return last_subsent



def main_NPVP(tree):
    """
    tree = sentence.parse_tree for sentence consisting of one subsent
    tree = last_subsent for sentence consisting of more than one subsents
    """
    if tree != None:
        for st in tree.subtrees():
            for ch in range(len(st)):
                try:
                    if st[ch].label() == 'NP' and any(x for x in st[ch+1:] if x.label()=='VP') == True:
                        NP = st[ch]
                        VP = [x for x in st[ch+1:] if x.label()== 'VP'][0]
                        return NP,VP                
                    elif (st[ch].label() == 'VP' and st[ch+1].label() == 'NP'):
                        NP = st[ch+1]  
                        VP = st[ch] 
                        return NP,VP
                except (AttributeError, IndexError):
                    pass


def NPVP_spans(sentence,NP,VP):
    sentence_words = sentence.words
    subject_words = NP.leaves()
    VP_words = VP.leaves()
    subject_beg = [i for i in range(len(sentence_words)-len(subject_words)+1) if sentence_words[i] == subject_words[0] and sentence_words[i+len(subject_words)-1] == subject_words[-1]]
    subject_end = [i+ len(subject_words)-1 for i in subject_beg]
    VP_beg = [i for i in range(len(sentence_words)-len(VP_words)+1) if sentence_words[i] == VP_words[0] and sentence_words[i+len(VP_words)-1] == VP_words[-1]]
    VP_end = [i+ len(VP_words)-1 for i in VP_beg]
    subject_span = (subject_beg[0],subject_end[0])    
    VP_span = (VP_beg[0],VP_end[0])
    return subject_span,VP_span


def subject_coref_id(sentence,subject_span):
    coref = sentence.coref_spans
    try:
        subject_coref= next(x for (x,y) in coref if y == subject_span)   
        return subject_coref
    except StopIteration:
        subject_coref = 'Noinfo'
        return subject_coref
        

def non_subjects_coref_id(sentence,VP_span):
    coref = sentence.coref_spans
    non_subjects_coref_id = []
    VP_beg = VP_span[0]
    VP_end = VP_span[1]   
    for (x,y) in coref:
        if y[0] >= VP_beg and y[1] <= VP_end:
            non_subjects_coref_id.append(x)    
    return non_subjects_coref_id



def non_subjects_coref_span(sentence,VP_span):
    coref = sentence.coref_spans
    non_subjects = []
    VP_beg = VP_span[0]
    VP_end = VP_span[1]   
    for i in coref:
        if i[1][0] >= VP_beg and i[1][1] <= VP_end:
            non_subjects.append(i) 
    return non_subjects



"""
part of speech checker
"""
# apply for the subject antecedent    
def subject_nextmention_pos(phrase):
    if len(phrase) == 1 and phrase[0].label() == 'PRP':
        pos = 'PRP'
        return pos 
    else:
        pos = 'nonPRP'
        return pos
    

def non_subject_POS(non_subjects_span,sentence):
    if non_subjects_span[0] == non_subjects_span[1] and sentence.pos_tags[non_subjects_span[1]] == 'PRP':
        non_subject_pos = 'PRP'
        return non_subject_pos
        
    else: 
         non_subject_pos = 'nonPRP'
         return non_subject_pos

    
def main_verb(VP):
    lemmatizer = WordNetLemmatizer()
    for st in VP.subtrees():
        if st.label() == 'VP':
            children = [i for i in st if i.label()=='VP']
            if len(children) == 0:
                for sst in st.subtrees():
                    if sst.label().startswith('V') and sst.height() == 2:
                        verb = sst.leaves()[0]
                        lemma=lemmatizer.lemmatize(verb,'v')
                        return lemma
    





def contrastmarker(sentence):  
    """
    define explicit markers of Contrast
    """
    tree = sentence.parse_tree
    if tree != None :
        leaves = tree.leaves()
        if len(leaves) >2:
            for n in range(3):
                if leaves[n] in ['But','but', 'Yet', 'yet', 'Conversely', 'conversely'] :
                    return True
                if leaves[n] in ['On','on'] and leaves[n+1] in ['the'] and leaves[n+2] in ['contrary']:
                    return True
                if leaves[n] in ['By','by', 'In', 'in'] and leaves[n+1] in ['contrast', 'comparison']:
                    return True
                if leaves[n] in ['However','however'] :
                    return True
                if leaves[n] in ['Nonetheless','nonetheless', 'Nevertheless', 'nevertheless'] :
                    return True
                if leaves[n] in ['On','on'] and leaves[n+1] in ['the'] and leaves[n+2] in ['other'] and leaves[n+3] in ['hand']:
                    return True


            
def next_mention_coref(next_mention_span,sentence):
    coref = sentence.coref_spans
    if True:
        try:
            next_mention_coref= next(x for (x,y) in coref if y == next_mention_span)   
        except StopIteration:
            next_mention_coref = 'Noinfo'
        return next_mention_coref



def phrase_is_pronoun(phrase):
    if len(phrase) == 1 and phrase[0].label() == 'PRP':
        return True
    


def print_sentence_to_file(sentence):
    leaves = sentence.words
    words = " ".join(leaves)
    return words
   


def non_subject_antecedent_words(sentence,span):
    words = sentence.words
    non_subject = words[span[0]:span[1]+1]
    return non_subject


def exclude_first_second_person_pronoun (subject_span, sentence):
    words = sentence.words
    first_second_pronouns = ['I','me','Me','my','My','you','You','your','Your','we','We','Our','our','us','Us']
    subject_words = words[subject_span[0]:subject_span[1]+1]
    if len(subject_words) == 1 and subject_words[0] in first_second_pronouns:
        return True
    else:
        return False



#########################
        
def main():

    
    contrast_lemmas = []
    contrast_lemmas2 = []
    subject_lemmas = []
    non_subject_lemmas = []

    

    
    mydata = onto.Ontonotes()
    sentences1 = mydata.dataset_iterator(file_path="corpora/OntoNotes/conll-formatted-ontonotes-5.0/data/")
    sentences2 = mydata.dataset_iterator(file_path="corpora/OntoNotes/conll-formatted-ontonotes-5.0/data/")    
    sentence2 = next(sentences2)
    subject = 0
    pronominal_subject_as_antecedent = 0
    first_second_person_antecedent = 0 
    pronominalized_next_mention_first_second_person_antecedent = 0
    non_subject =0
    total =0
    pronominalized_non_subject=0
    pronominalized_subject =0
    
    df_contrast = []
    df_contrast_coreference_type = []
    df_contrast_context = []
    df_contrast_sentence_id = []
    df_contrast_document_id = []
    df_contrast_proOrNot = []
    df_contrast_12person = []
    df_contrast_proAntecedent = []
    df_contrast_subjType  = []
    
    while True:
        try:
            sentence1 = next(sentences1)
            sentence2 = next(sentences2)
            tree1 = sentence1.parse_tree
            tree2 = sentence2.parse_tree
            if tree1 != None and tree2 != None and\
             contrastmarker(sentence2):
                # "NP connective VP" or "connective NP VP"
                number_subsents = count_subsents(tree1)                        
                if number_subsents == None:
                    tree = tree1                
                else:
                    tree = multiplesubsents(tree1)
                if main_NPVP(tree) != None and main_NPVP(tree2) != None:
                    NP = main_NPVP(tree)[0]
                    VP = main_NPVP(tree)[1]
                    subject_span = NPVP_spans(sentence1,NP,VP)[0]
                    VP_span = NPVP_spans(sentence1,NP,VP)[1]
                    subject_coref_no = subject_coref_id(sentence1,subject_span)  
                    non_subjects_coref_no = non_subjects_coref_id(sentence1,VP_span)
                    next_mention = main_NPVP(tree2)[0]
                    VP2 = main_NPVP(tree2)[1]
                    next_mention_span = NPVP_spans(sentence2,next_mention,VP2)[0]
                    next_mention_coref_no = subject_coref_id(sentence2,next_mention_span)  
                    
                    non_subjects = non_subjects_coref_span(sentence1,VP_span)                    
                    subject_pos = subject_nextmention_pos(NP)
                    nextmention_pos = subject_nextmention_pos(next_mention)
                    lemma = main_verb(VP)                    
                    lemma2 = main_verb(VP2)
                    if next_mention_coref_no != 'Noinfo' :
                        words1 = print_sentence_to_file(sentence1)
                        words2 = print_sentence_to_file(sentence2)
                        full_context = words1 + "//" +words2

                        total += 1
                        print('total:',total)
                        print(words1)
                        print(words2)
                        contrast_lemmas.append(lemma)
                        contrast_lemmas2.append(lemma2)
                        
                        df_contrast.append("contrast")
                        df_contrast_context.append(full_context)
                        doc_id = sentence1.document_id
                        sent_id = sentence1.sentence_id
                        df_contrast_sentence_id.append(str(sent_id))                          
                        df_contrast_document_id.append(doc_id)   
                        
                                                
                        if exclude_first_second_person_pronoun (subject_span, sentence1) == True:                                    
                            df_contrast_subjType.append('first_second_pronoun')
                        elif subject_pos== 'PRP' and exclude_first_second_person_pronoun (subject_span, sentence1) == False:
                            df_contrast_subjType.append('other_pronoun')
                        else:
                            df_contrast_subjType.append('non_pronoun')                            
                        
                        
                        
                        if subject_coref_no == next_mention_coref_no:
                            subject += 1                                 
                            df_contrast_coreference_type.append ("subject")
                            subject_lemmas.append(lemma)
                            if subject_pos== 'PRP':
                                df_contrast_proAntecedent.append('pronoun')
                                pronominal_subject_as_antecedent +=1
                            else:
                                df_contrast_proAntecedent.append('non pronoun')
                                                                        
                            if phrase_is_pronoun(next_mention) == True: 
                                df_contrast_proOrNot.append('pronoun')
                                pronominalized_subject += 1
                            else: 
                                df_contrast_proOrNot.append('non pronoun')
                                
                            if exclude_first_second_person_pronoun (subject_span, sentence1) == True:                                    
                                df_contrast_12person.append('True')
                                first_second_person_antecedent += 1
                                if phrase_is_pronoun(next_mention) == True: 
                                    pronominalized_next_mention_first_second_person_antecedent += 1
                            else:
                                df_contrast_12person.append('False')
                                    
                                
                                
                        elif len(non_subjects_coref_no) >0 and any(i for i in non_subjects_coref_no if i == next_mention_coref_no):
                                words1 = print_sentence_to_file(sentence1)
                                words2 = print_sentence_to_file(sentence2)
                                non_subject += 1
                                non_subject_lemmas.append(lemma)
                                
                                df_contrast_coreference_type.append ("non_subject")
                                df_contrast_proAntecedent.append("non_subject")
                                df_contrast_12person.append('non_subject')
                                
                                non_subject_antecedent_span = next(y for (x,y) in non_subjects if x == next_mention_coref_no)
                                                               
                                if phrase_is_pronoun(next_mention) == True:     
                                    df_contrast_proOrNot.append('pronoun')
                                    pronominalized_non_subject += 1
                                else:
                                    df_contrast_proOrNot.append('non pronoun')

                        else:
                            df_contrast_coreference_type.append ("other")
                            df_contrast_proAntecedent.append("other")
                            df_contrast_12person.append('other')

                            if phrase_is_pronoun(next_mention) == True:     
                                df_contrast_proOrNot.append('pronoun')
                            else:
                                df_contrast_proOrNot.append('non pronoun')

        except (TypeError,AttributeError):
                        print('error')
                        print(tree1.leaves())
                        print(tree2.leaves())
                        print(tree1)
                        print(tree2)
                        print('============')
        except StopIteration:
            break
        

    print('contrast_total=',total)
    print('contrast_subject=',subject)
    print('contrast_pronominalized_subject=',pronominalized_subject)
    print('contrast_pronominal_subject_as_antecedent=', pronominal_subject_as_antecedent)
    print('contrast_percentage_pronominal_subject_antecedent=', pronominal_subject_as_antecedent/subject)
    print('contrast_non_subject=',non_subject)
    print('contrast_pronominalized_non_subject=',pronominalized_non_subject)
    print('contrast_first_second_person_antecedent =',first_second_person_antecedent)
    print('contrast_pronominalized_next_mention_first_second_person_antecedent=',pronominalized_next_mention_first_second_person_antecedent)
    print("contrast_valid_subject_samples = ", subject - first_second_person_antecedent)
    print("contrast_valid_pronominal_subject_samples =", pronominalized_subject - pronominalized_next_mention_first_second_person_antecedent)
  
    return df_contrast, contrast_lemmas, df_contrast_coreference_type, df_contrast_context, df_contrast_sentence_id, df_contrast_document_id, df_contrast_proOrNot, df_contrast_12person, df_contrast_proAntecedent, df_contrast_subjType


      

