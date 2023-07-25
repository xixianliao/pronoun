#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import OntoNotes_AllenNLP as onto
from nltk.stem import WordNetLemmatizer 




# single S or S CC S     
def count_subsents(tree):
    #None -> only one sentence, 1-> more than one subsentence
    number_subsents=0
    if tree != None:
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






# apply for the subject antecedent    
def subject_nextmention_pos(phrase):
    """
    part of speech checker
    """    
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
    





def narrationmarker(sentence):  

    """
    define explicit markers of Narration
    """
    
    tree = sentence.parse_tree
    pos = sentence.pos_tags
    if tree != None:
        leaves = tree.leaves()
        for n in range(3):
            if len(leaves) > 3 and leaves[n] in ['And','and'] and leaves[n+1] in ['then','next','later'] and pos[n+1] == 'RB':
                return True
            if len(leaves) > 3 and leaves[n] in ['After','after'] and leaves[n+1] in ['it','that']:
                for st in tree.subtrees():
                    if st.label() == 'PP' and st.leaves() == leaves[:n+2]:
                        return True
            if len(leaves) > 3 and leaves[0] in ['Then','then','Next','next','Later','later','Afterwards','afterwards','afterward','Afterward', 'subsequently', 'Subsequently', 'thereafter', 'Thereafter'] and pos[0] == 'RB' :
                return True
            if len(leaves) > 3 and leaves[n] in ['after','later'] and pos[n-1] in ['NNS','NN'] and pos[n-2] in ['CD','DT']:
                # six days later
                for st in tree.subtrees():
                    if st.label() == 'ADVP' and st.leaves() == leaves[n-2:n+1]:
                        return True

                          
def narrationmarker2(sentence):
    # The beginging of the second sentence: NP then(ADVP) VP
    tree = sentence.parse_tree
    if tree != None:
        leaves = tree.leaves()
        for st in tree.subtrees():
            if st.label() == 'S' and len(st) > 2:            
                if st[0].label() == 'NP' and st[1].leaves()[0] == 'then' and\
                st[2].label() == 'VP':
                    i = [i for i in range(len(leaves)) if leaves[i:i+len(st.leaves())] == st.leaves()]                        
                    if i ==[0]:                    
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



  
def exclude_first_second_person_pronoun (subject_span, sentence):
    words = sentence.words
    first_second_pronouns = ['I','me','Me','my','My','you','You','your','Your','we','We','Our','our','us','Us']
    subject_words = words[subject_span[0]:subject_span[1]+1]
    if len(subject_words) == 1 and subject_words[0] in first_second_pronouns:
        return True
    else: 
        return False
    

    
########################################

def main():

    

    non_subject_pos_chain = []
    
    narration_lemmas = []
    subject_lemmas = []
    non_subject_lemmas = []
    
    

    genres = []

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
    
    # prepare for the dataframe
    df_narration = []
    df_narration_coreference_type = []
    df_narration_context = []
    df_narration_sentence_id =[]
    df_narration_document_id = []
    df_narration_proOrNot = []
    df_narration_12person = []
    df_narration_proAntecedent = []
    df_narration_subjType  = []

    
    while True:
        try:
            sentence1 = next(sentences1)
            sentence2 = next(sentences2)
            pos = sentence1.pos_tags
            tree1 = sentence1.parse_tree
            tree2 = sentence2.parse_tree
            
            if tree1 != None and tree2 != None and\
            (narrationmarker2(sentence2) or narrationmarker(sentence2)):
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
                    lemma = main_verb(VP)      # main verb in the first proposition
                    if next_mention_coref_no != 'Noinfo' :
                        total += 1
                        words1 = print_sentence_to_file(sentence1)
                        words2 = print_sentence_to_file(sentence2)
                        full_context = words1 + "//" +words2
                
                        print('total:',total)
                        print(words1)
                        print(words2)
                        genres.append(sentence1.document_id[:2])
                        
                        df_narration.append("narration")
                        df_narration_context.append(full_context)
                        doc_id = sentence1.document_id
                        sent_id = sentence1.sentence_id
                        df_narration_sentence_id.append(str(sent_id))                          
                        df_narration_document_id.append(doc_id)     
                        narration_lemmas.append(lemma)
                        
                        
                        if exclude_first_second_person_pronoun (subject_span, sentence1) == True:                                    
                            df_narration_subjType.append('first_second_pronoun')
                        elif subject_pos== 'PRP' and exclude_first_second_person_pronoun (subject_span, sentence1) == False:
                            df_narration_subjType.append('other_pronoun')
                        else:
                            df_narration_subjType.append('non_pronoun')    



                        if subject_coref_no == next_mention_coref_no:                                    
                                
                                subject += 1  
                                subject_lemmas.append(lemma)
                                
                                df_narration_coreference_type.append ("subject")
                                
                                if subject_pos== 'PRP':
                                    df_narration_proAntecedent.append('pronoun')
                                    pronominal_subject_as_antecedent +=1

                                else:
                                    df_narration_proAntecedent.append('non pronoun')
                                    

                                if phrase_is_pronoun(next_mention) == True: 
                                    df_narration_proOrNot.append('pronoun')
                                    pronominalized_subject += 1
                                else:
                                    df_narration_proOrNot.append('non pronoun')
                                    
                                if exclude_first_second_person_pronoun (subject_span, sentence1) == True:                                    
                                    df_narration_12person.append('True')
                                    first_second_person_antecedent += 1
                                    if phrase_is_pronoun(next_mention) == True: 
                                        pronominalized_next_mention_first_second_person_antecedent += 1
                                else:
                                    df_narration_12person.append('False')

   
                        elif len(non_subjects_coref_no) >0 and any(i for i in non_subjects_coref_no if i == next_mention_coref_no):
                                words1 = print_sentence_to_file(sentence1)
                                words2 = print_sentence_to_file(sentence2)

                                non_subject += 1
                                df_narration_coreference_type.append ("non_subject")
                                df_narration_proAntecedent.append('non_subject')
                                df_narration_12person.append('non_subject')
                                
                                non_subject_lemmas.append(lemma)
                                non_subject_antecedent_span = next(y for (x,y) in non_subjects if x == next_mention_coref_no)
                                non_subject_pos = non_subject_POS(non_subject_antecedent_span,sentence1)
                                pos = non_subject_pos + '_' + nextmention_pos
                                non_subject_pos_chain.append(pos)                                                                                                
                                if phrase_is_pronoun(next_mention) == True: 
                                    df_narration_proOrNot.append('pronoun')
                                    pronominalized_non_subject += 1
                                else:
                                    df_narration_proOrNot.append('non pronoun')
                        else:
                            df_narration_coreference_type.append ("other")
                            df_narration_proAntecedent.append('other')
                            df_narration_12person.append('other')
                            if phrase_is_pronoun(next_mention) == True: 
                                df_narration_proOrNot.append('pronoun')
                            else:
                                df_narration_proOrNot.append('non pronoun')
                            
                            
        except (TypeError,AttributeError):
                        print(tree1.leaves())
                        print(tree2.leaves())
                        print(tree1)
                        print(tree2)
        except StopIteration:
            break

    print('narration_total=',total)
    print('narration_subject=',subject)
    print('narration_pronominalized_subject=',pronominalized_subject)
    print('narration_pronominal_subject_as_antecedent=', pronominal_subject_as_antecedent)
    print('narration_percentage_pronominal_subject_antecedent=', pronominal_subject_as_antecedent/subject)
    print('narration_non_subject=',non_subject)
    print('narration_pronominalized_non_subject=',pronominalized_non_subject)
    print('narration_first_second_person_antecedent =',first_second_person_antecedent)
    print('narration_pronominalized_next_mention_first_second_person_antecedent=',pronominalized_next_mention_first_second_person_antecedent)
    print("narration_valid_subject_samples = ", subject - first_second_person_antecedent)
    print("narration_valid_pronominal_subject_samples =", pronominalized_subject - pronominalized_next_mention_first_second_person_antecedent)

   
    return df_narration, narration_lemmas, df_narration_coreference_type, df_narration_context, df_narration_sentence_id, df_narration_document_id, df_narration_proOrNot, df_narration_12person, df_narration_proAntecedent, df_narration_subjType

