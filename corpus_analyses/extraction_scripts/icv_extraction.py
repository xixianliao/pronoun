#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#Libraries
import pandas as pd
import OntoNotes_AllenNLP as onto



def NPVP_in_S(tree):
    def main_verb(VP):
        for st in VP.subtrees():
            if st.label() == 'VP':
                children = [i for i in st if i.label()=='VP']
                if len(children) == 0:
                    for sst in st.subtrees():
                        if sst.label().startswith('V') and sst.height() == 2:
                            verb = sst.leaves()[0]
                            return verb        
    tree_is_s = []
    verbs = []
    
    if tree != None:
        st = tree[0]
        
        for ch in range(len(st)):
            try:
                if st[ch].label() == 'NP' and any(x for x in st[ch+1:] if x.label()=='VP') == True:
                    NP = st[ch]
                    VP = [x for x in st[ch+1:] if x.label()== 'VP'][0]
                    main_v = main_verb(VP)
                    tree_is_s.append([st,NP])
                    verbs.append(main_v)
            except (AttributeError, IndexError):
                pass                
        
    return tree_is_s,verbs


def antecedent_span (pos, srl, ARG0_start,ARG1_start):
    ARG0_end = ARG0_start + srl.count('I-ARG0')
    ARG1_end = ARG1_start + srl.count('I-ARG1')
    ARG1_pos = pos[ARG1_start] 
    ARG0_pos = pos[ARG0_start]                
    if ARG0_pos == 'IN':
        ARG0_span = (ARG0_start+1,ARG0_end)
    else:    
        ARG0_span = (ARG0_start,ARG0_end)
    if ARG1_pos == 'IN':
        ARG1_span = (ARG1_start+1,ARG1_end)
    else:    
        ARG1_span = (ARG1_start,ARG1_end)                                              
        
    return ARG0_span, ARG1_span



def coref_id (col_coref,span):
    try:
        no_ref_chain = next(x for (x,y) in col_coref if y == span)
    except StopIteration:
        no_ref_chain = 'None'
    return no_ref_chain     


def main_NPVP(tree):
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

def list_NPVP_in_S(tree):
    def main_verb(VP):
        for st in VP.subtrees():
            if st.label() == 'VP':
                children = [i for i in st if i.label()=='VP']
                if len(children) == 0:
                    for sst in st.subtrees():
                        if sst.label().startswith('V') and sst.height() == 2:
                            verb = sst.leaves()[0]
                            return verb        
    verbs = [] 
    nps = []
    if tree != None:                      
        for st in tree.subtrees():
            if st.label() == "S":                        
                for ch in range(len(st)):
                    try:
                        if st[ch].label() == 'NP' and any(x for x in st[ch+1:] if x.label()=='VP') == True:
                            NP = st[ch]
                            VP = [x for x in st[ch+1:] if x.label()== 'VP'][0]
                            main_v = main_verb(VP)
                            main_n = NP
                            verbs.append(main_v)
                            nps.append(main_n)
                    except (AttributeError, IndexError):
                        pass                
                
    return nps,verbs



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



if __name__ == "__main__":


    subject_biased_list = [
        'agitate', 'amaze', 'amuse', 'anger', 'annoy', 'antagonize', 'apologize', 'appal',
        'attract', 'betray', 'bore', 'bug', 'call', 'captivate', 'charm', 'concern', 'confess',
        'daunt', 'delight', 'disappoint', 'echo', 'enrage', 'enthral', 'entice', 'entrance',
        'exasperate', 'excite', 'fascinate', 'frighten', 'frustrate', 'gladden', 'infuriate',
        'inspire', 'intimidate', 'intrigue', 'irritate', 'lie', 'madden', 'mesmerise', 'peeve',
        'please', 'provoke', 'repel', 'repulse', 'revolt', 'scar', 'sicken', 'telephone',
        'trail', 'trouble', 'unnerve', 'upset', 'worry', 'wow'
        ]
    
    object_biased_list = [
        'admire', 'adore', 'applaud', 'appreciate', 'calm', 'carry', 'celebrate', 'comfort',
        'commend', 'congratulate', 'console', 'correct', 'counsel', 'despise', 'detest', 'dislike',
        'distrust', 'dread', 'employ', 'envy', 'fancy', 'favour', 'fear', 'feed', 'guide', 'hat',
        'idolize', 'laugh', 'like', 'loathe', 'love', 'mourn', 'notice', 'penalize', 'pick', 'pity',
        'praise', 'prize', 'punish', 'resent', 'respect', 'reward', 'scold', 'spank', 'sue', 'thank',
        'treasure', 'value'
        ]

    list_verbTypes = [subject_biased_list, object_biased_list]


    # counter for subject-biased ICVs
    number_no_reference_annotated = 0
    number_subject_coreference = 0
    number_object_coreference = 0
    number_other_coreference = 0 
    number_pronominal_subject = 0
    number_pronominal_object = 0
    total = 0

    # # counter for ibject-biased ICVs
    number_no_reference_annotated_obj = 0
    number_subject_coreference_obj = 0
    number_object_coreference_obj = 0
    number_other_coreference_obj = 0 
    number_pronominal_subject_obj = 0
    number_pronominal_object_obj = 0
    total_obj = 0
    
    
    df_icv_verbtype = []
    df_icv_verb = []
    df_icv_coreference_type = []
    df_icv_context = []
    df_icv_sentence_id = []
    df_icv_document_id = []
    df_icv_proOrNot = []

    mydata = onto.Ontonotes()
    sentences = mydata.dataset_iterator(file_path="corpora/OntoNotes")
    sentences2 = mydata.dataset_iterator(file_path="corpora/OntoNotes")
#    verb_type = object_biased_list
    sentence2 = next(sentences2)
    
    while True:
        try:
            # object_biased:
            sentence = next(sentences)              
            sentence2 = next(sentences2)
            lemmas = sentence.predicate_lemmas
            tree = sentence.parse_tree
            senses= sentence.word_senses
            words = sentence.words
            args = sentence.srl_frames
            coref = sentence.coref_spans
            pos = sentence.pos_tags 
            doc = sentence.document_id
            
            
            for verb_type in list_verbTypes:                
                verbs = {word_index:verb for word_index,verb in enumerate(lemmas) if verb in verb_type}       
                if len(verbs) > 0: # there are icvs in the sentence
                   for i,predicate_args in enumerate(args):
                       
                       if  verb_type == subject_biased_list and ('B-ARG0' in predicate_args[1] and 'B-ARG1' in predicate_args[1] and ('B-ARG2' not in predicate_args[1])) and\
                       (predicate_args[1].index('B-V') in verbs.keys()):
                            srl = predicate_args[1]
                            predicate = predicate_args[0]
                            mainverbs_in_sentence =  NPVP_in_S(tree)[1]
                            # pass if icv is not a main predicate in the sentence
                            if predicate not in mainverbs_in_sentence:
                                continue
                            mainverb_start = srl.index('B-V')
                            ARG0_start = srl.index('B-ARG0')   # subject
                            ARG1_start = srl.index('B-ARG1')   # object
                                                                            
                            # subject_object, skip that-clause/quotes as object
                            if (ARG0_start > ARG1_start) or\
                            (words[ARG1_start] == "that" and pos[ARG1_start] == "IN") or\
                            (words[ARG1_start] == "to" and pos[ARG1_start] == "TO") or\
                            (pos[ARG1_start] in ["IN", 'VBG']) or\
                            (pos[mainverb_start+1] in ['RP', 'RB']) or\
                            ("``" in words[mainverb_start:ARG1_start]): 
                                continue
                            
                            ARG0_span, ARG1_span = antecedent_span (pos, srl, ARG0_start,ARG1_start)    
                            ARG0_coref = coref_id (coref,ARG0_span)  
                            ARG1_coref = coref_id (coref,ARG1_span)
                            
                            # decide to look for next mention in the same sentence or in the following one
                            # try to find a next mention in the current sentence
                            next_mention_start = None
                            if  i != len(args)+1:                                             
                                for other_srl in args[i+1:]: 
                                    try:
                                        first_arg_index,semanticrolelabel = next((index,sr) for (index, sr) in enumerate(other_srl[1]) if (sr != 'O') and (sr in ['B-ARG0', 'B-ARG1', 'B-ARG2']))                                  
                                        other_verb_index = other_srl[1].index('B-V')
                                        if first_arg_index > other_verb_index :
                                            continue
                                        
                                        if first_arg_index > ARG1_span[1] and\
                                        (words[other_verb_index] in list_NPVP_in_S(tree)[1]):
                                            next_mention_start = first_arg_index
                                            next_mention_end = next_mention_start + other_srl[1].count('I'+semanticrolelabel[-5:])
                                            next_mention_pos = pos[next_mention_start]
                                            if next_mention_pos  == 'IN':
                                                next_mention_span = (next_mention_start+1,next_mention_end)
                                            else:    
                                                next_mention_span = (next_mention_start,next_mention_end)                  
                                            
                                            next_mention_coref_no = coref_id(coref,next_mention_span)
                                            break
                                    except (StopIteration, IndexError): 
                                        pass                        
    
                                                        
                            # next_mention in next sentence
                            if next_mention_start == None:
                                tree2 = sentence2.parse_tree
                                coref2 = sentence2.coref_spans
                                pos2 = sentence2.pos_tags                             
                                next_mention = main_NPVP(tree2)[0]
                                VP2 = main_NPVP(tree2)[1]
                                next_mention_span = NPVP_spans(sentence2,next_mention,VP2)[0]
                                next_mention_pos = pos2[next_mention_span[0]]                            
                                next_mention_coref_no = coref_id(coref2,next_mention_span)  # a number or "None"
                                
                            if next_mention_coref_no == "None" and ARG0_coref == "None" and\
                            ARG1_coref == "None" :
                                number_no_reference_annotated += 1
    
                            elif next_mention_coref_no == "None" :
                                number_other_coreference += 1
                                total += 1
                                df_icv_verbtype.append('subject_biased')
                                df_icv_verb.append(lemmas[srl.index('B-V')])
                                df_icv_coreference_type.append('other')
                                df_icv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                df_icv_sentence_id.append(str(sentence.sentence_id))
                                df_icv_document_id.append(doc)
                                df_icv_proOrNot.append('other') 
                                
                            elif next_mention_coref_no == ARG0_coref:
                                number_subject_coreference += 1
                                total += 1
                                
                                df_icv_verbtype.append('subject_biased')
                                df_icv_verb.append(lemmas[srl.index('B-V')])
                                df_icv_coreference_type.append('subject')
                                df_icv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                df_icv_sentence_id.append(str(sentence.sentence_id))
                                df_icv_document_id.append(doc)
                                if next_mention_pos == "PRP":
                                    number_pronominal_subject += 1 
                                    df_icv_proOrNot.append('pronoun')
                                else:
                                    df_icv_proOrNot.append('non pronoun')
                                
                            elif next_mention_coref_no == ARG1_coref:
                                print('object coreference')
                                print(' '.join(words))
                                print(' '.join(sentence2.words))
                                number_object_coreference += 1
                                total += 1
                                df_icv_verbtype.append('subject_biased')
                                df_icv_verb.append(lemmas[srl.index('B-V')])
                                df_icv_coreference_type.append('object')
                                df_icv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                df_icv_sentence_id.append(str(sentence.sentence_id))
                                df_icv_document_id.append(doc)
                                if next_mention_pos == "PRP":
                                    number_pronominal_object += 1                             
                                    df_icv_proOrNot.append('pronoun')
                                else:
                                    df_icv_proOrNot.append('non pronoun')
                                    
                            else:
                                number_other_coreference += 1
                                total += 1
                                df_icv_verbtype.append('subject_biased')
                                df_icv_verb.append(lemmas[srl.index('B-V')])
                                df_icv_coreference_type.append('other')
                                df_icv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                df_icv_sentence_id.append(str(sentence.sentence_id))
                                df_icv_document_id.append(doc)
                                df_icv_proOrNot.append('other')                                 
                                
                            print("number_no_reference_annotated:", number_no_reference_annotated)
                            print('number_pronominal_subject:',number_pronominal_subject)
                            print("number_subject_coreference:",number_subject_coreference)
                            print('number_pronominal_object:',number_pronominal_object)                        
                            print("number_object_coreference:",number_object_coreference)
                            print("number_other_coreference:",number_other_coreference)
                            print(total)
                            print(predicate)
                            print(doc)
                            print('==========')




                       if verb_type == object_biased_list and  ('B-ARG0' in predicate_args[1] and 'B-ARG1' in predicate_args[1] and ('B-ARG2' not in predicate_args[1])) and\
                       ('B-ARG3' not in predicate_args[1]) and\
                       (predicate_args[1].index('B-V') in verbs.keys()):                      
                            srl = predicate_args[1]
                            predicate = predicate_args[0]
                            mainverbs_in_sentence =  NPVP_in_S(tree)[1]
                            # pass if icv is not a main predicate in the sentence
                            if predicate not in mainverbs_in_sentence:
                                continue
                            mainverb_start = srl.index('B-V')
                            ARG0_start = srl.index('B-ARG0')   # subject
                            ARG1_start = srl.index('B-ARG1')   # object
                            
                        
                         
                               
                            # subject_object, skip that-clause/quotes as object, carry out(RP)
                            if (ARG0_start > ARG1_start) or\
                            (words[ARG1_start] == "that" and pos[ARG1_start] == "IN") or\
                            (words[ARG1_start] == "to" and pos[ARG1_start] == "TO") or\
                            (pos[ARG1_start] in ["IN", 'VBG']) or\
                            (pos[mainverb_start+1] in ['RP', 'RB']) or\
                            ("``" in words[mainverb_start:ARG1_start]): 
                                continue
                            
                            ARG0_span, ARG1_span = antecedent_span (pos, srl, ARG0_start,ARG1_start)    
                            ARG0_coref = coref_id (coref,ARG0_span)  
                            ARG1_coref = coref_id (coref,ARG1_span)
                            
    
                            
                            # decide to look for next mention in the same sentence or in the following one
                            # try to find a next mention in the current sentence
                            next_mention_start = None
                            if  i != len(args)+1:                                             
                                for other_srl in args[i+1:]: 
                                    try:
                                        first_arg_index,semanticrolelabel = next((index,sr) for (index, sr) in enumerate(other_srl[1]) if (sr != 'O') and (sr in ['B-ARG0', 'B-ARG1', 'B-ARG2']))                                  
                                        other_verb_index = other_srl[1].index('B-V')
                                        if first_arg_index > other_verb_index :
                                            continue
                                        
                                        if first_arg_index > ARG1_span[1] and\
                                        (words[other_verb_index] in list_NPVP_in_S(tree)[1]):
                                            next_mention_start = first_arg_index
                                            next_mention_end = next_mention_start + other_srl[1].count('I'+semanticrolelabel[-5:])
                                            next_mention_pos = pos[next_mention_start]
                                            if next_mention_pos  == 'IN':
                                                next_mention_span = (next_mention_start+1,next_mention_end)
                                            else:    
                                                next_mention_span = (next_mention_start,next_mention_end)                  
                                            
                                            next_mention_coref_no = coref_id(coref,next_mention_span)
                                            break
                                    except (StopIteration, IndexError): 
                                        pass                        
    #
    #                       
                                      
                            # next_mention in next sentence
                            if next_mention_start == None:
                                tree2 = sentence2.parse_tree
                                coref2 = sentence2.coref_spans
                                pos2 = sentence2.pos_tags                             
                                next_mention = main_NPVP(tree2)[0]
                                VP2 = main_NPVP(tree2)[1]
                                next_mention_span = NPVP_spans(sentence2,next_mention,VP2)[0]
                                next_mention_pos = pos2[next_mention_span[0]]                            
                                next_mention_coref_no = coref_id(coref2,next_mention_span)  # a number or "None"
    
    
                                
                            if next_mention_coref_no == "None" and ARG0_coref == "None" and\
                            ARG1_coref == "None" :
                                number_no_reference_annotated_obj += 1
    
                            elif next_mention_coref_no == "None" :
                                number_other_coreference_obj += 1
                                total_obj += 1
                                df_icv_verbtype.append('object_biased')
                                df_icv_verb.append(lemmas[srl.index('B-V')])
                                df_icv_coreference_type.append('other')
                                df_icv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                df_icv_sentence_id.append(str(sentence.sentence_id))
                                df_icv_document_id.append(doc)
                                df_icv_proOrNot.append('other') 
                              
                                
                            elif next_mention_coref_no == ARG0_coref:
                                number_subject_coreference_obj += 1
                                total_obj += 1
                                df_icv_verbtype.append('object_biased')
                                df_icv_verb.append(lemmas[srl.index('B-V')])
                                df_icv_coreference_type.append('subject')
                                df_icv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                df_icv_sentence_id.append(str(sentence.sentence_id))
                                df_icv_document_id.append(doc)

                                if next_mention_pos == "PRP":
                                    number_pronominal_subject_obj += 1 
                                    df_icv_proOrNot.append('pronoun')
                                else:
                                    df_icv_proOrNot.append('non pronoun')
                                


                                
                                
                            elif next_mention_coref_no == ARG1_coref:
                                print('object coreference')
                                print(' '.join(words))
                                print(' '.join(sentence2.words))
                                number_object_coreference_obj += 1
                                total_obj += 1
                                df_icv_verbtype.append('object_biased')
                                df_icv_verb.append(lemmas[srl.index('B-V')])
                                df_icv_coreference_type.append('object')
                                df_icv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                df_icv_sentence_id.append(str(sentence.sentence_id))
                                df_icv_document_id.append(doc)
                                if next_mention_pos == "PRP":
                                    number_pronominal_object_obj += 1                             
                                    df_icv_proOrNot.append('pronoun')
                                else:
                                    df_icv_proOrNot.append('non pronoun')
                                
    
                            else:
                                number_other_coreference_obj += 1
                                total_obj += 1
                                df_icv_verbtype.append('object_biased')
                                df_icv_verb.append(lemmas[srl.index('B-V')])
                                df_icv_coreference_type.append('other')
                                df_icv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                df_icv_sentence_id.append(str(sentence.sentence_id))
                                df_icv_document_id.append(doc)
                                df_icv_proOrNot.append('other') 
                            
                            print("number_no_reference_annotated_obj:", number_no_reference_annotated_obj)
                            print('number_pronominal_subject_obj:',number_pronominal_subject_obj)
                            print("number_subject_coreference_obj:",number_subject_coreference_obj)
                            print('number_pronominal_object_obj:',number_pronominal_object_obj)                        
                            print("number_object_coreference_obj:",number_object_coreference_obj)
                            print("number_other_coreference_obj:",number_other_coreference_obj)
                            print('total_obj:',total_obj)
                            print('==========')
                            
    
        except StopIteration:
            break
        except (AttributeError, IndexError, TypeError, UnboundLocalError):
            pass
    
    
    d = {'document_id': df_icv_document_id,
         'verb_type': df_icv_verbtype, 
         'verb': df_icv_verb,
         'coreference_type': df_icv_coreference_type,
         'context': df_icv_context,
         'pronominalization': df_icv_proOrNot,
}
    
    df = pd.DataFrame.from_dict(data=d)
    
    # Save the DataFrame as a CSV file
    df.to_csv('../extracted_passages/icv_extracted_passages.csv', index=False)

        
    
    


