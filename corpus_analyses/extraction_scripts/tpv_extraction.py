#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#Libraries
import pandas as pd
import OntoNotes_AllenNLP as onto

        



def coref_id (col_coref,span):
    try:
        no_ref_chain = next(x for (x,y) in col_coref if y == span)
    except StopIteration:
        no_ref_chain = 'None'
    return no_ref_chain       
    

    
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


def antecedent_span (pos, srl, ARG0_start,ARG1_start, ARG2_start):
    ARG0_end = ARG0_start + srl.count('I-ARG0')
    ARG1_end = ARG1_start + srl.count('I-ARG1')
    ARG2_end = ARG2_start + srl.count('I-ARG2')
    ARG1_pos = pos[ARG1_start] 
    ARG0_pos = pos[ARG0_start]                
    ARG2_pos = pos[ARG2_start]
    if ARG0_pos == 'IN':
        ARG0_span = (ARG0_start+1,ARG0_end)
    else:    
        ARG0_span = (ARG0_start,ARG0_end)
    if ARG1_pos == 'IN':
        ARG1_span = (ARG1_start+1,ARG1_end)
    else:    
        ARG1_span = (ARG1_start,ARG1_end)                                              
    if ARG2_pos == 'IN':
        ARG2_span = (ARG2_start+1,ARG2_end)
    else:    
        ARG2_span = (ARG2_start,ARG2_end) 
        
    return ARG0_span, ARG1_span, ARG2_span
                                        


           


if __name__ == "__main__":
    
    
    # predicate_argument structure for goal_source
        # ARG0 -> goal, ARG2 -> source
        # goal_source: index(ARG2) > index (ARG0) 
        # ARG0 get ARG1 from_ARG2, it is likely that ARG2 contains preposition
        
    
    # predicate_argument structure for source_goal
        # ARG0 -> source, ARG2 -> goal
        # source_goal: index(ARG2) > index (ARG0) 
        # ARG0 give ARG1 to_ARG2, it is likely that ARG2 contains preposition        
        
        
    
    # (verb, sense) dictionary 
    source_goal = {"bring":"1","give":"1", "hand": "1", "loan":"1",
                   "offer":"1","pass":"4","pay":"1", "rent":"1", 
                      "sell":"1", "send":"1", "show":"1", "teach":"1",                 
                    "tell":"4","throw":"2","toss":"2"
                       }

    goal_source = {"accept":"1","borrow":"1","buy":"1","catch":"1",
                   "get":"1", "grab":"1", "hear":"1", 
                       "inherit":["1","2"],"learn":"1", "purchase":"1","receive":"1",                    
                       "rent":"1", "snatch":"1", "take":"1"}
                
    
    list_verbTypes = [source_goal, goal_source]

    number_no_reference_annotated = 0
    number_source_coreference = 0
    number_goal_coreference = 0
    number_theme_coreference = 0
    number_other_coreference = 0 
    number_pronominal_source = 0
    number_pronominal_goal = 0
    total = 0


    # counter for source_goal
    number_no_reference_annotated_sg = 0    
    number_source_coreference_sg = 0
    number_goal_coreference_sg = 0
    number_theme_coreference_sg = 0
    number_other_coreference_sg = 0 
    number_pronominal_goal_sg = 0
    number_pronominal_source_sg = 0    
    total_sg = 0
    

    df_tpv_verbtype = []
    df_tpv_verb = []
    df_tpv_coreference_type = []
    df_tpv_context = []
    df_tpv_sentence_id = []
    df_tpv_document_id = []
    df_tpv_proOrNot = []


    mydata = onto.Ontonotes()
    sentences = mydata.dataset_iterator(file_path="corpora/OntoNotes")
    sentences2 = mydata.dataset_iterator(file_path="corpora/OntoNotes")
    sentence2 = next(sentences2)
    
    
    while True:
        try:
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
                
                verbs = {word_index:verb for word_index,verb in enumerate(lemmas) if verb in verb_type.keys()}              
                if len(verbs) > 0: # there are tpvs in the sentence
                   for i,predicate_args in enumerate(args):
                       if  ('B-ARG0' in predicate_args[1] and 'B-ARG1' in predicate_args[1] and 'B-ARG2' in predicate_args[1]) and\
                       (predicate_args[1].index('B-V') in verbs.keys()):
                            srl = predicate_args[1]
                            predicate = predicate_args[0]
                            mainverbs_in_sentence =  NPVP_in_S(tree)[1]
                            # pass if tpv is not a main predicate in the sentence
                            if predicate not in mainverbs_in_sentence:
                                continue
                            mainverb_start = srl.index('B-V')
                            ARG0_start = srl.index('B-ARG0')   # source in source_goal, goal in goal_source
                            ARG1_start = srl.index('B-ARG1')   # theme
                            ARG2_start = srl.index('B-ARG2')   # goal in source_goal, source in goal_source
                            
                            
    
                            if verb_type == goal_source:
                                        
                                # goal_source
                                if (ARG0_start > ARG1_start) or (ARG0_start > ARG2_start) or\
                                words[ARG2_start] == "to": 
                                    continue
        
                                 
                                ARG0_span, ARG1_span, ARG2_span = antecedent_span (pos, srl, ARG0_start,ARG1_start, ARG2_start)    
                                ARG0_coref = coref_id (coref,ARG0_span)  
                                ARG1_coref = coref_id (coref,ARG1_span)                
                                ARG2_coref = coref_id (coref,ARG2_span)
        
                
                                for ii, semrole in enumerate(srl[::-1]):                
                                    if semrole != 'O':  
                                        negative_end_index = -ii-1 # count from last to the first
                                        end_index = len(srl)+(-ii-1)                                
                                        break
                                
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
                                            if first_arg_index > ARG2_span[1] and\
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
                                ARG1_coref == "None" and ARG2_coref == "None":
                                    number_no_reference_annotated += 1
        
                                elif next_mention_coref_no == "None" :
         
                                    number_other_coreference += 1
                                    total += 1
        
                                    df_tpv_verbtype.append('goal_source')
                                    df_tpv_verb.append(lemmas[srl.index('B-V')])
                                    df_tpv_coreference_type.append('other')
                                    df_tpv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                    df_tpv_sentence_id.append(str(sentence.sentence_id))
                                    df_tpv_document_id.append(doc)
                                    df_tpv_proOrNot.append('other')
                                    
                                elif next_mention_coref_no == ARG0_coref:
                                    print('goal coreference')
                                    print(words)
                                    print(' '.join(sentence2.words))
                                    number_goal_coreference += 1
                                    total += 1
                                    df_tpv_verbtype.append('goal_source')
                                    df_tpv_verb.append(lemmas[srl.index('B-V')])
                                    df_tpv_coreference_type.append('goal')
                                    df_tpv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                    df_tpv_sentence_id.append(str(sentence.sentence_id))
                                    df_tpv_document_id.append(doc)
                                    if next_mention_pos == "PRP":
                                        number_pronominal_goal += 1 
                                        df_tpv_proOrNot.append('pronoun')
                                    else:
                                        df_tpv_proOrNot.append('non pronoun')
                                    
                                    
                                elif next_mention_coref_no == ARG1_coref:
                                    print('theme coreference')
                                    print(words)
                                    print(' '.join(sentence2.words))
                                    number_theme_coreference += 1
                                    total += 1
                                    df_tpv_verbtype.append('goal_source')
                                    df_tpv_verb.append(lemmas[srl.index('B-V')])
                                    df_tpv_coreference_type.append('theme')
                                    df_tpv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                    df_tpv_sentence_id.append(str(sentence.sentence_id))
                                    df_tpv_document_id.append(doc)
                                    df_tpv_proOrNot.append('theme')
                                    
                                elif next_mention_coref_no == ARG2_coref:
                                    print('source coreference')
                                    print(words)
                                    print(' '.join(sentence2.words))
                                    number_source_coreference += 1
                                    total += 1
                                    df_tpv_verbtype.append('goal_source')
                                    df_tpv_verb.append(lemmas[srl.index('B-V')])
                                    df_tpv_coreference_type.append('source')
                                    df_tpv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                    df_tpv_sentence_id.append(str(sentence.sentence_id))
                                    df_tpv_document_id.append(doc)                            
                                    if next_mention_pos == "PRP":
                                        number_pronominal_source += 1 
                                        df_tpv_proOrNot.append('pronoun')
                                    else:
                                        df_tpv_proOrNot.append('non pronoun')
        
                                    
                                    
                                else:
                                    number_other_coreference += 1
                                    total += 1
                                    df_tpv_verbtype.append('goal_source')
                                    df_tpv_verb.append(lemmas[srl.index('B-V')])
                                    df_tpv_coreference_type.append('other')
                                    df_tpv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                    df_tpv_sentence_id.append(str(sentence.sentence_id))
                                    df_tpv_document_id.append(doc)
                                    df_tpv_proOrNot.append('other')
                                    
                                                        
                                print('==========')
                                print('goal_source')
                                print("number_no_reference_annotated:", number_no_reference_annotated)
                                print('number_pronominal_source:',number_pronominal_source)
                                print("number_source_coreference:",number_source_coreference)
                                print('number_pronominal_goal:',number_pronominal_goal)                        
                                print("number_goal_coreference:",number_goal_coreference)
                                print("number_theme_coreference:",number_theme_coreference)
                                print("number_other_coreference:",number_other_coreference)
                                print("total:",total)
                                
                                
                            else:                                
                                
                                # source_theme_to_goal
                                if (ARG0_start > ARG1_start) or (ARG0_start > ARG2_start) or\
                                (ARG1_start > ARG2_start): 
                                    continue
                                
                                ARG0_span, ARG1_span, ARG2_span = antecedent_span (pos, srl, ARG0_start,ARG1_start, ARG2_start)    
                                ARG0_coref = coref_id (coref,ARG0_span)  
                                ARG1_coref = coref_id (coref,ARG1_span)                
                                ARG2_coref = coref_id (coref,ARG2_span)
                                                
                                                                
                                # locate where the TPV construction ends
                                for ii, semrole in enumerate(srl[::-1]):                
                                    if semrole != 'O':  
                                        negative_end_index = -ii-1 # count from last to the first
                                        end_index = len(srl)+(-ii-1)                                
                                        break
                                
                                next_mention_start = None
                                if  i != len(args)+1:                                             
                                    for other_srl in args[i+1:]: 
                                        try:
                                            first_arg_index,semanticrolelabel = next((index,sr) for (index, sr) in enumerate(other_srl[1]) if (sr != 'O') and (sr in ['B-ARG0', 'B-ARG1', 'B-ARG2']))                                  
                                            other_verb_index = other_srl[1].index('B-V')
                                            if first_arg_index > other_verb_index :
                                                continue
                                            if first_arg_index > ARG2_span[1] and\
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
                                ARG1_coref == "None" and ARG2_coref == "None":
                                    number_no_reference_annotated_sg += 1                            
                                    print('no reference annotated')
                                    print(words)
                                    print(' '.join(sentence2.words))
        
                                elif next_mention_coref_no == "None" :
                                    print('other coreference')
                                    print(words)
                                    print(' '.join(sentence2.words))
                                    number_other_coreference_sg += 1
                                    total_sg += 1
                                    df_tpv_verbtype.append('source_goal')
                                    df_tpv_verb.append(lemmas[srl.index('B-V')])
                                    df_tpv_coreference_type.append('other')
                                    df_tpv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                    df_tpv_sentence_id.append(str(sentence.sentence_id))
                                    df_tpv_document_id.append(doc)
                                    df_tpv_proOrNot.append('other')
        
                                  
                                    
                                elif next_mention_coref_no == ARG2_coref:
                                    print('goal coreference')
                                    print(words)
                                    print(' '.join(sentence2.words))                            
                                    number_goal_coreference_sg += 1
                                    total_sg += 1
                                    df_tpv_verbtype.append('source_goal')
                                    df_tpv_verb.append(lemmas[srl.index('B-V')])
                                    df_tpv_coreference_type.append('goal')
                                    df_tpv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                    df_tpv_sentence_id.append(str(sentence.sentence_id))
                                    df_tpv_document_id.append(doc)
        
                                    if next_mention_pos == "PRP":
                                        number_pronominal_goal_sg += 1 
                                        df_tpv_proOrNot.append('pronoun')
                                    else:
                                        df_tpv_proOrNot.append('non pronoun')                                
                                    
                                elif next_mention_coref_no == ARG1_coref:
                                    print('theme coreference')
                                    print(words)
                                    print(' '.join(sentence2.words))
                                    number_theme_coreference_sg += 1
                                    total_sg += 1
                                    df_tpv_verbtype.append('source_goal')
                                    df_tpv_verb.append(lemmas[srl.index('B-V')])
                                    df_tpv_coreference_type.append('theme')
                                    df_tpv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                    df_tpv_sentence_id.append(str(sentence.sentence_id))
                                    df_tpv_document_id.append(doc)
                                    df_tpv_proOrNot.append('theme')
                                    
                                    
                                elif next_mention_coref_no == ARG0_coref:
                                    print('source coreference')
                                    print(words)
                                    print(' '.join(sentence2.words))
                                    number_source_coreference_sg += 1
                                    total_sg += 1
                                    df_tpv_verbtype.append('source_goal')
                                    df_tpv_verb.append(lemmas[srl.index('B-V')])
                                    df_tpv_coreference_type.append('source')
                                    df_tpv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                    df_tpv_sentence_id.append(str(sentence.sentence_id))
                                    df_tpv_document_id.append(doc)                                    
                            
                                    if next_mention_pos == "PRP":
                                        number_pronominal_source_sg += 1 
                                        df_tpv_proOrNot.append('pronoun')
                                    else:
                                        df_tpv_proOrNot.append('non pronoun')                                
                                
                                    
                                else:
                                    number_other_coreference_sg += 1
                                    total_sg += 1
                                    df_tpv_verbtype.append('source_goal')
                                    df_tpv_verb.append(lemmas[srl.index('B-V')])
                                    df_tpv_coreference_type.append('other')
                                    df_tpv_context.append(' '.join(sentence.words) + "//" + ' '.join(sentence2.words))
                                    df_tpv_sentence_id.append(str(sentence.sentence_id))
                                    df_tpv_document_id.append(doc)
                                    df_tpv_proOrNot.append('other')                            
        
                                
                                print('==========')
                                print('source_goal')
                                print("number_no_reference_annotated_sg:", number_no_reference_annotated_sg)
                                print('number_pronominal_source_sg:',number_pronominal_source_sg)
                                print("number_source_coreference_sg:",number_source_coreference_sg)
                                print('number_pronominal_goal_sg:',number_pronominal_goal_sg)                        
                                print("number_goal_coreference_sg:",number_goal_coreference_sg)
                                print("number_theme_coreference_sg:",number_theme_coreference_sg)
                                print("number_other_coreference_sg:",number_other_coreference_sg)
                                print("total_sg:",total_sg)
                            
                            
    
               
           
        except StopIteration:
            break
        except (AttributeError, IndexError, TypeError, UnboundLocalError):
            pass
        
    
    d = {'document_id': df_tpv_document_id,
         'verb_type': df_tpv_verbtype, 
         'verb': df_tpv_verb,
         'coreference_type': df_tpv_coreference_type,
         'context': df_tpv_context,
         'pronominalization': df_tpv_proOrNot,
}
    
    df = pd.DataFrame.from_dict(data=d)
    
    # Save the DataFrame as a CSV file
    df.to_csv('../extracted_passages/tpv_extracted_passages.csv', index=False)

        
        
            

