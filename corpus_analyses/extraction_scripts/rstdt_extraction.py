#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from __future__ import print_function
import educe.rst_dt
from educe.rst_dt import ptb
import os
import re

from nltk.tree import Tree
from nltk.corpus import BracketParseCorpusReader 

from intervaltree import Interval, IntervalTree


import nltk
import string
from nltk.tokenize import word_tokenize

from nltk.stem import WordNetLemmatizer 


import pandas as pd
import sys

# XML content
import xml.etree.ElementTree as ET
import glob



############define data directory############################################

# relative to the educe docs directory
data_dir = '/Users/python/rst_discourse_treebank/data'
rst_corpus_dir = '{dd}/RSTtrees-WSJ-main-1.0/COMPLETE'.format(dd=data_dir)

# read and load the documents from the WSJ
rst_reader = educe.rst_dt.Reader(rst_corpus_dir)
rst_wsj_files = rst_reader.files(doc_glob='wsj_*')
rst_wsj_corpus = rst_reader.slurp_subcorpus(rst_wsj_files,verbose=True)


#load the penn treebank


PTB_file_location = '/Users/python/rst_discourse_treebank/PTBIII/parsed/mrg/wsj'
ptb_reader = ptb.BracketParseCorpusReader(PTB_file_location, '.*\.mrg')
    
PTB_corpus_loader = educe.rst_dt.ptb.PtbParser(PTB_file_location)


for key in rst_wsj_corpus.keys():
    doc = rst_wsj_corpus[key]
    rst_docplus = educe.rst_dt.corpus.RstDtParser(rst_corpus_dir,doc)
    docplus = rst_docplus.decode(key)    
    break
 
    
# define the path for the ARRAU corpus
arrau_path = '/Users/python/ARRAU/data'


###############locate samples from RST-DT#########################################

# preview samples of discourse relations
#def text_snippet(text):
#    "short text fragment"
#    if len(text) < 43:
#        return text
#    else:
#        return "{0}...{1}".format(text[:20], text[-20:])
#
#def preview_standoff(tystr, context, anno):
#    "simple glimpse at a standoff annotation"
#    span = anno.text_span()
#    text = context.text(span)
#    return "{tystr} at {span}:\t{snippet}".format(tystr=tystr,
#                                                 span=span,
#                                                 snippet=text_snippet(text))


def custom_word_tokenize(text):
    # Tokenize the text using nltk.word_tokenize
    tokens = nltk.word_tokenize(text)

    # Customize tokenization for "Mr."
    for i in range(len(tokens)):
        if tokens[i] == "Mr" and i < len(tokens) - 1 and tokens[i + 1] == ".":
            tokens[i] = "Mr."
            tokens[i + 1] = ""

    # Filter out empty strings
    tokens = [token for token in tokens if token]

    return tokens


def fileid_from_key(key):
    file_id = re.findall(r'\d+',str(key))[0] 
    return file_id


def text_oriente(subtree):
    index = str(subtree.text_span())
    start = int(re.findall(r'\d+',index)[0])
    end = int(re.findall(r'\d+',index)[1])
    ex_rst_txt_filename = '{corpus}/{doc}'.format(corpus=rst_corpus_dir,
                                      doc=key.doc)
    with open(ex_rst_txt_filename) as ifile:
        ex_txt = ifile.read()
    targeted_text = ex_txt[start:end] 
    return targeted_text



# show what's beneath these educe tokens
def str_tree(tree):
    if isinstance(tree, Tree):
        return Tree(str(tree.label()), map(str_tree, tree))
    else:
        return str(tree)
    
    
#upgrade simple document to documentplus, with PTB tree and tokenized texts
def doc_to_docplus(key):    
    docplus = rst_docplus.decode(key)    
    tokenized_docplus = PTB_corpus_loader.tokenize(docplus)
    final_docplus = PTB_corpus_loader.parse(tokenized_docplus)
    return final_docplus


# get parse tree for nuclear and satelite
def parseTree (nuclear_span, satelite_span, doc_parse_tree):
    trees_nodeS = []
    
    for tree in filter(None, doc_parse_tree):
        for t in tree.subtrees():
            if t.label() in ['S', 'SINV']:
                trees_nodeS.append(t)  

    t1 = None
    t2 = None

    
    for t in trees_nodeS:               
        if nuclear_span.char_start >= t.span.char_start and nuclear_span.char_end <= t.span.char_end:
            t1 = t
        if satelite_span.char_start == t.span.char_start : 
            t2 = t        
            
    return t1,t2



# find the main NP (subject) and the main VP 
def main_NPVP(tree,nuclear_span_beg,nuclear_span_end):    
    # also skip "that's because"    
    for kid in tree.subtrees():
        if kid._label in ['S','SINV']:
            for ch in range(len(kid)):
                if kid[ch]._label == 'NP' and\
                    any(st._label == 'VP' for st in list(kid.children)[ch+1:]) == True and\
                    kid[ch].span.char_start >= nuclear_span_beg and\
                    kid[ch].span.char_start < nuclear_span_end:
                        if kid[ch+1]._label == 'VP' and len(kid[ch+1]) > 1 and kid[ch+1][0]._label in ['VBZ','VBD'] and\
                        kid[ch+1][1]._label == 'SBAR' and kid[ch+1][1][0]._label == 'IN' and kid[ch+1][1][0][0].word == 'because' :
                            continue
                        else:
                            VP = next(x for x in list(kid.children)[ch+1:] if x._label == 'VP')
                            NP = kid[ch]
                            return NP,VP        
                elif (kid[ch]._label == 'VP' and len(str_tree(kid[ch]).leaves()) == 1 and kid[-1] != kid[ch] and kid[ch+1]._label == 'NP'):                                                            
                    if kid[ch].span.char_start >= nuclear_span_beg and\
                    kid[ch].span.char_start < nuclear_span_end:
                        VP = kid[ch]
                        NP = kid[ch+1]
                        return NP,VP






############## extract coref annotation from ARRAU (xml files)  #######



def word_id (fileid, target_text):
    
    """"
    return a list of word ids for a span of text
    """
      
    directory_path = arrau_path
    file_pattern = os.path.join(directory_path, '**/wsjarrau_{}_words.xml'.format(fileid)) 
    # Find matching file paths
    matching_files = glob.glob(file_pattern, recursive=True)
    file_path = matching_files[0] 

    # Read XML content from file
    with open(file_path, 'r') as file:
        xml_content = file.read()    
    # Parse XML
    root = ET.fromstring(xml_content)
    
    # Initialize an empty list to store all words with their IDs
    words_with_ids = []
    
    # Loop over all 'word' elements in the XML
    for word in root.findall('word'):
        # Get the ID attribute and the text of the word
        id = word.attrib['id']
        text = word.text
    
        # Append to the list
        words_with_ids.append((text, id))
    
    # Define the target words
    target_words = custom_word_tokenize(target_text)

    
    # Initialize a list to store the IDs
    ids = []
    
    # Search for the target phrase in the list of words with IDs
    for i in range(len(words_with_ids) - len(target_words) + 1):
        if all(words_with_ids[i + j][0] == target_word for j, target_word in enumerate(target_words)):
            ids = [words_with_ids[i + j][1] for j in range(len(target_words))]
            break
        
    if len(ids) == 0:
       # print("No matching word IDs found in the XML.")
       # print(fileid)
       # print(target_words)

        
        # Threshold of matching words to consider it a valid match
        threshold = len(target_words) * 0.5    
        
        # Iterate through the words_with_ids list
        for i in range(len(words_with_ids) - len(target_words) + 1):
            matching_words = 0
            for j in range(len(target_words)):
                if words_with_ids[i + j][0] == target_words[j]:
                    matching_words += 1
            
            # If the number of matching words exceeds the threshold, store the IDs
            if matching_words >= threshold:
                ids = [words_with_ids[i + j][1] for j in range(len(target_words))]
                break


    return ids






def get_word_indices(passage, phrase, passage_word_indices):
    
    """
    get word indices of a phrase within a passage given word index of each word in the passage
    e.g, passage = nuclear, phrase = NP
    """
    
    # Define the character span for the target NP
    phrase_start = phrase.text_span().char_start
    phrase_end = phrase.text_span().char_end
    
    # Define the target nuclear/satelite span range
    passage_start = passage.text_span().char_start
    passage_end = passage.text_span().char_end

    # text
    passage_text = text_oriente(passage)
    phrase_text = text_oriente(phrase)
   # passage_text = nuclear_text
    #phrase_text = 'Big Board traders'

    # Correct the start and end of phrase based on passage start
    relative_phrase_start = phrase_start - passage_start
    relative_phrase_end = phrase_end - passage_start

    
    # Tokenize the passage and phrase
    passage_tokens = custom_word_tokenize(passage_text)
    phrase_tokens = custom_word_tokenize(phrase_text)


    # Get the portion of passage up to the end of the phrase
    passage_up_to_phrase_end = passage_text[:relative_phrase_end]


    # Calculate the word count in the passage up to the end of the phrase
    word_count_up_to_phrase_end = len(custom_word_tokenize(passage_up_to_phrase_end))

    # The phrase might not start at the beginning of a word, so we need to handle that.
    # Get the portion of the passage up to the start of the phrase
    passage_up_to_phrase_start = passage_text[:relative_phrase_start]

    # Calculate the word count in the passage up to the start of the phrase
    word_count_up_to_phrase_start = len(custom_word_tokenize(passage_up_to_phrase_start))

    # Get the indices of words in the phrase
    phrase_word_indices = passage_word_indices[word_count_up_to_phrase_start:word_count_up_to_phrase_end]


    
    return phrase_word_indices





def extract_coref_xml(fileid):
    data_path = arrau_path
    file_pattern = os.path.join(data_path, '**/wsjarrau_{}_coref_level.xml'.format(fileid))

    # Find matching file paths
    matching_files = glob.glob(file_pattern, recursive=True)
    file_path = matching_files[0]
    tree = ET.parse(file_path)
    root = tree.getroot()

    coref_dict = {}

    # Iterate over the 'markable' elements
    for markable in root.findall('.//{www.eml.org/NameSpaces/coref}markable'):
        try:
            # markable_id = markable.attrib['id']
            span = markable.attrib['span']
            # person = markable.attrib['person']
            # gram_fnc = markable.attrib['gram_fnc']
            coref_set = markable.attrib['coref_set']

            # Add the span and coref set to the dictionary
            coref_dict[span] = coref_set

            # Print the extracted attributes
            # print("Markable ID:", markable_id)
            # print("Span:", span)
            # print("Person:", person)
            # print("Grammatical Function:", gram_fnc)
            # print("Coref Set:", coref_set)
            # print(file_path)
            # print()
        except KeyError:
            pass

    return coref_dict




def coref_set_phrase (coref_dic, phrase_wordID):
    """
    extract the coref set for NP from a coref dictionary for the whole document
    """
    
    if len(phrase_wordID)  == 1:
        try:
            phrase_corefSet = coref_dic[phrase_wordID[0]]
        except KeyError:
            # next-mention wrongly detected in file 1193 "Yesterday, just a day after Mr. Roman announced he would leave to take a top post at American Express,"
            # next-mention word ID wrongly extracted in file 1322 "On the other hand, we have economic news that is {expected to be} relatively positive for the bond market."
            if phrase_wordID == ['word_525'] or phrase_wordID == ['word_387']:
                phrase_corefSet = 'no set'
            # next-mention word ID wrongly identified in file 1163 "And Gorky, considered the father of Soviet socialist realism."
            if phrase_wordID == ['word_222']:
                phrase_corefSet = coref_dic['word_222..word_230']
            # file 1376
            if phrase_wordID == ['word_797']:
                phrase_corefSet = coref_dic['word_799']
            # file 1367
            if phrase_wordID == ['word_323']:
                phrase_corefSet = coref_dic['word_324']
            # file 0632
            if phrase_wordID == ['word_81']:
                phrase_corefSet = coref_dic['word_82']   
            # file 1318
            if phrase_wordID == ['word_177']:
                phrase_corefSet = coref_dic['word_176']       
            # file 1150
            if phrase_wordID == ['word_60']:
                phrase_corefSet = coref_dic['word_63']   

                
                
    elif len(phrase_wordID) > 1:
        try:
            key = phrase_wordID[0] + ".." + phrase_wordID[-1]
            phrase_corefSet = coref_dic[key]
        except KeyError:
            try:
                # sometimes, the end of our NP contains punctuation whereas Arrau's mention does not
                key = phrase_wordID[0] + ".." + phrase_wordID[-2]
                phrase_corefSet = coref_dic[key]
            except KeyError:
                try:
                    # sometimes, the word ID boundaries are a bit different
                    key = phrase_wordID[1] + ".." + phrase_wordID[-1]
                    phrase_corefSet = coref_dic[key]
                except KeyError:
                    # file 1337
                    if phrase_wordID[0] == 'word_931' and phrase_wordID[-1] == 'word_943':
                        phrase_corefSet = coref_dic['word_930..word_941']
                    # there is a NP wrongly detected in file 2358 "when a steep run-up in world oil prices sent the index surging at double-digit annual rates."
                    if phrase_wordID == ['word_416', 'word_417', 'word_418', 'word_419', 'word_420', 'word_421', 'word_422']:
                        phrase_corefSet = 'no set'
                    # NP wordIDs boundaried wrongly identified in file 1163 "Ms. Bogart, an acclaimed creator of deconstructed dramatic collages that tear into such sacred texts as Rodgers and Hammerstein's "South Pacific,""
                    if phrase_wordID == ['word_25', 'word_26', 'word_27', 'word_28', 'word_29', 'word_30', 'word_31', 'word_32', 'word_33', 'word_34', 'word_35', 'word_36', 'word_37', 'word_38', 'word_39', 'word_40', 'word_41', 'word_42', 'word_43', 'word_44', 'word_45', 'word_46', 'word_47', 'word_48', 'word_49', 'word_50']:
                        phrase_corefSet = coref_dic['word_25..word_48']
                    # NP wordIDs boundaried wrongly identified in file 1963 "But the effort became snagged on the question of what would""
                    if phrase_wordID == ['word_73', 'word_74']:
                        phrase_corefSet = coref_dic['word_72..word_73']
                    # long NP in file 1158    
                    if phrase_wordID == ['word_179', 'word_180']:
                        phrase_corefSet = coref_dic['word_159..word_180'] 
                    # file 2343
                    if phrase_wordID == ['word_31', 'word_32']:
                        phrase_corefSet = coref_dic['word_33..word_34']   
                    if phrase_wordID == ['word_309', 'word_310', 'word_311', 'word_312', 'word_313']:
                        phrase_corefSet = coref_dic['word_309..word_329']  
                    # file 1373
                    if phrase_wordID == ['word_139', 'word_140', 'word_141', 'word_142', 'word_143', 'word_144', 'word_145']:
                        phrase_corefSet = coref_dic['word_138..word_143']  
                    # file 1366
                    if phrase_wordID[0] == 'word_673' and phrase_wordID[-1] == 'word_682' :
                        phrase_corefSet = coref_dic['word_676..word_683']
                    # file 1136
                    if phrase_wordID[0] == 'word_290' and phrase_wordID[-1] == 'word_295' :
                        phrase_corefSet = coref_dic['word_292..word_295']
                    if phrase_wordID[0] == 'word_322' and phrase_wordID[-1] == 'word_329' :
                        phrase_corefSet = coref_dic['word_324..word_330']                    
                    # file 0692
                    if phrase_wordID[0] == 'word_837' and phrase_wordID[-1] == 'word_848' :
                        phrase_corefSet = coref_dic['word_837..word_846']
                    # file 0688
                    if phrase_wordID[0] == 'word_328' and phrase_wordID[-1] == 'word_354' :
                        phrase_corefSet = coref_dic['word_329..word_353']        
                    # file 2354
                    if phrase_wordID[0] == 'word_93' and phrase_wordID[-1] == 'word_98' :
                        phrase_corefSet = coref_dic['word_95..word_98']     
                        
                    # file 1102
                    if phrase_wordID[0] == 'word_59' and phrase_wordID[-1] == 'word_67' :
                        phrase_corefSet = coref_dic['word_59..word_60']  
                    # file 0604
                    if phrase_wordID[0] == 'word_394' and phrase_wordID[-1] == 'word_399' :
                        phrase_corefSet = coref_dic['word_395..word_398']    
                    # file 2320
                    if phrase_wordID[0] == 'word_385' and phrase_wordID[-1] == 'word_387' :
                        phrase_corefSet = coref_dic['word_387']      
                    # file 1158
                    if phrase_wordID[0] == 'word_520' and phrase_wordID[-1] == 'word_525' :
                        phrase_corefSet = 'no set'             
                    if phrase_wordID[0] == 'word_619' and phrase_wordID[-1] == 'word_623' :
                        phrase_corefSet = coref_dic['word_621..word_625']     
                    # file 0632    
                    if phrase_wordID[0] == 'word_111' and phrase_wordID[-1] == 'word_122' :
                        phrase_corefSet = coref_dic['word_112..word_121']    
                    # file 2358
                    if phrase_wordID[0] == 'word_833' and phrase_wordID[-1] == 'word_838' :
                        phrase_corefSet = coref_dic['word_835..word_838']       
                    # file 1398
                    if phrase_wordID[0] == 'word_49' and phrase_wordID[-1] == 'word_50' :
                        phrase_corefSet = coref_dic['word_47..word_50']   
                    # file 1150
                    if phrase_wordID[0] == 'word_43' and phrase_wordID[-1] == 'word_55' :
                        phrase_corefSet = coref_dic['word_42..word_55']                       
                    
                    
    else: 
        phrase_corefSet = 'No phrase wordID provided'

    return phrase_corefSet



def coref_set_nonSubjects (coref_dic, phrase_wordID, passage_word_indices):
    
    """
    extract the coref_set for all the non-subject referents
    """
    
    phrase_wordID_start = int(phrase_wordID[0].split('_')[-1])
    phrase_wordID_end = int(phrase_wordID[-1].split('_')[-1])
    passage_wordID_start = int(passage_word_indices[0].split('_')[-1])
    passage_wordID_end = int(passage_word_indices[-1].split('_')[-1])
    non_subject_corefSet = []
    

    for key,value in coref_dic.items(): 
        pattern = r'\d+'
        wordID = re.findall(pattern, key)
        wordID = [int(item) for item in wordID]

        try:
            if all(passage_wordID_start < number < phrase_wordID_start for number in wordID) or\
                all(phrase_wordID_end < number < passage_wordID_end for number in wordID):
                    non_subject_corefSet.append(value)
        except TypeError:
            print("passage_wordID_end:", passage_wordID_end)
            print("wordID:", wordID)

    
    return non_subject_corefSet


############### for the analysis #############################        

#lemma of the main verb in the first sentence
def main_verb(VP):
    lemmatizer = WordNetLemmatizer()
    for st in VP.subtrees():
        if st.label() == 'VP':
            children = [i for i in st if i.label()=='VP']
            if len(children) == 0:
                for sst in st.subtrees():
                    if sst.label().startswith('V') and sst.height() == 2:
                        verb = sst.leaves()[0]
                        lemma=lemmatizer.lemmatize(verb.word,'v')
                        return lemma
    


#whether the antecedent is first- second person pronoun or not 
def first_second_person_pronoun (NP):
    first_second_pronouns = ['I','me','Me','my','My','you','You','your','Your','we','We','Our','our','us','Us']
    NP_leaves = NP.leaves()
    return (len(NP_leaves) == 1 and NP_leaves[0].word in first_second_pronouns)



#whether antecedent is a pronoun or not
def phrase_is_pronoun(NP):
    NP_leaves = NP.leaves()
    return (len(NP_leaves) == 1 and NP_leaves[0].tag == 'PRP')
    

  
  


        
 #################################################################   
 
if __name__ == '__main__':
    
           
    

    narration = ['temporal-before', 'temporal-after', 'temporal-same-time',
                'Sequence', 'Inverted-Sequence']
    
    contrast = ['Contrast', 'concession', 'antithesis']
    
    result = {'Cause-Result': 'NN',
                        'Consequence': 'NN',
                        'explanation-argumentative': 'SN',
                        'result': 'SN',
                        'consequence-s': 'NS',
                        'consequence-n': 'SN',
                        'evidence': 'SN',
                        'reason': 'SN',
                        'cause': 'NS'}
    

    
    list_relations = {'narration': narration, 'contrast': contrast, 'result': result}
    
    
     
 
    subject_coref_n = 0
    non_subject_coref_n = 0
    subejct_nm_pro_n = 0
    non_subejct_nm_pro_n = 0
    valid = 0
    errors =[]
    errors_n = 0
    
    df_docID = []
    df_passage=[]
    df_relation = []
    df_rstdt_relation = []
    df_rstdt_nuclearity = []
    df_lemma = []
    df_coref_type = []
    df_nm_pro = []
    df_12person = [] # only check for subject antecedent 
    df_proAntecedent = []  # only check for subject antecedent 
    
  
    
    for relation_name, relation_category  in list_relations.items():
    
        for key in rst_wsj_corpus.keys():
                        
            fileid = fileid_from_key(key)   # e.g., 1106
            ex_doc = rst_wsj_corpus[key] 
            ex_subtree = ex_doc
            ex_context = ex_doc.label().context
            
        
            for tree in ex_subtree.subtrees():
                simple_tree = educe.rst_dt.SimpleRSTTree.from_rst_tree(tree)     # easier representation of RST trees to work with   
                node = simple_tree.label()
                if type(relation_category) == list and node.rel in relation_category or\
                type(relation_category) == dict and node.rel in relation_category and node.nuclearity == relation_category[node.rel]:
                    ex_docplus = doc_to_docplus(key)
                    ex_tokens = ex_docplus.tkd_tokens                        
                    ex_parse_tree = ex_docplus.tkd_trees
                    nuclear = simple_tree._members()[0]
                    satelite = simple_tree._members()[1]
                    nuclear_span = nuclear.label().span
                    satelite_span = satelite.label().span  
                    nuclear_text = text_oriente(nuclear)
                    satelite_text = text_oriente(satelite)
                    
                    t,t2 = parseTree (nuclear_span, satelite_span, ex_parse_tree)
        
                        # both of the two spans contain NP and VP 
                    if t != None and t2 != None and main_NPVP(t,nuclear_span.char_start,nuclear_span.char_end) != None and\
                    main_NPVP(t2,satelite_span.char_start,satelite_span.char_end) != None:                                
                        
                        try:   
                            
                            valid += 1
                            
                            full_context = nuclear_text + "//" + satelite_text
        
                            
                            print('__________________')
                            print('valid:',valid)  
                            print(node.rel)
                            print(nuclear_text)
                            print()
                            print(satelite_text)                                          
                                
                            
                            NP = main_NPVP(t,nuclear_span.char_start,nuclear_span.char_end)[0]  
                            VP = main_NPVP(t,nuclear_span.char_start,nuclear_span.char_end)[1]
                            lemma = main_verb(VP)
                            NP_leaves = str_tree(NP).leaves()
                            VP_leaves = str_tree(VP).leaves()                    
                            nuclear_word_indices = word_id (fileid, nuclear_text)
                            
                            coref_dic = extract_coref_xml(fileid)
                            NP_wordID = get_word_indices(nuclear, NP, nuclear_word_indices)
                            # get coref set number for the subject 
                            subj_coref = coref_set_phrase (coref_dic, NP_wordID)
                            # get coref set number for all non-subject markables
                            non_subjs_coref = coref_set_nonSubjects (coref_dic, NP_wordID, nuclear_word_indices)
                                                                                                                                                
                              # coref of next mention
                            satelite_word_indices = word_id (fileid, satelite_text)
                            next_mention = main_NPVP(t2,satelite_span.char_start,satelite_span.char_end)[0] 
                            NM_leaves = str_tree(next_mention).leaves()
                            NM_wordID = get_word_indices(satelite, next_mention, satelite_word_indices)
                            NM_coref = coref_set_phrase (coref_dic, NM_wordID)  
                            
                            df_docID.append('wsj_'+fileid)
                            df_passage.append(full_context)
                            df_relation.append(relation_name)
                            df_rstdt_relation.append(node.rel)
                            df_rstdt_nuclearity.append(node.nuclearity)
                            df_lemma.append(lemma)
                            df_nm_pro.append('pronoun' if phrase_is_pronoun(next_mention) else 'non pronoun') 
                                    
                            if any(char.isdigit() for char in NM_coref) and any(char.isdigit() for char in subj_coref) and NM_coref == subj_coref:
                                subject_coref_n +=1
                                df_coref_type.append('subject')
                                
                                
                                if phrase_is_pronoun(next_mention) == True: 
                                    subejct_nm_pro_n += 1
                                        
                                df_12person.append('TRUE' if first_second_person_pronoun(NP) else 'FALSE')    
                                df_proAntecedent.append('TRUE' if phrase_is_pronoun(NP) else 'FALSE')
        
        
                            elif any(char.isdigit() for char in NM_coref) and len(non_subjs_coref) >0 and any(i for i in non_subjs_coref if i == NM_coref):
        
                                non_subject_coref_n += 1
                                df_coref_type.append('non_subject')
                                df_12person.append('non_subject')
                                df_proAntecedent.append('non_subject')
                                                   
                                if phrase_is_pronoun(next_mention) == True: 
                                    non_subejct_nm_pro_n += 1
                            else:
                                df_coref_type.append('other')
                                df_12person.append('other')
                                df_proAntecedent.append('other')                                
                                    
                        except (TypeError,IndexError, UnboundLocalError):                            
                            errors.append('...................')
                            errors_n += 1
                            print("errors_n:",errors_n)
                            errors.append(fileid)
                            errors.append(nuclear_span)
                            errors.append(satelite_span)
                            errors.append(NP_leaves)
                            errors.append(NP_wordID)
                            errors.append(NM_leaves)
                            errors.append(NM_wordID)
                            errors.append(text_oriente(nuclear))
                            errors.append(text_oriente(satelite))                                           
                                              
        
        
    
    d2 = {'document_id': df_docID,
         'coherence_relation': df_relation, 
         'rstdt_relation': df_rstdt_relation,
         'rstdt_nuclearity': df_rstdt_nuclearity,
         'verb': df_lemma,
         'coreference_type': df_coref_type,
         'context': df_passage,
         'pronominalization': df_nm_pro,
         '12person': df_12person,
         'proAntecedent': df_proAntecedent}
    
    df2 = pd.DataFrame.from_dict(data=d2)
    

    
    # coreference type
    df2.loc[df2['coreference_type'] == 'subject', 'subject_or_not'] = 'subject'
    df2.loc[df2['coreference_type'].isin(['non_subject','other']), 'subject_or_not'] = 'not_subject'
    
    df2.loc[df2['coreference_type'] == 'non_subject', 'non_subject_or_not'] = 'non_subject'
    df2.loc[df2['coreference_type'].isin(['subject','other']), 'non_subject_or_not'] = 'not_non_subject'
        
    
    # Save the DataFrame as a CSV file
    df2.to_csv('rstdt_extracted_passages.csv', index=False)




