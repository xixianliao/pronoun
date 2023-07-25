# This script runs three scripts: ontonotes_result.py, ontonotes_contrast.py and
# ontonotes_narration.py. It creates a dataframe consisting of all the samples extracted 
# for the three coherence relations.

# Prior to executing the script, ensure that all the necessary libraries have been installed. 
# Additionally, verify that the working directory is set to the directory of this source file.


import pandas as pd
from nltk.stem import WordNetLemmatizer 
import ontonotes_result
import ontonotes_contrast
import ontonotes_narration


###### OntoNotes #######

if __name__ == '__main__':

    
    df_result, result_lemmas, df_result_coreference_type, df_result_context, df_result_sentence_id, df_result_document_id, df_result_proOrNot, df_result_12person, df_result_proAntecedent, df_result_subjType = ontonotes_result.main()
    df_contrast, contrast_lemmas, df_contrast_coreference_type, df_contrast_context, df_contrast_sentence_id, df_contrast_document_id, df_contrast_proOrNot, df_contrast_12person, df_contrast_proAntecedent, df_contrast_subjType = ontonotes_contrast.main()
    df_narration, narration_lemmas, df_narration_coreference_type, df_narration_context, df_narration_sentence_id, df_narration_document_id, df_narration_proOrNot, df_narration_12person, df_narration_proAntecedent, df_narration_subjType = ontonotes_narration.main()
    
    
    df_coherence_relation = df_narration + df_result + df_contrast 
    df_verb = narration_lemmas + result_lemmas + contrast_lemmas 
    df_coreference_type = df_narration_coreference_type + df_result_coreference_type + df_contrast_coreference_type 
    df_context = df_narration_context + df_result_context + df_contrast_context 
    #df_sentence_id = df_narration_sentence_id + df_result_sentence_id + df_contrast_sentence_id 
    df_document_id = df_narration_document_id + df_result_document_id + df_contrast_document_id 
    df_proOrNot = df_narration_proOrNot + df_result_proOrNot + df_contrast_proOrNot 
    df_12person =  df_narration_12person + df_result_12person + df_contrast_12person 
    df_proAntecedent = df_narration_proAntecedent + df_result_proAntecedent + df_contrast_proAntecedent 
    df_subjType = df_narration_subjType + df_result_subjType + df_contrast_subjType 
    
    
    d = {'document_id': df_document_id,
         #'sentence_id': df_sentence_id,
         'coherence_relation': df_coherence_relation, 
         'verb': df_verb,
         'coreference_type': df_coreference_type,
         'context': df_context,
         'pronominalization': df_proOrNot,
         '12person': df_12person,
         'proAntecedent': df_proAntecedent,
         'subj_antecedent_type': df_subjType}
    
    df = pd.DataFrame.from_dict(data=d)
    
    
    # coreference type
    df.loc[df['coreference_type'] == 'subject', 'subject_or_not'] = 'subject'
    df.loc[df['coreference_type'].isin(['non_subject','other']), 'subject_or_not'] = 'not_subject'
    
    df.loc[df['coreference_type'] == 'non_subject', 'non_subject_or_not'] = 'non_subject'
    df.loc[df['coreference_type'].isin(['subject','other']), 'non_subject_or_not'] = 'not_non_subject'
    
    
    # Save the DataFrame as a CSV file
    df.to_csv('../extracted_passages/ontonotes_extracted_passages.csv', index=False)












