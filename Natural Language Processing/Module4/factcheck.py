# factcheck.py

import torch
from typing import List
import numpy as np
import spacy
import gc
import nltk
#from sklearn.feature_extraction.text import TfidfVectorizer
from nltk.translate.bleu_score import corpus_bleu
#import pandas as pd
from nltk.tokenize import word_tokenize
#import evaluate
from nltk.tokenize import sent_tokenize

from nltk.corpus import stopwords
nltk.download('stopwords')
nltk.download('punkt')
class FactExample:
    """
    :param fact: A string representing the fact to make a prediction on
    :param passages: List[dict], where each dict has keys "title" and "text". "title" denotes the title of the
    Wikipedia page it was taken from; you generally don't need to use this. "text" is a chunk of text, which may or
    may not align with sensible paragraph or sentence boundaries
    :param label: S, NS, or IR for Supported, Not Supported, or Irrelevant. Note that we will ignore the Irrelevant
    label for prediction, so your model should just predict S or NS, but we leave it here so you can look at the
    raw data.
    """
    def __init__(self, fact: str, passages: List[dict], label: str):
        self.fact = fact
        self.passages = passages
        self.label = label

    def __repr__(self):
        return repr("fact=" + repr(self.fact) + "; label=" + repr(self.label) + "; passages=" + repr(self.passages))


class EntailmentModel:
    def __init__(self, model, tokenizer):
        self.model = model
        self.tokenizer = tokenizer

    def check_entailment(self, premise: str, hypothesis: str):
        with torch.no_grad():
            # Tokenize the premise and hypothesis
            inputs = self.tokenizer(premise, hypothesis, return_tensors='pt', truncation=True, padding=True)
            # Get the model's prediction
            outputs = self.model(**inputs)
            logits = outputs.logits

        # Note that the labels are ["entailment", "neutral", "contradiction"]. There are a number of ways to map
        # these logits or probabilities to classification decisions; you'll have to decide how you want to do this.
        #print("LOGITS")
        #print(logits)
        probs=torch.softmax(logits,dim=1)
        #print("PROBS")
        #print(probs)
        #maximum=torch.argmax(probs)
        #maximum=maximum.numpy()
        #print("LOGITS")
        #probs=torch.softmax(logits)
        #print(logits)
        #prob_entail=logit2prob(logits[0][0])
        #prob_contradiction=logit2prob(logits[0][2])
        #prob_neutral=logit2prob(logits[0][1])
        #if maximum==0:
        #    return True
        #else:
        #    return False 
        #print("PROB ENTAIL")
        #print(prob_entail)
        #if maximum==0 or (prob_entail>=0.5 and prob_contradiction<0.05) or (prob_neutral>=0.5 and prob_contradiction<0.3):
        #    entail=True 
        #else:
        #    entail=False 
        
        #print("prob entail",prob_entail,"prob neutral",prob_neutral,"prob contradiction",prob_contradiction)
        #if (prob_entail>=0.2 and prob_neutral<0.5) or (prob_entail>=0.2 and prob_contradiction<0.2 and prob_neutral<0.9)  :
        #    entail=True 
        #else:
        #    entail=False 
       

        #raise Exception("Not implemented")

        # To prevent out-of-memory (OOM) issues during autograding, we explicitly delete
        # objects inputs, outputs, logits, and any results that are no longer needed after the computation.
        entail_l=logits[0][0]
        #print("ENTAIL_L")
        #print(entail_l)
        entail_l=entail_l.numpy()
        
        neutral_l=logits[0][1]
        neutral_l=neutral_l.numpy()
        
        cont_l=logits[0][2]
        cont_l=cont_l.numpy()
        
        
        del inputs, outputs, logits
        gc.collect()
        prob_entail=probs[0][0]
        prob_entail=prob_entail.numpy()
        
        prob_neutral=probs[0][1]
        prob_neutral=prob_neutral.numpy()
        
        prob_cont=probs[0][2]
        prob_cont=prob_cont.numpy()
        
        
        
        #print("ENTAIL")
        #print(prob_entail)
        #return entail_l,neutral_l, cont_l
        return prob_entail,prob_neutral, prob_cont



class FactChecker(object):
    """
    Fact checker base type
    """

    def predict(self, fact: str, passages: List[dict]) -> str:
        """
        Makes a prediction on the given sentence
        :param fact: same as FactExample
        :param passages: same as FactExample
        :return: "S" (supported) or "NS" (not supported)
        """
        raise Exception("Don't call me, call my subclasses")


class RandomGuessFactChecker(object):
    def predict(self, fact: str, passages: List[dict]) -> str:
        prediction = np.random.choice(["S", "NS"])
        return prediction


class AlwaysEntailedFactChecker(object):
    def predict(self, fact: str, passages: List[dict]) -> str:
        return "S"

def remove_stp_words(words:List[str])->List[str]:
    forbidden=["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","&","'d", "'ll", "'re", "'s", "'ve", 'could', 'might', 'must', "n't", 'need', 'sha', 'wo', 'would',"''","co-writer\\/director","writer\\/director","having","had","-lrb-","-rrb-" ,"x.","#","$","'m","'d","him","me","my","'ll","'re","\\*","\\*\\*\\*","itself","your","they","he","'`'","will","'`'","cgi","just","some","from","all","if","but","like","there","what","about","ms.","her","their","too","into","or","are","up","'`'","has","so","his","have","j.","with","\\/","x","one","this","himself","were","'ve","we","'`'","``","'","be","by","n't","at","us","you","i","mr.","--","two","as","-","for","...","its","an","in","that","on","of","the","to","it","is","The","a","A","And","and",",", "!","?","''","'s",".",":",";","}","{"]
    #"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","&"
    #forbidden.extend(stopwords.words('english'))
    #forbidden=stopwords.words('english')
    #forbidden=["'d", "'ll", "'re", "'s", "'ve", 'could', 'might', 'must', "n't", 'need', 'sha', 'wo', 'would',".",":",";","}","{"]

    letters_and_numbers=["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","1","2","3","4","5","6","7","8","9","0","10"]
    stp_words=stopwords.words('english')#forbidden.extend(stopwords.words('english'))
    words2=[]
    for i in range(len(words)):
        #words[i] not in stp_words 
        if words[i] not in stp_words and words[i] not in forbidden:
            not_add=0
            for letter in words[i]:
                if letter not in letters_and_numbers:
                    not_add=1
                    break
            if not_add!=1:   
                if words[i][-1]==".":
                    words2.append(words[i][:-1])
                else:    
                    words2.append(words[i])
        
    return words2


    

class WordRecallThresholdFactChecker(object):
    def predict(self, fact: str, passages: List[dict]) -> str:
        #rouge = evaluate.load('rouge')
        #forbidden=['*', '\\', '`', 'mr', 'ms',"''","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","&","co-writer\\/director","writer\\/director","having","had","-lrb-","-rrb-" ,"x.","#","$","'m","'d","him","me","my","'ll","'re","\\*","\\*\\*\\*","itself","your","they","he","'`'","will","'`'","cgi","just","some","from","all","if","but","like","there","what","about","ms.","her","their","too","into","or","are","up","'`'","has","so","his","have","j.","with","\\/","x","one","this","himself","were","'ve","we","'`'","``","'","be","by","n't","at","us","you","i","mr.","--","two","as","-","for","...","its","an","in","that","on","of","the","to","it","is","The","a","A","And","and",",", "!","?","''","'s",".",":",";","}","{"]
        #forbidden.extend(stopwords.words('english'))
        #forbidden=stopwords.words('english')
        
        
        forbidden=["'d", "'ll", "'re", "'s", "'ve", 'could', 'might', 'must', "n't", 'need', 'sha', 'wo', 'would',".",":",";","}","{"]
        #".",":",";","}","{"
        #forbidden=["'d", "'ll", "'re", "'s", "'ve", 'could', 'might', 'must', "n't", 'need', 'sha', 'wo', 'would',"''","co-writer\\/director","writer\\/director","having","had","-lrb-","-rrb-" ,"x.","#","$","'m","'d","him","me","my","'ll","'re","\\*","\\*\\*\\*","itself","your","they","he","'`'","will","'`'","cgi","just","some","from","all","if","but","like","there","what","about","ms.","her","their","too","into","or","are","up","'`'","has","so","his","have","j.","with","\\/","x","one","this","himself","were","'ve","we","'`'","``","'","be","by","n't","at","us","you","i","mr.","--","two","as","-","for","...","its","an","in","that","on","of","the","to","it","is","The","a","A","And","and",",", "!","?","''","'s",".",":",";","}","{"]

        forbidden.extend(stopwords.words('english'))
        fact2=fact
        l_fact=len(fact2)
        fact=word_tokenize(fact.lower())
        fact=remove_stp_words(fact)
        another_set=set()
        s_fact=set(fact)
        #print("FACT SET")
        #print(s_fact)
        all_passages=set()
        all_texts=[]
        sim_list=[]
        sim_rouge=[]
        for i in range (len(passages)):
            text=passages[i]["text"].lower()
            all_texts.append(text)
            text=word_tokenize(text)
            text=remove_stp_words(text)
            #print(text)
            #txt_to_add=""
            #for word in text:
            #    txt_to_add=txt_to_add+" "+word
            #print(txt_to_add)
            all_passages=all_passages.union(set(text))

            jaccard_similarity=len(s_fact.intersection(set(text)))/len(s_fact.union(set(text)))
            sim_list.append(jaccard_similarity)
            
            
            #sim_rouge.append(rouge)
        
        #rouge=rouge.compute(predictions=fact, references=passages)
        score = corpus_bleu([all_texts],[fact2], weights=(3, 1,1,1))

        all_texts.append(fact2.lower())
        #tfidf_vectorizer = TfidfVectorizer(use_idf=True,tokenizer=word_tokenize,ngram_range=(1, 1), stop_words=forbidden, strip_accents = "ascii",norm="l2")
        #X = tfidf_vectorizer.fit_transform(all_texts)
        #df = pd.DataFrame(X.toarray(), columns=tfidf_vectorizer.get_feature_names_out())
        #X_fact=tfidf_vectorizer.fit_transform([fact2])
        #df_fact = pd.DataFrame(X_fact.toarray(), columns=tfidf_vectorizer.get_feature_names_out())
        #print("df ")
        #print(df)
        #print("df second row ")
        
        #print(df.columns)
        #last_column=df_fact.columns[-1]
        
        #cos_sim=[]    
        #for i in range(len(passages)-1):
        #    cos=np.dot(np.squeeze(np.array(df.iloc[[i]])),np.squeeze(np.array(df.iloc[[-1]])))/(len(all_texts[1])*l_fact)
        #    cos_sim.append(cos)
        jaccard_similarity_all=len(s_fact.intersection(all_passages))/len(s_fact.union(all_passages))
        j_MODified=len(s_fact.intersection(all_passages))/len(s_fact)
        #print("SCORE",score)
        #cos_max=max(cos_sim)
        sim_max=max(sim_list)  
        m_slist=np.mean(sim_list)
        #m_cos=np.mean(cos_sim)
        #sim_max>0.0253    0.6877828054298643
        #(m_slist>=0.007)and 
        #jaccard_similarity_all>0.0046     0.6063348416289592
        #print(sim_rouge)
        
        #jaccard_similarity_all>0.0046 and sim_max>0.0253 0.6923076923076923
        #jaccard_similarity_all>0.005 and sim_max>0.0253    0,696
        #jaccard_similarity_all>0.0055 and sim_max>0.0253    0.7013574660633484
        #jaccard_similarity_all>0.006 and sim_max>0.0253    7013574660633484
        # cos_max>1.4e-6 and jaccard_similarity_all>0.006 and sim_max>0.025 Accuracy: 156/221 = 0.7058823529411765
        #cos_max>1.7e-6 and jaccard_similarity_all>0.006 and sim_max>0.025   ACC 0.7104072398190046
        #cos_max>2.1e-6 and jaccard_similarity_all>0.006 and sim_max>0.025:   acc  0.7149321266968326
        #cos_max>2.3e-6 and jaccard_similarity_all>0.006 and sim_max>0.025:  : acc 0.7194570135746606 
        # cos_max>2.9e-6 and jaccard_similarity_all>0.006 and sim_max>0.025    0.7330316742081447
        #cos_max>2.8e-6 and jaccard_similarity_all>0.006 and sim_max>0.025:    0.7375565610859729
            
        t_cs=3.0e-6
        t_j_s_as=0.006
        t_s_ms=0.0217
        #and cos_max>t_cs
        if  j_MODified>0.5  and jaccard_similarity_all>t_j_s_as and sim_max>t_s_ms: 
            return "S"
        else:
            return "NS"

def overlap(fact,passages):
    #rouge = evaluate.load('rouge')
    #forbidden=['*', '\\', '`', 'mr', 'ms',"''","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","&","co-writer\\/director","writer\\/director","having","had","-lrb-","-rrb-" ,"x.","#","$","'m","'d","him","me","my","'ll","'re","\\*","\\*\\*\\*","itself","your","they","he","'`'","will","'`'","cgi","just","some","from","all","if","but","like","there","what","about","ms.","her","their","too","into","or","are","up","'`'","has","so","his","have","j.","with","\\/","x","one","this","himself","were","'ve","we","'`'","``","'","be","by","n't","at","us","you","i","mr.","--","two","as","-","for","...","its","an","in","that","on","of","the","to","it","is","The","a","A","And","and",",", "!","?","''","'s",".",":",";","}","{"]
    #forbidden.extend(stopwords.words('english'))
    #forbidden=stopwords.words('english')
    
    
    forbidden=["'d", "'ll", "'re", "'s", "'ve", 'could', 'might', 'must', "n't", 'need', 'sha', 'wo', 'would',".",":",";","}","{"]
    #".",":",";","}","{"
    #forbidden=["'d", "'ll", "'re", "'s", "'ve", 'could', 'might', 'must', "n't", 'need', 'sha', 'wo', 'would',"''","co-writer\\/director","writer\\/director","having","had","-lrb-","-rrb-" ,"x.","#","$","'m","'d","him","me","my","'ll","'re","\\*","\\*\\*\\*","itself","your","they","he","'`'","will","'`'","cgi","just","some","from","all","if","but","like","there","what","about","ms.","her","their","too","into","or","are","up","'`'","has","so","his","have","j.","with","\\/","x","one","this","himself","were","'ve","we","'`'","``","'","be","by","n't","at","us","you","i","mr.","--","two","as","-","for","...","its","an","in","that","on","of","the","to","it","is","The","a","A","And","and",",", "!","?","''","'s",".",":",";","}","{"]

    forbidden.extend(stopwords.words('english'))
    fact2=fact
    l_fact=len(fact2)
    fact=word_tokenize(fact.lower())
    fact=remove_stp_words(fact)
    another_set=set()
    s_fact=set(fact)
    #print("FACT SET")
    #print(s_fact)
    all_passages=set()
    all_texts=[]
    sim_list=[]
    sim_rouge=[]
    for i in range (len(passages)):
        text=passages[i]["text"].lower()
        all_texts.append(text)
        text=word_tokenize(text)
        text=remove_stp_words(text)
        #print(text)
        #txt_to_add=""
        #for word in text:
        #    txt_to_add=txt_to_add+" "+word
        #print(txt_to_add)
        all_passages=all_passages.union(set(text))

        jaccard_similarity=len(s_fact.intersection(set(text)))/len(s_fact.union(set(text)))
        sim_list.append(jaccard_similarity)
        
        
        #sim_rouge.append(rouge)
    
    #rouge=rouge.compute(predictions=fact, references=passages)
    score = corpus_bleu([all_texts],[fact2], weights=(3, 1,1,1))

    all_texts.append(fact2.lower())
    #tfidf_vectorizer = TfidfVectorizer(use_idf=True,tokenizer=word_tokenize,ngram_range=(1, 1), stop_words=forbidden, strip_accents = "ascii",norm="l2")
    #X = tfidf_vectorizer.fit_transform(all_texts)
    #df = pd.DataFrame(X.toarray(), columns=tfidf_vectorizer.get_feature_names_out())
    #X_fact=tfidf_vectorizer.fit_transform([fact2])
    #df_fact = pd.DataFrame(X_fact.toarray(), columns=tfidf_vectorizer.get_feature_names_out())
    #print("df ")
    #print(df)
    #print("df second row ")
    
    #print(df.columns)
    #last_column=df_fact.columns[-1]
    
    #cos_sim=[]    
    #for i in range(len(passages)-1):
    #    cos=np.dot(np.squeeze(np.array(df.iloc[[i]])),np.squeeze(np.array(df.iloc[[-1]])))/(len(all_texts[1])*l_fact)
    #    cos_sim.append(cos)
    jaccard_similarity_all=len(s_fact.intersection(all_passages))/len(s_fact.union(all_passages))
    j_MODified=len(s_fact.intersection(all_passages))/len(s_fact)
    #print("SCORE",score)
    #cos_max=max(cos_sim)
    sim_max=max(sim_list)  
    m_slist=np.mean(sim_list)
    #m_cos=np.mean(cos_sim)
    #sim_max>0.0253    0.6877828054298643
    #(m_slist>=0.007)and 
    #jaccard_similarity_all>0.0046     0.6063348416289592
    #print(sim_rouge)
    
    #jaccard_similarity_all>0.0046 and sim_max>0.0253 0.6923076923076923
    #jaccard_similarity_all>0.005 and sim_max>0.0253    0,696
    #jaccard_similarity_all>0.0055 and sim_max>0.0253    0.7013574660633484
    #jaccard_similarity_all>0.006 and sim_max>0.0253    7013574660633484
    # cos_max>1.4e-6 and jaccard_similarity_all>0.006 and sim_max>0.025 Accuracy: 156/221 = 0.7058823529411765
    #cos_max>1.7e-6 and jaccard_similarity_all>0.006 and sim_max>0.025   ACC 0.7104072398190046
    #cos_max>2.1e-6 and jaccard_similarity_all>0.006 and sim_max>0.025:   acc  0.7149321266968326
    #cos_max>2.3e-6 and jaccard_similarity_all>0.006 and sim_max>0.025:  : acc 0.7194570135746606 
    # cos_max>2.9e-6 and jaccard_similarity_all>0.006 and sim_max>0.025    0.7330316742081447
    #cos_max>2.8e-6 and jaccard_similarity_all>0.006 and sim_max>0.025:    0.7375565610859729
        
    t_cs=3.0e-6
    t_j_s_as=0.006
    t_s_ms=0.0217
    #and cos_max>t_cs
    return j_MODified



def logit2prob(logit):
    odds= np.exp(logit)
    prob=odds / (1 + odds)
    return(prob)



def remove_stp_words2(words:List[str])->List[str]:
    #forbidden=["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","&","'d", "'ll", "'re", "'s", "'ve", 'could', 'might', 'must', "n't", 'need', 'sha', 'wo', 'would',"''","co-writer\\/director","writer\\/director","having","had","-lrb-","-rrb-" ,"x.","#","$","'m","'d","him","me","my","'ll","'re","\\*","\\*\\*\\*","itself","your","they","he","'`'","will","'`'","cgi","just","some","from","all","if","but","like","there","what","about","ms.","her","their","too","into","or","are","up","'`'","has","so","his","have","j.","with","\\/","x","one","this","himself","were","'ve","we","'`'","``","'","be","by","n't","at","us","you","i","mr.","--","two","as","-","for","...","its","an","in","that","on","of","the","to","it","is","The","a","A","And","and",",", "!","?","''","'s",".",":",";","}","{"]
    #"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","&"
    #forbidden.extend(stopwords.words('english'))
    #forbidden=stopwords.words('english')
    forbidden=["<",">","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","'d", "'ll", "'re", "'s", "'ve", "n't", 'sha', 'wo',".",":",";","}","{"]
    #forbidden=["<",">","s","'d", "'ll", "'re", "'s", "'ve", "n't", 'sha', 'wo',".",":",";","}","{","/"]

    #'could', 'might', 'must','need','would',
    #forbidden=["'d", "'ll", "'re", "'s", "'ve", "n't", 'sha', 'wo',".",":",";","}","{"]

    forbidden=["<",">","s","/","}","{",".",":",";"]
    letters_and_numbers=["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","1","2","3","4","5","6","7","8","9","0"]
    #stp_words=stopwords.words('english')#forbidden.extend(stopwords.words('english'))
    stp_words=[]
    words2=[]
    for i in range(len(words)):
        #words[i] not in stp_words 
        if words[i] not in stp_words and words[i] not in forbidden:
            not_add=0
            for letter in words[i]:
                if letter not in letters_and_numbers:
                    not_add=1
                    break
            if not_add!=1:   
                if words[i][-1]==".":
                    words2.append(words[i][:-1])
                else:    
                    words2.append(words[i])
        
    return words2

class EntailmentFactChecker(object):
    def __init__(self, ent_model):
        self.ent_model = ent_model

    def predict(self, fact: str, passages: List[dict]) -> str:
        overlap_score=overlap(fact, passages)
        if overlap_score>0.3:
            string_fact=""
            fact=fact.lower()
            
            fact1=word_tokenize(fact)
            fact_no_stpwords=remove_stp_words2(fact1)
            s_fact=set(fact_no_stpwords)
    
            #print("ORIGINAL FACT")
            #print(fact)
            #print("LIST FACT NO STPWORDS")
            #print(fact_no_stpwords)
            for j in range(len(fact_no_stpwords)):
                if len(string_fact)==0:
                    string_fact=string_fact+fact_no_stpwords[j]
                else:
                    string_fact=string_fact+" "+fact_no_stpwords[j]
            #print("STRING FACT")
            #print(string_fact)
            passages_no_stpwords=[]
            string_all=""
            string_all2=""
            pair_passages=[]
            for i in range(len(passages)):
                #print("ORIGINAL PASSAGE")
                #print(passages[i]["text"].lower())
                if len(string_all)==0:   
                    string_all=string_all+passages[i]["text"].lower()
                else:
                    string_all=string_all+" " +passages[i]["text"].lower()
                
                if len(string_all2)==0:
                        string_all2=string_all2+passages[i]["text"].lower()
                else:
                    string_all2=string_all2+" "+passages[i]["text"].lower()
                if (i+1)%2==0:
                    pair_passages.append(string_all2)
                    string_all2=""
                    
                    
                tokenizer_passgs=word_tokenize(passages[i]["text"].lower())
                previous=0
                sentences=[]
                txts_o2=passages[i]["text"].lower().split("<s>")
                for text_o in txts_o2:
                    tokenizer_o=word_tokenize(text_o.lower())
                    #print("TOKENIZER_O")
                    #print(tokenizer_o)
                    sentences.append(tokenizer_o)
                for l in range(len(tokenizer_passgs)):
                    if tokenizer_passgs[l]==".":
                        sentence=tokenizer_passgs[previous:l]
                        sentences.append(sentence)
                        previous=l+1
                sentences_2nd_tokeninze=sent_tokenize(passages[i]["text"].lower())
                for text_2ndtokenize in sentences_2nd_tokeninze:
                    sentences.append(word_tokenize(text_2ndtokenize))
                    
                    
                    
                sentences.append(passages[i]["text"].lower())
                txt_ln=len(passages[i]["text"])
                sentences.append(passages[i]["text"].lower()[:int(np.floor(txt_ln))])
                sentences.append(passages[i]["text"].lower()[int(np.floor(txt_ln)):])
    
                for sentence in sentences: 
                    
                    list_wnostopwords=remove_stp_words2(sentence)
        
                    j_MODified=len(s_fact.intersection(set(list_wnostopwords)))/len(s_fact)
                    if j_MODified>0.3: 
                        string_pass=""
                        for j in range (len(list_wnostopwords)):
                            if len(string_pass)==0:
                                string_pass=string_pass+list_wnostopwords[j]
                            else:
                                string_pass=string_pass+" "+list_wnostopwords[j]
                        #print("ORIGINAL SENTENCE")
                        #print(sentence)
                        #print("STRING TO USE")
                        #print(string_pass)
                        passages_no_stpwords.append(string_pass)
            #print("STRING NO STPWORDS")
            #print(passages_no_stpwords)
            
            list_all_nostpwords=remove_stp_words2(string_all)
            string_final_all=""
            j_MODified_all=len(s_fact.intersection(set(list_all_nostpwords)))/len(s_fact)
            if j_MODified_all>0.3:
                for k in range(len(list_all_nostpwords)):
                    if len(string_final_all)==0:
                        string_final_all=string_final_all+list_all_nostpwords[k]
                    else:
                        string_final_all=string_final_all+" "+list_all_nostpwords[k]
    
                passages_no_stpwords.append(string_final_all)     
    
                    
            for h in range(len(pair_passages)):
                text_tokenp=word_tokenize(pair_passages[h])
                no_stop_words_pairs=remove_stp_words2(text_tokenp)
                j_MODified_pairs=len(s_fact.intersection(set(no_stop_words_pairs)))/len(s_fact)
                if j_MODified_pairs>0.3:
                    string_pair=""
                    for k in range(len(no_stop_words_pairs)):
                        if len(string_final_all)==0:
                            string_pair=string_pair+no_stop_words_pairs[k]
                        else:
                            string_pair=string_pair+" "+no_stop_words_pairs[k]
    
                    passages_no_stpwords.append(string_pair)     
    
            #if entail>cont and entail > neutral and entail>0.47: -> 0.8144796380090498
            #if entail>cont and entail > neutral and entail>0.5: -> 0.8190045248868778
            entails=[]
            neutrals=[]
            conts=[]
            for i in range(len(passages_no_stpwords)):      
                entail,neutral,cont=self.ent_model.check_entailment(passages_no_stpwords[i],string_fact)
                entails.append(entail)
                neutrals.append(neutral)
                conts.append(cont)
                if entail>cont and entail > neutral and entail>=0.5 and neutral<0.45 and cont<=0.45:
                    return "S"
        return "NS"
        #if len(passages_no_stpwords)!=0:
        #    maximum=max(entails)
        #    index=np.argmax(entails)
        #    print("entail",maximum,"neutral",neutrals[index],"cont",conts[index])
        #else:
        #    print("SIN COINCIDENCIAS")
        #return "NS"
            #logits=logits.numpy()
            #if entail == True :
            #    return "S"
        #print("ENTAILS")
        #print(entails)
        #print("---")
        #if len(entails)!=0:
        #    maximum=max(entails)
        #    index=np.argmax(entails)
            #maximum>0.01 ->0.7601
            #maximum>0.01 and neutrals[index]<=0.6->0.7601
            #maximum>0.01 and neutrals[index]<=0.7 -> 0.746606334841629
            #maximum>0.01 and neutrals[index]<=0.6)or  (maximum>0.01 and conts[index]<0.02 ->0.7873303167420814 CON FORBIDDEN WORDS Y STOPWORDS
            # (maximum>0.01 and neutrals[index]<=0.61) or  (maximum>0.01 and conts[index]<0.021) ->   0.7737556561085973 SIN FORBIDDEN WORDS Y STOPWORDS
            # (maximum>0.01 and neutrals[index]<=0.65) or  (maximum>0.01 and conts[index]<0.025)  -> 0.7873303167420814  FORBIDDEN WORDS Y STOPWORDS
            #(maximum>0.01 and neutrals[index]<=0.65) or  (maximum>0.01 and conts[index]<0.025) ->0.7918552036199095  FORBIDDEN WORDS Y STOPWORDS
            #(maximum>0.01 and neutrals[index]<=0.65) or  (maximum>0.01 and conts[index]<0.024) -> 0.7963800904977375
            # (maximum>0.01 and neutrals[index]<=0.65) or  (maximum>0.01 and conts[index]<0.025) -> 0.8009049773755657 sin palabras extras separandolas con <s>
            #(maximum>0.06 and neutrals[index]<=0.64) or  (maximum>0.06 and conts[index]<0.024):# and neutrals[index]<=0.65) or  (maximum>0.01 and conts[index]<0.024):#(neutrals[index]< conts[index] and maximum>0.005 and neutrals[index]<=0.4) :
        #    if (maximum>0.06 and neutrals[index]<=0.1): #and conts[index]<0.002):# and neutrals[index]<=0.64) or  (maximum>0.06 and conts[index]<0.024): #(maximum>0.01 and neutrals[index]<=0.85):
        #        print("ENTRO")
        #        return "S"
        #    else:
        #        return "NS"
        #else:
        #    return "NS"

       


# OPTIONAL
class DependencyRecallThresholdFactChecker(object):
    def __init__(self):
        self.nlp = spacy.load('en_core_web_sm')

    def predict(self, fact: str, passages: List[dict]) -> str:
        raise Exception("Implement me")

    def get_dependencies(self, sent: str):
        """
        Returns a set of relevant dependencies from sent
        :param sent: The sentence to extract dependencies from
        :param nlp: The spaCy model to run
        :return: A set of dependency relations as tuples (head, label, child) where the head and child are lemmatized
        if they are verbs. This is filtered from the entire set of dependencies to reflect ones that are most
        semantically meaningful for this kind of fact-checking
        """
        # Runs the spaCy tagger
        processed_sent = self.nlp(sent)
        relations = set()
        for token in processed_sent:
            ignore_dep = ['punct', 'ROOT', 'root', 'det', 'case', 'aux', 'auxpass', 'dep', 'cop', 'mark']
            if token.is_punct or token.dep_ in ignore_dep:
                continue
            # Simplify the relation to its basic form (root verb form for verbs)
            head = token.head.lemma_ if token.head.pos_ == 'VERB' else token.head.text
            dependent = token.lemma_ if token.pos_ == 'VERB' else token.text
            relation = (head, token.dep_, dependent)
            relations.add(relation)
        return relations

