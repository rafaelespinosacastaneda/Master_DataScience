# models.py

from sentiment_data import *
from utils import *

from collections import Counter
import numpy as np
import random
import nltk
from nltk.corpus import stopwords

nltk.download('stopwords')

class FeatureExtractor(object):
    """
    Feature extraction base type. Takes a sentence and returns an indexed list of features.
    """
    def get_indexer(self):
        raise Exception("Don't call me, call my subclasses")

    def extract_features(self, sentence: List[str], add_to_indexer: bool=False) -> Counter:
        """
        Extract features from a sentence represented as a list of words. Includes a flag add_to_indexer to
        :param sentence: words in the example to featurize
        :param add_to_indexer: True if we should grow the dimensionality of the featurizer if new features are encountered.
        At test time, any unseen features should be discarded, but at train time, we probably want to keep growing it.
        :return: A feature vector. We suggest using a Counter[int], which can encode a sparse feature vector (only
        a few indices have nonzero value) in essentially the same way as a map. However, you can use whatever data
        structure you prefer, since this does not interact with the framework code.
        """
        raise Exception("Don't call me, call my subclasses")


class UnigramFeatureExtractor(FeatureExtractor):
    """
    Extracts unigram bag-of-words features from a sentence. It's up to you to decide how you want to handle counts
    and any additional preprocessing you want to do.
    """
    def __init__(self, indexer: Indexer):
        self.indexer=indexer 
        self.word_r=[] 
        self.forbidden_words=["1","2","3","4","5","6","7","8","9","0","10","co-writer\\/director","1952","writer\\/director","having","had","-lrb-","-rrb-" ,"x.","#","$","'m","'d","him","me","my","'ll","'re","\\*","\\*\\*\\*","itself","your","they","he","'`'","will","'`'","cgi","just","some","from","all","if","but","like","there","what","about","ms.","her","their","too","into","or","are","up","'`'","has","so","his","have","j.","with","\\/","x","one","this","himself","were","'ve","we","'`'","``","'","be","by","n't","at","us","you","i","mr.","--","two","as","-","for","...","its","an","in","that","on","of","the","to","it","is","The","a","A","And","and",",", "!","?","''","'s",".",":",";"]
        self.most_common=[]
        self.positive_words=["greatest","good", "charming", "authentic","great","fun","interesting","delightful","impressive","brilliantly","brilliantly"]
        self.negative_words=["foolish","awkward","absurd","horrible","bizarre","stupid","badly","bad","ridiculous","pathetic"]
        self.most_common_dict={}
        self.total_mostcommon=0
        self.pos_neg=0
        self.max_words=8000
    def get_indexer(self):
        return self.indexer 
    def all_repeated(self,sentence):
        for word in sentence:
            word=word.lower()
            if self.indexer.contains(word) and word not in self.forbidden_words :
                self.word_r.append(word)

    def keep_most_repeated(self,max_words=8000):
        self.most_common=Counter(self.word_r).most_common(max_words)
        self.max_words=max_words
        self.indexer.ints_to_objs ={}
        self.indexer.objs_to_ints={}
        for i in range(50,max_words):
            self.indexer.add_and_get_index(self.most_common[i][0])
        total=0
        for i in range(50,max_words):
            w=self.most_common[i][0]
            count=self.most_common[i][1]
            self.most_common_dict[w]=count
            total=total+count
        self.total_mostcommon=total
            
            

        
    def extract_features(self, sentence: List[str], add_to_indexer: bool=False)-> Counter:
        
        for i in range(len(sentence)):
            sentence[i]=sentence[i].lower()
            
        c=Counter(sentence)
        
        for word in self.forbidden_words:
            if c[word]!=0:
                del c[word]
        
        if add_to_indexer== True:
            for word in c.keys():
                self.indexer.add_and_get_index(word)
        else: 
            dicc={}
          
            for word in c.keys():
                if self.indexer.index_of(word)!=-1: 
                    dicc[self.indexer.index_of(word)]=1#c[word]/len(c)#1 #1
                    if  word in self.positive_words:
                        dicc[self.indexer.index_of(word)]=10 #*c[word]
                    if word in self.negative_words:
                        dicc[self.indexer.index_of(word)]=-10 #*c[word]
                else:
                    dicc[-1]=1
                    
            
            return dicc

class BigramFeatureExtractor(FeatureExtractor):
    """
    Bigram feature extractor analogous to the unigram one.
    """
    def __init__(self, indexer: Indexer):
        self.max_words=8000
        self.indexer=indexer 
        self.word_r=[] 
        self.forbidden_words=["1","2","3","4","5","6","7","8","9","0","10","co-writer\\/director","1952","writer\\/director","having","had","-lrb-","-rrb-" ,"x.","#","$","'m","'d","him","me","my","'ll","'re","\\*","\\*\\*\\*","itself","your","they","he","'`'","will","'`'","cgi","just","some","from","all","if","but","like","there","what","about","ms.","her","their","too","into","or","are","up","'`'","has","so","his","have","j.","with","\\/","x","one","this","himself","were","'ve","we","'`'","``","'","be","by","n't","at","us","you","i","mr.","--","two","as","-","for","...","its","an","in","that","on","of","the","to","it","is","The","a","A","And","and",",", "!","?","''","'s",".",":",";"]
        self.most_common=[]
        self.positive_words=["greatest","good", "charming", "authentic","great","fun","interesting","delightful","impressive","brilliantly","brilliantly"]
        self.negative_words=["foolish","awkward","absurd","horrible","bizarre","stupid","badly","bad","ridiculous","pathetic"]
        self.most_common_dict={}
        self.total_mostcommon=0
        self.pos_neg=0
        self.max_words=8000
    def get_indexer(self):
        return self.indexer 
    def all_repeated(self,sentence):
        for word in sentence:
            word=word.lower()
            if self.indexer.contains(word) and word not in self.forbidden_words :
                self.word_r.append(word)
  
    def keep_most_repeated(self,max_words=8000):
        self.most_common=Counter(self.word_r).most_common(max_words)
        self.max_words=max_words
        self.indexer.ints_to_objs ={}
        self.indexer.objs_to_ints={}

    
    def extract_features(self, sentence: List[str], add_to_indexer: bool=False)-> Counter:
        
        bigram_dicc=Counter(sentence)
        bigram=[]
        
        for i in range(len(sentence)-1):
            bigram.append(sentence[i]+"|"+sentence[i+1] )
        bigram_dicc=Counter(bigram)
        
        if add_to_indexer== True:
            for word in bigram_dicc.keys():
                self.indexer.add_and_get_index(word)
        
        else: 
            dicc={}
            for bigram in bigram_dicc.keys():
                if self.indexer.index_of(bigram)!=-1: 
                    dicc[self.indexer.index_of(bigram)]=1#c[word]/len(c)#1 #1
                    if  bigram in self.positive_words:
                        dicc[self.indexer.index_of(bigram)]=10 #*c[word]
                    if bigram in self.negative_words:
                        dicc[self.indexer.index_of(bigram)]=-10 #*c[word]
                else:
                    dicc[-1]=1
                    
                
            return dicc
        

        
            
        
        
            
    
            
        


class BetterFeatureExtractor(FeatureExtractor):
    """
    Better feature extractor...try whatever you can think of!
    """
    def __init__(self, indexer: Indexer):
        self.max_words=8000
        self.indexer=indexer 
        self.word_r=[] 
        self.word_r_bigrams=[]
        self.forbidden_words=["co-writer\\/director","1952","writer\\/director","1","2","3","4","5","6","7","8","9","0","10","having","had","-lrb-","-rrb-" ,"x.","#","$","'m","'d","him","me","my","'ll","'re","\\*","\\*\\*\\*","itself","your","they","he","'`'","will","'`'","cgi","just","some","from","all","if","but","like","there","what","about","ms.","her","their","too","into","or","are","up","'`'","has","so","his","have","j.","with","\\/","x","one","this","himself","were","'ve","we","'`'","``","'","be","by","n't","at","us","you","i","mr.","--","two","as","-","for","...","its","an","in","that","on","of","the","to","it","is","The","a","A","And","and",",", "!","?","''","'s",".",":",";"]
        
        self.forbidden_words_for_b=["The","the","a","co-writer\\/director","1952","writer\\/director","1","2","3","4","5","6","7","8","9","0","10","having","had","-lrb-","-rrb-" ,"x.","#","$","'m","'d","him","me","my","'ll","'re","\\*","\\*\\*\\*","itself","your","they","he","'`'","will","'`'","cgi","just","some","from","all","if","but","like","there","what","about","ms.","her","their","too","into","or","are","up","'`'","has","so","his","have","j.","with","\\/","x","one","this","himself","were","'ve","we","'`'","``","'","be","by","n't","at","us","you","mr.","--","two","as","-","for","...","its","an","in","that","on","of","the","to","it","is",",", "!","?","''","'s",".",":",";"]

        self.most_common=[]
        self.most_common_b=[]
        self.positive_words=["greatest","good", "charming", "authentic","great","fun","interesting","delightful","impressive","brilliantly","brilliantly"]
        self.negation_pos=[]
        self.negative_words=["foolish","awkward","absurd","horrible","bizarre","stupid","badly","bad","ridiculous","pathetic"]
        self.negation_negative=[]
        self.most_common_dict={}
        self.total_mostcommon=0
        self.pos_neg=0
        self.max_words=8000
        self.ufraction_positivity_negativity={}
        self.bfraction_positivity_negativity={}
        self.tfraction_positivity_negativity={}
        self.all_train=[]
        self.most_negative=[]
        self.most_positive=[]
        
        
        for word in self.positive_words:
            self.negation_pos.append("not|"+ word)
            self.negation_pos.append("no|"+ word)
        
        for word in self.negative_words:
            self.negation_negative.append("not|"+ word)
            self.negation_negative.append("no|"+ word)
            
    def get_indexer(self):
        return self.indexer 
    def all_repeated(self,sentence):
        for word in sentence:
            word=word.lower()
            if word not in self.forbidden_words :
                self.word_r.append(word)
       

    def most_repeated(self,max_words=50):
        
        
        for i in range(len(self.all_train)):
            for word in self.all_train[i].words:
                if word not in self.forbidden_words:    
                    self.word_r.append(word)
        
        most_common=Counter(self.word_r).most_common(max_words)
        
        for i in range(len(self.most_common)):
            self.most_common.append(most_common[i][0])
        
        #bigrams
        for i in range(len(self.all_train)):
            for  j in range(len(self.all_train[i].words)-1):
                self.word_r_bigrams.append(self.all_train[i].words[j]+"|"+self.all_train[i].words[j+1])
        
        most_common_b=Counter(self.word_r_bigrams).most_common(10000)
       
        for i in range(len(most_common_b)):
            self.most_common_b.append(most_common_b[i][0])
        
        #trigrams
    
    def highest_fraction_pos_neg(self):
        fracc_and_common_big={}
        for i in range(len(self.all_train)):
            for j in range(len(self.all_train[i].words)-1):
                self.all_train[i].words[j]=self.all_train[i].words[j].lower()
                if (self.all_train[i].words[j] not in self.forbidden_words_for_b) and (self.all_train[i].words[j+1] not in self.forbidden_words_for_b):
                    bigram=self.all_train[i].words[j] +"|"+self.all_train[i].words[j+1]
                    if bigram in self.most_common_b: 
                        if self.all_train[i].label==0:
                            label=-1
                        elif self.all_train[i].label==1:
                            label=1
                        if bigram in fracc_and_common_big:
                            fracc_and_common_big[bigram]=fracc_and_common_big[bigram]+label
                        elif bigram not in fracc_and_common_big:
                            fracc_and_common_big[bigram]=label

        
        sorted_fracc_common_big=sorted(fracc_and_common_big.items(), key=lambda x:x[1])

        
        for i in range(len(sorted_fracc_common_big)):
            if  sorted_fracc_common_big[i][1]<=-3:
                self.most_negative.append(sorted_fracc_common_big[i][0])
            if sorted_fracc_common_big[i][1]>=3:
                self.most_positive.append(sorted_fracc_common_big[i][0])
        


    
    def extract_features(self, sentence: List[str], add_to_indexer: bool=False)-> Counter:
        
        unigram=[]
        bigram=[]
        trigram=[]
        
     
            
        for i in range(len(sentence)):
            sentence[i]=sentence[i].lower()
            if sentence[i] not in self.forbidden_words:
                unigram.append(sentence[i])
            if i+1<=len(sentence)-1:
                bigram.append(sentence[i]+"|"+sentence[i+1] )
            if i+2<=len(sentence)-1:
                trigram.append(sentence[i]+"|"+sentence[i+1]+"|"+sentence[i+2])
        bigram_dicc=Counter(bigram)
        unigram_dicc=Counter(unigram)
        trigram_dicc=Counter(trigram)
        
       # for word in self.forbidden_words:
       #     if unigram_dicc[word]!=0:
        #        del unigram_dicc[word]
        
        pos_neg_local=self.pos_neg
        if add_to_indexer== True:
            for bigram in bigram_dicc.keys():
                if bigram in self.most_positive:
                    self.indexer.add_and_get_index(bigram)
                if bigram in self.most_negative:
                    self.indexer.add_and_get_index(bigram)
                    
                    
        
                
            self.pos_neg=pos_neg_local
            for trigram in trigram_dicc.keys():
                
                if self.indexer.index_of(trigram)!=-1:
                    self.tfraction_positivity_negativity[self.indexer.index_of(trigram)]=self.tfraction_positivity_negativity[self.indexer.index_of(trigram)]+1*self.pos_neg
                elif self.indexer.index_of(trigram)==-1:
                    self.indexer.add_and_get_index(trigram)
                    self.tfraction_positivity_negativity[self.indexer.index_of(trigram)]=1*self.pos_neg
            
            for unigram in unigram_dicc.keys():
                if unigram in self.positive_words:
                    self.pos_neg=1   
                if unigram in self.negative_words:
                    self.pos_neg=-1
                if self.indexer.index_of(unigram)!=-1 and unigram not in self.most_common:
                    self.ufraction_positivity_negativity[self.indexer.index_of(unigram)]=self.ufraction_positivity_negativity[self.indexer.index_of(unigram)]+1*self.pos_neg
                elif self.indexer.index_of(unigram)==-1 and unigram not in self.most_common:
                    self.indexer.add_and_get_index(unigram)
                    self.ufraction_positivity_negativity[self.indexer.index_of(unigram)]=self.pos_neg
                    
                
        
        else: 
            dicc={}
            for bigram in bigram_dicc.keys():
                if self.indexer.index_of(bigram)!=-1 and bigram in self.most_positive:    
                    dicc[self.indexer.index_of(bigram)]=1#(self.bfraction_positivity_negativity[self.indexer.index_of(bigram)])/len(self.bfraction_positivity_negativity)#c[word]/len(c)#1 #1
                if self.indexer.index_of(bigram)!=-1 and bigram in self.most_negative:    
                    dicc[self.indexer.index_of(bigram)]=-1#(self.bfraction_positivity_negativity[self.indexer.index_of(bigram)])/len(self.bfraction_positivity_negativity)#c[word]/len(c)#1 #1
            
            for unigram in unigram_dicc.keys():
                if self.indexer.index_of(unigram)!=-1:    
                    dicc[self.indexer.index_of(unigram)]=1#(self.ufraction_positivity_negativity[self.indexer.index_of(unigram)])/len(self.ufraction_positivity_negativity)#c[word]/len(c)#1 #1    
                    
            for trigram in trigram_dicc.keys():
                if self.indexer.index_of(trigram)!=-1:    
                    dicc[self.indexer.index_of(trigram)]=1#(self.tfraction_positivity_negativity[self.indexer.index_of(trigram)])/len(self.tfraction_positivity_negativity)#c[word]/len(c)#1 #1
            
            return dicc
        


class SentimentClassifier(object):
    """
    Sentiment classifier base type
    """
    def predict(self, sentence: List[str]) -> int:
        """
        :param sentence: words (List[str]) in the sentence to classify
        :return: Either 0 for negative class or 1 for positive class
        """
        raise Exception("Don't call me, call my subclasses")


class TrivialSentimentClassifier(SentimentClassifier):
    """
    Sentiment classifier that always predicts the positive class.
    """
    def predict(self, sentence: List[str]) -> int:
        return 1


class PerceptronClassifier(SentimentClassifier):
    """
    Implement this class -- you should at least have init() and implement the predict method from the SentimentClassifier
    superclass. Hint: you'll probably need this class to wrap both the weight vector and featurizer -- feel free to
    modify the constructor to pass these in.
    """
    def __init__(self):
        self.weights=[]
        self.featurizer=None
    
    def predict(self, sentence: List[str]) -> int:
        s_features=self.featurizer.extract_features(sentence)
        z=np.sum([s_features[key]*self.weights[key] for key in s_features.keys() ])
        if z>0:
            return 1
        else :
            return 0


class LogisticRegressionClassifier(SentimentClassifier):
    """
    Implement this class -- you should at least have init() and implement the predict method from the SentimentClassifier
    superclass. Hint: you'll probably need this class to wrap both the weight vector and featurizer -- feel free to
    modify the constructor to pass these in.
    """
    def __init__(self):
        self.weights=[]
        self.featurizer=None
    def predict(self, sentence: List[str]) -> int:
        s_features=self.featurizer.extract_features(sentence)
        z=np.sum([s_features[key]*self.weights[key] for key in s_features.keys() ])
        prob_1= np.exp(z)/(1+np.exp(z))

        if prob_1>=0.5:
            return 1
        else :
            return 0


def train_perceptron(train_exs: List[SentimentExample], feat_extractor: FeatureExtractor) -> PerceptronClassifier:
    """
    Train a classifier with the perceptron.
    :param train_exs: training set, List of SentimentExample objects
    :param feat_extractor: feature extractor to use
    :return: trained PerceptronClassifier model
    """
    
    feat_extractor.forbidden_words.extend(stopwords.words('english'))
    np.random.seed(10)
    
    
    if feat_extractor.__class__.__name__ =="BetterFeatureExtractor":
        feat_extractor.all_train=train_exs
        feat_extractor.most_repeated()
        feat_extractor.highest_fraction_pos_neg()
        for i in range(len(train_exs)):
            feat_extractor.pos_neg=train_exs[i].label
            feat_extractor.extract_features(train_exs[i].words,True)
        
    
    if feat_extractor.__class__.__name__ =="UnigramFeatureExtractor" or feat_extractor.__class__.__name__ =="BigramFeatureExtractor":
        for i in range(len(train_exs)):
            feat_extractor.extract_features(train_exs[i].words,True)
            
        
        
        
   
    
    
    if feat_extractor.__class__.__name__ =="UnigramFeatureExtractor":
        for i in range(len(train_exs)):
            feat_extractor.all_repeated(train_exs[i].words)
        feat_extractor.keep_most_repeated()




    num_epochs=60
    
    
    

    alpha=0.065
    nms=np.arange(len(train_exs))
   
    weights=np.zeros(len(feat_extractor.get_indexer().ints_to_objs)+1)
    
    
    if feat_extractor.__class__.__name__ =="BetterFeatureExtractor":
        num_epochs=100
        alpha=1
    for i in range(num_epochs):
        np.random.shuffle(nms)
        for j in nms:
            label=train_exs[j].label
            if label==0:
                label=-1
            feat_extractor.pos_neg=label
            features=feat_extractor.extract_features(train_exs[j].words)
            z=np.sum([features[key]*weights[key] for key in features.keys() ])
            if z>0:
                y_pred=1
            if z<=0:
                y_pred=-1
            if label*z<=0:
                for pos in features.keys():
                    weights[pos]=weights[pos]+1/2*alpha*(label-y_pred)*features[pos]
                
            """
            features=np.array(feat_extractor.extract_features(train_exs[j].words))
            if np.sum(features)!=0:
                z=np.sum(features*weights)
                if z>0:
                    y_pred=1
                if z<=0:
                    y_pred=-1
                if label*z<=0: 
                    weights=weights+1/2*alpha*(label-y_pred)*features
            """
 
        
        pos_n_alpha=alpha-i/1000
        if pos_n_alpha>0:
            alpha=pos_n_alpha
        else:
            alpha=alpha/2
            
                    
    model_perceptron=PerceptronClassifier()
    model_perceptron.featurizer=feat_extractor
    model_perceptron.weights=weights
    return model_perceptron
                
            

def train_logistic_regression(train_exs: List[SentimentExample], feat_extractor: FeatureExtractor) -> LogisticRegressionClassifier:
    """
    Train a logistic regression model.
    :param train_exs: training set, List of SentimentExample objects
    :param feat_extractor: feature extractor to use
    :return: trained LogisticRegressionClassifier model
    """
    feat_extractor.forbidden_words.extend(stopwords.words('english'))
    np.random.seed(10)
    
    
    
    
    
    if feat_extractor.__class__.__name__ =="BetterFeatureExtractor":
        feat_extractor.all_train=train_exs
        feat_extractor.most_repeated()
        feat_extractor.highest_fraction_pos_neg()
        for i in range(len(train_exs)):
            feat_extractor.pos_neg=train_exs[i].label
            feat_extractor.extract_features(train_exs[i].words,True)
    
    
    
    
    
    if feat_extractor.__class__.__name__ =="UnigramFeatureExtractor" or feat_extractor.__class__.__name__ =="BigramFeatureExtractor":
        for i in range(len(train_exs)):
            feat_extractor.extract_features(train_exs[i].words,True)
    if feat_extractor.__class__.__name__ =="UnigramFeatureExtractor":
        for i in range(len(train_exs)):
            feat_extractor.all_repeated(train_exs[i].words)
        
        feat_extractor.keep_most_repeated()

    num_epochs=8
   
    alpha=0.8899000000000011 
    nms=np.arange(len(train_exs))
   
    weights=np.zeros(len(feat_extractor.get_indexer().ints_to_objs)+1)
    
    if feat_extractor.__class__.__name__ =="BetterFeatureExtractor":
        num_epochs=60
        alpha=alpha_a_borrar=0.8899000000000011 

    for i in range(num_epochs):
        np.random.shuffle(nms)
        for j in nms:
            label=train_exs[j].label
            if label==0:
                label=-1
            feat_extractor.pos_neg=label
            features=feat_extractor.extract_features(train_exs[j].words)
            z=np.sum([features[key]*weights[key] for key in features.keys() ])
            
            prob_1= np.exp(z)/(1+np.exp(z))
            
            prob_n1= 1/(1+np.exp(z))
            
            if label==1:
                for pos in features.keys():
                    weights[pos]=weights[pos]+alpha*(label-prob_1)*features[pos]
            if label==-1:
                for pos in features.keys():
                    weights[pos]=weights[pos]+alpha*(label+prob_n1)*features[pos]    
  
        if feat_extractor.__class__.__name__ =="BetterFeatureExtractor":
            pos_n_alpha=alpha-i/1000
            if pos_n_alpha>0:
                alpha=pos_n_alpha
            else:
                alpha=alpha/2
        if feat_extractor.__class__.__name__ =="UnigramFeatureExtractor":  
            if alpha>0.01:    
                alpha=alpha/(i*0.965+1 )  
        

            
    model_LR=LogisticRegressionClassifier()
    model_LR.featurizer=feat_extractor
    model_LR.weights=weights
    return model_LR


def train_model(args, train_exs: List[SentimentExample], dev_exs: List[SentimentExample]) -> SentimentClassifier:
    """
    Main entry point for your modifications. Trains and returns one of several models depending on the args
    passed in from the main method. You may modify this function, but probably will not need to.
    :param args: args bundle from sentiment_classifier.py
    :param train_exs: training set, List of SentimentExample objects
    :param dev_exs: dev set, List of SentimentExample objects. You can use this for validation throughout the training
    process, but you should *not* directly train on this data.
    :return: trained SentimentClassifier model, of whichever type is specified
    """
    # Initialize feature extractor
    if args.model == "TRIVIAL":
        feat_extractor = None
    elif args.feats == "UNIGRAM":
        # Add additional preprocessing code here
        feat_extractor = UnigramFeatureExtractor(Indexer())
    elif args.feats == "BIGRAM":
        # Add additional preprocessing code here
        feat_extractor = BigramFeatureExtractor(Indexer())
    elif args.feats == "BETTER":
        # Add additional preprocessing code here
        feat_extractor = BetterFeatureExtractor(Indexer())
    else:
        raise Exception("Pass in UNIGRAM, BIGRAM, or BETTER to run the appropriate system")

    # Train the model
    if args.model == "TRIVIAL":
        model = TrivialSentimentClassifier()
    elif args.model == "PERCEPTRON":
        model = train_perceptron(train_exs, feat_extractor)
    elif args.model == "LR":
        model = train_logistic_regression(train_exs, feat_extractor)
    else:
        raise Exception("Pass in TRIVIAL, PERCEPTRON, or LR to run the appropriate system")
    return model