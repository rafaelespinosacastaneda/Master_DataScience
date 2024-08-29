# models.py

import torch
import torch.nn as nn
from torch import optim
import numpy as np
import random
from sentiment_data import *

random.seed(10)

class SentimentClassifier(object):
    """
    Sentiment classifier base type
    """

    def predict(self, ex_words: List[str], has_typos: bool) -> int:
        """
        Makes a prediction on the given sentence
        :param ex_words: words to predict on
        :param has_typos: True if we are evaluating on data that potentially has typos, False otherwise. If you do
        spelling correction, this parameter allows you to only use your method for the appropriate dev eval in Q3
        and not otherwise
        :return: 0 or 1 with the label
        """
        raise Exception("Don't call me, call my subclasses")

    def predict_all(self, all_ex_words: List[List[str]], has_typos: bool) -> List[int]:
        """
        You can leave this method with its default implementation, or you can override it to a batched version of
        prediction if you'd like. Since testing only happens once, this is less critical to optimize than training
        for the purposes of this assignment.
        :param all_ex_words: A list of all exs to do prediction on
        :param has_typos: True if we are evaluating on data that potentially has typos, False otherwise.
        :return:
        """
        return [self.predict(ex_words, has_typos) for ex_words in all_ex_words]


class TrivialSentimentClassifier(SentimentClassifier):
    def predict(self, ex_words: List[str], has_typos: bool) -> int:
        """
        :param ex:
        :return: 1, always predicts positive class
        """
        return 1


class NeuralSentimentClassifier(SentimentClassifier):
    """
    Implement your NeuralSentimentClassifier here. This should wrap an instance of the network with learned weights
    along with everything needed to run it on new data (word embeddings, etc.). You will need to implement the predict
    method and you can optionally override predict_all if you want to use batching at inference time (not necessary,
    but may make things faster!)
    """
    def __init__(self):
       
        self.word_embeddings=None
        self.network=None
        self.embedding_layer=None
    
    def predict(self, ex_words: List[str], has_typos: bool) -> int:
        """
        Makes a prediction on the given sentence
        :param ex_words: words to predict on
        :param has_typos: True if we are evaluating on data that potentially has typos, False otherwise. If you do
        spelling correction, this parameter allows you to only use your method for the appropriate dev eval in Q3
        and not otherwise
        :return: 0 or 1 with the label
        """
        
        #self.word_embeddings=self.network.word_embeddings
        #self.embedding_layer=self.network.embedding_layer

        #x=ex_words
        #indices=[]
        #for i in range(len(x)):
        #    index=self.word_embeddings.word_indexer.index_of(x[i])
        #    if index<0:
        #        index=1
        #    indices.append(index)
        #print("Indices")
        #print(indices)
        #print(torch.Tensor(indices).int())
        #tensors_words_embedding=self.embedding_layer(torch.Tensor(indices).int())
        #sum_tensors=tensors_words_embedding[0]
        #for i in range(1,len(tensors_words_embedding)):
        #    sum_tensors=sum_tensors+tensors_words_embedding[i]
        #average_tensors=sum_tensors/len(tensors_words_embedding)
        
        
        #log_probs = self.network.forward(average_tensors)
        log_probs = self.network.forward(ex_words)
        prediction = torch.argmax(log_probs)
        return prediction 
    
    def predict_all(self, all_ex_words: List[List[str]], has_typos: bool) -> List[int]:
        """
        You can leave this method with its default implementation, or you can override it to a batched version of
        prediction if you'd like. Since testing only happens once, this is less critical to optimize than training
        for the purposes of this assignment.
        :param all_ex_words: A list of all exs to do prediction on
        :param has_typos: True if we are evaluating on data that potentially has typos, False otherwise.
        :return:
        """
        return [self.predict(ex_words, has_typos) for ex_words in all_ex_words]






class network(nn.Module):
    """
    Defines the core neural network for doing multiclass classification over a single datapoint at a time. This consists
    of matrix multiplication, tanh nonlinearity, another matrix multiplication, and then
    a log softmax layer to give the ouputs. Log softmax is numerically more stable. If you take a softmax over
    [-100, 100], you will end up with [0, 1], which if you then take the log of (to compute log likelihood) will
    break.

    The forward() function does the important computation. The backward() method is inherited from nn.Module and
    handles backpropagation.
    """
    def __init__(self, inp, hid, out,word_embeddings):
        """
        Constructs the computation graph by instantiating the various layers and initializing weights.

        :param inp: size of input (integer)
        :param hid: size of hidden layer(integer)
        :param out: size of output (integer), which should be the number of classes
        """
        super(network, self).__init__()
        self.drop_out_layer=torch.nn.Dropout(p=0.3, inplace=False)
        self.drop_out_layer2=torch.nn.Dropout(p=0.3, inplace=False)

        self.V = nn.Linear(inp, hid)
        self.word_embeddings=word_embeddings
        # self.g = nn.Tanh()
        self.g1 = nn.ReLU()
        self.g2 = nn.ReLU()
        self.g3 = nn.ReLU()

        self.embedding_layer=word_embeddings.get_initialized_embedding_layer(frozen=False) 
        self.W_1 = nn.Linear(hid, hid)
        #self.W_2 = nn.Linear(hid, hid)
        #self.W_3 = nn.Linear(hid, hid)
        #self.W_4 = nn.Linear(hid, hid)
        self.W_5 = nn.Linear(hid, out)
        self.log_softmax = nn.LogSoftmax(dim=0)
        #self.softmax = nn.Softmax(dim=0)
        nn.init.xavier_uniform_(self.V.weight)
        nn.init.xavier_uniform_(self.W_1.weight)
        #nn.init.xavier_uniform_(self.W_2.weight)
        #nn.init.xavier_uniform_(self.W_3.weight)
        #nn.init.xavier_uniform_(self.W_4.weight)
        nn.init.xavier_uniform_(self.W_5.weight)

       

    def forward(self, x):
        """
        Runs the neural network on the given data and returns log probabilities of the various classes.

        :param x: a [inp]-sized tensor of input data
        :return: an [out]-sized tensor of log probabilities. (In general your network can be set up to return either log
        probabilities or a tuple of (loss, log probability) if you want to pass in y to this function as well
        """
        indices=[]
        for i in range(len(x)):
            index=self.word_embeddings.word_indexer.index_of(x[i])
            if index<0:
                index=1
            indices.append(index)
        #print("Indices")
        #print(indices)
        #print(torch.Tensor(indices).int())
        #tensors_words_embedding=self.embedding_layer(torch.Tensor(indices).int())
        #sum_tensors=tensors_words_embedding[0]
        #for i in range(1,len(tensors_words_embedding)):
        #    sum_tensors=sum_tensors+tensors_words_embedding[i]
        #average_tensors=sum_tensors/len(tensors_words_embedding)
        average_tensors=torch.Tensor(self.embedding_layer(torch.Tensor(indices).int())).mean(dim=0)
        #t=self.drop_out_layer(average_tensors)
        t=self.g1(self.V(average_tensors))
        t=self.drop_out_layer(t)
        t=self.g2(self.W_1(t))
        t=self.drop_out_layer2(t)
        #t=self.drop_out_layer(t)
        #t=self.g(self.W_2(t))
        #t=self.g(self.W_3(t))
        #t=self.g(self.W_4(t))
        t= self.g3( self.W_5(t))
        t=self.log_softmax(t)
        return t
        #return self.log_softmax(t)

def form_input(x) -> torch.Tensor:
    """
    Form the input to the neural network. In general this may be a complex function that synthesizes multiple pieces
    of data, does some computation, handles batching, etc.

    :param x: a [num_samples x inp] numpy array containing input data
    :return: a [num_samples x inp] Tensor
    """
    return torch.from_numpy(x).long()









def train_deep_averaging_network(args, train_exs: List[SentimentExample], dev_exs: List[SentimentExample],
                                 word_embeddings: WordEmbeddings, train_model_for_typo_setting: bool) -> NeuralSentimentClassifier:
    """
    :param args: Command-line args so you can access them here
    :param train_exs: training examples
    :param dev_exs: development set, in case you wish to evaluate your model during training
    :param word_embeddings: set of loaded word embeddings
    :param train_model_for_typo_setting: True if we should train the model for the typo setting, False otherwise
    :return: A trained NeuralSentimentClassifier model. Note: you can create an additional subclass of SentimentClassifier
    and return an instance of that for the typo setting if you want; you're allowed to return two different model types
    for the two settings.
    """
    inp_size=word_embeddings.get_embedding_length()
    out_size=2
    num_classes=2
    """
    avg_embed=[]
    for i in range(len(train_exs)):
        avg_embed.append([  0   for i in range( word_embeddings.get_embedding_length() )  ])
        for j in range(len(train_exs[i].words)):
            avg_embed[-1]=avg_embed[-1]+word_embeddings.get_embedding(train_exs[i].words[j])
        
        avg_embed[-1]=avg_embed[-1]/word_embeddings.get_embedding_length()
            
    """ 
    count=0
    total=0
    for i in range(len(train_exs)):      
        print(train_exs[i].label)
        count=count+train_exs[i].label
        total=total+1
    print(count/total)
    num_epochs = 5
    ffnn = network(inp_size, 120, out_size,word_embeddings)
    #ffnn.embedding_layer=word_embeddings.get_initialized_embedding_layer()
    initial_learning_rate =0.0001 # 0.001
    optimizer = optim.Adam(ffnn.parameters(), lr=initial_learning_rate)
    #scheduler = torch.optim.lr_scheduler.ExponentialLR(optimizer,gamma=0.995)
    embedding_layer=word_embeddings.get_initialized_embedding_layer() 
    #the_loss=nn.NLLLoss()
    the_loss=nn.CrossEntropyLoss()
    for epoch in range(0, num_epochs):
        ex_indices = [i for i in range(0, len(train_exs))]
        random.shuffle(ex_indices)
        total_loss = 0.0
              
        for idx in ex_indices:
            #avg_embed=[  0   for i in range( word_embeddings.get_embedding_length() )  ]
            #for j in range(len(train_exs[idx].words)):
            #    avg_embed=avg_embed+word_embeddings.get_embedding(train_exs[idx].words[j])  
            #avg_embed=avg_embed/len(train_exs[idx].words)
            #print(np.sum(avg_embed))
            #x = form_input(avg_embed)
            #print("LAYER EMBEDDING")
            #print(embedding_layer(torch.tensor([5,0])))
            #print("Pase")
            y = train_exs[idx].label
            # Build one-hot representation of y. Instead of the label 0 or 1, y_onehot is either [0, 1] or [1, 0]. This
            # way we can take the dot product directly with a probability vector to get class probabilities.
            y_onehot = torch.zeros(num_classes)
            # scatter will write the value of 1 into the position of y_onehot given by y
            y_onehot.scatter_(0, torch.from_numpy(np.asarray(y,dtype=np.int64)), 1)
            # Zero out the gradients from the FFNN object. *THIS IS VERY IMPORTANT TO DO BEFORE CALLING BACKWARD()*
           
            ffnn.zero_grad()
            x=train_exs[idx].words
            log_probs = ffnn.forward(x)
            #print(log_probs)
            # Can also use built-in NLLLoss as a shortcut here but we're being explicit here
            #print(type(log_probs))
            #log_probs=log_probs.type(torch.LongTensor)
            
            #loss = the_loss(log_probs,torch.tensor(y))
            loss=torch.neg(log_probs).dot(y_onehot)
            total_loss += loss
            # Computes the gradient and takes the optimizer step
            loss.backward()
            #print(loss)
            optimizer.step()
        train_correct = 0
        for idx in range(0, len(dev_exs)):
            x=dev_exs[idx].words
            y = dev_exs[idx].label
            log_probs = ffnn.forward(x)
            prediction = torch.argmax(log_probs)
            #print(prediction)
            #print(y)
            if y == prediction:
                train_correct += 1
        print(repr(train_correct) + "/" + repr(len(dev_exs)) + " correct after training")

            
        #scheduler.step()
        print("Total loss on epoch %i: %f" % (epoch, total_loss))
        #print("Learning rate %f",scheduler.get_last_lr()[0])
    classifier=NeuralSentimentClassifier()
    classifier.network=ffnn
    
    return classifier
        
    
    