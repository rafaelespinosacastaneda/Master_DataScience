# models.py

import numpy as np

import torch
import torch.nn as nn
import random
from torch import optim
class LanguageModel(object):

    def get_next_char_log_probs(self, context) -> np.ndarray:
        """
        Returns a log probability distribution over the next characters given a context.
        The log should be base e

        NOTE: You should make sure you call model.eval() to determinize inference here (turns off dropout
        layers in TransformerEncoder).
        :param context: the string context that the LM conditions on
        :return: A numpy vector log P(y | context) where y ranges over the output vocabulary.
        """
        raise Exception("Only implemented in subclasses")


    def get_log_prob_sequence(self, next_chars, context) -> float:
        """
        Scores a bunch of characters following context. That is, returns
        log P(nc1, nc2, nc3, ... | context) = log P(nc1 | context) + log P(nc2 | context, nc1), ...
        The log should be base e

        NOTE: You should make sure you call model.eval() to determinize inference here (turns off dropout
        layers in TransformerEncoder).
        :param next_chars:
        :param context:
        :return: The float probability
        """
        raise Exception("Only implemented in subclasses")


class UniformLanguageModel(LanguageModel):
    def __init__(self, voc_size):
        self.voc_size = voc_size

    def get_next_char_log_probs(self, context):
        return np.ones([self.voc_size]) * np.log(1.0/self.voc_size)

    def get_log_prob_sequence(self, next_chars, context):
        return np.log(1.0/self.voc_size) * len(next_chars)


class NeuralLanguageModel(LanguageModel):
    def __init__(self):
        self.model=None
        self.vocab_index=None

    def get_next_char_log_probs(self, context):
        
        #print("context")
        #print(context)
        self.model.eval()

        aux=np.zeros(len(context))
        for i in range(len(context)):
            aux[i]=self.vocab_index.index_of(context[i])
            
        
        if len(aux)>=20:
            #aux[-20]= 26
            
            chars_to_use=torch.from_numpy(aux[-20:]).long()
            log_probs=self.model(chars_to_use)
            np_log_probs = log_probs.detach().numpy()
            return np_log_probs[-1]
            
        
        if len(aux)<20:
            aux2=np.array([])
            for i in range(len(aux)):
                aux2=np.append(aux2,aux[i])
            #aux2=np.insert(aux2,0,26)
            num_padd=20-len(aux2)
            for i in range(num_padd):
                #aux2=np.insert(aux2,0,26)
                aux2=np.append(aux2,26)
            chars_to_use=torch.from_numpy(aux2).long()

            log_probs=self.model(chars_to_use)
            np_log_probs = log_probs.detach().numpy()
            if len(aux)==0:
                return np_log_probs[len(aux)]
            else :
                return np_log_probs[len(aux)-1]

        
        #print("LOG PROBS")
        #print(type(log_probs))
        #print(log_probs)
      
        
        
            
            

    def get_log_prob_sequence(self, next_chars, context):
        
        
        
        
        
        log_probs=[]
        #charlog_probs=self.get_next_char_log_probs(context)
        
        #next_chars_log_prob=charlog_probs[self.vocab_index.index_of(next_chars[0])]
        #log_probs.append(next_chars_log_prob)
        
        #all_text=context+next_chars[0:]
        
        
       
            
        for i in range(len(next_chars)):
            charlog_probs = self.get_next_char_log_probs(context + next_chars[0:i])
            log_prob_from_single_probs = charlog_probs[self.vocab_index.index_of(next_chars[i])]

            log_probs.append(log_prob_from_single_probs)
           
              
           
            
            
            
            
            
        
        #chunks_labels = [all_text[i:i+n] for i in range(0, len(all_text), n)]
            
          
        #   for i in range(len(chunks_labels)):
        #       aux=np.zeros(len(chunks_labels[i]))
        #       for k in range(len(context)):
        #           aux[k]=self.vocab_index.index_of(context[k])
        #       l_probs=self.model(torch.from_numpy(aux).long())
        #       l_probs=l_probs.detach().numpy()
        #       for j in range(len(chunks_labels[i])-1):
        #           log_probs.append(l_probs[j][self.vocab_index.index_of(chunks_labels[i][j+1])])
    
    
        #    else:
        #        for i in range(len(next_chars)):
        #            charlog_probs = self.get_next_char_log_probs(context + next_chars[0:i])
        #            log_prob_from_single_probs = charlog_probs[self.vocab_index.index_of(next_chars[i])]
        
        #            log_probs.append(log_prob_from_single_probs)
                 
               
                   
                
              
        sum_log_probs=np.sum(log_probs)
                
        return float(sum_log_probs)


class Transformer(nn.Module):
    def __init__(self, vocab_size, num_positions, d_model, d_internal, num_classes, num_layers):
        """
        :param vocab_size: vocabulary size of the embedding layer
        :param num_positions: max sequence length that will be fed to the model; should be 20
        :param d_model: see TransformerLayer
        :param d_internal: see TransformerLayer
        :param num_classes: number of classes predicted at the output layer; should be 3
        :param num_layers: number of TransformerLayers to use; can be whatever you want
        """

        super().__init__()
        #NHEAD=7
        #nhead=4
        #nhead=4
        #dim_feedforward=50
        self.transformer_layer1=nn.TransformerEncoderLayer(d_model=d_model, nhead=2,dim_feedforward=500,dropout=0.3)
        self.transformer_layerE1=nn.TransformerEncoder(self.transformer_layer1, num_layers=num_layers)
        
        self.transformer_layer2=nn.TransformerEncoderLayer(d_model=d_model, nhead=2,dim_feedforward=500,dropout=0.3)
        self.transformer_layerE2=nn.TransformerEncoder(self.transformer_layer2, num_layers=num_layers)

        
        self.transformer_layer3=nn.TransformerEncoderLayer(d_model=d_model, nhead=4,dim_feedforward=50,dropout=0.3)
        self.transformer_layerE3=nn.TransformerEncoder(self.transformer_layer3, num_layers=num_layers)

 
        
        self.emb=nn.Embedding(vocab_size, d_internal)
        self.last_layer=nn.Linear(d_internal, num_classes)
        self.non_linear=nn.ReLU()
        self.linear=nn.Linear(d_internal, d_internal)

        self.drop_out_layer=torch.nn.Dropout(p=0.3, inplace=False)

        
        self.positional_encoding=PositionalEncoding(d_model,num_positions=20, batched=False)
        nn.init.xavier_uniform_(self.last_layer.weight)
        nn.init.xavier_uniform_(self.linear.weight)

        

    def forward(self, indices):
        """

        :param indices: list of input indices
        :return: A tuple of the softmax log probabilities (should be a 20x3 matrix) and a list of the attention
        maps you use in your layers (can be variable length, but each should be a 20x20 matrix)
        """
        maps=[]
        embeded_indices=self.emb(indices)
        
        sz=indices.shape[0]
        mask = (torch.triu(torch.ones(sz, sz)) == 1).transpose(0, 1)
        mask = mask.float().masked_fill(mask == 0, float('-inf')).masked_fill(mask == 1, float(0.0))
       
        embedded_w_pos=self.positional_encoding(embeded_indices)
        output_t1=self.transformer_layerE1(embedded_w_pos,mask=mask,is_causal=True)
        maps.append(output_t1)
        output_t2=self.transformer_layerE2(output_t1,mask=mask,is_causal=True)
        #output_t2=self.transformer_layerE3(output_t2,mask=mask,is_causal=True)

        #maps.append(output_t2)
        #output_t2=self.drop_out_layer(output_t2)
        #output=self.linear(output_t2) CHECAR
        #output_t2=self.non_linear(output_t2)
        output_t2= self.last_layer(output_t2)
        #output=self.non_linear_last(output)
        soft_layer=nn.LogSoftmax(dim=1)
        output=soft_layer(output_t2)
        #print("OUTPUT TENSOR SHAPE")
        #print(output.size())
        return output

        
        



# Implementation of positional encoding that you can use in your network
class PositionalEncoding(nn.Module):
    def __init__(self, d_model: int, num_positions: int=20, batched=False):
        """
        :param d_model: dimensionality of the embedding layer to your model; since the position encodings are being
        added to character encodings, these need to match (and will match the dimension of the subsequent Transformer
        layer inputs/outputs)
        :param num_positions: the number of positions that need to be encoded; the maximum sequence length this
        module will see
        :param batched: True if you are using batching, False otherwise
        """
        super().__init__()
        # Dict size
        self.emb = nn.Embedding(num_positions, d_model)
        self.batched = batched

    def forward(self, x):
        """
        :param x: If using batching, should be [batch size, seq len, embedding dim]. Otherwise, [seq len, embedding dim]
        :return: a tensor of the same size with positional embeddings added in
        """
        # Second-to-last dimension will always be sequence length
        input_size = x.shape[-2]
        indices_to_embed = torch.tensor(np.asarray(range(0, input_size))).type(torch.LongTensor)
        if self.batched:
            # Use unsqueeze to form a [1, seq len, embedding dim] tensor -- broadcasting will ensure that this
            # gets added correctly across the batch
            emb_unsq = self.emb(indices_to_embed).unsqueeze(0)
            return x + emb_unsq
        else:
            return x + self.emb(indices_to_embed)





def train_lm(args, train_text, dev_text, vocab_index):
    """
    :param args: command-line args, passed through here for your convenience
    :param train_text: train text as a sequence of characters
    :param dev_text: dev text as a sequence of characters
    :param vocab_index: an Indexer of the character vocabulary (27 characters)
    :return: a NeuralLanguageModel instance trained on the given data
    """
    #40 40 ->65
    #2
    #vocab_size, num_positions, d_model, d_internal, num_classes, num_layers):
    #5
    model=Transformer(27, 20, 28, 28, 27, 2)
    
    model.zero_grad()
    model.train()
    #lr=1e-4
    optimizer = optim.Adam(model.parameters(), lr=1e-4)
    #20
    
    scheduler = torch.optim.lr_scheduler.ExponentialLR(optimizer,gamma=0.995)
    # CON 40 SALE!
    num_epochs =30
    loss_fcn = nn.NLLLoss()
    #n = 19
    
     
    #chunks = np.array([" "+train_text[i:i+n] for i in range(0, len(train_text), n)])
    #chunks_labels = np.array([train_text[i-1:i+n] for i in range(1, len(train_text), n)])
    n=20
    chunks_labels = [train_text[i:i+n] for i in range(0, len(train_text), n)]
    chunks=[]
    for i in range(len(chunks_labels)):
        chunks.append(" "+chunks_labels[i][0:-1])
    #n = 20
    chunks_labels_n = [train_text[i:i+n] for i in range(0, len(train_text), n)]
    """
    n_cklabels=len(chunks_labels_n)
    for i in range(n_cklabels):
        chunks.append("  "+chunks_labels_n[i][1:-1])
        chunks_labels.append(chunks_labels_n[i])
    
    n_cklabels=len(chunks_labels_n)
    for i in range(n_cklabels):
        chunks.append("   "+chunks_labels_n[i][2:-1])
        chunks_labels.append(chunks_labels_n[i])
    
    n_cklabels=len(chunks_labels_n)
    for i in range(n_cklabels):
        chunks.append("    "+chunks_labels_n[i][3:-1])
        chunks_labels.append(chunks_labels_n[i])
    
    n_cklabels=len(chunks_labels_n)
    for i in range(n_cklabels):
        chunks.append("     "+chunks_labels_n[i][4:-1])
        chunks_labels.append(chunks_labels_n[i])
    
    n_cklabels=len(chunks_labels_n)

    for i in range(n_cklabels):
        chunks.append("      "+chunks_labels_n[i][5:-1])
        chunks_labels.append(chunks_labels_n[i])
        
    n_cklabels=len(chunks_labels_n)
    for i in range(n_cklabels):
        chunks.append("       "+chunks_labels_n[i][6:-1])
        chunks_labels.append(chunks_labels_n[i])
    
    n_cklabels=len(chunks_labels_n)
    
    for i in range(n_cklabels):
        chunks.append("        "+chunks_labels_n[i][7:-1])
        chunks_labels.append(chunks_labels_n[i])
    
    n_cklabels=len(chunks_labels_n)

    for i in range(n_cklabels):
        chunks.append("         "+chunks_labels_n[i][8:-1])
        chunks_labels.append(chunks_labels_n[i])
    
    n_cklabels=len(chunks_labels_n)

    for i in range(n_cklabels):
        chunks.append("          "+chunks_labels_n[i][9:-1])
        chunks_labels.append(chunks_labels_n[i])
    
    n_cklabels=len(chunks_labels_n)

    for i in range(n_cklabels):
        chunks.append("          "+chunks_labels_n[i][10:-1])
        chunks_labels.append(chunks_labels_n[i])
    
    """
        
    
    
    
    

    #chunks = np.array([train_text[i:i+n] for i in range(0, len(train_text), n)])
    #chunks_labels = np.array([train_text[i-1:i+n] for i in range(2, len(train_text), n)])

    
    
    #chunks = np.array([" "+train_text[i:i+n] for i in range(0, len(train_text), n)])
    #chunks_labels = np.array([train_text[i:i+n] for i in range(1, len(train_text), n)])
    chunks_to_use=[]
    chunks_labels_to_use=[]

    
    for i in range(len(chunks)):
        aux=np.zeros(len(chunks[i]))
        for j in range(len(chunks[i])):
            aux[j]=vocab_index.index_of(chunks[i][j])
        chunks_to_use.append(aux)
    
    for i in range(len(chunks_labels)):
        aux=np.zeros(len(chunks_labels[i]))
        for j in range(len(chunks_labels[i])):
            aux[j]=vocab_index.index_of(chunks_labels[i][j])
        chunks_labels_to_use.append(aux)
    
   
    
    
    #chunks_to_use[-1]=np.delete(chunks_to_use[-1],-1)
    
 
    """
    more_chunks=[]
    more_chunks_labels=[]
    for i in range(len(chunks_to_use)):
        more_chunks.append(chunks_to_use[i])
        more_chunks_labels.append(chunks_labels_to_use[i])
        for j in range(len(chunks_to_use[i])-1):
            more_chunks[-1][j]=26
            chunks_to_use.append(more_chunks[-1])
            chunks_labels_to_use.append(more_chunks_labels[-1])

    """     
    #print(chunks_to_use[-1])
    #print(chunks_labels_to_use[-1])

    
    
    #for i in range(len(more_chunks)) :
    #    chunks_to_use.append(more_chunks[i])
    #    chunks_labels_to_use.append(more_chunks_labels[i])
        
    
    
    
    
    
    
    #print(len(chunks_to_use[-1]))
    #print(len(chunks_labels_to_use[-1]))
    #print("EL CHUNK A ENTRENAR")
    #print(chunks_to_use[-1])
    #print("LOS LABELS DE ENTRENAMIENTO ")

    #print(chunks_labels_to_use[-1])
    
    for t in range(0, num_epochs):
        loss_this_epoch = 0.0
        #random.seed(t)
        # You can use batching if you'd like
        ex_idxs = [i for i in range(0, len(chunks))]
        #model.forward(train[0].input_tensor)
        
        
        
        random.shuffle(ex_idxs)
        for ex_idx in ex_idxs:
            model.zero_grad()
            """
            if len(chunks_to_use[ex_idx])<20 or len(chunks_labels_to_use[ex_idx])<20:
                num_padd_chunks=20-len(chunks_to_use[ex_idx])
                num_padd_chunks_labels_to_use=20-len(chunks_labels_to_use[ex_idx])
                for i in range(num_padd_chunks):
                    chunks_to_use[ex_idx]=np.insert(chunks_to_use[ex_idx],0,26)
                for i in range(num_padd_chunks_labels_to_use):
                    chunks_labels_to_use[ex_idx]=np.insert(chunks_labels_to_use[ex_idx],0,26)
                    #print(chunks_labels_to_use[ex_idx])
            """
            #print(type(chunks_to_use))
            #print(type(chunks_labels_to_use))
            if len(chunks_to_use[ex_idx]) != 20:
                print("chunks_to_use LENGTH  different of 20")
            if len(chunks_labels_to_use[ex_idx]) != 20:
                print("chunks_labels_to_use LENGTH  different of 20")
            pred=model.forward(torch.from_numpy(chunks_to_use[ex_idx]).long())
            loss = loss_fcn(pred,torch.from_numpy(chunks_labels_to_use[ex_idx]).long()) # TODO: Run forward and compute loss
            # 
            # loss.backward()
            # optimizer.step()
            loss_this_epoch += loss.item()
            loss.backward()
            optimizer.step()
        scheduler.step()
        print("Epoch ->",t)
        print("Loss this epoch -> ",loss_this_epoch)
    model.eval()
    
    language_model=NeuralLanguageModel()
    language_model.model=model
    language_model.vocab_index=vocab_index
    return language_model 
    