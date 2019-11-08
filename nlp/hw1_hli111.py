import nltk
from nltk import stem
import spacy
from spacy.lemmatizer import Lemmatizer
from nltk import word_tokenize
import math
import pandas as pd
import numpy as np

nltk.download('wordnet')

# Question 1


def solve(word_list):
    wnl = stem.WordNetLemmatizer()
    porter = stem.porter.PorterStemmer()
    a = [porter.stem(word) for word in word_list]
    b = [wnl.lemmatize(word) for word in word_list]
    lemmatizer = Lemmatizer()
    c = [lemmatizer.lookup(word) for word in word_list]
    res = {}
    res['a'] = a
    res['b'] = b
    res['c'] = c
    return res

word_list = ['logistic', 'logistics', 'shoe', 'shoes']
problem1 = solve(word_list)

# Question 2


def tokenize(text):
    words = word_tokenize(text)
    words = [word.lower() for word in words if word.isalpha()]
    return words


def get_ngrams(text, min_n, max_n):
    # Exercise: FILL IN METHOD
    text = tokenize(text)
    ngram_dict = {}
    for n in range(min_n, max_n + 1):
        for k in range(len(text) - n + 1):
            ngram = ' '.join(text[k:k + n])
            if ngram not in ngram_dict:
                ngram_dict[ngram] = 1
            else:
                ngram_dict[ngram] += 1
    return ngram_dict

# Question 3


def get_vocab_frequency(list_of_strings):
    wordSet = ' '.join(list_of_strings)
    wordSet = list(set(tokenize(wordSet)))
    list_of_dicts = [get_dict(string,wordSet) for string in list_of_strings]
    return list_of_dicts


def get_dict(string, wordSet):
    s = tokenize(string)
    str_dic = {word: 0 for word in wordSet}
    for word in s:
        if word in str_dic:
            str_dic[word] += 1/len(s)
    return str_dic

# Question 4


def get_idf(list_of_strings):
    wordSet = ' '.join(list_of_strings)
    wordSet = list(set(tokenize(wordSet)))

    N = len(list_of_strings)
    idf_dict = {word: 0 for word in wordSet}

    for string in list_of_strings:
        string = list(set(tokenize(string)))
        for word in string:
            idf_dict[word] += 1

    for word, val in idf_dict.items():
        idf_dict[word] = math.log10(N / float(val))

    return idf_dict


def calculate_tfidf(tf, idf):
    tfidf = {}
    for word, val in tf.items():
        tfidf[word] = val * idf[word]
    return tfidf


def get_tfidf(list_of_strings):
    tfs = get_vocab_frequency(list_of_strings)
    idf = get_idf(list_of_strings)
    tfidf = [calculate_tfidf(tf, idf) for tf in tfs]
    tfidf = pd.DataFrame(tfidf).values
    tfidf = np.asmatrix(tfidf)
    return tfidf
