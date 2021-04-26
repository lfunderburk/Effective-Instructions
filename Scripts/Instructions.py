# -*- coding: utf-8 -*-
"""
Created on Fri Nov 27 12:23:45 2020

@author: hanh

Code for part of the statistical analysis in "Instructions" paper

"""

# environment 


# import packages
import pandas as pd
import numpy as np
#from scipy import stats
import statsmodels.formula.api as smf
import statsmodels.api as sm 
import argparse
#import os
from pathlib import Path


pd.options.mode.chained_assignment = None

class InstructionsData:
    def __init__(self, file_location):
        self.file_location = file_location
        
    def read_data(self):
        data = pd.read_csv(self.file_location)
        return data


class AverageNMB(InstructionsData):
    pass

class Regression():
    def __init__(self, data, resp_var, exp_var):
        self.data = data
        self.resp_var = resp_var
        self.exp_var = exp_var
    
    def transform_data(self):
        data = self.data
        data = treatment_variable(data)
        data = create_NMB(data)
        dummies = pd.get_dummies(data['treat'])
        data = pd.concat([data, dummies], axis=1)
        clean_data = data
        return clean_data
    
    def var_list(self, baseline):
        exp_var = self.exp_var
        
        rel_var = exp_var.symmetric_difference([baseline])
        list_of_items = ""
        for item in rel_var:
            list_of_items += (item + '+')
    
        list_of_items = list_of_items[:-1]
        return list_of_items
    
    def l_reg (self, baseline, list_of_items):
        resp_var = self.resp_var
        
        dat2 = self.transform_data()
        # drop variable treat, t1 and variable specified as baseline
        dat2 = dat2.drop(['treat', 't1', baseline], axis=1)  
        
        model = smf.ols(resp_var + ' ~ ' + list_of_items, dat2).fit()
        return model.summary()

    def logistic_reg(self, baseline):
        exp_var = self.exp_var
        rel_var = exp_var.symmetric_difference([baseline])
        
        dat_log = self.transform_data()
        dat_log = dat_log.drop(['treat', baseline], axis=1)
        
        X = dat_log[[*rel_var]]
        Y = dat_log[['NMB']]
        
        log_reg = sm.Logit(Y, X).fit() 
        return log_reg.summary()
        

def check_info(data):
    '''

    Parameters 
    ----------
    data : object of type DataFrame
        DESCRIPTION. for now, a pandas dataframe

    Returns
    -------
    info : object of type dict
        DESCRIPTION. dictionary with a key "shape" containing dimensions of 
        the dataframe

    '''
    info = {}
    info["shape"] = data.shape
    return info


def create_NMB(data): # Doing the task and doing it early
    '''
    add description      
    '''
    cond = (data['Time'] <= 1260) & (data['Time']>0) # (before period 22 - second 1261 to 1320)
    data['NMB'] = np.where( cond, 1, 0)
    return data


def treatment_variable(data):
    '''
        
    Returns
    -------
    data : TYPE
        DESCRIPTION.

    '''
    # create a list of our conditions
    try:
        conditions = [
        (data['Quiz']==0),
        (data['AnswersGiven']==0) & (data['Quiz']==1) & (data['Enhanced']==0),
        (data['Paid']==0) & (data['AnswersGiven']==1) & (data['instructionsTwice']==0) & (data['Paper']==0),
        (data['Paid']==1),
        (data['instructionsTwice']==1), 
        (data['Paper']==1),
        (data['AnswersGiven']==0) & (data['Quiz']==1) & (data['Enhanced']==1)
        ]    
        # create a list of the values we want to assign for each condition
        values = ['t1', 't2', 't3', 't4','t5','t6','t7']
        # create a new column and use np.select to assign values to it using our lists as arguments
        data['treat'] = np.select(conditions, values)
    except:
        print("the names of the columns may not match, check column names")
    #return [conditions,values]
    return data


def get_arguments():
    parser = argparse.ArgumentParser(
                                     formatter_class = argparse.RawDescriptionHelpFormatter,
                                     description="Parsing treatment vars")
    parser.add_argument("data_location",
                        help = "location of the data")
    parser.add_argument("response_variable",
                        help = "target variable to predict")
    parser.add_argument("explanatory_variable",
                        help = "set of predictors")
    parser.add_argument("baseline",
                        help = "a treatment as a baseline for comparison")
    options = parser.parse_args()
    return options


if __name__ == '__main__':
    options = get_arguments()
    file_location = Path(options.data_location)
    input_resp_var = options.response_variable
    input_exp_var = options.explanatory_variable
    input_baseline = options.baseline

    data_object = InstructionsData(file_location)
    data = data_object.read_data()
    dat = data[data['Treatment'] == 'Low']
    shape_dic = check_info(dat)
    print(shape_dic)
    

    reg = Regression(data = dat, resp_var = input_resp_var, 
                           exp_var = set(input_exp_var.split(",")))


    treat_var = reg.var_list(input_baseline)
    
    #print(reg.l_reg(input_baseline, treat_var))
    
    #reg.transform_data().l_reg('t3')



