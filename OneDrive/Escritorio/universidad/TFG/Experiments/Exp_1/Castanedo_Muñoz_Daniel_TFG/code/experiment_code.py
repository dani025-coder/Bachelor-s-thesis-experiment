
def generate_prompt(system_condition, user_condition):
    """ Load prompt information stored in prompts.json"""
    
    with open('prompt_tfg.json', 'r') as file:
        prompts = json.load(file)
    #We choose the personality of ChatGPT. Each personality will have observations with treatment AE and control.
    if system_condition == 1 or system_condition == 2:
       system_prompt = (prompts['system_prompt_AI'])

    elif system_condition == 3 or system_condition == 4:
        system_prompt = (prompts['system_prompt_human'])
        
    elif system_condition == 5 or system_condition == 6:
        system_prompt = (prompts['system_prompt_human_p'])
        
    #We choose the treatment (control / with AE)
    if user_condition == 1:
        user_prompt = (prompts['user_prompt_NAE'] )
        
    else:
        user_prompt = (prompts['user_prompt_AE'])
        
    #Combinations (system_condition, user_condition): (1,1), (1,2), (2,1), (2,2), ..., (6,2)
    return system_prompt, user_prompt

def invoke_gpt(model, temperature, system_prompt, user_prompt):
    """Invoke the GPT LLM through the OpenAI API"""
    key = " insert API key "
    openai.api_key = key
    completion = openai.ChatCompletion.create(
          model= model,
          messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt}
                
        
            ],

            temperature = temperature,
            max_tokens = 2000
        )
    
    return completion 



def completion_to_outcome(completion):
    """Clean the completion object returned by the OpenAI API"""
    outcome_dict = dict(completion.choices[0].message) 
    outcome = outcome_dict['content']
    # Use regular expression to find all numerical sequences and join them with spaces
    outcome_numerical = ' '.join(re.findall(r'\d+', outcome))
    return outcome, outcome_numerical


def valid_answer(outcome_numerical):
    """Filter GPT outcomes"""
    outcome_numerical_list = outcome_numerical.split()
    if len(outcome_numerical_list) == 4:
        if int(outcome_numerical_list[1]) < 5 and int(outcome_numerical_list[-1]) < 5 and int(outcome_numerical_list[2]) < 16:
            return True
    return False

    
        
    
    
def NA_count(na_counts, system_condition):
    """Count NAs"""
    na_counts[system_condition - 1] += 1
    return na_counts

    

def classify_outcome(outcome_numerical):
    """Separate outcome in different variables"""
    outcome_numerical_list = outcome_numerical.split()
    task_1_data, enrolment_data, cont_rate_data, type_inv_data = map(int, outcome_numerical_list)
    return task_1_data, enrolment_data, cont_rate_data, type_inv_data



def append_to_csv(filename, data, columns=None):
    """Append the data to filename.csv """
    SUBFOLDER = 'data'
    os.makedirs(SUBFOLDER, exist_ok=True)  # Create the subfolder if it doesn't exist
    filepath = os.path.join(SUBFOLDER, filename)
    new_data = pd.DataFrame(data, columns=columns)
    # Check if the file exists to determine if headers should be written
    write_headers = not os.path.exists(filepath)
    new_data.to_csv(filepath, mode='a', header=write_headers, index=False)
    new_data_2 = pd.read_csv('data/raw_outcomes_file.csv')
    #print(new_data_2)
    new_data_2.to_excel('data/data_1_clean.xlsx', index=False)



def generate_observation(system_condition, user_condition, na_counts):
    """"Generate a single observation from ChatGPT"""
    system_prompt, user_prompt = generate_prompt(system_condition, user_condition)
    print("system_prompt is: ", system_prompt)


    completion = invoke_gpt(model, temperature, system_prompt, user_prompt)
    outcome, outcome_numerical = completion_to_outcome(completion)
    print("The outcome is: ", outcome)

    
    if valid_answer(outcome_numerical):

        task_1_data, option_data, cont_rate_data, type_inv_data = classify_outcome(outcome_numerical)
        print(task_1_data, option_data, cont_rate_data, type_inv_data)
        #Create variables from the data collected
        b_gpt, human, human_p = create_v_s_prompt(system_prompt)
        aut_enrol = create_v_AE(user_condition)
        #create variable default (=1 if selects default, =0 if does not select default)
        default = create_v_default(option_data, user_condition)
        participation = create_v_participation(option_data)
        no_inv, bond_market, stock_market = create_v_type_inv(type_inv_data)
    
        data_list = [b_gpt, human, human_p, task_1_data, option_data, participation, default, aut_enrol, cont_rate_data, type_inv_data, no_inv, bond_market, stock_market]
        append_to_csv(
            filename = 'raw_outcomes_file.csv',
            data = [data_list],
            columns = ['b_gpt'] + ['human'] + ['human_p'] + ['task_1'] + ['option'] + ['participation'] + ['default'] + ['aut_enrol'] + ['cont_rate'] + ['type_investment'] + ['no_inv'] + ['bond_market'] + ['stock_market'])
    elif valid_answer(outcome_numerical) == False:
        na_counts = NA_count(na_counts, system_condition)
        print("the NAs respecting the 6 categories are the following, respectively: ", na_counts)
        generate_observation(system_condition, user_condition, na_counts)




def generate_batch(n_batch, system_condition, user_condition):
    """Generate a batch of obervations from a parameter list"""
    n_treatments = 2
    n_systems = 3
    for i in range(n_treatments*n_systems):
        for j in range(n_batch):
            generate_observation(system_condition, user_condition, na_counts)
        system_condition, user_condition = update_treatments(system_condition, user_condition) 

            
        
def update_treatments(system_condition, user_condition):
    """Update condition variables to update the treatments and prompts in update_prompts"""

    system_condition += 1
       
    if user_condition == 1:
        user_condition += 1 
    else:
        user_condition = user_condition - 1   
    
    return system_condition, user_condition

 
def create_v_s_prompt(system_prompt):
    """Create a qualitative variable for system_prompt"""

    if system_prompt == "Behave as baseline ChatGPT":
        b_gpt = 1
        human = 0
        human_p = 0

    elif system_prompt == "Behave as a human":
        b_gpt = 0
        human = 1
        human_p = 0

    elif system_prompt == "Behave as a human with the following characteristic 'procrastination tendency'":
        b_gpt = 0
        human = 0
        human_p = 1

    return b_gpt, human, human_p


def create_v_AE(user_condition):
    """create variable AE (=1 if AE is applied, =0 if AE not applied)"""

    if user_condition == 1:
        aut_enrol = 0
    else:
        aut_enrol = 1

    return aut_enrol


def create_v_default(enrolment_data, user_condition):
    """create variable default (=1 if selects default, =0 if does not select default)"""


    if enrolment_data == 1 and user_condition == 1:
        default = 1
    elif enrolment_data == 2 and user_condition == 1:
        default = 0
    elif enrolment_data == 1 and user_condition == 2:
        default = 0
    elif enrolment_data == 2 or enrolment_data == 3 and user_condition == 2:
        default = 1

    return default 


def create_v_participation(option_data):
    """create variable participation 1 if the individual participates in the plan, 0 if not"""
    if option_data == 1:
        participation = 0
    else:
        participation = 1

    return participation


def create_v_type_inv(type_inv_data):
    """create qualitative variables for type_inv_data"""

    if type_inv_data == 0:
        no_inv = 1
        bond_market = 0
        stock_market = 0

    elif type_inv_data == 1:
        no_inv = 0
        bond_market = 1
        stock_market = 0

    elif type_inv_data == 2:
        no_inv = 0
        bond_market = 0
        stock_market = 1

    return no_inv, bond_market, stock_market




import xlsxwriter 
import re
import random
import datetime
import pandas as pd
import os
import json
import openai 
import requests
from itertools import product
from dotenv import load_dotenv



# Define experiment parameters
model = "gpt-3.5-turbo" 
temperature = 1
n_batch = 1000 #There are 6 combinations of treatment and prompt. n_batch determines how many observations within each combination.
n = n_batch*6
system_condition = 1 #Variable which uptates when we finish 2 sets of observations. Updates system_prompt
user_condition = 1 #Variable which uptates when we finish 1 set of observations. Updates user_prompt
na_counts = [0, 0, 0, 0, 0, 0]


#Run experiment
generate_batch(n_batch, system_condition, user_condition)
