import os
import json
import requests
import pandas as pd
from rapidfuzz import fuzz, process
import re

# load parameters
path_to_secr = os.path.abspath(os.path.dirname(os.path.dirname(__file__))) + '\config.json'
with open(f'{path_to_secr}', "r") as f:
    config = json.load(f)
API_KEY = config["census"]["key"]  # Replace with your actual Census API key
BASE_URL = config["census"]["url"]

# specify global parameters
YEAR = "2013"
DATASET = "acs/acs1"
# GEOGRAPHY = "state:*"  #by state
GEOGRAPHY = "us:1"  #us total

def get_variable_metadata(year=YEAR, dataset=DATASET):
    url = f"https://api.census.gov/data/{year}/{dataset}/variables.json"
    response = requests.get(url)
    return response.json()["variables"]

def search_variable(
    search_concept,
    search_term, 
    df, 
    top_n=5
):
    df_concept = df[df["concept"].str.contains(search_concept,case=False,na=False)]
    choices = df_concept["label"]
    print(df_concept)
    matches = process.extract(search_term, choices, scorer=fuzz.token_sort_ratio, limit=top_n)
    result = []
    for match in matches:
        idx = df_concept[choices == match[0]].index[0]
        result.append(df_concept.loc[idx])
    return pd.DataFrame(result)

DATASET = "acs/acs5"
vars_dict = get_variable_metadata()
df_vars = pd.DataFrame([
    {"name": name, "label": meta.get("label", ""), "concept": meta.get("concept", "")}
    for name, meta in vars_dict.items()
])

# results = search_variable("SEX BY AGE","sex by age", df_vars, top_n=10)
# print(results)
# B01001

def get_variables(
    table_prefix,
    df,
    collapse = False
):
    df_filter = df[df["name"].str.startswith(table_prefix)]
    df_filter = df_filter.sort_values("name").reset_index(drop=True)
    if collapse:
        return df_filter['name'].unique()
    else:
        return df_filter

# VARIABLES = get_variables("B01001_",df_vars)
# print(VARIABLES)

# get single year age-sex distribution
VARIABLES = get_variables("B01001_",df_vars,True)
# url = f"{BASE_URL}/{YEAR}/{DATASET}"
# params = {
#     "get": ",".join(VARIABLES),
#     "for": GEOGRAPHY,
#     "key": API_KEY
# }

# response = requests.get(url, params=params)
# if response.status_code != 200:
#     raise Exception(f"API call failed: {response.status_code} - {response.text}")

# data = response.json()
# data_df = pd.DataFrame(data[1:], columns=data[0])
# data_df['yr'] = YEAR
# print(data_df)


# get multiple year age-sex distribution
def get_multi_acs1(
    base_url,
    dataset,
    geography,
    key,
    yr_lst,
    var_lst
):
    # api search parameters
    params = {
        "get": ",".join(var_lst),
        "for": geography,
        "key": key
    }

    # initialize dataframe
    data_df = pd.DataFrame()

    # loop over years
    yr_lst.append(yr_lst[-1]+1)
    for yr in yr_lst:
        url = f"{base_url}/{yr}/{dataset}"
        response = requests.get(url, params=params)

        if response.status_code != 200:
            raise Exception(f"API call failed: {response.status_code} - {response.text}")

        data = response.json()
        new_df = pd.DataFrame(data[1:], columns=data[0])
        new_df['yr'] = yr
        data_df = pd.concat([data_df,new_df],ignore_index=True)
    
    # return dataframe
    return data_df

# get multi-year sex-by-age distributions
sexbyage = get_multi_acs1(
    base_url = BASE_URL,
    dataset = DATASET,
    geography = GEOGRAPHY,
    key = API_KEY,
    yr_lst = list(range(2011,2020)),
    var_lst = VARIABLES
)
sexbyage.drop(columns = ['us'],inplace=True)
sexbyage_long = sexbyage.melt(id_vars = ['yr','B01001_001E'],var_name = 'name',value_name='num')
sexbyage_long.rename(columns={'B01001_001E': 'denom'}, inplace=True)
sexbyage_long[['denom','num']] = sexbyage_long[['denom','num']].astype(int)
sexbyage_long['wt'] = sexbyage_long['num']/sexbyage_long["denom"]

# map ACS variables to CDM concepts
def extract_age_bound(s):
    if not isinstance(s, str):
        return pd.Series([None, None])  # Handles None or non-string types
    
    nums = [int(n) for n in re.findall(r'\d+', s)]
    
    if len(nums) == 2:
        return pd.Series([nums[0], nums[1]])  # if two numbers
    elif len(nums) == 1:
        if nums[0] <= 5:
            return pd.Series([0, nums[0]])  # if a single number <= 5
        elif nums[0] >= 85:
            return pd.Series([nums[0], 200])  # if a single number between 5 and 85
        elif 5 <= nums[0] <= 85:
            return pd.Series([nums[0], nums[0]])  # if a single number
    return pd.Series([None, None])  # No match

VAR_MAP = get_variables("B01001_",df_vars)
split_label = VAR_MAP['label'].str.split('!!',expand=True)
split_label.columns = ['estimated','total','sex','age']
split_label.drop(columns = ['estimated','total'],inplace = True)
split_label[['age_lb','age_ub']] = split_label['age'].apply(extract_age_bound)
VAR_MAP = pd.concat([VAR_MAP,split_label],axis = 1)
VAR_MAP =  VAR_MAP[~(VAR_MAP['age'].isna() | (VAR_MAP['age_lb'].isna()&VAR_MAP['age_ub'].isna()))]

# join together and create the standardized population distribution table
sexbyage_ref = pd.merge(
    sexbyage_long,
    VAR_MAP,
    on = 'name',
    how = 'inner'
)
sexbyage_ref = sexbyage_ref[['yr','name','sex','age','age_lb','age_ub','wt']]
sexbyage_ref['sex'] = sexbyage_ref['sex'].str[0]
path_to_ref = os.path.abspath(os.path.dirname(os.path.dirname(os.path.dirname(__file__)))) + '/ref'
sexbyage_ref.to_csv(f"{path_to_ref}/census_sexbyage.csv", index=False)


