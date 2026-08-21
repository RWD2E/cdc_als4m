import pandas as pd
import json
import urllib.request as urlreq
import os
import re

def split_part_multisql(
    which_sql, #["snow","postgres","spark","mysql","sqlserver","oracle"]
    string,
    delimiter,
    index
):
    sqlqry = ''
    if which_sql in ('snow','postgres'):
        sqlqry += '''
            split_part('''+ string +''','''+ "'"+ delimiter +"'"+''','''+ str(index) +''')
        '''
    elif which_sql in ('spark','mysql'):
        sqlqry += '''
            substring_index('''+ string +''','''+ "'"+ delimiter +"'"+''','''+ str(index) +''')
        '''
    elif which_sql == 'sqlserver':
        sqlqry += '''
            string_split('''+ string +''','''+ "'"+ delimiter +"'" +''','''+ str(index) +''')
        '''
    elif which_sql == 'sqlserver':
        sqlqry += '''
            regexp_substr('''+ string +''', [^'''+ "'"+ delimiter +"'" +''']+,1,'''+ str(index) +''')
        '''
    else:
        Warning("The SQL version is not supported!")

    return sqlqry

class JsonBlockVS:
    TOPIC_ENCODER = {
        "1": "characteristic",
        "2": "history",
        "3": "event",
        "4": "finding",
        "5": "diagnostics/intervention",
        "6": "economics",
        "7": "nonclinical"
    }

    PURPOSE_ENCODER = {
        "1": {
            "1":"demographics",
            "2":"socioeconomics"
        },
        "2":{
            "1":"family history",
            "2":"medical history"
        },
        "3": {
            "1":"neurologic",
            "2":"pulmonary/respiratory",
            "3":"psychiatric/behavioral",
            "4":"cardiovascular",
            "5":"gastrointestinal",
            "6":"hepatobiliary",
            "7":"renal/urinary",
            "8":"endocrine/metabolic",
            "9":"musculoskeletal",
            "10":"immunologic/infectious",
            "11":"dermatologic",
            "12":"hematologic",
            "13":"oncologic",
            "14":"reproductive",
            "15":"general signs/symptoms",
            "16":"functional",
            "17":"ophthalmologic",
            "18":"otolaryngologic",
            "19":"oral/dental",
            "20":"rheumatologic",
            "21":"genetic/congenital",
            "22":"trauma/injury",
            "23":"toxicologic/poisoning",
            "24":"perioperative/anesthesia-related",
            "25":"nutrition-related",
            "26":"sleep disorders",
            "27":"pediatric/neonatal-specific",
            "28":"gynecologic/obstetric",
            "29":"allergic/hypersensitivity",
            "30":"PRO",
            "31":"adverse event"
        },
        "4":{
            "1":"biological/molecular markers",
            "2":"anthropometric measures",
            "3":"physiological markers",
            "4":"functional markers",
            "5":"imaging markers",
            "6":"behavioral markers",
            "7":"environmental markers"
        },
        "5":{
            "1":"exposure",
            "2":"concomitant medications",
            "3":"interventional precedures",
            "4":"diagnostic precedures",
            "5":"substance use",
            "6":"devices",
            "7":"care setting"
        },
        "6":{
            "1":"insurance provider",
            "2":"insurance plan",
            "3":"utilization"
        },
        "7":{
            "1":"other"
        }
    }

    CLASS_ENCODER = {
        "1": "core",
        "2": "recommended",
        "3": "supplemental",
        "4": "exploratory"
    }

    VALUETYPE_ENCODER = {
        "1": "nominal",
        "2": "ordinal",
        "3": "discrete",
        "4": "continuous",
        "5": "boolean",
        "6": "date"
    }

    VALUERANGE_ENCODER = {
        "1": {},  
        "2": {}, 
        "3": {
            "min":int(),
            "max":int(),
            "low": {
                "value": int(),
                "unit": "",
                "incld": 1
            },
            "high": {
                "value": int(),
                "unit": "",
                "incld": 1
            }
        },
        "4": {
            "min":float(),
            "max":float(),
            "low": {
                "value": float(),
                "unit": "",
                "incld": 1
            },
            "high": {
                "value": float(),
                "unit": "",
                "incld": 1
            }
        },
        "5": {
            "0": "absence",
            "1": "presence"
        },
        "6": {}
    }

    SYSTEM_ENCODER = {
        "1":"loinc",
        "2":"icd9cm",
        "3":"icd9proc",
        "4":"icd10cm",
        "5":"icd10pcs",
        "6":"snomedct",
        "7":"cpt4",
        "8":"hpc",
        "9":"ndc",
        "10":"atc",
        "11":"rxnorm",
        "12":"ndfrt",
        "13":"cvx",
        "14":"drg",
        "15":"npiTaxonomy",
        "16":"cmsSpecialty",
        "17":"cmsTaxonomy",
        "18":"pcornet",
        "99":"other"
    }

    PROPERTY_ENCODER = {
        "1":"codePrecision",
        "2":"codeRange",
        "3":"codeList"
    }

    OP_ENCODER = {
        "1":{
            "1":"descendent-of",
            "2":"child-of",
            "3":"descendent-leaf"
        },
        "2":{
            "1":"in",
            "2":"not-in"
        },
        "3":{
            "1":"exists"
        }
    }

    def __init__(self,filepath,idstarter,idlength):
        self.filepath = filepath
        self.idstarter = idstarter
        self.idlength = idlength
    
    def get_existing_id(self):
        with open(self.filepath,'r') as file:
            jdata = json.load(file)
        existing_id_num = [int(num) for item in jdata for num in re.findall(r'\d+', item['id'])]
        return existing_id_num

    def generate_new_id(self) -> str:
        new_id = max(self.get_existing_id())+1
        return self.idstarter + str(new_id).zfill(self.idlength)

    # Function to create a JSON data block
    def create_json_data_block(self):
        data_block = {
            "id": self.generate_new_id(),
            "name": input("Enter name: "),
            "description": input("Enter description: ")
        }
        # prompt topic selection
        for key, value in self.TOPIC_ENCODER.items(): print(f"{key}:{value}")
        topic_choice = input("Enter choice: ")
        data_block["topic"] = self.TOPIC_ENCODER[topic_choice]

        # prompt purpose selection
        for key, value in self.PURPOSE_ENCODER[topic_choice].items(): print(f"{key}:{value}")
        data_block["purpose"] = self.PURPOSE_ENCODER[topic_choice][input("Enter choice: ")]

        # prompt relatedArtifact selections
        data_block["relatedArtifact"] = dict()
        for key, value in self.CLASS_ENCODER.items(): print(f"{key}:{value}")
        data_block["relatedArtifact"]["class"] = self.CLASS_ENCODER[input("Enter choice: ")]

        for key, value in self.VALUETYPE_ENCODER.items(): print(f"{key}:{value}")
        valuetype_choice = input("Enter choice: ")
        data_block["relatedArtifact"]["valueType"] = self.VALUETYPE_ENCODER[valuetype_choice]
        data_block["relatedArtifact"]["valueRange"] = self.VALUERANGE_ENCODER[valuetype_choice]

        # prompt coding system selection
        # allow multiple include blocks
        data_block["compose"] = {
            "include": []
        }
        while True:
            add_include = input("Do you want to add an 'include' block? (1=yes/0=no): ").strip().lower()
            if add_include == '1':
                for key, value in self.SYSTEM_ENCODER.items(): print(f"{key}:{value}")
                include_entry = {
                    "system": self.SYSTEM_ENCODER[input("Enter choice: ")],
                    "concept": [],
                    "filter": []
                }
                data_block["compose"]["include"].append(include_entry)
                
                # optional concept block
                while True:
                    add_concept = input("Do you want to add a concept? (1=yes/0=no): ").strip().lower()
                    if add_concept == '1':
                        concept_entry = {
                            "code": input("Enter code: "),
                            "display": input("Enter display: ")
                        }
                        data_block["compose"]["include"][-1]["concept"].append(concept_entry)
                    else:
                        if not data_block["compose"]["include"][-1]["concept"]:
                            del data_block["compose"]["include"][-1]["concept"]  # Remove if empty
                        break
                    
                # optional filter block
                while True:
                    add_filter = input("Do you want to add a filter? (1=yes/0=no): ").strip().lower()
                    if add_filter == '1':
                        for key, value in self.PROPERTY_ENCODER.items(): print(f"{key}:{value}")
                        property_choice = input("Enter property: ")
                        for key2, value2 in self.OP_ENCODER[property_choice].items(): print(f"{key2}:{value2}")
                        op_choice = input("Enter operation: ")
                        input_value = input("Enter value (separated by comma): ")
                        filter_entry = {
                            "property": self.PROPERTY_ENCODER[property_choice],
                            "op": self.OP_ENCODER[property_choice][op_choice],
                            "value": [item.strip() for item in input_value.split(',')]
                        }
                        data_block["compose"]["include"][-1]["filter"].append(filter_entry)
                    else:
                        if not data_block["compose"]["include"][-1]["filter"]:
                            del data_block["compose"]["include"][-1]["filter"]  # Remove if empty
                        break
            else:
                break

        return data_block

    def add_json_block(self):
        new_block = self.create_json_data_block()
        print(new_block)
        confirm_add = input("Do you want to add the new json block? (1=yes/0=no): ").strip().lower()
        if confirm_add == '1':
            # open existing file and add json block
            with open(self.filepath,'r') as file:
                jdata = json.load(file)
            jdata.append(new_block)
            # overwrite with the new json file
            with open(self.filepath,'w') as file:
                json.dump(jdata,file,indent = 4)
            return "The new json data block is added"
        else:
            return "The new json data block is rejected"

def expand_range(range_expression):
    range_lst = range_expression.split('-')
    prefix = range_lst[0][0:1]
    if prefix.isalpha():
        range_expand = [prefix + str(y) for y in list(range(int(range_lst[0][1:]),int(range_lst[1][1:])+1))]
    else:
        range_expand = [str(y) for y in list(range(int(range_lst[0]),int(range_lst[1])+1))]
    return range_expand

def json2ref(
    json_url, #url to valueset json file (rawcontent)
    save_csv_to #location to save the csv file
):
    # load json file
    json_url = urlreq.urlopen(json_url)
    json_file = json.loads(json_url.read())
    
    # collect selected keys
    csv_lst = []
    for item in json_file:
        for chunk in item["compose"]["include"]:
            add_chunk = {
                'id':item["id"],
                'name':item["name"],
                'description':item["description"],
                'codesystem':chunk["system"]
            }
            if "filter" in chunk:
                for filter_item in chunk["filter"]:
                    if filter_item['property'] == 'codeRange':
                        code_expand = [expand_range(x) for x in filter_item['value']]
                        add_chunk['code'] = code_expand
                        add_chunk['op'] = ['exists']*len(code_expand)
                    else: 
                        add_chunk['code'] = filter_item['value']
                        add_chunk['op'] = [filter_item["op"]]*len(filter_item['value'])
                    
            if "concept" in chunk: 
                add_chunk['code'] = [x["code"] for x in chunk["concept"]]
                add_chunk['op'] = ['exists' if 'op' not in x else x["op"] for x in chunk["concept"]]
    
            csv_lst.append(add_chunk)

    # create DF from json data and expand
    df = pd.DataFrame(csv_lst)
    def expand_row(row):
        # Zip the lists and repeat the other columns
        return pd.DataFrame({
            'code': row['code'],
            'op': row['op'],
            **{col: [row[col]] * len(row['code']) for col in df.columns if col not in ['code', 'op']}
        })
    expanded_df = pd.concat(df.apply(expand_row, axis=1).tolist(), ignore_index=True)
    expanded_df = expanded_df[['id','name','description','codesystem','code','op']]
    expanded_df = expanded_df.explode('code')

    # save to csv
    expanded_df.to_csv(save_csv_to,index=False)
    return('new valueset saved as ref csv')

class QueryFromJson:
    # Class-level cache for cdtype_encoder to remember input across instances
    _cdtype_cache = {}
    
    def __init__(
        self,
        url, #url to json file
        sqlty, #which type of sql ["snow","postgres","spark","mysql","sqlserver","oracle"]
        cd_field, #code field
        date_fields, #list of all date fields
        other_fields, #list of other fields needed to be retained
        srctbl_name, #source table name
        sel_keys = list(), #list of selected keys to be queried, can be empty
        sel_domain = "", #specify concept domains for more efficient prompting ["dx","px","lab","rx"], can be empty
        cdtype_field = "", #code type field, can be empty
        val_field = "" #value field, can be empty
    ):
        self.url = url
        self.sqlty = sqlty
        self.cd_field= cd_field
        self.cdtype_field = cdtype_field
        self.date_fields = date_fields
        self.other_fields = other_fields
        self.srctbl_name = srctbl_name
        self.sel_keys = sel_keys
        self.sel_domain = sel_domain
        self.val_field = val_field
        self._cdtype_cache = {}

    @staticmethod
    def add_quote(lst):
        lst_quote = ["'"+str(x)+"'" for x in lst]
        return (lst_quote)
    
    @staticmethod
    def parse_filter(lst):
        cddict = {}
        for item in lst:
            if item["property"]=="codePrecision" and item["op"]=="descendent-of":
                decimal = [len(x.split('.')[1]) if '.' in x else 0 for x in item["value"]]
                try: cddict["0"] = [item["value"][index] for index, current_value in enumerate(decimal) if current_value == 0]
                except ValueError: return None
                try: cddict["1"] = [item["value"][index] for index, current_value in enumerate(decimal) if current_value == 1]
                except ValueError: return None
                try: cddict["2"] = [item["value"][index] for index, current_value in enumerate(decimal) if current_value == 2]
                except ValueError: return None
                try: cddict["3"] = [item["value"][index] for index, current_value in enumerate(decimal) if current_value == 3]
                except ValueError: return None

            elif item["property"]=="codeRange" and item["op"]=="in":
                cddict["9"] = []
                for x in item["value"]:
                    key_quote = expand_range(x)
                    cddict["9"].extend(key_quote)

            elif item["property"]=="codeList" and item["op"]=="exists":
                cddict["9"] = item["value"]

            else:
                print("filter property or op not defined in the valuset json file.")
                pass
        
        cddict = {k: v for k, v in cddict.items() if v}
        return(cddict)
    
    @staticmethod
    def parse_concept(lst):
        codes = [item["code"] for item in lst]
        return(codes)

    def gen_cdtype_encoder(self):
        # initialize persistent storage on the object if needed
        if not hasattr(self, "_cdtype_cache"):
            self._cdtype_cache = {}

        domain_to_codes = {
            "dx":  ["icd9cm", "icd10cm", "snomed", "drg"],
            "px":  ["icd9proc", "icd10pcs", "cpt", "hpc"],
            "lab": ["loinc"],
            "rx":  ["rxnorm", "ndc"]
        }

        # Flatten and sort all keys
        allkeys = sorted({code for codes in domain_to_codes.values() for code in codes})

        # Determine which codes to ask for
        if self.sel_domain == "":
            codes_to_prompt = allkeys
        elif self.sel_domain in domain_to_codes:
            codes_to_prompt = domain_to_codes[self.sel_domain]
        else:
            raise ValueError(f"Unknown sel_domain '{self.sel_domain}'. ")

        allprompts = {code: f"Enter Code Type Value for {code}: " for code in allkeys} 
        cdtype_encoder = {}
        for code in codes_to_prompt:
            # Check if this code type already has a cached value
            if code in QueryFromJson._cdtype_cache:
                cdtype_encoder[code] = QueryFromJson._cdtype_cache[code]
            else:
                # Prompt for input and cache it
                user_input = input(allprompts[code])
                cdtype_encoder[code] = user_input
                QueryFromJson._cdtype_cache[code] = user_input

        return(cdtype_encoder)
        
    def gen_qry_ref(self):     
        # load json valueset file
        if self.url.startswith(("http://", "https://")):
            # Remote URL
            with urlreq.urlopen(self.url) as response:
                json_file = json.loads(response.read())
        else:
            # Local file path
            with open(self.url, "r", encoding="utf-8") as f:
                json_file = json.load(f)

        # load cdtype mapping
        cdtype_map = self.gen_cdtype_encoder()

        # generate reference dictionary for where clause
        qry_out = {}
        for x in json_file:  
            qryx_orlst = []
            for y in x["compose"]["include"]:
                if y["system"] in cdtype_map: 
                    # code type 
                    qry_cdtype = " "
                    if self.cdtype_field != "":
                        qry_cdtype += self.cdtype_field + "='" + cdtype_map[y["system"]] + "' and "
                    
                    # value range
                    qry_val = " "
                    if x["relatedArtifact"]["valueType"]=="continuous" and self.val_field != "":
                        if "high" in x["relatedArtifact"]["valueRange"]:
                            incld_dict = {0:">", 1:">="}
                            high_val = x["relatedArtifact"]["valueRange"]["high"]["value"]
                            incld_ind = x["relatedArtifact"]["valueRange"]["high"]["incld"]
                            qry_val += " and " + self.val_field + incld_dict[incld_ind] + str(high_val)

                        if "low" in x["relatedArtifact"]["valueRange"]:
                            incld_dict = {0:"<", 1:"<="}
                            low_val = x["relatedArtifact"]["valueRange"]["low"]["value"]
                            incld_ind = x["relatedArtifact"]["valueRange"]["low"]["incld"]
                            qry_val += " and " + self.val_field + incld_dict[incld_ind] + str(low_val)

                    # codes
                    qryxy_orlst = []
                    if "filter" in y: 
                        cdref = self.parse_filter(y["filter"])
                        if '0' in cdref:
                            if y["system"] in ('icd9cm','icd10cm'):
                                qryxy_orlst.append(
                                    split_part_multisql(self.sqlty,self.cd_field,'.',1) + ''' in ('''+ ','.join(self.add_quote(cdref["0"])) +''')
                                ''')
                            else:
                                # e.g., icd10pcs has hierarchical structure
                                qryxy_orlst.append('''
                                    substring('''+ self.cd_field +''',1,'''+ str(len(cdref["0"][0])) +''') in ('''+ ','.join(self.add_quote(cdref["0"])) +''')
                                ''')
                        elif '1' in cdref:
                            qryxy_orlst.append('''
                                substring('''+ self.cd_field +''',1,5) in ('''+ ','.join(self.add_quote(cdref["1"])) +''')
                            ''')
                        elif '2' in cdref:
                            qryxy_orlst.append('''
                                substring('''+ self.cd_field +''',1,6) in ('''+ ','.join(self.add_quote(cdref["2"])) +''')
                            ''')
                        else:
                            cdref_leaf = (cdref.get("3") or []) + (cdref.get("9") or [])
                            qryxy_orlst.append('''
                                ''' + self.cd_field + ''' in ('''+ ','.join(self.add_quote(cdref_leaf)) +''')
                            ''')
                        qryx_orlst.append(qry_cdtype + ''' (''' + ' or '.join(qryxy_orlst) + ''')''' + qry_val)

                    else:
                        cdref_leaf = self.parse_concept(y["concept"])
                        qryx_orlst.append(qry_cdtype + self.cd_field + ''' in ('''+ ','.join(self.add_quote(cdref_leaf)) + ''')''' + qry_val)

                else: 
                    pass

            # add non-empty query entry to dict
            if len(qryx_orlst) > 0: 
                qry_out[x["name"]] = ') OR ('.join(qryx_orlst)

        return qry_out 
    
    def gen_qry(self):
        selqry_lst = []
        qry_dict = self.gen_qry_ref()
        for k,v in qry_dict.items():
            if len(self.sel_keys) == 0 or k in self.sel_keys:
                nondate_fields = self.other_fields + [self.cd_field]+([self.cdtype_field] if self.cdtype_field else ["'"+self.cd_field+"'"])
                if len(self.date_fields) > 1:
                    date_field_func = "coalesce(" + ','.join(self.date_fields) + ")"
                else:
                    date_field_func = self.date_fields[0]
                selqry_lst.append('''
                    select ''' + ','.join(nondate_fields) + 
                        " ," + date_field_func + " as CD_DATE" + 
                        " ,'"+ k +"' as CD_GRP" '''
                    from '''+ self.srctbl_name +'''
                    where ('''+ v +''')
                ''')
        complt_qry = ' union all '.join(selqry_lst)
        return(complt_qry)

