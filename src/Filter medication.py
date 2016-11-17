
# coding: utf-8

# ![GCAT | Genomes for Life](images/logo-GCAT.png)

# # Filter medication

# In[1]:

import difflib
import re
import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.pylab import *

# ## Load tables
# 
# - Qüestionari
# - Participants

# In[2]:

mica = pd.read_csv("/home/labs/dnalab/share/lims/R/gcat-cohort/output/export/QUESTIONARI/data.csv", low_memory=False)
participants = pd.read_csv("/home/labs/dnalab/share/lims/R/gcat-cohort/output/export/Participants/data.csv")

all = participants
all = pd.merge(all, mica, on=['entity_id'])


# In[3]:

coded = list(all.columns[all.columns.str.contains('_MEDICACION')])
columns = coded.copy()
columns.append('entity_id')


# ## Coded medication
# 
# Falten els AINES!!

# In[4]:

others = list(all.columns[all.columns.str.contains('_MEDICACION_.*_TIPO')])
columns = others.copy()
columns.append('entity_id')

ds = all.loc[:, all.columns.isin(columns)]


# In[5]:

coded_meds = pd.melt(ds, id_vars = ['entity_id'], value_vars = list(others))
coded_meds = coded_meds[~coded_meds.value.isnull()]
coded_meds.head()


# In[6]:

def to_illness(colname):
    colname = re.sub(r"(_MEDICACION)", "", colname)
    colname = re.sub(r"(^MEDICACION_)", "", colname)
    colname = re.sub(r"(_TIPO)", "", colname)
    colname = re.sub(r"(_OTROS)", "", colname)
    colname = re.sub(r"(_\d)", "", colname)
    colname = re.sub(r"(SALUD_HOMBRE_)", "", colname)
    colname = re.sub(r"(SALUD_MUJER_)", "", colname)
    colname = re.sub(r"(\d)", "", colname)
    return colname

def to_atc(value):
    return re.sub(r"([,;].*$)", "", value)

def to_name(value):
    return re.sub(r"(^.*[,;])", "", value)

coded_meds.variable = coded_meds.variable.apply(lambda x: to_illness(x))
coded_meds['CODI_ATC'] = coded_meds.value.apply(lambda x: to_atc(x))
coded_meds['NOMBRE'] = coded_meds.value.apply(lambda x: to_name(x))

coded_meds[coded_meds.variable == 'HTA'].head()


# ## Load ATC codes

# In[11]:

atc = pd.read_csv('inst/extdata/medications/Codi_ATC.csv', sep=';', encoding='latin1')
atc.head()


# In[12]:

prostata = pd.read_csv('inst/extdata/medications/Medicaments_Prostata.csv', sep=',')
prostata.head()


# In[13]:

hormones = pd.read_csv('inst/extdata/medications/hormones.csv', sep=',')
hormones.head()


# ## Select hand-written medication columns

# In[14]:

others = list(all.columns[all.columns.str.contains('_MEDICACION_.*_OTROS')])
columns = others.copy()
columns.append('entity_id')

df = all.loc[:, all.columns.isin(columns)]


# In[15]:

a = pd.melt(df, id_vars = ['entity_id'], value_vars = others)
a = a[~a.value.isnull()]
a.variable = a.variable.apply(lambda x: to_illness(x))
a[a.variable == 'HTA'].head()


# In[16]:

import string

def clean(val):
    val = val.upper()
    val = val.translate(str.maketrans({key: None for key in string.punctuation}))
    val = re.sub(r"(\d+)", "", val)
    val = re.sub(r"( MG$)", "", val)
    val = re.sub(r"( G$)", "", val)
    val = val.strip()
    return val

def compare(x, atc, threshold):
    
    medications = list(map(lambda x:x.lower(), list(atc.NOMBRE)))
    ls = difflib.get_close_matches(x.lower(), medications, 1, threshold)

    ls = ''.join(ls)

    field_codi_name = "{}_{}".format("CODI_ATC", threshold)
    field_name_name = "{}_{}".format("NOMBRE", threshold)

    if ls in medications:
        return pd.Series({field_codi_name: atc.loc[medications.index(ls), 'CODI_ATC'],
                          field_name_name: atc.loc[medications.index(ls), 'NOMBRE']})
    return pd.Series({field_codi_name: None, field_name_name: None})


# ## Explore similarity thresholds

# In[17]:

medications = pd.concat([atc, prostata, hormones], ignore_index=True)
b = a.copy()
b['clean'] = b['value'].apply(lambda x: clean(x))


# In[18]:

c = b['clean'].apply(lambda x: compare(x, atc, 1))
b = pd.concat([b, c], axis=1)

c = b['clean'].apply(lambda x: compare(x, atc, 0.9))
b = pd.concat([b, c], axis=1)

#c = b['clean'].apply(lambda x: compare(x, atc, 0.8))
#b = pd.concat([b, c], axis=1)

#c = b['clean'].apply(lambda x: compare(x, atc, 0.7))
#b = pd.concat([b, c], axis=1)


# ## Merge categorical and hand-written medications

# In[19]:

hand_written_meds = b
hand_written_meds = hand_written_meds.rename(columns = {'CODI_ATC_0.9' : 'CODI_ATC'})
hand_written_meds = hand_written_meds.rename(columns = {'NOMBRE_0.9' : 'NOMBRE'})
hand_written_meds = hand_written_meds[~hand_written_meds.CODI_ATC.isnull()]


# In[20]:

columns = [
    'variable',
    'value',
    'CODI_ATC',
    'NOMBRE'
]
hand_written_meds[hand_written_meds.variable == 'HTA'][hand_written_meds.CODI_ATC_1.isnull()].head()[columns]


# ## Write all medication

# In[21]:

columns = ['entity_id', 'variable', 'CODI_ATC', 'NOMBRE']
merged_meds = pd.concat([coded_meds[columns], hand_written_meds[columns]])
merged_meds.columns = ['entity_id', 'CONDITION', 'ATC_CODE', 'DRUG_NAME']
merged_meds.to_csv('output/medications/data.csv', index=False)


# ## Write errors

# In[22]:

errors = b
errors.to_csv('output/medications/errors.csv', index=False)

