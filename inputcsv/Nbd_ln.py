#!/usr/bin/env python
# coding: utf-8

# In[2]:


import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


# In[109]:


house_id = np.loadtxt("ResidentialArea10k.csv",skiprows=1,usecols=3,delimiter=",")


# In[ ]:





# In[93]:


hid = np.ones((2500,2))
for i in range(0,2500):
    hid[i][0] = i+1
print(hid)


# In[6]:


r = np.random.lognormal(4.586,0.198,100)
print(r)
plt.hist(r,bins='fd')


# In[8]:


nid_list = []
for i in range(30):
    nid_list.append(i+1)
print(nid_list)


# In[10]:


nid = []
for i in range(30):
    rd = int(np.random.lognormal(4.586,0.198))
    for j in range(rd):
        nid.append(nid_list[i])
        
nid_final = nid[0:2500]
print(len(nid_final))


# In[106]:


for i in range(2500):
    hid[i][1] = nid_final[i]

print(hid)


# In[113]:


print(house_id)


# In[114]:


nbd_id = []
for i in range(len(house_id)):
    house = house_id[i]
    for k in range(len(hid)):
        if house == hid[k][0]:
            nbd_id.append(hid[k][1])
            
            


# In[119]:


df = pd.read_csv("ResidentialArea10k.csv")


# In[120]:


df["Neihbourhood ID"] = nbd_id


# In[121]:


df.to_csv("ResidentialArea10k.csv", index=False)


# In[ ]:




