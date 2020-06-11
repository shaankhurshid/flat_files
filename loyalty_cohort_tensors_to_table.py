import os
import h5py
import numpy as np
import pandas as pd
from collections import defaultdict
import time


DIR = '/data/cvrepo/loyalty-cohort-tensors/2020-04-24/'
paths = [os.path.join(DIR, f) for f in os.listdir(DIR)][:1000]


table = []
now = time.time()
for i, path in enumerate(paths):
    print(i / len(paths) * 100, end='\r')
    with h5py.File(path, 'r') as hd5:
        for file_type in hd5.keys():
            for instance in hd5[file_type].keys():
                for data_type in hd5[file_type][instance].keys():
                    table.append({
                        'path': path,
                        'instance': instance,
                        'data_type': data_type,
                        'value': hd5[file_type][instance][data_type][()],
                    })
df = pd.DataFrame(table)
print(time.time() - now, 'seconds elapsed')
df.to_csv('loyalty_cohort_hf.csv', index=False)
