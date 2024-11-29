# python script

# list all letters in the alphabet in caps
# save into letters

import string

letters = list(string.ascii_uppercase)

# add the following letters to "letters": "CS", "DZS", "GY", "LY", "NY", "SZ", "TY", "ZS"

letters.extend(["CS", "DZS", "GY", "LY", "NY", "SZ", "TY", "ZS"])
letters.sort()

# for each element in "letters", combine "https://uesz.nytud.hu/files/uesz-", "letter", ".pdf" into a string and save these into a list

urls = []
for letter in letters:
    urls.append(f"https://uesz.nytud.hu/files/uesz-{letter}.pdf")
    
# for each url in urls, curl url into ~/Github/RaczRebrus2024cont/uesz/uesz-<letter>.pdf

import os

for url in urls:
    os.system(f"curl {url} -o ~/Github/RaczRebrus2024cont/uesz/{url.split('/')[-1]}")

# check that the files are downloaded

for url in urls:
    assert os.path.exists(f"~/Github/RaczRebrus2024cont/uesz/{url.split('/')[-1]}")
    

# no Q, W, Y, but I missed Oe and Ue, I did this by hand. Whoops.
