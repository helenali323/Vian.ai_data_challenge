import re
# Question 1
# a
vals = [1,-2,3,-9,-7,4,-1,2,7,-3,26]
cols = [0,3,1,4,2,3,0,1,2,0,2]
rows = [0,0,1,1,2,2,3,3,3,4,4]

coo = {'vals':vals, 'cols':cols, 'rows':rows}

# b
vals = [1, -2, 3, -9,-7,4,-1,2,7,-3,26]
cols = [0,3,1,4,2,3,0,1,2,0,2]
row_ptr = [0,2,4,6,9,11,None,None,None,None,None]
csr = {'vals':vals, 'cols':cols, 'row_ptr':row_ptr}


# Question 2
def get_dimensions(text):
    # something with regex
    searchObj = re.search(r'(.*?)x(.*)', text)
    dim1 = searchObj.group(1)
    dim2 = searchObj.group(2)
    return int(dim1), int(dim2)


# Question 3
def get_pdf_names(text):
    # something with regex
    searchObj = re.search(r'(.*)\.pdf$', text)
    if searchObj:
        pdf_name = searchObj.group(1)
    else:
        pdf_name = None
    return pdf_name