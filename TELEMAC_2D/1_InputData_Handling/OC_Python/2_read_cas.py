"""\
Summary:
    In this module TELEMAC input file '.cas' is  called then the necessary
    information for other module is extracted and saved it in approprate
    variable that can be callable latter in other modules
Date: Jan 31, 2020
Author: Alireza Asadi
"""
#
#
# ---------------------------------------------------------------------------- #
# ---------------------------- DLibrary Packages ----------------------------- #
#
import sys, os
import re

#
#
# ---------------------------------------------------------------------------- #
# --------------------------- Define directories ----------------------------- #
#
# define the main directory
Cur_dir = os.getcwd()  # define the current directory
main_dir = os.path.dirname(Cur_dir) # define the main directory

# define the tides' directory and file path
print('-'*79)
sce_num = input('Which Scenario number are you desiring to simulate? ')
print('-'*79)
cas_dir = os.path.join(main_dir, 'Scenarios\\Scenario_{}\\Telemac'.format(sce_num))
cas_path = os.path.join(cas_dir, 'Sabay_sce10_2min_May28_2020.cas')

#
#
# ---------------------------------------------------------------------------- #
# -------------------------- Define functions -------------------------------- #
#
def rdFileContent(file):
    """\
    Comments:
        This function gets the file name and save it as a list of lines
    Arguments:
        file -- file path
    Returns:
        int_lines -- list of lines related to called file
    """
    Op_file = open(file, 'r')
    int_lines = Op_file.readlines()
    Op_file.close()
    return(int_lines)

def saveCAS_dic(int_list):
    """\
    Comments:
        This function gets a list of cas file and save it as a dictionary file
    Arguments:
        int_list -- a list of lines in .cas file
    Returns:
        dict_cas -- a dictionary of commands in the .cas file
    """
    dict_cas = dict()
    key_on = False
    for line in int_list:

        # to make sure if any command continue in next line or not
        if key_on == True:
            dict_cas[dict_key] = line
            key_on = False
            continue
        if  re.search('^/', line): continue
        matchObj = re.search(r'(.*)[:=](.*)', line, re.M|re.I)
        if matchObj:
            if matchObj.group(2) == '':
                key_on = True
            dict_key = re.sub('  +|\t', '',matchObj.group(1))
            dict_val = matchObj.group(2)
            dict_cas[dict_key] = dict_val
    return(dict_cas)
#
#
# ---------------------------------------------------------------------------- #
# ----------------------------- read files ---------------------------------- #
#
CAS_file_dict = saveCAS_dic(rdFileContent(cas_path))
TimeStep_Sec = int(CAS_file_dict)['TIME STEP'])
Org_Date = re.sub(' *','', CAS_file_dict)['ORIGINAL DATE OF TIME'])
Num_TimeStep = int(CAS_file_dict)['NUMBER OF TIME STEPS'])
print('\n\n')
print('-'*79)
print('-'*31, 'Module read_cas', '-'*31 )
print('Time step of TELEMAC model is', TimeStep_Sec, 'seconds')
print('Orginal date of time in TELEMAC is', Org_Date)
print('Number of Time Steps is', Num_TimeStep)
print('-'*27, 'End of Module read_cas!', '-'*27)
