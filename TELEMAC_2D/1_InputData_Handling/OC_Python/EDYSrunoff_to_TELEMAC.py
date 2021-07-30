"""\
Description:
    Changing the time step of the time series of the runoff created by
    EDYS and save it as a csv file readable for prosou.f subroutine in TELEMAC
    programming Model.
Date: Feb 15, 2020
Author: Alireza Asadi
"""
#
#
# ---------------------------------------------------------------------------- #
# -------------------------- library Some Packages --------------------------- #
#
import numpy as np
import sys, os
import math, datetime, re
from time import strptime
from colorama import init
from termcolor import colored
init()
#
print ('\n\n')
print ('-'*79)
print ('-'*25, 'Module EDYSrunoff_to_TELEMAC', '-'*26)
print ('...\n'*3)
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
# ------------------------------- Define Class ------------------------------- #
#
class runoff:
    """\
    Comments:
        This class gets daily runoff data (In some days there is no runoff data),
        recognize the missing data and consider as a zero value and then change
        the time step from daily to 5 minutes data.
    Attributes:

    """
    def __init__(self, cas_path,runoff_path, num_node = 590):
        """\
        Comments:
            This function get time step and list of lines related to runoff Data
            and save it as initial attrinutes to use it for later operations
        Argumments:
            cas_path: path through the .cas data file
            runoff_path: path through the runoff data sended by Dr. Coldren
            num_node: number of nodes that runoff will be asigned to

        """
        self.t_step = int(saveCAS_dic(rdFileContent(cas_path))['TIME STEP'])
        self.St_Date = re.sub(' *','',\
                    saveCAS_dic(rdFileContent(cas_path))['ORIGINAL DATE OF TIME'])
        self.n_pt = Num_TimeStep = int(saveCAS_dic(rdFileContent(cas_path))['NUMBER OF TIME STEPS'])
        self.data = rdFileContent(runoff_path)
        self.runoff = []
        self.num_node = int(num_node)
        self.conversion()

    def conversion(self):
        """\
        Comments:
            This function first tries to read list of lines of runoff data and
            then change to list of words and finally transfer to numpy array.
        Argumments:
            None
        Returns:
            None
        """
        lst = []
        cnt = 0

        for line in self.data:
            cnt += 1
            if cnt == 1: continue

            words_lst = [0]
            words = line.split(',')
            words[-1] = words[-1].strip()
            words_lst.extend(words)

            if cnt == 2:
                yy_st = 1987; mm_st = strptime(words[2],'%b').tm_mon; dd_st = int(words[3])
                hr_st = 0; min_st = 0; ss_st = 0
                date_time_st = datetime.datetime(yy_st, mm_st, dd_st, hr_st, min_st, ss_st)
                date_memory = date_time_st
                cnt_idx = 0
            cnt_idx += 1
            yy = yy_st + int(words[1]) - 1; mm = strptime(words[2],'%b').tm_mon; dd = int(words[3])
            hr = 0; min = 0; ss = 0
            date_time = datetime.datetime(yy, mm, dd, hr, min, ss)
            words_lst[2] = date_time.year; words_lst[3] = date_time.month
            missing_days = (date_time - date_memory).days
            days_itv = (date_time - date_time_st).days
            sec_itv = days_itv * 24 * 60 * 60
            if cnt_idx%self.num_node == 1:
    #            print('cnt_idx is:',cnt_idx)
                if missing_days > 1:
                    lst_size = len(lst)
                    print(lst_size)
                    lst_new = lst.copy()
                    missing_date = date_memory
                    for i in range(1,missing_days):
                        step_date = datetime.timedelta(days=1)
                        missing_date += step_date
                        for j in range(self.num_node):
                            words_lst_miss = lst_new[lst_size - self.num_node + j].copy()
                            words_lst_miss[2] = missing_date.year
                            words_lst_miss[3] = missing_date.month
                            words_lst_miss[4] = missing_date.day
                            words_lst_miss[5] = 0
                            sec_itv_miss = (days_itv - (missing_days - i))*24*60*60
                            words_lst_miss[7] = sec_itv_miss
                            lst.append(words_lst_miss)
            if cnt_idx%self.num_node == 0:
                date_memory = date_time
                cnt_idx = 0
            words_lst.append(sec_itv)
            lst.append(words_lst)
        self.runoff = lst

'''\
with open(output_path, 'w') as w:
    for m_runoff in lst_runoff:
        value = str(m_runoff[0]) + ', ' + str(m_runoff[1]) + ', ' + \
            str(m_runoff[2]) + ', ' + str(m_runoff[3]) + ', ' + str(m_runoff[4]) + \
            ', ' + str(m_runoff[5]) + ', ' + str(m_runoff[6]) +  ', ' + str(m_runoff[7]) + "\n"
        w.write(value)
w.close()
'''
print('-'*21, 'End of Module EDYSrunoff_to_TELEMAC!', '-'*22 )
