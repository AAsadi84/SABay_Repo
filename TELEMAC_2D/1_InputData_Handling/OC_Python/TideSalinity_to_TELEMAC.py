
"""\
Description:
    Changing the time step of the time series of the tide and Salinity
    and save it as a csv file readable for bord.f subroutine in TELEMAC
    programming Model.
Date: Jan 27, 2020
Author: Alireza Asadi
"""
#
#
# ---------------------------------------------------------------------------- #
# -------------------------- library Some Packages --------------------------- #
#
import numpy as np
import sys, os
import datetime, math
from colorama import init
from termcolor import colored
init()
#
print('\n\n')
print('-'*79)
print('-'*23, 'Module TideSalinity_to_TELEMAC', '-'*24 )
#
#
# ---------------------------------------------------------------------------- #
# ----------------------------- Define functions ----------------------------- #
#
def TideSalinity(lst_OpF, n_pt,  Org_Date_lst = None, Num_TimeStep = 0):
    """\
    Comments:
        This function gets list of lists related to two input files corsponding
        to tide and salinity values for two open sea side boundaries and change
        time step from hour to 5 minutes
    Arguments:
        lst_OpF -- list that contains a list of lines related to tide and salinity
        n_pt -- number of time step for each hour
        Org_Date_lst -- Original date of TELEMAC Model
        Num_TimeStep -- Number of time steps that Telemac Will be run
    Return:
        tide_salinity_sub -- numpy file including tide and salinity data
    """

    def conversion(t_lines = None):
        """\
        Comments:
            This function first tries to read list of lines of tidal data and
            then change to list of words and finally transfer to numpy array.
        Arguments:
            t_lines -- tide's input data saved as a list of lines
        Returns:
            output_np -- numpy array of attributes that stored in corresponding csv file
        """
        lst = []
        cnt = 0
        for line in t_lines:
            cnt += 1
            words = line.split(',')
            words[-1] = words[-1].strip()
            del words[8:10]
            if cnt == 1:
                nonlocal header
                header = [s.replace('"', '') for s in words]
                continue
            lst.append(words)
        output_np = np.array(lst, dtype = 'float')
        return(output_np)

    def TimeStep_Operation(vector_np = None, ntp = 12):
        """\
        Comments:
            This function gets the numpy array of function conversion and
            adds 3 more tides to each interval (change time step from hour to
            quarter of hour)
        Arguments:
            t_lines -- tide's input data saved as a list of lines
            ntp -- number of time step in one hour
        Returns:
            t_s_final -- the new tide elevation or Salinity data
        """
        y2 = vector_np[1:] # get the water elevation and remove the first element
        y1 = vector_np[0:(len(array_np)-1)] # get the water elevation and remove the last element
        m = y2 -y1 # slope of the water elevation in time
        t_s = y1
        for i in range(1,ntp):
            t_s_01 = ((1./ntp) * i * m) + y1 # add each time step(quarter of hour) to tide
            t_s = np.concatenate((t_s,t_s_01), axis = None)
        t_s = t_s.reshape((ntp,len(y1)))
        t_s_value = t_s.T
        t_s_final = t_s_value.reshape((t_s_value.size,1))
        return t_s_final

    i_cnt_OpF = 0
    header = []
    for i_OpF in lst_OpF:
        i_cnt_OpF +=1
        array_np = conversion(i_OpF) # get the return value of the function conversion
        # _______________________________________________________
        # Read tide and Salinity array and change it to np vector
        col_idx_Tide = header.index('Elev') # find the index of the tide
        Tide_Vnp = array_np[:,col_idx_Tide]
        col_idx_Salinity = header.index('Salinity') # find the index of the Salinity
        Salinity_Vnp = array_np[:,col_idx_Salinity]
        # ___________________________________________________________
        # create a first tide and salinity vector for 5 min time step
        if i_cnt_OpF == 1:
            Tide_Vnp_NTS = TimeStep_Operation(Tide_Vnp, n_pt)
            Salinity_Vnp_NTS = TimeStep_Operation(Salinity_Vnp, n_pt)
        # concatenate tides and Salinities seperately
        if i_cnt_OpF > 1:
            Tide_Vnp_NTS = np.concatenate((Tide_Vnp_NTS,TimeStep_Operation(Tide_Vnp, n_pt)), axis = 1)
            Salinity_Vnp_NTS = np.concatenate((Salinity_Vnp_NTS,TimeStep_Operation(Salinity_Vnp, n_pt)), axis = 1)

    # Concatinate tide and salinity together
    tide_salinity = np.concatenate((Tide_Vnp_NTS, Salinity_Vnp_NTS), axis = 1)

    # define the datetime
    dt = datetime.datetime(1987, 1, 1, 0, 0, 0)
    step = datetime.timedelta(minutes = 60/n_pt)
    ts_new_len = len(tide_salinity )

    print ('tide length:', ts_new_len)
    date_time = []
    for i in range(ts_new_len):
         date_time.append(dt.strftime('%m-%d-%Y (%H:%M:%S)'))
         dt += step

    # Orginal date of time
    Org_Date_lst = Org_Date.split(';')
    Org_Date_new = Org_Date_lst[1] + '-' + Org_Date_lst[2] + '-' + Org_Date_lst[0]
    st_idx = date_time.index(Org_Date_new +' (00:00:00)')
    end_idx = st_idx + Num_TimeStep + 1
    tide_salinity_sub = tide_salinity[st_idx:end_idx] # subset the tide values
    print ('Date Time Index is:', st_idx)
    print ('End time index is:', end_idx)
    print (tide_salinity_sub[:13])
    return tide_salinity_sub

print('-'*20, 'End of Module TideSalinity_to_TELEMAC', '-'*20 )
