"""\
Description:
    Convert the time step for the fresh-water discharges coresponding to each
    scenario and save it as a csv file readable for 'prosou.f' subroutines in
    TELEMAC-2D programming model.
Date: Feb 04, 2020
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
print('-'*21, 'Module FreshwaterDischatge_to_TELEMAC', '-'*21 )
print('...\n'*3)
#
#
# ---------------------------------------------------------------------------- #
# ------------------------------- Define Class ------------------------------- #
#
class INTRPL_DIS:
    """\
    Comments:
        This class takes the list of lines corresponded to fresh-water discharges
        as a .csv file and change the time step.
        Caution! it is only applicable to the files with the same format and
        content of the file that saved in corresponding directory.
    Attributes:

    """
    def __init__(self, Dis_lst, TimeStep_Sec, St_Date_lst, Num_TimeStep):
        """\
        Comments:
            Define some initial value to the objects
        Arguments:
            Dis_lst -- list of lines related to Discharge data file
            n_pt -- number of time step for each hour
            St_Date_lst -- Original date of TELEMAC Model
            Num_TimeStep -- Number of time steps that Telemac Will be run
        Attributes:
        """
        self.dis_lst = Dis_lst
        self.n_pt = int(1*3600/TimeStep_Sec)  # how many step we have in each hour
        self.St_Date = St_Date_lst
        self.NTS = Num_TimeStep
        self.discharge = []
        self.init_arr = []
        self.initDate = []
        self.date_lst = []

    def conversion(self):
        """\
        Comments:
            This function first tries to read list of lines of discharge data
             and then change to list of words and finally transfer to numpy
             array.
        Returns:
            output_lst -- list of attributes that stored in corresponding
                csv file
        """
        lst = []
        cnt = 0
        for line in self.dis_lst:
            cnt += 1
            words = line.split(',')
            date_hr = words[0].replace('/', '-')
            words[-1] = words[-1].strip()
            del words[0]
            if cnt <= 2: continue
            lst.append(words)
            self.initDate.append(date_hr)
        output_np = np.array(lst, dtype = 'float')
        self.init_arr = output_np
        return(self.init_arr)

    def Interpolation(self, vector_np):
        """\
        Comments:
            This function gets the numpy array of function conversion and
            adds 3 more discharges to each interval (change time step from
            hour to quarter of hour).
        Arguments:
            arr_np -- self.output_np
        Returns:
            q_final -- the new discharge data
        """
        nt = self.n_pt
        y2 = vector_np[1:] # get the discharge and remove the first element
        y1 = vector_np[0:(len(vector_np)-1)] # get the discharge and remove the last element
        m = y2 -y1 # slope of the discharge in time
        discharge = y1 # y intersection
        for i in range(1,nt):
            discharge_01 = (1./nt * i * m) + y1 # add each time step(quarter of hour) to discharge
            discharge = np.concatenate((discharge,discharge_01), axis = None)
        discharge = discharge.reshape((nt,len(y1)))
        q_value = discharge.T
        q_final = q_value.reshape((q_value.size,1))
        return (q_final)

    def Dis_Operation(self):
        """\
        Comments:
            This function is the main function that operate all the process
            that is necessary for the operation. (Get the initial values, send
            the command to other functions and finally create a np. array
            for discharge)
        """
        # exacute required functions
        arr_np = self.conversion()
        self.time_set()

        for i in range(0,arr_np.shape[1]):
            vector_np = arr_np[:,i]

            disch = self.Interpolation(vector_np)
            if i == 0:
                disch_new = disch # asign the first column to the variable
            else:
                disch_new = np.concatenate((disch_new, disch), axis = 1) # concatenate different column
        self.discharge = disch_new
        return(self.discharge)

    def time_set(self):
        """\
        Comments:
            This function creates a time set for converted discharge based on
            the input date-time data
        """
        int_mins = 60/self.n_pt
        for i_date in self.initDate:
            for i_int in range(self.n_pt):
                i_int_min =  '{0:0=2d}'.format(int(int_mins * i_int))
                self.date_lst.append(i_date.replace('00', i_int_min))
    def sub_set(self):
        """\
        Comments:
            This function make a subset of the discharge value
        """
        # Orginal date of time
        self.Dis_Operation()
        Org_Date_lst = Org_Date.split(';')
        yr =int(Org_Date_lst[0]); mth = int(Org_Date_lst[1]); dd = int(Org_Date_lst[2])
        Org_Date_new = '{}-{}-{}'.format(mth, dd, yr)
        st_idx = self.date_lst.index(Org_Date_new +' 0:00')
        end_idx = st_idx + self.NTS + 1
        # Subset the interpolated discharge based on the information of telemac Model
        Dis_SubSet = self.discharge[st_idx:end_idx]
        return(Dis_SubSet)


print('-'*17, 'End of Module FreshwaterDischatge_to_TELEMAC!', '-'*17 )
