"""\
Title:
    Read the wind data and extract the their spead and direction and
    change the time step from an hour to 5 minutes.
Date:   Feb 07, 2020
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
print('-'*27, 'Module wind_to_TELEMAC', '-'*28 )
#
#
# ---------------------------------------------------------------------------- #
# ------------------------------- Define Class ------------------------------- #
#
class wind:
    """\
    Comments:
        This class takes the list of lines corresponded to wind data that as
        a .txt file and change the time step.
        Caution! it is only applicable to the files with the same format and
        content of the file that saved in corresponding directory.
    Attributes:

    """
    def __init__(self, wind_lst, TimeStep_Sec, St_Date, Num_TimeStep):
        """\
        Comments:
            Define some initial value to the objects
        Arguments:
            Dis_lst -- list of lines related to Discharge data file
            n_pt -- number of time step for each hour
            St_Date -- Original date of TELEMAC Model
            Num_TimeStep -- Number of time steps that Telemac Will be run
        Attributes:
        """
        self.wind_lst = wind_lst
        self.n_pt = int(1*3600/TimeStep_Sec)  # how many step we have in each hour
        self.St_Date = St_Date
        self.NTS = Num_TimeStep
        self.header = []


    def conversion(self, data_lines):
        """\
        Comments:
            This function first tries to read list of lines of data and
            then change to list of words and finally transfer to numpy array.
        Arguments:
            data_lines -- input data saved as a list of lines
        Returns:
            output_lst -- list of attributes that stored in corresponding csv
                        file, if data data cannot read as a numpy format
        """
        lst = []
        for line in data_lines:
            words = line.split(',')
            lst.append(words)
        output_lst = lst
        return(output_lst)

    def wind_operation(self):
        """\
        Comments: This function gets the list of wind from function
                conversion and change it to time step we desire
        Arguments:
            wind_lines -- Wind's input data saved as a list of lines
        Returns:
            wind_final -- the new wind data
        """
        nt = self.n_pt
        wind_lines = self.wind_lst
        time_step = 3600/nt
        time_step_min = time_step/60
        # To make sure if our time step is greater than one minute
        if time_step_min < 1:
            print("The time step you choose is smaller than one minutes!")
            print("Code needs to be modified!")
            sys.exit()

        lst_data = self.conversion(wind_lines)
        new_lst = []
        cnt = 0
        resi = 0
        # Cleaning up process (find the columns corresponded to wind data we need)
        for i_lst in lst_data:
            # scape the first line
            if cnt == 0:
                self.header = i_lst[0].strip().split()
                DateTime_Idx = self.header.index('YR--MODAHRMN')
                cnt = 1
                date_time = datetime.datetime(1800,1,1,0,0,0)
                date_time_lag = date_time
                continue

            # remove the '\n' and split each variable to a certain list
            i_lst = i_lst[0].strip().split()
            i_datetime = list(str(i_lst[DateTime_Idx]))

            yr = int(''.join(i_datetime[0:4]))
            mm = int(''.join(i_datetime[4:6]))
            dd = int(''.join(i_datetime[6:8]))
            hr = int(''.join(i_datetime[8:10]))
            min = int(''.join(i_datetime[10:12]))
            sec = int(0)

          # Update the date and save the previous date-time
            date_time_old = date_time
            date_time = datetime.datetime(yr, mm, dd, hr, min, sec)
         # save the previous data and use them for furture calculation
            if cnt == 1:
                cnt = 2
                key_open = True
                wind_sp = float(i_lst[4]) ; wind_dir = float(i_lst[3])
                # set the first row time set to one which is multiples of time step
                min_change = math.floor(min*60/time_step)
                min_new = min
                if min_change != 0:
                    min_new = int(min_change * time_step / 60)
                date_time = datetime.datetime(yr, mm, dd, hr, min_new, sec)
                date_time_lag = date_time
                continue

            cnt += 1
            dt = date_time - date_time_old

          # scape the line that time is smaller than time step we have
            if (dt.days * 3600*24 + dt.seconds) < time_step:
                if key_open == True:
                    print('Datetime for the ones smaller than time step is:\n', date_time)
                    date_time = date_time_lag
                continue

            key_open = False

          # update wind spead and its direction
            wind_sp_old = wind_sp; wind_dir_old = wind_dir
            try:
                if float(i_lst[4]) == 0 :
                    wind_sp  = 0 ; wind_dir = 990
                else :
                    wind_sp = float(i_lst[4]) ; wind_dir = float(i_lst[3])
            except:
                wind_sp  = 0 ; wind_dir = 990

          # calculate the time lag
            dt_lag = (date_time - date_time_lag)
            dt_lag_sec = dt_lag.days*3600*24 + dt_lag.seconds

            if dt_lag_sec/3600 > 48:    #<<=== if time step greater that 2 days consider the wind equal zero
                dt_lag_sec_resi = (dt_lag_sec + resi)
                n_subset = math.floor(dt_lag_sec_resi/time_step)
                resi = dt_lag_sec_resi - n_subset * time_step
                ts_min = int(time_step_min)
                value = [[(date_time_lag + datetime.timedelta(minutes=ts_min*i)),0,0]\
                            for i in range(n_subset)]
                new_lst.extend(value)

            else:
                dt_lag_sec_resi = (dt_lag_sec + resi)
                n_subset = math.floor(dt_lag_sec_resi/time_step)
                resi = dt_lag_sec_resi - n_subset * time_step
                ts_min = int(time_step_min)
                wind_vx_ms = - wind_sp_old * math.cos(math.radians(90-wind_dir_old)) * 0.44704 #  0.44704 conversion value from mile/hr to m/s
                wind_vy_ms = - wind_sp_old * math.sin(math.radians(90-wind_dir_old)) * 0.44704
                value = [[(date_time_lag + datetime.timedelta(minutes=ts_min*i)),wind_vx_ms,wind_vy_ms]\
                            for i in range(n_subset)]
                new_lst.extend(value)

      # update date_time lag
            dt_lst_v = datetime.timedelta(minutes=ts_min*n_subset)
            date_time_lag = date_time_lag + dt_lst_v

        wind_final = new_lst
        return(wind_final)

    def sub_set(self):
        """\
        Comments:
            This function make a subset of the wind data
        """
        # exacute the function wind_operation
        wind_dataset = self.wind_operation()
        wind_date_set = [i[0] for i in wind_dataset]
        # orginal date of time for TELEMAC model uses to subset the pre_evp
        Org_Date_lst = self.St_Date.split(';')
        Org_date_new = datetime.datetime(int(Org_Date_lst[0]), \
                int(Org_Date_lst[1]), int(Org_Date_lst[2]) , 0, 0 , 0)
        print('Orginal date of time in PreEvap_to_TELEMAc Module is',\
                Org_date_new)
        st_idx = wind_date_set.index(Org_date_new)
        print('The index of This module sub-set is', st_idx)
        end_idx = st_idx + self.NTS + 2

        wind_subset = wind_dataset[st_idx:end_idx]
        return(wind_subset)

print('-'*23, 'End of Module wind_to_TELEMAC!', '-'*24 )
