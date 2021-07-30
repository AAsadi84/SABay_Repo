"""\
Description:
    Calculating the net precept and then changing the time step of the
    Net-precipit and save it as a csv file readable for prosou.f subroutines
    in TELEMAC-2D programming Model.
    Caution! input data are monthly data and has a specific format any
    format except this can not read by this module. It needs some upgrading
    to do so.
Date: Feb 05, 2020
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
print('-'*26, 'Module PreEvap_to_TELEMAC', '-'*26 )
#
#
# ---------------------------------------------------------------------------- #
# ------------------------------- Define Class ------------------------------- #
#
class net_precpt:
    """\
    Comments:
        In this class, we change the time step of the precipitation and evaporation
        in diferent class function and find the net precipirtation by subtrackning
        the precipitaion and evaporation ==> {net_precep =  (precp - evap)}
    """
    preFr_yr = 1980; preFr_mth = 12
    evpFr_yr = 1980; evpFr_mth = 12
    def __init__(self,pre_evp_lines, TimeStep_Sec, St_Date_lst ,Num_TimeStep):
        """\
        Comments:
            Define some initial value to the objects
        Arguments:
            pre_evp_Lines -- list that contains the list of lines related to
                both Precipitation ND Evaporation. Caution! first list related to
                precipitation and second one related to Evaporation
            n_pt -- number of time step for each hour
            St_Date_lst -- Original date of TELEMAC Model
            Num_TimeStep -- Number of time steps that Telemac Will be run
        Attributes:
        """
        self.preLines = pre_evp_lines[0]
        self.evpLines = pre_evp_lines[1]
        self.n_pt = int(1*3600/TimeStep_Sec)  # how many step we have in each hour
        self.St_Date = St_Date_lst
        self.NTS = Num_TimeStep
        self.days = []
        self.st_year = 1986
        self.st_month = 12
        self.precp = []
        self.evp = []
    def conversion(self, data_lines):
        """\
        Comments:
            This function first tries to read list of lines of Precipitation and
            evaporation then change to list of words and finally transfer to
            numpy array.
        Arguments:
            data_lines -- list of lines corresponing to evap or Precp
        Returns:
            output_lst -- list of attributes that stored in corresponding csv
                file, either Precipitation or Evaporation
        """
        lst = []
        for line in data_lines:
            words = line.split(',')
            lst.append(words)
        output_lst = lst
        return(output_lst)


    def precp_operation(self):
        """\
        Comments:
            This function gets the list of precipitation from function
            conversion and change it from daily to quarter of an hour
        Arguments:

        Returns:
            self.precp -- the new precipitation data
        """
        lst_data = self.conversion(self.preLines)
        global preFr_yr, preFr_mth
        preFr_yr = int(lst_data[0][0]); preFr_mth = int(lst_data[0][1])
        self.days = [i[2] for i in lst_data]
        new_lst = []
        for i_lst in lst_data:
            i_lst = list(map(lambda s: s.strip(), i_lst))
            i_lst.remove('')
            new_value = []
            for n in range(int(i_lst[2])):
                # add and change the units from in per day to meter per secend
                rep_val = [float(i_lst[n+3])*0.0254/(24*3600)]*24*self.n_pt
                new_value.extend(rep_val)
            new_lst.extend(new_value)
        precp_final = np.array(new_lst, dtype = 'float')
        precp_final = precp_final.reshape(precp_final.size,1)
        self.precp = precp_final

    def evp_operation(self):
        """\
        Comments:
            This function gets the list of evaporation from function conversion
             and change it from daily to quarter of an hour.
        Arguments:

        Returns:
            evp_final  -- the new precipitation data
        """
        lst_data_evp = self.conversion(self.evpLines)

        # check if starting times for both precp and evap are the same
        date_evp = [i[0] for i in lst_data_evp]
        date_evp_st = date_evp[0].split('-')
        evpFr_yr = int(date_evp_st[0]); evpFr_mth = int(date_evp_st[1])
        if evpFr_yr==preFr_yr and evpFr_mth==preFr_mth:
            self.st_year = evpFr_yr; self.st_month = evpFr_mth
            print('Both Evaporation and Precepitation data files are start at the same date!')
        else:
            try:
                self.st_year = preFr_yr; self.st_month = preFr_mth
                date_pre = '{}'.format(preFr_yr)+'-'+'{0:0=2d}'.format(preFr_mth)
                idx_pre_st = date_evp.index(date_pre)
                print(colored('Date of Precp data starts after Evap data!', \
                            'green'))
                lst_data_evp = lst_data_evp[idx_pre_st:]
            except:
                idx_evp_st = (int((evpFr_yr - preFr_yr)*12 + (evpFr_mth - preFr_mth)))
                print('idx_evp_st value:', idx_evp_st)
                if idx_evp_st < len(self.days) and  idx_evp_st > 0:
                    self.st_year = evpFr_yr; self.st_month = evpFr_mth
                    print(colored('Date of Precp data starts before Evap data!', \
                                'yellow'))
                    self.days = self.days[idx_evp_st:]
                else:
                    print(colored('The index is out of the self.days range!', \
                            'red'))
                    sys.exit()

        new_lst = []
        for i_lst in range(min(len(lst_data_evp), len(self.days))):
            new_evp = list(map(lambda s: s.strip(), lst_data_evp[i_lst]))
            evp_value_monthly = float(new_evp[1])
            days_per_month = int(self.days[i_lst])
            # add and change the units from in per month to meter per secend
            evp_val = [evp_value_monthly*0.0254/(days_per_month*24*3600)]*\
                                                days_per_month*24*self.n_pt
            new_lst.extend(evp_val)
        evp_final = np.array(new_lst, dtype = 'float')
        evp_final = evp_final.reshape(evp_final.size,1)
        self.evp = evp_final

    def net_precipitation(self):
        """\
        Comments:
            This function exacutes the evp_operation and precp_operation
            and concatenate both based on the minimum length
        Arguments:

        Returns:
            net_precp -- net precipitation = Precipitation - Gross Evaporation
        """
        # execute functions
        self.precp_operation()
        self.evp_operation()
        # find the net_precep
        min_length = min(len(self.precp), len(self.evp))
        net_precp = self.precp[:min_length] - self.evp[:min_length]
        return(net_precp)
    def sub_set(self):
        """\
        Comments:
            This function make a subset of the Net Precipitation
        """
        net_precp_val = self.net_precipitation()

        ################################################################################
        # define the datetime
        ################################################################################
        dt = datetime.datetime(self.st_year, self.st_month, 1, 0, 0, 0)
        step = datetime.timedelta(minutes = 60/self.n_pt)
        new_len = len(net_precp_val)

        date_time = []
        for i in range(new_len):
             date_time.append(dt.strftime('%m-%d-%Y (%H:%M:%S)'))
             dt += step
        # orginal date of time for TELEMAC model uses to subset the pre_evp
        Org_Date_lst = self.St_Date.split(';')
        Org_Date_new = '{}-{}-{}'.format(Org_Date_lst[1],Org_Date_lst[2],Org_Date_lst[0])
        print('Orginal date of time in PreEvap_to_TELEMAc Module is',\
                Org_Date_new)
        st_idx = date_time.index(Org_Date_new +' (00:00:00)')
        print('The index of This module sub-set is', st_idx)
        end_idx = st_idx + self.NTS + 1
        # Subset the interpolated discharge based on the information of telemac Model
        PreEvp_SubSet = net_precp_val[st_idx:end_idx]
        return(PreEvp_SubSet)

print('-'*22, 'End of Module PreEvap_to_TELEMAC!', '-'*22 )
