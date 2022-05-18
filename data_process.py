#!/usr/bin/python3
# -*- coding: utf-8 -*-


import pandas as pd
import math
pd.set_option('display.max_columns', None)



def filter_data():
	"""
	filt需要的資料
	:return: new_df
	"""
	# 3: season 4: point 7: tide
	df_new = pd.DataFrame()
	df = pd.read_excel('data_oil.xlsx', sheet_name='整體資料')
	for i in range(0, len(df.index)):
		if(df.iloc[i][7] == 'L'):
			if pd.isnull(df.iat[i,2]) or pd.isnull(df.iat[i,9]):
				continue
			df_new = df_new.append(df.iloc[i])
	return df_new

def process_na_val(df):
	"""
	補空值
	
	SP : 1
	SM : 2
	A  : 3
	W  : 4
	"""
	df.reset_index(drop=True, inplace=True)
	for i in range(10, df.shape[1]):
		for j in range(0, df.shape[0]):
			if pd.isnull(df.iloc[j][i]):
				df.iat[j, i] = 0.0001
			if df.iloc[j][i] == 0:
				df.iat[j, i] = 0.0001
			if df.iat[j, i] == "<0.01":
				df.iat[j,i] = 0.0001
			if df.iat[j, i] == "沙地":
				df.iat[j, i] = 0.0001
			# if(df.iat[j, 3] == "SP"):
			# 	df.iat[j, 3] = 1
			# if (df.iat[j, 3] == "SM"):
			# 	df.iat[j, 3] = 2
			# if (df.iat[j, 3] == "A"):
			# 	df.iat[j, 3] = 3
			# if (df.iat[j, 3] == "W"):
			# 	df.iat[j, 3] = 4
			
	return df


def trandform_water_data(df):
	"""
	12 DO (mg L-1): Log (x)
	14 NO2- (mg L-1): Log (x)
	15 NO3- (mg L-1): Log (x)
	16 NH3 (mg L-1): Log (x)
	17 PO43- (mg L-1): Log (x)
	18 SS (mg L-1): Log (x)
	"""
	df_new = df
	
	for i in range(0, df.shape[0]):
		df_new.iat[i, 12] = math.log((df.iat[i, 12]))
	
	for i in range(0, df.shape[0]):
		df_new.iat[i, 14] = math.log((df.iat[i, 14]))
	
	for i in range(0, df.shape[0]):
		df_new.iat[i, 15] = math.log((df.iat[i, 15]))
	
	for i in range(0, df.shape[0]):
		df_new.iat[i, 16] = math.log((df.iat[i, 16]))
		
	for i in range(0, df.shape[0]):
		df_new.iat[i, 17] = math.log((df.iat[i, 17]))
	
	for i in range(0, df.shape[0]):
		df_new.iat[i, 18] = math.log((df.iat[i, 18]))

	return df_new
	
	
def buttom_pollution(df):
	"""
	底質污染物
	25~32
	"""
	df_new = df
	
	for j in range(25, 33):
		for i in range(0, df.shape[0]):
			df_new.iat[i, j] = math.log((df.iat[i, j]))
	
	return df_new
	
def land_factor(df):
	"""
	底質
	"""
	df_new = df
	
	for j in range(21, 25):
		for i in range(0, df.shape[0]):
			df_new.iat[i, j] = math.asin(math.sqrt((df.iat[i, j]/100)))
	return df_new
	
def biology_factor(df):
	df_new = df
	
	for j in range(33, 35):
		for i in range(0, df.shape[0]):
			df_new.iat[i, j] = math.asin(math.sqrt((df.iat[i, j] / 100)))
	return df_new
	

df1 = filter_data()
df1 = process_na_val(df1)
df1 = trandform_water_data(df1)
df1 = buttom_pollution(df1)
df1 = land_factor(df1)
df1 = biology_factor(df1)
print(df1)
df1.to_excel('data_oil_mod.xlsx')