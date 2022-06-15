#!/usr/local/bin/python3
# -*- coding: utf-8 -*-

import pandas as pd
import math
pd.set_option('display.max_columns', None)

def filter_water_data(df):
	"""
	filt需要的資料
	:return: new_df
	"""
	# 3: season 4: point 7: tide
	df_new = pd.DataFrame()
	for i in range(0, df.shape[0]):
		if(df.iat[i,7] == 'L'):
			if pd.isnull(df.iat[i,2]) or pd.isnull(df.iat[i,9]):
				continue
			df_new = df_new.append(df.iloc[i])
	return df_new

def filter_bio_data(df):
	df_new = pd.DataFrame()
	for i in range(0, df.shape[0]):
		if(df.iloc[i][7] == 'L'):
			if(pd.isnull(df.iat[i, 36])):
				continue
			df_new = df_new.append(df.iloc[i])
	return df_new

def process_na_val(df):
	"""
	補空值
	"""
	for j in range(0, df.shape[0]):
		for i in range(10, 18):
			if df.iat[j, i] == 0 or type(df.iat[j, i]) == str():
				df.iat[j, i] = 0.0001
			
	return df


def trandform_water_data(df):
	"""
	12 DO (mg L-1): Log (x)
	14 NO2- (mg L-1): Log (x)
	15 NO3- (mg L-1): Log (x)
	16 NH3 (mg L-1): Log (x)
	17 PO43- (mg L-1): Log (x)
	"""
	for i in range(0, df.shape[0]):
		for j in range(12, 18):
			if j == 13: continue
			df.iat[i, j] = math.log((df.iat[i, j]))

	return df
	
	
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
	
	
def biology_factor(df):
	for j in range(33, 49):
		for i in range(0, df.shape[0]):
			if pd.isnull(df.iat[i, j]):
				df.iat[i, j] = 0
			if j == 35 or j == 36 or j == 38 or j == 39 or j == 41 or j == 47:
				df.iat[i, j] = math.log(df.iat[i, j]+1)
			if j == 48:
				df.iat[i, j] = math.sqrt(df.iat[i, j]+0.5)
	
	return df
	
def filter_buttom_pollution_data(df):
	df_new = pd.DataFrame()
	for i in range(0, df.shape[0]):
		if(df.iloc[i][7] == 'L'):
			if(pd.isnull(df.iat[i, 25])):
				continue
			df_new = df_new.append(df.iloc[i])
			for j in range(25, 33):
				if(df_new.iat[-1, j] == 0):
					df_new.iat[-1,j] = 0.0001

	return df_new

def filter_land_data(df):
	df_new = pd.DataFrame()
	for i in range(0, df.shape[0]):
		if(df.iloc[i][7] == 'L'):
			if(pd.isnull(df.iat[i, 21])):
				continue
			df_new = df_new.append(df.iloc[i])

	return df_new

def land_factor(df):
	"""
	底質
	"""
	for i in range(0, df.shape[0]):
		for j in range(21, 25):
			df.iat[i, j] = math.asin(math.sqrt((df.iat[i, j]/100)))
	return df

data = pd.read_excel('/Users/wujunwei/Documents/GitHub/SEM/CPC-DataAnalytics/code/data_oil.xlsx', sheet_name='整體資料')
data.reset_index(drop = True, inplace = True)
# print(data)

df1 = filter_water_data(data)
df1 = process_na_val(df1)
df1 = trandform_water_data(df1)
df1.reset_index(drop = True, inplace = True)

df2 = filter_bio_data(data)
df2 = biology_factor(df2)
df2.reset_index(drop = True, inplace = True)

df3 = filter_buttom_pollution_data(data)
df3 = buttom_pollution(df3)
df3.reset_index(drop = True, inplace = True)

df4 = filter_land_data(data)
df4 = land_factor(df4)
df4.reset_index(drop = True, inplace = True)

df1.to_excel('/Users/wujunwei/Documents/GitHub/SEM/CPC-DataAnalytics/code/data_oil_water.xlsx')
df2.to_excel('/Users/wujunwei/Documents/GitHub/SEM/CPC-DataAnalytics/code/data_oil_bio.xlsx')
df3.to_excel('/Users/wujunwei/Documents/GitHub/SEM/CPC-DataAnalytics/code/data_oil_pollute.xlsx')
df4.to_excel('/Users/wujunwei/Documents/GitHub/SEM/CPC-DataAnalytics/code/data_oil_land.xlsx')