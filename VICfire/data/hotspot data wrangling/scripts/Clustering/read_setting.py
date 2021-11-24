"""
Script name: read_setting.py
Description: A script to read setting of the clustering algorithm
Last modified: 2020-09-21
Author: Weihao (Patrick) Li
"""

def read_setting_from_txt(file_path = "setting.txt"):
	"""
	Read setting from the txt file.
	Input: 
		file_path: file_path and name of the csv file
	Output:
		active_time: length of time bushfires remain active
		adj_distance: density-based distance used in the clustering algorithm
	"""

	with open("scripts/Clustering/setting.txt", "r") as f:
		dic = {"active_time": None, "adj_distance": None}

		for line in f:
			line_string = line.split('=')
			if "active_time" in line_string[0].lower():
				dic['active_time'] = int(line_string[1].replace(' ','').replace('\n',''))

			if "adj_distance" in line_string[0].lower():
				dic['adj_distance'] = int(line_string[1].replace(' ','').replace('\n',''))

		if None in dic.values():
			raise Exception("Either value of active_time or adj_distance doesn't exist.")

		return dic['active_time'], dic['adj_distance']
		
