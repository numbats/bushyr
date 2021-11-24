"""
Script name: read_hotspots.py
Description: A script to read hotspots from the csv file.
Last modified: 2020-09-10
Author: Weihao (Patrick) Li
"""

from numpy import genfromtxt

def read_hotspots_from_csv(file_path = 'data/VIC_hotspots_before_clustering.csv'):
	"""
	Read hotspots from the csv file.
	Input: 
		file_path: file_path and name of the csv file
	Output:
		ID: the unique indentifier of the hotspot
		lon: longitude
		lat: latitude
		time_id: observered time formatted in indexes (discrete & integer values)
	"""
	data = genfromtxt(
		file_path, 
		delimiter = ',', 
		skip_header = 1, 
		dtype = [('id', 'int32'), ('lon', 'float32'), ('lat', 'float32'), ('time_id', 'int32')]
		)

	ID = data['id']
	time_id = data['time_id']
	lon = data['lon']
	lat = data['lat']

	return ID, lon, lat, time_id
