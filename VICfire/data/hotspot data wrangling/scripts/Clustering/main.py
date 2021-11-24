"""
Script name: main.py
Description: A script to perform the second version of the clustering algorithm on hotspots data.
			 This algorithm provides a method to identify dynamic bushfires from satellite hotspots data.
			 The detail of this algorithm can be found in CLUSTERING.md
Last modified: 2020-09-21
Author: Weihao (Patrick) Li
"""

from read_hotspots import read_hotspots_from_csv
from clustering_core import CLUSTERER
from read_setting import read_setting_from_txt
from numpy import savetxt
import time

def main():

	# Setup timer
	timer = {"Begin":time.time()}

	# Read hotspots data from csv file
	ID, lon, lat, time_id = read_hotspots_from_csv()
	active_time, adj_dist = read_setting_from_txt()

	timer['Read Data'] = time.time()

	# Build instance of the clustering algorithm with given parameters
	clusterer = CLUSTERER(
		active_time = active_time,
		adj_dist = adj_dist
		)

	# Run the algorithm
	clusterer.cluster(
		ID = ID, 
		lon = lon, 
		lat = lat, 
		time_id = time_id
		)

	timer['Run Algorithm'] = time.time()

	# Save result in csv file
	savetxt(
		'data/VIC_hotspots_after_clustering.csv',
		clusterer.memberships.astype(int),
		delimiter = ',',
		fmt = '%i',
		header = 'fire_id',
		comments = ""
		)

	timer['Finish'] = time.time()

	# Provide summary in txt format
	with open('scripts/Clustering/summary.txt', 'w') as f:
		f.write('Summary of the Clustering Result\n')
		f.write('Time\n')
		f.write("{} {} \n".format('	Read Data: ', timer['Read Data'] - timer['Begin']))
		f.write("{} {} \n".format('	Run Algorithm: ', timer['Run Algorithm'] - timer['Read Data']))
		f.write("{} {} \n".format('	Finish: ', timer['Finish'] - timer['Run Algorithm']))
		f.write('Setting\n')
		f.write("{} {} \n".format('	active_time: ', active_time))
		f.write("{} {} \n".format('	adj_dist: ', adj_dist))
		f.write('Result\n')
		f.write("{} {} \n".format('	Number of Observations: ', len(lon)))
		f.write("{} {} \n".format('	Number of Clusters: ', max(clusterer.memberships.tolist())))
		f.write("{} {} \n".format('	Length of Time: ', clusterer.maxtime))
		



if __name__ == "__main__":
	main()


