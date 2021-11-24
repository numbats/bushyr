"""
Script name: main.py
Description: A script to perform parameter tuning of the clustering algorithm
Last modified: 2020-09-14
Author: Weihao (Patrick) Li
"""

from read_hotspots import read_hotspots_from_csv
from clustering_core import CLUSTERER
from read_setting import read_setting_from_txt
from os import remove

def main():

	ID, lon, lat, time_id = read_hotspots_from_csv()

	try:
		remove("data/clustering_grid.csv")
	except OSError:
		pass

	with open('data/clustering_grid.csv', 'a') as f:
		f.write("active_time,adj_dist,count\n")

	for active_time in [x * 3 for x in range(1, 13)]:
		for adj_dist in [x * 1000 for x in range(1, 11)]:

			print("Running", active_time, adj_dist)
	
			clusterer = CLUSTERER(
				active_time = active_time,
				adj_dist = adj_dist
				)

			clusterer.cluster(
				ID = ID, 
				lon = lon, 
				lat = lat, 
				time_id = time_id
				)

			count = max(clusterer.memberships.tolist())

			with open('data/clustering_grid.csv', 'a') as f:
				f.write("{},{},{}\n".format(active_time, adj_dist, count))

			print("count:", count)

			del clusterer





if __name__ == "__main__":
	main()
