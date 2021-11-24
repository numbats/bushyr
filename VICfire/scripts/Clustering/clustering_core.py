"""
Script name: clustering_core.py
Description: The core part of the clustering algorithm.
Last modified: 2020-09-14
Author: Weihao (Patrick) Li
"""

from tqdm import tqdm
import numpy as np

class CLUSTERER:
	
	def __init__(self, active_time = 24, adj_dist = 3000):
		"""
		Initialize the instance
		Input:
			active_time: length of time bushfires remain active
			adj_dist: density-based distance used in the clustering algorithm
		"""

		self.active_time = active_time
		self.adj_dist = adj_dist
		self.memberships = None

	def cluster(self, ID, lon, lat, time_id):
		"""
		Cluster hotspots into bushfires
		Input:
			self: instance
			ID: unique indetifier
			lon: a vector of longitude
			lat: a vector of latitude
			time_id: observered time formatted in indexes (discrete & integer values)
		"""

		# Compute the maximum timestamps
		self.maxtime = np.max(time_id).tolist()

		# Initialize the memberships
		self.memberships = np.zeros(len(lon), dtype = "int32")


		# Loop through every timestamp
		for i in tqdm(range(1, self.maxtime + 1), ascii = True):
			if np.any(time_id == i):

				# Extract all revelant timestamps and compute the memberships
				indexes = np.where((time_id <= i) & (max(1, i - self.active_time) <= time_id))
				temp_memberships = np.zeros(len(lon), dtype = "int32")
				temp_memberships[indexes] = self.compute_memberships(lon[indexes], lat[indexes])

				# Define current timestamp and past timestamps
				current = np.where(time_id == i)
				past = np.where((time_id < i) & (max(1, i - self.active_time) <= time_id))

				# No previous points
				if len(current[0]) == len(indexes[0]):
					self.memberships[indexes] = temp_memberships[indexes] + np.max(self.memberships)

				else:

					# Assign memberships to points in current timestamp
					self.adjust_memberships(
						temp_memberships,
						current,
						past,
						lon,
						lat
						)

	def adjust_memberships(self, temp_memberships, current, past, lon, lat):
		"""
		Adjust memberships in current timestamp based on previous clustering result
		Input:
			temp_memberships: memberships with incorrect labels
			current: current timestamp
			past: past timestamps
			lon: a vector of longitude
			lat: a vector of latitude
		"""

		# Convert lon and lat to radians
		rc_lon = np.radians(lon[current])
		rc_lat = np.radians(lat[current])
		rp_lon = np.radians(lon[past])
		rp_lat = np.radians(lat[past])

		# Define the correctly labelled membership in current and past timestamps
		correct_c_memberships = np.zeros(len(rc_lon), dtype = "int32")
		correct_p_memberships = self.memberships[past]

		# Collect current incorrect memberships
		c_memberships = temp_memberships[current]
		p_memberships = temp_memberships[past]

		# New clusters
		new_clusters = []

		# Loop through every points in current timestamp
		for i in range(len(rc_lon)):

			# If the point in current timestamp share the same cluster with a point in previous timestamps
			if np.any(c_memberships[i] == p_memberships):

				# Compute the distance vector
				distance = self.point_to_vector_geodist(rc_lon[i], rc_lat[i], rp_lon, rp_lat)

				if len(p_memberships == c_memberships[i]) != len(distance):
					raise Exception("The length of distance does not match with the length of memberships")

				# Assign missing value to points not sharing the same cluster
				distance[p_memberships != c_memberships[i]] = np.nan

				# Select the non nan minium element, assign its correct past membership to the point in current timestamp 
				correct_c_memberships[i] = correct_p_memberships[distance == np.nanmin(distance)][0]

			else:

				new_clusters.append(i)

		# Get new clusters incorrect labels
		correct_c_memberships[new_clusters] = self.revise_memberships(c_memberships[new_clusters])
		correct_c_memberships[new_clusters] = correct_c_memberships[new_clusters] + np.max(self.memberships)

		if np.any(correct_c_memberships == 0):
			raise Exception("correct_c_memberships contains zeros")
		# Assign them back to the global memberships vector
		self.memberships[current] = correct_c_memberships


	def revise_memberships(self, memberships):
		"""
		Revise memberships to let it start from 1
		Input:
			memberships: a vector of memberships
		"""

		# Build a replacement dict
		dic = {key: index + 1 for index, key in enumerate(np.unique(memberships).tolist())}

		# Replace the memberships
		memberships = memberships.tolist()
		for i in range(len(memberships)):
			memberships[i] = dic[memberships[i]]

		return np.array(memberships, dtype = "int32")






	def compute_memberships(self, lon, lat):
		"""
		Compute memberships using Breadth First Search
		Input:
			lon: a vector of longitude
			lat: a vector of latitude
		Output:
			memberships: a vector of memberships
		"""

		# Define a membership vector
		memberships = np.zeros(len(lon), dtype = "int32")

		# If there is only one point, assign 1, then return
		if len(memberships) == 1:
			memberships[0] = 1
			return memberships

		# Convert lon and lat to radians
		rlon = np.radians(lon)
		rlat = np.radians(lat)

		# Define label number
		label = 0

		# While there are still unlabelled points
		while np.any(memberships == 0):

			# print(np.count_nonzero(memberships == 0))

			# Define a new label
			label = label + 1

			# Find the first unlabelled point
			first_point = np.where(memberships == 0)[0][0]

			# Push the first point to the queue
			queue = np.array(first_point, dtype = "int32")
			queue = np.reshape(queue, (1))

			# Label the first point
			memberships[queue[0]] = label


			# Set head and tail pointer
			head = 0
			tail = 0

			# Start BFS
			while head <= tail:

				# print("    queue:", len(queue))
				if len(queue) > len(lon):
					raise Exception("length of queue out of boundary")
				# print(queue)
				# print(memberships)

				# Computer the distance vector
				distance = self.point_to_vector_geodist(
					rlon[queue[head]],
					rlat[queue[head]],
					rlon,
					rlat
					)

				if len(distance) != len(memberships):
					raise Exception("The length of distance does not match with the length of memberships")

				# Get the extensions of the point
				extensions = np.where((distance <= self.adj_dist) & (memberships == 0))[0]

				# print(distance)
				# print(extensions)

				# Append extensions to queue
				queue = np.append(queue, extensions)

				# Label extensions points
				memberships[extensions] = label

				# Update tail
				tail = tail + len(extensions) 

				# Update head
				head = head + 1

		return memberships




	def point_to_vector_geodist(self, P_rlon, P_rlat, V_rlon, V_rlat):
		"""
		Using Haversine formulat to compute the distance from a point to a vector of points.
		Adpoted from https://stackoverflow.com/questions/19412462/getting-distance-between-two-points-based-on-latitude-longitude.
		Input:
			P_rlon: radians of longitude
			P_rlat: radians of latitude
			V_rlon: a vector of radians of longitude
			V_rlat: a vector of radians of latitude
		Output:
			distance: a vector of distance
		"""

		lon1 = P_rlon
		lat1 = P_rlat
		lon2 = V_rlon
		lat2 = V_rlat

		dlon = lon2 - lon1
		dlat = lat2 - lat1

		a = np.sin(dlat / 2)**2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon / 2)**2
		c = 2 * np.arctan2(np.sqrt(a), np.sqrt(1 - a))
		distance = 6373.0 * c * 1000

		return distance



				


