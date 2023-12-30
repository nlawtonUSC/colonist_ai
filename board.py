import json
import numpy as np
import copy
import random
from enum import Enum

import torch
import torch.nn as nn

import colorama

Phase = Enum('Phase', [
	'SETUP',
	'PREROLL',
	'MAIN',
	'ROAD_BUILDING_PREROLL_1',
	'ROAD_BUILDING_PREROLL_2',
	'ROAD_BUILDING_MAIN_1',
	'ROAD_BUILDING_MAIN_2',
	'PLACE_ROBBER_PREROLL',
	'PLACE_ROBBER_MAIN',
	'CHOOSE_STEAL_TARGET_PREROLL',
	'CHOOSE_STEAL_TARGET_MAIN',
	'STEAL_PREROLL',
	'STEAL_MAIN',
	'DISCARD',
	'DRAW_DEV_CARD',
	'ROLL'
], start=0)

HexType = Enum('HexType', [
	'BRICK',
	'LUMBER',
	'WOOL',
	'GRAIN',
	'ORE',
	'OCEAN',
	'DESERT'
], start=0)

ResourceType = Enum('ResourceType', [
	'BRICK',
	'LUMBER',
	'WOOL',
	'GRAIN',
	'ORE'
], start=0)

HarborType = Enum('HarborType', [
	'GENERIC',
	'BRICK',
	'LUMBER',
	'WOOL',
	'GRAIN',
	'ORE'
], start=0)

DevelopmentCardType = Enum('DevelopmentCardType', [
	'KNIGHT',
	'VICTORY_POINT',
	'YEAR_OF_PLENTY',
	'MONOPOLY',
	'ROAD_BUILDING'
], start=0)

class Hex:
	def __init__(self, x, y, type, number):
		"""
		A class for representing hexes on a Catan board.

		Args:
			x (int): the x-index of the hex in the hex map.
			y (int): the y-index of the hex in the hex map.
			type (str): the type of hex, e.g., "ocean", "brick", "desert", etc.
			number (int): the number token placed on the hex. If the hex does
				not have a number token, e.g., the hex is a desert or ocean,
				then `number` should be 0.
		"""
		self.x = x
		self.y = y
		self.type = type
		self.number = number
		self.robber = False
		# The 0-th settlement/city is at the top of the hex. Indices
		# increment clockwise. Since each vertex can belong to three different
		# hexes, we will only record vertex information for `v=0,1`.
		self.settlements = [None for i in range(2)]
		self.cities = [None for i in range(2)]
		# The 0-th road is incident to the 0-th and 1-st hex vertices. Indices
		# increment clockwise. Since each edge can belong to two different
		# hexes, we will only record edge information for `e=0,1,2`.
		self.roads = [None for i in range(3)]

class Harbor:
	def __init__(self, hex_coord, v, vertices, type):
		"""
		A class for representing Catan harbors.

		Args:
			hex_coord (tuple): The (x, y) coordinate of the hex that the
				harbor should be drawn to.
			vertices (list): A list of (x, y, i) tuples indicating which
				vertices can access the harbor.
			type (str): The type of the harbor, e.g., "generic" or "brick".
		"""
		self.hex_coord = hex_coord
		self.v = v
		self.vertices = vertices
		self.type = type

	def __str__(self):
		if self.type == HarborType.GENERIC:
			return "3:1"
		else:
			return "2:1"

class Action:
	def __init__(self, name, board):
		self.name = name
		self.board = board

	def __str__(self):
		return self.name

class Player:
	def __init__(self, name):
		"""
		A class for storing player-specific information.

		This class store player-specific information such as:
			* The player's state, including their resources; number of road, 
				settlement, and city pieces; development cards, and number of
				victory points.
			* Methods for calculating valid moves, including valid road,
				settlement, and city placements; valid road, settlement, and
				city purchases; and valid development card plays.
		"""
		self.name = name
		self.roads = []
		self.settlements = []
		self.cities = []
		self.harbors = []
		self.longest_road_player = False
		self.largest_army_player = False
		self.army_size = 0
		self.longest_road_length = 0
		self.resources = [0] * len(ResourceType)
		self.development_cards = [0] * len(DevelopmentCardType)
		self.development_cards_bought_this_turn = [0] * len(DevelopmentCardType)
		self.played_development_card_this_turn = False

	def victory_points(self):
		total = 0
		total += self.development_cards[DevelopmentCardType.VICTORY_POINT.value]
		total += self.development_cards_bought_this_turn[DevelopmentCardType.VICTORY_POINT.value]
		total += len(self.settlements)
		total += 2 * len(self.cities)
		total += 2 if self.largest_army_player else 0
		total += 2 if self.longest_road_player else 0
		return min([total, 10])

	def roll(self, roll_number, board):
		"""
		Distribute resources after rolling the dice.
		`roll_number` is the number that was rolled.
		"""
		for settlement in self.settlements:
			for hex_coord in board.hexes_neighboring_vertex(*settlement):
				h = board.hex(*hex_coord)
				if h.number == roll_number and hex_coord != board.robber:
					resource_type = h.type
					if board.resource_bank[resource_type.value] >= 1:
						self.resources[resource_type.value] += 1
						board.resource_bank[resource_type.value] -= 1

		for city in self.cities:
			for hex_coord in board.hexes_neighboring_vertex(*city):
				h = board.hex(*hex_coord)
				if h.number == roll_number and hex_coord != board.robber:
					resource_type = h.type
					if board.resource_bank[resource_type.value] >= 2:
						self.resources[resource_type.value] += 2
						board.resource_bank[resource_type.value] -= 2

	def valid_place_road_actions(self, board):
		ret = []
		road_setup = board.phase == Phase.SETUP \
			and len(self.settlements) > len(self.roads)
		road_main = board.phase == Phase.MAIN \
			and self.resources[ResourceType.LUMBER.value] >= 1 \
			and self.resources[ResourceType.BRICK.value] >= 1
		road_building = board.phase == Phase.ROAD_BUILDING_PREROLL_1 \
			or board.phase == Phase.ROAD_BUILDING_PREROLL_2 \
			or board.phase == Phase.ROAD_BUILDING_MAIN_1 \
			or board.phase == Phase.ROAD_BUILDING_MAIN_2
		if not (road_setup or road_main or road_building):
			return ret
		if len(self.roads) == board.max_roads and not road_building:
			return ret
		"""
		During setup, the second road must be placed adjacent to the second
		settlement.
		"""
		if road_setup:
			for settlement in self.settlements:
				has_incident_road = False
				for (x, y, i) in board.incident_edges(*settlement):
					h = board.hex(x, y)
					if h.roads[i] != None:
						has_incident_road = True
						break
				if not has_incident_road:
					for (x, y, i) in board.incident_edges(*settlement):
						action_name = f"setup road ({x}, {y}, {i})"
						next_board = copy.deepcopy(board)
						h = next_board.hex(x,y)
						h.roads[i] = self.name
						p = next_board.cur_player()
						p.roads.append((x, y, i))		
						two_roads_placed = all([len(p.roads) == 2 for p in next_board.players])
						one_road_placed = all([len(p.roads) == 1 for p in next_board.players])
						at_least_one_road_placed = all([len(p.roads) >= 1 for p in next_board.players])
						if two_roads_placed:
							next_board.phase = Phase.PREROLL
						elif one_road_placed:
							pass
						elif at_least_one_road_placed:
							next_board.turn -= 1
						else:
							next_board.turn += 1	
						a = Action(action_name, next_board)
						ret.append(a)
		elif (road_main or road_building) and not (len(self.roads) == board.max_roads):
			for y in range(board.height):
				for x in range(board.width):
					h = board.hex(x, y)
					for e in range(len(h.roads)):
						# check if a road can be built at `e`.
						road_empty = (h.roads[e] == None)
						# check if the player owns an adjacent road
						land_adj = False
						for (i, j) in board.hexes_neighboring_edge(x, y, e):
							if board.hex(i, j).type != HexType.OCEAN:
								land_adj = True 
								break
						adjacent_road = False
						for (i, j, f) in board.adjacent_edges(x, y, e):
							if board.hex(i,j).roads[f] == self.name:
								adjacent_road = True
								break
						# check if the player owns an adjacent settlement
						adjacent_settlement = False
						for (i, j, v) in board.incident_vertices(x, y, e):
							if board.hex(i,j).settlements[v] == self.name:
								adjacent_settlement = True
								break
						# check if the player owns an adjacent city
						adjacent_city = False
						for (i, j, v) in board.incident_vertices(x, y, e):
							if board.hex(i,j).cities[v] == self.name:
								adjacent_city = True
								break
						road_buildable = road_empty and land_adj and ( \
							adjacent_road \
							or adjacent_settlement \
							or adjacent_city
						)
						if road_buildable:
							# TODO: Recompute longest road
							action_name = f"build road ({x}, {y}, {e})"
							next_board = copy.deepcopy(board)
							h = next_board.hex(x, y)
							h.roads[e] = self.name
							p = next_board.cur_player()
							if board.phase == Phase.MAIN:
								p.resources[ResourceType.LUMBER.value] -= 1
								p.resources[ResourceType.BRICK.value] -= 1
								next_board.resource_bank[ResourceType.LUMBER.value] += 1
								next_board.resource_bank[ResourceType.BRICK.value] += 1
							elif board.phase == Phase.ROAD_BUILDING_PREROLL_1:
								next_board.phase = Phase.ROAD_BUILDING_PREROLL_2
							elif board.phase == Phase.ROAD_BUILDING_PREROLL_2:
								next_board.phase = Phase.PREROLL
							elif board.phase == Phase.ROAD_BUILDING_MAIN_1:
								next_board.phase = Phase.ROAD_BUILDING_MAIN_2
							elif board.phase == Phase.ROAD_BUILDING_MAIN_2:
								next_board.phase = Phase.MAIN
							p.roads.append((x, y, e))
							# TODO: Longest road
							p.longest_road_length = len(p.roads)
							if p.longest_road_length > next_board.longest_road_length and p.longest_road_length >= 5:
								if next_board.longest_road_player != None:
									q = next_board.get_player(next_board.longest_road_player)
									q.longest_road_player = False
								p.longest_road_player = True
								next_board.longest_road_player = p.name
								next_board.longest_road_length = p.longest_road_length
							a = Action(action_name, next_board)
							ret.append(a)
		if road_building and (len(ret) == 0 or len(self.roads) == board.max_roads): 
			# If in road building phase and there are no valid places to put a road,
			# road building ends and the board returns to preroll or main phase.
			action_name = "pass"
			next_board = copy.deepcopy(board)
			if board.phase in [Phase.ROAD_BUILDING_PREROLL_1, Phase.ROAD_BUILDING_PREROLL_2]:
				next_board.phase = Phase.PREROLL
			else:
				next_board.phase = Phase.MAIN
			a = Action(action_name, next_board)
			ret.append(a)
		return ret

	def valid_place_settlement_actions(self, board):
		ret = []
		settlement_setup = board.phase == Phase.SETUP \
			and len(self.settlements) <= len(self.roads)
		settlement_build = board.phase == Phase.MAIN \
			and self.resources[ResourceType.LUMBER.value] >= 1 \
			and self.resources[ResourceType.BRICK.value] >= 1 \
			and self.resources[ResourceType.WOOL.value] >= 1 \
			and self.resources[ResourceType.GRAIN.value] >= 1 
		if not (settlement_setup or settlement_build):
			return ret
		if len(self.settlements) == board.max_settlements:
			return ret
		for y in range(board.height):
			for x in range(board.width):
				h = board.hex(x, y)
				for v, settlement in enumerate(h.settlements):
					# check that nothing is already built at the vertex.
					settlement_empty = (h.settlements[v] == None)
					city_empty = (h.cities[v] == None)
					# check that there are no adjacent settlements or cities
					empty_neighbors = True
					for (i, j, u) in board.adjacent_vertices(x, y, v):
						if board.hex(i,j).settlements[u] != None:
							empty_neighbors = False
							break
						if board.hex(i,j).cities[u] != None:
							empty_neighbors = False
							break
					# check that there is at least one incident road owned
					# by the player
					road_neighbor = False
					for (i, j, e) in board.incident_edges(x, y, v):
						if board.hex(i,j).roads[e] == self.name:
							road_neighbor = True
							break
					# check that the vertex is adjacent to land.
					land_adj = False
					for (i, j) in board.hexes_neighboring_vertex(x, y, v):
						if board.hex(i,j).type != HexType.OCEAN:
							land_adj = True
							break
					# debug
					#if x == 0 and y == 2 and v == 1:
					#	print('settlement empty: ', settlement_empty)
					#	print('city empty: ', city_empty)
					#	print('empty neighbors: ', empty_neighbors)
					#	print('road neighbor: ', road_neighbor)
					#	print('land adj: ', land_adj)
					#	exit(0)
					settlement_buildable = settlement_empty \
						and city_empty \
						and empty_neighbors \
						and land_adj \
						and (road_neighbor or settlement_setup)
					if settlement_buildable:
						action_name = f"build settlement ({x}, {y}, {v})"
						next_board = copy.deepcopy(board)
						h = next_board.hex(x, y)
						h.settlements[v] = self.name
						p = next_board.cur_player()
						# player receives resources when placing settlement on setup
						if settlement_setup:
							for (i, j) in board.hexes_neighboring_vertex(x, y, v):
								nh = board.hex(i,j)
								if nh.type not in [HexType.OCEAN, HexType.DESERT]:
									resource_type = ResourceType[nh.type.name].value
									p.resources[resource_type] += 1
									next_board.resource_bank[resource_type] -= 1
						# settlement cost
						elif not settlement_setup:
							p.resources[ResourceType.LUMBER.value] -= 1
							p.resources[ResourceType.BRICK.value] -= 1
							p.resources[ResourceType.WOOL.value] -= 1
							p.resources[ResourceType.GRAIN.value] -= 1
							next_board.resource_bank[ResourceType.LUMBER.value] += 1
							next_board.resource_bank[ResourceType.BRICK.value] += 1
							next_board.resource_bank[ResourceType.WOOL.value] += 1
							next_board.resource_bank[ResourceType.GRAIN.value] += 1
						# record settlement
						p.settlements.append((x, y, v))
						# record harbors
						for harbor in next_board.harbors:
							if (x, y, v) in harbor.vertices:
								p.harbors.append(harbor)
						a = Action(action_name, next_board)
						ret.append(a)
		return ret

	def valid_place_city_actions(self, board):
		ret = []
		city_build = board.phase == Phase.MAIN \
			and self.resources[ResourceType.GRAIN.value] >= 2 \
			and self.resources[ResourceType.ORE.value] >= 3 
		if not city_build:
			return ret
		if len(self.cities) == board.max_cities:
			return ret
		for i, (x, y, v) in enumerate(self.settlements):
			action_name = f"build city ({x}, {y}, {v})"
			next_board = copy.deepcopy(board)
			h = next_board.hex(x, y)
			h.cities[v] = self.name
			h.settlements[v] = None
			p = next_board.cur_player()
			p.resources[ResourceType.GRAIN.value] -= 2
			p.resources[ResourceType.ORE.value] -= 3
			next_board.resource_bank[ResourceType.GRAIN.value] += 2
			next_board.resource_bank[ResourceType.ORE.value] += 2
			p.settlements.pop(i)
			p.cities.append((x, y, v))
			a = Action(action_name, next_board)
			ret.append(a)
		return ret

	def valid_buy_dev_card_actions(self, board):
		ret = []
		# can only buy dev cards during main phase
		if board.phase != Phase.MAIN:
			return ret
		# cannot buy dev cards if there are none left to buy
		if len(board.development_cards) == 0:
			return ret
		# buying a dev card requires one wool, grain, and ore.
		dev_card_resource_reqs_met = self.resources[ResourceType.WOOL.value] >= 1 \
			and self.resources[ResourceType.GRAIN.value] >= 1 \
			and self.resources[ResourceType.ORE.value] >= 1
		if not dev_card_resource_reqs_met:
			return ret
		action_name = f"buy development card"
		next_board = copy.deepcopy(board)
		p = next_board.cur_player()
		p.resources[ResourceType.WOOL.value] -= 1
		p.resources[ResourceType.GRAIN.value] -= 1
		p.resources[ResourceType.ORE.value] -= 1
		next_board.resource_bank[ResourceType.WOOL.value] += 1
		next_board.resource_bank[ResourceType.GRAIN.value] += 1
		next_board.resource_bank[ResourceType.ORE.value] += 1
		next_board.phase = Phase.DRAW_DEV_CARD
		a = Action(action_name, next_board)
		ret.append(a)
		return ret

	def valid_place_robber_actions(self, board):
		# check board is in PLACE_ROBBER phase
		# cannot place robber at its current hex.
		# cannot place robber on ocean hexes.
		ret = []
		if board.phase not in [Phase.PLACE_ROBBER_PREROLL, Phase.PLACE_ROBBER_MAIN]:
			return ret
		rx, ry = board.robber
		for y in range(board.height):
			for x in range(board.width):
				h = board.hex(x, y)
				is_land = h.type != HexType.OCEAN
				moves_robber = not (x == rx and y == ry)
				if is_land and moves_robber:
					action_name = f"move robber ({x}, {y})"
					next_board = copy.deepcopy(board)
					next_board.robber = (x, y)
					if board.phase == Phase.PLACE_ROBBER_PREROLL:
						next_board.phase = Phase.CHOOSE_STEAL_TARGET_PREROLL
					elif board.phase == Phase.PLACE_ROBBER_MAIN:
						next_board.phase = Phase.CHOOSE_STEAL_TARGET_MAIN
					a = Action(action_name, next_board)
					ret.append(a)
		return ret

	def valid_robber_steal_actions(self, board):
		ret = []
		if board.phase not in [Phase.CHOOSE_STEAL_TARGET_PREROLL, Phase.CHOOSE_STEAL_TARGET_MAIN]:
			return ret
		p = board.cur_player()
		for v in board.vertices_neighboring_hex(*board.robber):
			for q in board.players:
				diff_player = (p.name != q.name)
				player_adj = (v in q.settlements or v in q.cities)
				has_rsrc = (sum(q.resources) > 0)
				if diff_player and player_adj and has_rsrc:
					action_name = f"steal {q.name}"
					next_board = copy.deepcopy(board)
					if board.phase == Phase.CHOOSE_STEAL_TARGET_PREROLL:
						next_board.phase = Phase.STEAL_PREROLL
					elif board.phase == Phase.CHOOSE_STEAL_TARGET_MAIN:
						next_board.phase = Phase.STEAL_MAIN
					next_board.robber_target = q.name
					a = Action(action_name, next_board)
					ret.append(a)
		if len(ret) == 0:
			action_name = f"no valid steal target"
			next_board = copy.deepcopy(board)
			if board.phase == Phase.CHOOSE_STEAL_TARGET_PREROLL:
				next_board.phase = Phase.PREROLL
			elif board.phase == Phase.CHOOSE_STEAL_TARGET_MAIN:
				next_board.phase = Phase.MAIN
			a = Action(action_name, next_board)
			ret.append(a)
		return ret

	def valid_trade_actions(self, board):
		# for each resource the player can receive:
		# if the player has a resource harbor, may trade 2:1
		# if the player has a generic harbor, may trade 3:1
		# else, may trade 4:1
		ret = []
		if board.phase != Phase.MAIN:
			return ret
		for offer_resource_type, offer_resource_count in enumerate(self.resources):
			# check if the player can trade at a resource harbor
			for harbor in self.harbors:
				if harbor.type == offer_resource_type and offer_resource_count >= 2:
					for receive_resource_type in range(len(self.resources)):
						if offer_resource_type != receive_resource_type and board.resource_bank[receive_resource_type] > 0:
							action_name = f"trade 2:1 ({offer_resource_type}, {receive_resource_type})"
							next_board = copy.deepcopy(board)
							p = next_board.cur_player()
							p.resources[offer_resource_type] -= 2
							p.resources[receive_resource_type] += 1
							next_board.resource_bank[offer_resource_type] += 2
							next_board.resource_bank[receive_resource_type] -= 1
							a = Action(action_name, next_board)
							ret.append(a)
					break
			# check if the player can trade at a generic harbor
			for harbor in self.harbors:
				if harbor.type == "generic" and offer_resource_count >= 3:
					for receive_resource_type in range(len(self.resources)):
						if offer_resource_type != receive_resource_type and board.resource_bank[receive_resource_type] > 0:
							action_name = f"trade 3:1 ({offer_resource_type}, {receive_resource_type})"
							next_board = copy.deepcopy(board)
							p = next_board.cur_player()
							p.resources[offer_resource_type] -= 3
							p.resources[receive_resource_type] += 1
							next_board.resource_bank[offer_resource_type] += 3
							next_board.resource_bank[receive_resource_type] -= 1
							a = Action(action_name, next_board)
							ret.append(a)
				break
			# check if the player can trade with the bank
			if offer_resource_count >= 4:
				for receive_resource_type in range(len(self.resources)):
					if offer_resource_type != receive_resource_type and board.resource_bank[receive_resource_type] > 0:
						action_name = f"trade 4:1 ({offer_resource_type}, {receive_resource_type})"
						next_board = copy.deepcopy(board)
						p = next_board.cur_player()
						p.resources[offer_resource_type] -= 4
						p.resources[receive_resource_type] += 1
						next_board.resource_bank[offer_resource_type] += 4
						next_board.resource_bank[receive_resource_type] -= 1
						a = Action(action_name, next_board)
						ret.append(a)
		return ret

	def valid_development_card_actions(self, board):
		# a dev card cannot be played the turn that it is bought
		# cannot play more than one dev card each turn
		# dev cards can be played during PREROLL or MAIN phases.
		# knight
		# monopoly
		# road building
		# year of plenty
		ret = []
		# A player can play a development card during the per-roll or main phase.
		if board.phase not in [Phase.MAIN, Phase.PREROLL]:
			return ret
		# A player cannot play more than one development card per turn.
		if self.played_development_card_this_turn:
			return ret
		for development_card_type, development_card_count in enumerate(self.development_cards):
			# The player must have at least one card of the type being considered.
			if development_card_count == 0:
				continue
			# Play knight
			if development_card_type == DevelopmentCardType.KNIGHT.value:
				action_name = f"play knight"
				next_board = copy.deepcopy(board)
				p = next_board.cur_player()
				p.played_development_card_this_turn = True
				p.development_cards[DevelopmentCardType.KNIGHT.value] -= 1
				# The player enters a robber placement phase.
				# We need to remember whether the knight was played during
				# the pre-roll or main phase.
				if board.phase == Phase.PREROLL:
					next_board.phase = Phase.PLACE_ROBBER_PREROLL
				elif board.phase == Phase.MAIN:
					next_board.phase = Phase.PLACE_ROBBER_MAIN
				# The player's army size increases by one.
				p.army_size += 1
				if p.army_size > next_board.largest_army_size and p.army_size >= 3:
					if next_board.largest_army_player != None:
						q = next_board.get_player(next_board.largest_army_player)
						q.largest_army_player = False
					p.largest_army_player = True
					next_board.largest_army_player = p.name
					next_board.largest_army_size = p.army_size
				a = Action(action_name, next_board)
				ret.append(a)
			# Play monopoly
			if development_card_type == DevelopmentCardType.MONOPOLY.value:
				# To play monopoly, a player chooses a resource type.
				for resource_type in range(len(self.resources)):
					action_name = f"play monopoly {resource_type}"
					next_board = copy.deepcopy(board)
					p = next_board.cur_player()
					p.played_development_card_this_turn = True
					p.development_cards[DevelopmentCardType.MONOPOLY.value] -= 1
					# The player steals all resources of that type from every other player
					for q in next_board.players:
						resource_count = q.resources[resource_type]
						q.resources[resource_type] -= resource_count
						p.resources[resource_type] += resource_count
					a = Action(action_name, next_board)
					ret.append(a)
			# Play road building
			if development_card_type == DevelopmentCardType.ROAD_BUILDING.value:
				action_name = "play road building"
				next_board = copy.deepcopy(board)
				p = next_board.cur_player()
				p.played_development_card_this_turn = True
				p.development_cards[DevelopmentCardType.ROAD_BUILDING.value] -= 1
				if board.phase == Phase.PREROLL:
					next_board.phase = Phase.ROAD_BUILDING_PREROLL_1
				else:
					next_board.phase = Phase.ROAD_BUILDING_MAIN_1
				a = Action(action_name, next_board)
				ret.append(a)
			# Play year of plenty
			if development_card_type == DevelopmentCardType.YEAR_OF_PLENTY.value:
				# The player chooses two resource types with replacement.
				for i in range(len(self.resources)):
					for j in range(i, len(self.resources)):
						action_name = f"play year of plenty {i} {j}"
						next_board = copy.deepcopy(board)
						p = next_board.cur_player()
						p.played_development_card_this_turn = True
						p.development_cards[DevelopmentCardType.YEAR_OF_PLENTY.value] -= 1
						# The player receives one resource of each type.
						if board.resource_bank[i] >= 1:
							p.resources[i] += 1
							next_board.resource_bank[i] -= 1
						if board.resource_bank[j] >= 1:
							p.resources[j] += 1
							next_board.resource_bank[j] -= 1
						a = Action(action_name, next_board)
						ret.append(a)
		return ret

	def valid_discard_actions(self, board):
		# TODO
		# the player must discard half of all resource cards they control,
		# rounded down.
		# check that board is in DISCARD_PHASE
		ret = []
		if board.phase != Phase.DISCARD:
			return ret
		total_resources = sum(self.resources)
		if total_resources <= 7:
			action_name = f"no discard"
			next_board = copy.deepcopy(board)
			next_board.cur_player().discarding = False
			if next_board.next_player().discarding == False:
				next_board.turn += 1
				next_board.phase = Phase.PLACE_ROBBER_MAIN
			else:
				next_board.turn += 1
			a = Action(action_name, next_board)
			ret.append(a)
			return ret
		num_discard = total_resources // 2
		def recursive_discard(resources, num_discard):
			"""
			Returns a list of valid ways to discard `num_discard` resources.

			Args:
				resources (dict): resource_type (str) -> resource_count (int)
				num_discard (int): the number of resources the player needs
					to discard.

			Returns:
				A list of dicts representing different ways to discard
				`num_discard` resources, where each dict maps a resource_type
				to the number of resources of that type to discard.
			"""
			ret = []
			# check that there are enough cards left to discard
			num_remaining_resources = sum(resources)
			if num_remaining_resources < num_discard or num_discard < 0:
				return ret
			if num_remaining_resources == 0 and num_discard != 0:
				return ret
			if num_remaining_resources == 0 and num_discard == 0:
				ret.append(resources)
				return ret
			# iterate over resources in reverse order
			resource_type = len(resources) - 1
			resource_count = resources[resource_type]
			# `resource_discard_count` is the number of resources to discard of
			#  type `resource_type`.
			for resource_discard_count in range(min(num_discard+1, resource_count+1)):
				remaining_resources = copy.deepcopy(resources)
				remaining_resources.pop(resource_type)
				if num_discard - resource_discard_count < 0:
					break
				valid_discards = recursive_discard(remaining_resources, num_discard - resource_discard_count)
				for discard in valid_discards:
					discard.append(resource_discard_count)
				ret += valid_discards
			return ret
		for discard in recursive_discard(self.resources, num_discard):
			discard_str = ' '.join([list(ResourceType)[k].name + " " + str(v) for (k,v) in enumerate(discard)])
			action_name = f"discard {discard_str}"
			next_board = copy.deepcopy(board)
			p = next_board.cur_player()
			for k, v in enumerate(discard):
				p.resources[k] -= v
				next_board.resource_bank[k] += v
			p.discarding = False
			if next_board.next_player().discarding == False:
				next_board.phase = Phase.PLACE_ROBBER_MAIN
			else:
				next_board.turn += 1
			a = Action(action_name, next_board)
			ret.append(a)
		return ret

	def pass_action(self, board):
		# if PREROLL_PHASE, move to ROLL_PHASE
		# if MAIN_PHASE, increment `turn` and move to PREROLL_PHASE.
		ret = []
		next_board = copy.deepcopy(board)
		if board.phase == Phase.MAIN:
			# end turn, move to next player.
			p = next_board.cur_player()
			for k, v in enumerate(p.development_cards_bought_this_turn):
				p.development_cards[k] += v
				p.development_cards_bought_this_turn[k] = 0
			p.played_development_card_this_turn = False
			next_board.turn += 1
			next_board.phase = Phase.PREROLL
		elif board.phase == Phase.PREROLL:
			# do not play any dev cards, move to roll phase.
			next_board.phase = Phase.ROLL
		else:
			return ret
		a = Action("pass", next_board)
		ret.append(a)
		return ret

	def action_space(self, board):
		"""
		Returns a dictionary listing all possible the actions may take,
		assuming it's their turn.
		"""
		action_space = []
		# Place a road at location x.
		action_space += self.valid_place_road_actions(board)
		# Place a settlement at location x.
		action_space += self.valid_place_settlement_actions(board)
		# Place a city at location x.
		action_space += self.valid_place_city_actions(board)
		# Buy a development card.
		action_space += self.valid_buy_dev_card_actions(board)
		# Place robber at location x.
		action_space += self.valid_place_robber_actions(board)
		# Steal from player x.
		action_space += self.valid_robber_steal_actions(board)
		# Choose what type of resource to offer.
		action_space += self.valid_trade_actions(board)
		# Choose what type of resource to receive, either from trade, year of plenty, or monopoly.
		action_space += self.valid_development_card_actions(board)
		# Choose what resource to discard as a result of 7 being rolled.
		action_space += self.valid_discard_actions(board)
		# Roll/pass. 
		action_space += self.pass_action(board)
		return action_space

	def take_action(self, action_space):
		return

class Board:
	def __init__(self, template, player_names):
		"""
		A class for representing a Catan board state.

		The board is represented as a standard hex map.
		Even indexed rows are offset right, while odd indexed rows are offset
		left.

		Args:
			width (int): the number of hexes in each column.
			height (int): the number of hexes in each row. 
		"""
		self.template = template
		self.num_players = len(player_names)
		self.players = [Player(name) for name in player_names]
		self.turn = 0
		self.height = len(template['land_map'])
		self.width = len(template['land_map'][0])
		self.hex_map = self.gen_hex_map(
			template['land_map'],
			template['land_type_counts'],
			template['number_counts']
		)
		self.development_cards = list(np.random.permutation(
			[DevelopmentCardType[k.upper()] for (k,v) in template['development_cards'].items() for i in range(v)]
		))
		self.harbors = self.gen_harbors(
			template['harbor_locs'],
			template['harbor_type_counts']
		)
		self.resource_bank = [19 for resource_type in list(ResourceType)]
		self.development_card_bank = [template['development_cards'][development_card_type.name.lower()] for development_card_type in list(DevelopmentCardType)]
		self.largest_army_size = 0
		self.largest_army_player = None
		self.longest_road_length = 0
		self.longest_road_player = None
		self.phase = Phase.SETUP
		self.max_roads = 15
		self.max_settlements = 5
		self.max_cities = 5
		self.victory_points_to_win = 10

	def cur_player(self):
		player_idx = self.turn % self.num_players
		return self.players[player_idx]

	def next_player(self):
		player_idx = (self.turn + 1) % self.num_players
		return self.players[player_idx]

	def vertex_alias(self, x, y, i):
		"""
		Every vertex has three aliases, including itself.
		Returns the alias with index in {0,1}.
		"""
		l = -(y % 2)
		r = 1 + l
		if i == 0:
			return (x  , y  , 0)
		if i == 1:
			return (x  , y  , 1)
		if i == 2:
			return (x+r, y+1, 0)
		if i == 3:
			return (x+l, y+1, 1)
		if i == 4:
			return (x+l, y+1, 0)
		if i == 5:
			return (x-1, y  , 1)

	def adjacent_vertices(self, x, y, i):
		"""
		Given a vertex as (x, y, i), returns a list of adjacent vertices.
		`i` is in [0, 1].
		Each vertex has at most 3 adjacent vertices.
		"""
		l = -(y % 2)
		r = 1 + l
		ret = []
		if i == 0:
			# This is the top vertex of the hex.
			# Its neighbors are 
			ret.append((x-1, y  , 1)) # top-right of left hex
			ret.append((x  , y  , 1)) # top-right of this hex
			ret.append((x+l, y-1, 1)) # top-right of top-left hex
		if i == 1:
			# This is the top-right vertex of the hex.
			# Its neighbors are 
			ret.append((x  , y  , 0)) # top of this hex
			ret.append((x+1, y  , 0)) # top of right hex
			ret.append((x+r, y+1, 0)) # top of bottom-right hex
		ret = self.filter_oob(ret)
		return ret

	def incident_edges(self, x, y, i):
		"""
		Given a vertex (x, y, i), return a list of edges that are incident to
		that vertex.
		Each vertex has at most 3 incident edges.
		`i` is in [0, 1].
		"""
		l = -(y % 2)
		r = 1 + l
		ret = []
		if i == 0:
			# This is the top vertex of the hex.
			# Its incident edges are:
			ret.append((x  , y  , 0)) # top-right edge of this hex
			ret.append((x+l, y-1, 1)) # right edge of top-left hex
			ret.append((x+l, y-1, 2)) # bottom-right edge of top-left hex
		if i == 1:
			# This is the top-right vertex of the hex.
			# Its incident edges are:
			ret.append((x  , y  , 0)) # top-right edge of this hex
			ret.append((x  , y  , 1)) # right edge of this hex
			ret.append((x+r, y-1, 2)) # bottom-right edge of top-right hex
		ret = self.filter_oob(ret)
		return ret

	def adjacent_edges(self, x, y, i):
		"""
		Given an edge (x, y, i), returns a list of edges that share an endpoint
		with that edge.
		`i` is in [0, 1, 2].
		Each edge has at most 4 adjacent edges.
		"""
		l = -(y % 2)
		r = 1 + l
		ret = []
		if i == 0:
			# This is the top-right edge of the hex.
			# The two edges that share the left endpoint of `e` are:
			ret.append((x+l, y-1, 1)) # the right edge of the up-left hex
			ret.append((x+l, y-1, 2)) # the bottom-right edge of the up-left hex
			# The two edges that share the right endpoint of `e` are:
			ret.append((x  , y  , 1)) # the right edge of this hex
			ret.append((x+r, y-1, 2)) # the bottom-right edge of the up-right hex
		if i == 1:
			# This is the right edge of the hex.
			# The two edges that share the left endpoint of `e` are:
			ret.append((x  , y  , 0)) # the top-right edge of this hex
			ret.append((x+r, y-1, 2)) # the bottom-right edge of the up-right hex
			# The two edges that share the right endpoint of `e are:
			ret.append((x  , y  , 2)) # the bottom-right edge of this hex
			ret.append((x+r, y+1, 0)) # the top-right edge of the bottom-right hex
		if i == 2:
			# This is the bottom-right edge of the hex.
			# The two edges that share the left endpoint of `e` are:
			ret.append((x,   y  , 1)) # the right edge of this hex
			ret.append((x+r, y+1, 0)) # the top-right edge of the bottom-right hex
			# The two edges that share the right endpoint of `e` are:
			ret.append((x+l, y+1, 0)) # the top-right edge of the bottom-left hex
			ret.append((x+l, y+1, 1)) # the right edge of the bottom-left hex
		ret = self.filter_oob(ret)
		return ret

	def incident_vertices(self, x, y, i):
		"""
		Given an edge (x, y, i), return a list of vertices that are endpoints
		of that edge.
		`i` is in [0, 1, 2].
		Each edge has exactly 2 incident vertices.
		"""
		l = -(y % 2)
		r = 1 + l
		ret = []
		if i == 0:
			#This is the top-right edge of the hex.
			#The endpoints of `e` are:
			ret.append((x, y, 0)) # The top vertex of this hex
			ret.append((x, y, 1)) # The top-right vertex of this hex.
		if i == 1:
			#This is the right edge of the hex.
			#The endpoints of `e` are:
			ret.append((x  , y  , 1)) # the top-right vertex of this hex.
			ret.append((x+r, y+1, 0)) # the top vertex of the bottom-right hex
		if i == 2:
			#This is the bottom-right edge of the hex.
			#The endpoints of `e` are:
			ret.append((x+r, y+1, 0)) # the top vertex of the bottom-right hex
			ret.append((x+l, y+1, 1)) # the top-right vertex of the bottom-left hex
		ret = self.filter_oob(ret)
		return ret

	def vertices_neighboring_hex(self, x, y):
		"""Returns a list of vertices neighboring the given hex.

		Given the hex (x,y), returns a list of (x,y,i) coordinates of the
		vertices that are vertices of the hex. E.g., if a player places the
		robber at a hex (x,y), they may steal from a playe who has a settlement
		or city built on one of the vertices neighboring the hex.
		"""
		l = -(y % 2)
		r = 1 + l
		ret = []
		ret.append((x  , y  , 0))
		ret.append((x  , y  , 1))
		ret.append((x+r, y+1, 0))
		ret.append((x+l, y+1, 1))
		ret.append((x+l, y+1, 0))
		ret.append((x-1, y  , 1))
		return ret

	def hexes_neighboring_vertex(self, x, y, i):
		"""Returns a list of hexes neighboring the given vertex.

		Given the vertex (x,y,i), returns a list of (x,y) coordinates of the
		hexes that have that have (x,y,i) as a vertex. E.g., if a player has
		a settlement at the given vertex, they can receive resource by rolling
		the number on one of the vertex's neighboring hexes.
		"""
		l = -(y % 2)
		r = 1 + l
		ret = []
		ret.append((x,y)) # this hex
		if i == 0:
			# This is the top vertex of the hex. 
			# The neighboring hexes are:
			ret.append((x+l, y-1)) # top left
			ret.append((x+r, y-1)) # top right
		if i == 1:
			# This is the top-right vertex of the hex. 
			# The neighboring hexes are:
			ret.append((x+1, y  )) # right
			ret.append((x+r, y-1)) # top right
		ret = self.filter_oob(ret)
		return ret

	def hexes_neighboring_edge(self, x, y, i):
		l = -(y % 2)
		r = 1 + l
		ret = []
		ret.append((x,y))
		if i == 0:
			# This is the top-right edge. Its neighboring hex is:
			ret.append((x+r, y-1))
		if i == 1:
			# This is the right edge. Its neighboring hex is:
			ret.append((x+1, y))
		if i == 2:
			# This is the bottom-right edge. Its neighboring hex is:
			ret.append((x+r, y+1))
		ret = self.filter_oob(ret)
		return ret

	def filter_oob(self, coords):
		"""Filter out-of-bounds coordinates
		"""
		ret = []
		for coord in coords:
			xib = (0 <= coord[0] and coord[0] < self.width)
			yib = (0 <= coord[1] and coord[1] < self.height)
			if xib and yib:
				ret.append(coord)
		return ret

	def __str__(self):
		# TODO
		for i in range(self.height):
			line = ' '.join([str(x) for x in self.hex_map[i]])
			if i % 2 == 1:
				line = ' ' * 4 + line
			print(line)
		for harbor in self.harbors:
			print(str(harbor))
		return ""

	def gen_hex_map(self, land_map, land_type_counts, number_counts):
		"""
		Generates a random hex map.

		Args:
			land_map (int[][]): A 2D array of 0's an 1's indicating which
				hexes are land and which are ocean.
			land_type_counts (dict: str -> int): A dict that tells how many hexes
				of each type there should be.
			number_counts (int[]): An array of length 13, where the integer at
				index i indicates how many non-desert land hexes should have
				number i.
		"""
		height = len(land_map)
		width = len(land_map[0])
		# generate hex types
		hex_types = np.random.permutation(
			[HexType[k.upper()] for (k,v) in land_type_counts.items() for i in range(v)]
		)
		# generate hex numbers
		hex_numbers = np.random.permutation(
			[i for i in range(13) for j in range(number_counts[i])]
		)
		hex_map = [[None for i in range(width)] for j in range(height)]
		land_idx = 0
		number_idx = 0
		for i in range(height):
			for j in range(width):
				if land_map[i][j]:
					# hex is land
					hex_type = hex_types[land_idx]
					if hex_type == HexType.DESERT:
						# hex is desert
						hex_map[i][j] = Hex(i, j, hex_type, 0)
						hex_map[i][j].robber = True
						self.robber = (j, i)
					else:
						# hex is non-desert
						hex_number = hex_numbers[number_idx]
						hex_map[i][j] = Hex(i, j, hex_type, hex_number)
						number_idx += 1
					land_idx += 1
				else:
					# hex is ocean
					hex_map[i][j] = Hex(i, j, HexType.OCEAN, 0)
		return hex_map

	def hex(self, x, y):
		return self.hex_map[y][x]

	def get_player(self, name):
		names = [p.name for p in self.players]
		player_idx = names.index(name)
		return self.players[player_idx]

	def gen_harbors(self, harbor_locs, harbor_type_counts):
		harbors = []
		harbor_types = np.random.permutation(
			[HarborType[k.upper()] for (k,v) in harbor_type_counts.items() for i in range(v)]
		)
		num_harbors = len(harbor_types)
		for i in range(num_harbors):
			x = harbor_locs['x'][i]
			y = harbor_locs['y'][i]
			v = harbor_locs['v'][i]
			hex_coord = (x, y)
			v0 = self.vertex_alias(x, y, v)
			v1 = self.vertex_alias(x, y, (v+1)%6)
			vertices = [v0, v1]
			harbor = Harbor(
				hex_coord,
				v,
				vertices,
				harbor_types[i]
			)
			harbors.append(harbor)
		return harbors

	def __str__(self):
		# each hex is 7 chars tall and 11 chars wide
		img = [[" " for x in range(self.width * 11 + 5)] for y in range(self.height * 4 + 4)]
		for y in range(self.height):
			for x in range(self.width):
				hx = x * 10 + ((y+1) % 2) * 5 + 5
				hy = y * 4 + 4
				h = self.hex(x,y)
				# draw number
				if h.number != 0:
					img[hy-1][hx:hx+1] = str(h.number).ljust(2, ' ')
					img[hy][hx-2:hx+3] = '{:^5}'.format('o' * (6 - abs(h.number - 7)))
				# draw hex string
				if h.type != "ocean":
					img[hy+1][hx-3:hx+4] = '{:^7}'.format(h.type.name)
				elif (x,y) not in [harbor.hex_coord for harbor in self.harbors]:
					img[hy+1][hx-2:hx+2] = f"({x},{y})"
				# draw robber
				if (x, y) == self.robber:
					img[hy+2][hx:hx+1] = 'R'
				# draw vertices
				dx = [ 0,  5]
				dy = [-3, -1]
				for i in range(2):
					nh = self.hexes_neighboring_vertex(x, y, i)
					nh_types = [self.hex(*coord).type for coord in nh]
					land_adj = not all([t == HexType.OCEAN for t in nh_types])
					if not land_adj:
						continue
					if h.settlements[i] != None:
						img[hy+dy[i]][hx+dx[i]] = h.settlements[i].name[0].lower()
					elif h.cities[i] != None:
						img[hy+dy[i]][hx+dx[i]] = h.cities[i].name[0].upper()
					else:
						img[hy+dy[i]][hx+dx[i]] = 'x'
				# draw edges
				dx = [ 3, 5, 3]
				dy = [-2, 0, 2]
				for i in range(3):
					nh = self.hexes_neighboring_edge(x, y, i)
					nh_types = [self.hex(*coord).type for coord in nh]
					land_adj = not all([t == HexType.OCEAN for t in nh_types])
					if not land_adj:
						continue
					if h.roads[i] != None:
						img[hy+dy[i]][hx+dx[i]] = h.roads[i].name[0].lower()
					else:
						img[hy+dy[i]][hx+dx[i]] = '.'
		# draw harbors
		for harbor in self.harbors:
			(x, y) = harbor.hex_coord
			hx = x * 10 + ((y+1) % 2) * 5 + 5
			hy = y * 4 + 4
			# draw harbor type
			img[hy][hx-1:hx+2] = str(harbor)
			# draw harbor resource
			if harbor.type != HarborType.GENERIC:
				img[hy+1][hx-3:hx+4] = '{:^7}'.format(harbor.type.name)
			# draw harbor access lines
			for v in [harbor.v, (harbor.v + 1) % 6]:
				if v == 0:
					img[hy-2][hx] = '|'
				elif v == 1:
					img[hy-1][hx+4] = '-'
				elif v == 2:
					img[hy+1][hx+4] = '-'
				elif v == 3:
					img[hy+2][hx] = '|'
				elif v == 4:
					img[hy+1][hx-4] = '-'
				elif v == 5:
					img[hy-1][hx-4] = '-'
		ret = '\n'.join([''.join(line) for line in img])
		
		ret = ret.replace("8", colorama.Fore.MAGENTA + "8" + colorama.Style.RESET_ALL)
		ret = ret.replace("6", colorama.Fore.MAGENTA + "6" + colorama.Style.RESET_ALL)
		ret = ret.replace("r", colorama.Fore.RED + "r" + colorama.Style.RESET_ALL)
		ret = ret.replace(" R", colorama.Fore.RED + " R" + colorama.Style.RESET_ALL)
		ret = ret.replace("-R", "-" + colorama.Fore.RED + "R" + colorama.Style.RESET_ALL)
		ret = ret.replace("g", colorama.Fore.GREEN + "g" + colorama.Style.RESET_ALL)
		ret = ret.replace("G ", colorama.Fore.GREEN + "G" + colorama.Style.RESET_ALL + " ")
		ret = ret.replace("G-", colorama.Fore.GREEN + "G" + colorama.Style.RESET_ALL + "-")
		ret = ret.replace("b", colorama.Fore.BLUE + "b" + colorama.Style.RESET_ALL)
		ret = ret.replace("B ", colorama.Fore.BLUE + "B " + colorama.Style.RESET_ALL)
		ret = ret.replace("B-", colorama.Fore.BLUE + "B" + colorama.Style.RESET_ALL + "-")
		ret = ret.replace("y", colorama.Fore.YELLOW + "y" + colorama.Style.RESET_ALL)
		ret = ret.replace("Y", colorama.Fore.YELLOW + "Y" + colorama.Style.RESET_ALL)
		ret = ret.replace(" R ", colorama.Fore.MAGENTA + " R " + colorama.Style.RESET_ALL)
		ret = ret.replace("BRICK", colorama.Fore.RED + "BRICK" + colorama.Style.RESET_ALL)
		ret = ret.replace("LUMBER", colorama.Fore.GREEN + "LUMBER" + colorama.Style.RESET_ALL)
		ret = ret.replace("ORE", colorama.Fore.BLUE + "ORE" + colorama.Style.RESET_ALL)
		ret = ret.replace("GRAIN", colorama.Fore.YELLOW + "GRAIN" + colorama.Style.RESET_ALL)
		ret = ret.replace("WOOL", colorama.Fore.CYAN + "WOOL" + colorama.Style.RESET_ALL)
		
		return ret

	def winner(self):
		for p in self.players:
			if p.victory_points() >= self.victory_points_to_win:
				return p.name
		return None

	def vector_representation(self, rotate=0, debug=False):
		def one_hot(i, n):
			ret = [0] * n
			ret[i] = 1
			return ret
		def dense_count(i, n):
			return [1] * i + [0] * (n-i)
		ret = []
		# hex types
		hex_types = [one_hot(self.hex(x,y).type.value, len(HexType)) for y in range(self.height) for x in range(self.width)]
		for v in hex_types:
			ret.extend(v)
		if debug: print(len(ret))
		# hex numbers
		hex_numbers = [one_hot(self.hex(x,y).number, 13) for y in range(self.height) for x in range(self.width)]
		for v in hex_numbers:
			ret.extend(v)
		if debug: print(len(ret))
		# harbor types
		harbor_types = [one_hot(harbor.type.value, len(HarborType)) for harbor in self.harbors]
		for v in harbor_types:
			ret.extend(v)
		if debug: print(len(ret))
		# robber location
		robber_loc = [one_hot(self.robber[0], self.width), one_hot(self.robber[1], self.height)]
		for v in robber_loc:
			ret.extend(v)
		if debug: print(len(ret))
		# phase
		phase = [one_hot(self.phase.value, len(Phase))]
		for v in phase:
			ret.extend(v)
		if debug: print(len(ret))
		# largest army size
		largest_army_size = [dense_count(self.largest_army_size, 15)]
		for v in largest_army_size:
			ret.extend(v)
		if debug: print(len(ret))
		# largest army player
		if self.largest_army_player == None:
			largest_army_player = [one_hot(len(self.players), len(self.players)+1)]
		else:
			largest_army_player = [one_hot((self.largest_army_player.value + rotate) % 4, len(self.players)+1)]
		for v in largest_army_player:
			ret.extend(v)
		if debug: print(len(ret))
		# longest road length
		longest_road_length = [dense_count(self.longest_road_length, self.max_roads)]
		for v in longest_road_length:
			ret.extend(v)
		# longest road player
		if self.longest_road_player == None:
			longest_road_player = [one_hot(len(self.players), len(self.players)+1)]
		else:
			longest_road_player = [one_hot((self.longest_road_player.value + rotate) % 4, len(self.players)+1)]
		for v in longest_road_player:
			ret.extend(v)
		if debug: print(len(ret))
		# development cards remaining in bank
		development_cards_remaining = [dense_count(len(self.development_cards), 25)]
		for v in development_cards_remaining:
			ret.extend(v)
		if debug: print(len(ret))
		# player settlements
		settlements = []
		for y in range(self.height):
			for x in range(self.width):
				h = self.hex(x,y)
				for name in h.settlements:
					if name == None:
						settlements.append(one_hot(len(self.players), len(self.players)+1))
					else:
						settlements.append(one_hot((name.value + rotate) % 4, len(self.players)+1))
		for v in settlements:
			ret.extend(v)
		if debug: print(len(ret))
		# player cities
		cities = []
		for y in range(self.height):
			for x in range(self.width):
				h = self.hex(x,y)
				for name in h.cities:
					if name == None:
						settlements.append(one_hot(len(self.players), len(self.players)+1))
					else:
						settlements.append(one_hot((name.value + rotate) % 4, len(self.players)+1))
		for v in cities:
			ret.extend(v)
		if debug: print(len(ret))
		# player roads
		roads = []
		for y in range(self.height):
			for x in range(self.width):
				h = self.hex(x,y)
				for name in h.roads:
					if name == None:
						roads.append(one_hot(len(self.players), len(self.players)+1))
					else:
						roads.append(one_hot((name.value + rotate) % 4, len(self.players)+1))
		for v in roads:
			ret.extend(v)
		if debug: print(len(ret))
		# num settlements
		for p_idx in range(len(self.players)):
			p = self.players[(p_idx + rotate) % 4]
			ret.extend(dense_count(len(p.settlements), board.max_settlements))
		if debug: print(len(ret))
		# num cities
		for p_idx in range(len(self.players)):
			p = self.players[(p_idx + rotate) % 4]
			ret.extend(dense_count(len(p.cities), board.max_cities))
		if debug: print(len(ret))
		# num roads
		for p_idx in range(len(self.players)):
			p = self.players[(p_idx + rotate) % 4]
			ret.extend(dense_count(len(p.roads), board.max_roads))
		if debug: print(len(ret))
		# player resources
		resources = []
		for p_idx in range(len(self.players)):
			p = self.players[(p_idx + rotate) % 4]
			for v in p.resources:
				resources.append(dense_count(v, 19))
		for v in resources:
			ret.extend(v)
		if debug: print(len(ret))
		# player dev cards
		player_development_cards = []
		for p_idx in range(len(self.players)):
			p = self.players[(p_idx + rotate) % 4]
			for v in p.development_cards:
				player_development_cards.append(dense_count(v, 15))
		for v in player_development_cards:
			ret.extend(v)
		if debug: print(len(ret))
		# player dev cards bought this turn
		player_development_cards_bought_this_turn = []
		for p_idx in range(len(self.players)):
			p = self.players[(p_idx + rotate) % 4]
			for v in p.development_cards_bought_this_turn:
				player_development_cards_bought_this_turn.append(dense_count(v, 15))
		for v in player_development_cards_bought_this_turn:
			ret.extend(v)
		if debug: print(len(ret))
		# played dev card this turn
		for p_idx in range(len(self.players)):
			p = self.players[(p_idx + rotate) % 4]
			if p.played_development_card_this_turn:
				ret.append(1)
			else:
				ret.append(0)
		if debug: print(len(ret))
		# player army size
		player_army_size = []
		for p_idx in range(len(self.players)):
			p = self.players[(p_idx + rotate) % 4]
			player_army_size.append(dense_count(p.army_size, 15))
		for v in player_army_size:
			ret.extend(v)
		if debug: print(len(ret))
		# player longest road length
		player_longest_road_length = []
		for p_idx in range(len(self.players)):
			p = self.players[(p_idx + rotate) % 4]
			player_longest_road_length.append(dense_count(p.longest_road_length, 15))
		for v in player_longest_road_length:
			ret.extend(v)
		if debug: print(len(ret))
		# turn
		turn = [one_hot((self.turn + rotate) % 4, 4)]
		for v in turn:
			ret.extend(v)
		if debug: print(len(ret))
		# victory points
		victory_points = []
		for p_idx in range(len(self.players)):
			p = self.players[(p_idx + rotate) % 4]
			victory_points.append(dense_count(p.victory_points(), self.victory_points_to_win))
		for v in victory_points:
			ret.extend(v)
		if debug: print(len(ret))
		# TODO: add bank to representation.
		# TODO: limit `dense_count` size.
		return ret

	def play(self, policy, policy_optimizer=None, advantage=None, print_board=False):

		p = self.cur_player()
		if self.phase == Phase.ROLL:
			# roll
			next_board = copy.deepcopy(self)
			roll_1 = random.randint(1, 6)
			roll_2 = random.randint(1, 6)
			roll_number = roll_1 + roll_2
			if print_board:
				print('roll number: ', roll_number)
			if roll_number == 7:
				# discard
				next_board.phase = Phase.DISCARD
				for player in next_board.players:
					player.discarding = True
			else:
				# distribute resources
				for p in next_board.players:
					p.roll(roll_number, next_board)
				next_board.phase = Phase.MAIN
			return next_board
		elif self.phase == Phase.DRAW_DEV_CARD:
			# draw dev card
			next_board = copy.deepcopy(self)
			p = next_board.cur_player()
			card_idx = random.randint(0, len(next_board.development_cards)-1)
			card_drawn = next_board.development_cards.pop(card_idx)
			if print_board:
				print('card drawn: ', card_drawn)
			p.development_cards_bought_this_turn[card_drawn.value] += 1
			next_board.phase = Phase.MAIN
			return next_board
		elif self.phase in [Phase.STEAL_PREROLL, Phase.STEAL_MAIN]:
			next_board = copy.deepcopy(self)
			for q in next_board.players:
				if q.name == next_board.robber_target:
					robber_target = q
					break
			target_resources = robber_target.resources
			resources_enumerated = [ k for (k,v) in enumerate(target_resources) for i in range(v) ]
			resource_idx = random.randint(0, len(resources_enumerated)-1)
			resource_stolen = resources_enumerated[resource_idx]
			if print_board:
				print('robber target, resource stolen: ', robber_target.name, resource_stolen)
			robber_target.resources[resource_stolen] -= 1
			next_board.cur_player().resources[resource_stolen] += 1
			next_board.robber_target = None
			if self.phase == Phase.STEAL_PREROLL:
				next_board.phase = Phase.PREROLL
			elif self.phase == Phase.STEAL_MAIN:
				next_board.phase = Phase.MAIN
			return next_board
		# TODO: random action
		action_space = p.action_space(self)

		if print_board:
			print(self.__str__())
			print('turn: ', self.turn)
			print('phase: ', self.phase)
			print('player: ', self.cur_player().name)
			print('action_space: ')
			for i, a in enumerate(action_space):
				print(i, str(a))
			print('largest army: ', self.largest_army_player)
			print('longest road: ', self.longest_road_player)
			print('resources bank: ', self.resource_bank)
			print('development_card bank: ', self.development_card_bank)
			for q in board.players:
				print(
					q.name, 
					q.victory_points(), 
					q.resources, 
					q.development_cards, 
					q.largest_army_player,
					q.longest_road_player, 
					q.army_size, 
					q.longest_road_length
				)
		
		if policy == None or policy[board.turn % 4] == None:
			action_idx = random.randint(0, len(action_space)-1)
		else:
			model = policy[board.turn % 4]
			batch = []
			"""
			vec = [torch.tensor(a.board.vector_representation(rotate=4-(a.board.turn%4), debug=False)).unsqueeze(0) for a in action_space]
			for elem_idx, elem in enumerate(vec):
				if elem.numel() != 3286:
					# debug
					a = action_space[elem_idx]
					test = a.board.vector_representation(rotate=4-(a.board.turn%4), debug=True)
					print(len(test))
					exit(0)
			"""
			# I need to be able to predict this player's chance of winning no matter who's turn it is,
			# especially if looking at states deep in the search tree.
			batch = []
			for a in action_space:
				vec = a.board.vector_representation()
				#vec = a.board.vector_representation(rotate=4-(self.turn%4))
				if len(vec) != 3286:
					a.board.vector_representation(debug=True)
					#a.board.vector_representation(rotate=4-(self.turn%4), debug=True)
				batch.append(vec)
			batch = torch.tensor(batch, dtype=torch.float)
			logits = model(batch)[:,self.turn % 4]
			probs = torch.exp(logits - torch.logsumexp(logits, 0))
			probs = probs + 0.01
			probs = probs / probs.sum()
			if policy_optimizer != None:
				prob_ratio = probs / probs.detach()
				clipped_ratio = torch.clip(prob_ratio, max=1.2)
				L = -torch.sum(clipped_ratio * advantage(batch)[:,self.turn % 4])
				policy_optimizer.zero_grad()
				L.backward()
				policy_optimizer.step()
			action_idx = torch.multinomial(probs, 1)
		a = action_space[action_idx]
		if print_board:
			print('action: ', a.name)
		next_board = a.board
		return next_board

"""
I could train a model for each action type, where each model takes in just a
board description. Or, I could train a single model that takes as input a 
concatenation of a board description from before the action is taken and 
after the action is taken.

action space:
target_hex
target_vertex
target_player
building_type (settlement, city, road, dev_card)
trade_in (brick, lumber, ore, grain, wool)
trade_out (brick, lumber, ore, grain, wool)
play_dev_card (knight, victory_point, year_of_plenty, monopoly, road_building)
pass

input space:
for each vertex:
	settlement color
	city color
	road color
for each hex:
	hex type
	hex number
	has_robber
for each player:
	resources in hand
	number of development_cards in hand
	victory points
	has_longest_road
	has_largest_army
	for each resource:
		expected production
for each resource:
	number in bank
longest_road_length
my_longest_road_length
largest_army_size
my_army_size 

TODO:
Road Building
Longest Road
Custom boards
AI
Redo colors

     s     
  s  -  s  
s-       -s
s   05wR  s
s-       -s
  s  -  s  
     s     
"""

if __name__ == "__main__":
	seed = 1
	random.seed(seed)
	np.random.seed(seed)
	torch.manual_seed(seed)
	with open("standard_board.json", 'r') as f:
		template = json.load(f)
		PlayerNames = Enum('PlayerNames', [
			'RED',
			'BLUE',
			'GREEN',
			'YELLOW'
		], start=0)
		board = Board(template, PlayerNames)
		p = board.cur_player()
		input_dimension = len(board.vector_representation())
		policy = nn.Sequential(
			nn.Linear(input_dimension, 100),
			nn.ReLU(),
			nn.Linear(100, len(PlayerNames))
		)
		advantage = nn.Sequential(
			nn.Linear(input_dimension, 100),
			nn.ReLU(),
			nn.Linear(100, len(PlayerNames))
		)
		policy_optimizer = torch.optim.Adam(policy.parameters(), lr=1e-5)
		advantage_optimizer = torch.optim.Adam(advantage.parameters(), lr=1e-4)
		loss = nn.CrossEntropyLoss()
		for game in range(10000):
			board = Board(template, PlayerNames)
			winner = None
			batch = []
			for play_step in range(600):
				for rotate in range(4):
					vec = torch.tensor(board.vector_representation(rotate), dtype=torch.float).unsqueeze(0)
					if vec.numel() != 3286:
						print('debug: ', len(board.vector_representation(rotate, debug=True)))
					batch.append(vec)
				winner = board.winner()
				if winner != None:
					break
				board = board.play(
					[policy, policy, policy, policy], 
					policy_optimizer=policy_optimizer, 
					advantage=advantage, 
					print_board=(game==247)
				)
			if winner == None:
				winner = np.argmax([p.victory_points() for p in board.players])
			else:
				winner = winner.value
			batch = torch.cat(batch, 0)
			label = []
			for t in range(play_step+1):
				for rotate in range(4):
					game_label = torch.zeros(1, len(PlayerNames))
					game_label[:,(winner + rotate) % 4] = 1
					label.append(game_label)
			label = torch.cat(label, 0)
			output = advantage(batch)
			L = loss(output, label)
			advantage_optimizer.zero_grad()
			L.backward()
			advantage_optimizer.step()
			print('advantage loss: ', game, L.item(), 'winner: ', winner)

			if game % 100 == 0:
				print('eval: ')
				for eval_game in range(10):
					board = Board(template, PlayerNames)
					winner = None
					for play_step in range(600):
						winner = board.winner()
						if winner != None:
							break
						board = board.play([None, None, policy, None])
					if winner == None:
						winner = np.argmax([p.victory_points() for p in board.players])
					else:
						winner = winner.value
					print('\t', eval_game, play_step, ' winner: ', winner)