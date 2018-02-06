import json
import sys
import os
import re

from_dir = "../bga-scraper/bga-games/"
to_file = "bga-games.txt"

def toAlpha(num):
  alpha = "abcdefghi"
  return alpha[int(num) - 1]

def toStratsPawn(x, y):
  return toAlpha(x) + str(10 - int(y))

def toStratsWall(x, y, orientation):
  return toAlpha(x) + str(9 - int(y)) + orientation[0]

def transform(json_data):
  players = set(re.findall('"player_name":"([^"]*)"', json_data))
  if len(players) != 2:
    #print(len(players))
    #print(players)
    raise Exception("not 2 players")

  data = json.loads(json_data)

  record = ""
  for move in data["data"]["data"]:
    for details in move["data"]:
      if details["type"] == "playToken":
        x = details["args"]["x"]
        y = details["args"]["y"]
        strats = toStratsPawn(x, y)

        record += ";" + strats
      elif details["type"] == "playWall":
        x = details["args"]["x"]
        y = details["args"]["y"]
        orientation = details["args"]["orientation"]
        strats = toStratsWall(x, y, orientation)

        record += ";" + strats

      #if "quoridorstrats_notation" in details["args"]:
      #  if details["args"]["quoridorstrats_notation"] != strats:
      #    print(record)
      #    print(details["args"]["quoridorstrats_notation"])
      #    print(strats)
      #    raise Exception("conversion problem!")

  if len(record) < 30:
    raise Exception("too few moves")

  return record


files = ["10001598.js"]
files = os.listdir(from_dir)

to_file = open(to_file, "w")
for fname in files:
  #print(fname)
  json_data = open(from_dir + fname).read()
  try:
    transformed = transform(json_data)
    to_file.write(transformed[1:] + "\n")
  except Exception as e:
    print(e)
