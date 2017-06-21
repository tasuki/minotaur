import json
import os
import re
import time

from bs4 import BeautifulSoup

class Scraper:
  def __init__(self, httpClient):
    self.httpClient = httpClient

  def getDetailLink(self, gameId):
    response = self.httpClient.getGameInfo(gameId)
    soup = BeautifulSoup(response, "lxml")
    link = soup.find("a", { "class": "choosePlayerLink" })
    if link == None:
      print(response)
      return None

    return link["href"]

  def fetchGame(self, gameId):
    url = self.getDetailLink(gameId)
    if url == None:
      print("Url not found for game" + gameId)
      return

    response = self.httpClient.getPage(url)
    gamelog = re.search('g_gamelogs = (.*);$', response, re.MULTILINE)

    if gamelog == None:
      print("Couldn't parse game data for url: " + url)
      return

    record = gamelog.group(1)

    # verify valid json
    try:
      json.loads(record)
    except json.decoder.JSONDecodeError:
      print("Could not parse json!")
      print(record)
      return

    return record

  def fetchAndSave(self, gameId):
    fname = 'bga-games/%s.js' % gameId

    if os.path.isfile(fname):
      print("Already saved game " + gameId)
      return

    record = self.fetchGame(gameId)
    if record != None:
      f = open(fname, 'w')
      f.write(record)
      f.close()

      print("Saved game " + gameId)

  def getNextPage(self, timestamp):
    result = json.loads(self.httpClient.getGameList(timestamp))

    if result["status"] != 1:
      raise RuntimeError("Expected status 1")

    for game in result["data"]:
      html = game["html"]
      tableid = re.search('table=([0-9]+)', html)
      id = tableid.group(1)
      print(id)
      self.fetchAndSave(id)

    return game["timestamp"]

  def loopGameList(self, timestamp = int(time.time())):
    while True:
      print("")
      print("=======")
      print("fetching page " + str(timestamp))
      print("=======")
      print("")

      timestamp = self.getNextPage(timestamp)
