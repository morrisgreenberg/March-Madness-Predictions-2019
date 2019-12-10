# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import bs4
import requests
import os
from datetime import datetime
import time as tm
import random as rd
import re

'''temp = requests.get('http://www.sports-reference.com/cbb/schools/hampton/2018-gamelogs.html')

temp2 = requests.get('http://www.sports-reference.com/cbb/schools/')

temp3 = requests.get('https://www.sports-reference.com/cbb/boxscores/2018-12-01-19-duke.html')

content = temp.content
content2 = temp2.content
content3 = temp3.content

soup = bs4.BeautifulSoup(content)
soup2 = bs4.BeautifulSoup(content2)
soup3 = bs4.BeautifulSoup(content3)

teamEntries = list(soup.findAll("tr"))
teamEntries2 = list(soup2.findAll("tr"))
teamEntries3 = list(soup3.findAll("tr"))

# To update this section yearly!'''
start_dates = [datetime(2011,3,15,0,0), datetime(2012,3,13,0,0), 
               datetime(2013,3,19,0,0), datetime(2014,3,18,0,0), 
               datetime(2015,3,17,0,0), datetime(2016,3,15,0,0),
               datetime(2017,3,14,0,0), datetime(2018,3,13,0,0),
               datetime(2019,3,19,0,0)]

def findCollegeNames():
    colleges = []
    link = requests.get('http://www.sports-reference.com/cbb/schools/').content
    soup = bs4.BeautifulSoup(link)
    teamList = list(soup.findAll("tr"))
    
    for i, entry in enumerate(teamList[1:]):
        try:
            indInfoString = str(entry.findAll("td")[0].findAll("a")[0])
            bound1 = indInfoString.find('/cbb/schools/')+len('/cbb/schools/')
            bound2 = len(indInfoString)
            for j in range(0, len(indInfoString[bound1:])):
                if(indInfoString[bound1+j] == '/'):
                    bound2 = bound1+j
                    break
            colleges.append(indInfoString[bound1:bound2])
        except:
            colleges = colleges
    return(colleges)


def parseBRboxscore(link, gamedate):
    df = pd.DataFrame([], columns=["Date", "TEAM", "Player", "Minutes"])
    
    try:
        link_request = requests.get(link)
    except:
        requests.status_code = "Connection refused"
        print("RESULT FAILED: SITE BLOCKED GAME")
        tm.sleep(rd.uniform(10, 20))
        #it only tries re-fetching a search 3 times, otherwise moves on
        for i in range(0, 3):
            try:
                link_request = requests.get(link)
                break
            except:
                requests.status_code = "Connection refused"
                print("RESULT FAILED: SITE BLOCKED GAME")
                tm.sleep(rd.uniform(10, 20))
                
    if link_request.status_code != 404:
        soup = bs4.BeautifulSoup(link_request.content)
        
        game_name = soup.findAll("title")[0].text
        team1 = re.split(" vs. ", game_name)[0].encode('ascii', 'ignore')
        if len(re.split(" vs. ", game_name)) > 1:
            team2 = re.split(" Box Score", re.split(" vs. ", game_name)[1])[0].encode('ascii', 'ignore')
        else:
            return(None)
        
        team = None
        num_starters = 0
        search_entries = list(soup.findAll("tr"))
        for i, entry in enumerate(search_entries):
            if len(entry.findAll("th", attrs={'data-stat':"player"})) > 0:
                player = entry.findAll("th", attrs={'data-stat':"player"})[0].text.encode('ascii', 'ignore')
                if player==b'Starters':
                    num_starters += 1
                if player not in (b'Starters', b'Reserves', b'School Totals', b'Player'):
                    #print(player)
                    team = team1 if num_starters==1 else team2
                    minutes = entry.findAll("td", attrs={'data-stat':"mp"})[0].text.encode('ascii', 'ignore')
                    df.loc[len(df)] = [gamedate, team, player, minutes]
    
    return(df)

def parseBRstats(output_file, player_file, year):
    df = pd.DataFrame([], columns=["Date", "TEAM", "homeStatus", "Opponent", "oppHasLink", 
                                   "winner", "pointsFor", "pointsAllowed", "FG", "FGA", "FG_pc", 
                                   "3P", "3PA", "3P_pc", "FT", "FTA", "FT_pc", "ORB",
                                   "TRB", "AST", "STL", "BLK", "TOV", "PF",
                                   "FG_opp", "FGA_opp","FG_pc_opp", "3P_opp",
                                   "3PA_opp", "3P_pc_opp", "FT_opp", "FTA_opp", 
                                   "FT_pc_opp", "ORB_opp", "TRB_opp", "AST_opp", 
                                   "STL_opp", "BLK_opp", "TOV_opp", "PF_opp",
                                   "tourneyGame"])
    
    player_df = pd.DataFrame([], columns = ["Date", "TEAM", "Player", "Minutes"])
    for year in range(year, year+1):
        colleges = findCollegeNames()
        for col in colleges:
            url = 'http://www.sports-reference.com/cbb/schools/' + col + '/' + str(year) +'-gamelogs.html'
            try:
                html = requests.get(url)
            except:
                requests.status_code = "Connection refused"
                print("RESULT FAILED: SITE BLOCKED COLLEGE %s" % col)
                tm.sleep(rd.uniform(10, 20))
                #it only tries re-fetching a search 3 times, otherwise moves on
                for i in range(0, 3):
                    try:
                        html = requests.get(url)
                        break
                    except:
                        requests.status_code = "Connection refused"
                        print("RESULT FAILED: SITE BLOCKED COLLEGE %s" % col)
                        tm.sleep(rd.uniform(10, 20))
            if html.status_code != 404:
                soup = bs4.BeautifulSoup(html.content)
                teamEntries = list(soup.findAll("tr"))
                for i, entry in enumerate(teamEntries):
                
                    if(i % 100 == 0):
                        print("** parsed %i/%i %s %i entries" % (i, len(teamEntries), col, year))
                    
                    
                    teamStats = entry.findAll("td")
                    
                    game_link = None if len(teamStats) < 1 else 'http://www.sports-reference.com' + entry.findAll("a", href=True)[0]['href']
                    Date = None if len(teamStats) < 1 else teamStats[0].text.encode('ascii', 'ignore')
                    curr_date = start_dates[year-2011]
                    isTourneyGame = None if len(teamStats) < 1 else (datetime.strptime(teamStats[0].text, '%Y-%m-%d')>=curr_date)
                    homeStatus = None if len(teamStats) < 2 else teamStats[1].text.encode('ascii', 'ignore')
                    Opp = None if len(teamStats) < 3 else teamStats[2].text.encode('ascii', 'ignore')
                    oppHasLink = None if len(teamStats) <3 else len(teamStats[2].findAll("a", href=True))
                    winner = None if len(teamStats) < 4 else teamStats[3].text.encode('ascii', 'ignore')
                    PF = None if len(teamStats) < 5 else teamStats[4].text.encode('ascii', 'ignore')
                    PA = None if len(teamStats) < 6 else teamStats[5].text.encode('ascii', 'ignore')
                    FG = None if len(teamStats) < 7 else teamStats[6].text.encode('ascii', 'ignore')
                    FGA = None if len(teamStats) < 8 else teamStats[7].text.encode('ascii', 'ignore')
                    FG_pc = None if len(teamStats) < 9 else teamStats[8].text.encode('ascii', 'ignore')
                    ThreeP = None if len(teamStats) < 10 else teamStats[9].text.encode('ascii', 'ignore')
                    ThreePA = None if len(teamStats) < 11 else teamStats[10].text.encode('ascii', 'ignore')
                    ThreeP_pc = None if len(teamStats) < 12 else teamStats[11].text.encode('ascii', 'ignore')
                    FT = None if len(teamStats) < 13 else teamStats[12].text.encode('ascii', 'ignore')
                    FTA = None if len(teamStats) < 14 else teamStats[13].text.encode('ascii', 'ignore')
                    FT_pc = None if len(teamStats) < 15 else teamStats[14].text.encode('ascii', 'ignore')
                    ORB = None if len(teamStats) < 16 else teamStats[15].text.encode('ascii', 'ignore')
                    TRB = None if len(teamStats) < 17 else teamStats[16].text.encode('ascii', 'ignore')
                    AST = None if len(teamStats) < 18 else teamStats[17].text.encode('ascii', 'ignore')
                    STL = None if len(teamStats) < 19 else teamStats[18].text.encode('ascii', 'ignore')
                    BLK = None if len(teamStats) < 20 else teamStats[19].text.encode('ascii', 'ignore')
                    TOV = None if len(teamStats) < 21 else teamStats[20].text.encode('ascii', 'ignore')
                    Fouls = None if len(teamStats) < 22 else teamStats[21].text.encode('ascii', 'ignore')
                    FGo = None if len(teamStats) < 24 else teamStats[23].text.encode('ascii', 'ignore')
                    FGAo = None if len(teamStats) < 25 else teamStats[24].text.encode('ascii', 'ignore')
                    FG_pco = None if len(teamStats) < 26 else teamStats[25].text.encode('ascii', 'ignore')
                    ThreePo = None if len(teamStats) < 27 else teamStats[26].text.encode('ascii', 'ignore')
                    ThreePAo = None if len(teamStats) < 28 else teamStats[27].text.encode('ascii', 'ignore')
                    ThreeP_pco = None if len(teamStats) < 29 else teamStats[28].text.encode('ascii', 'ignore')
                    FTo = None if len(teamStats) < 30 else teamStats[29].text.encode('ascii', 'ignore')
                    FTAo = None if len(teamStats) < 31 else teamStats[30].text.encode('ascii', 'ignore')
                    FT_pco = None if len(teamStats) < 32 else teamStats[31].text.encode('ascii', 'ignore')
                    ORBo = None if len(teamStats) < 33 else teamStats[32].text.encode('ascii', 'ignore')
                    TRBo = None if len(teamStats) < 34 else teamStats[33].text.encode('ascii', 'ignore')
                    ASTo = None if len(teamStats) < 35 else teamStats[34].text.encode('ascii', 'ignore')
                    STLo = None if len(teamStats) < 36 else teamStats[35].text.encode('ascii', 'ignore')
                    BLKo = None if len(teamStats) < 37 else teamStats[36].text.encode('ascii', 'ignore')
                    TOVo = None if len(teamStats) < 38 else teamStats[37].text.encode('ascii', 'ignore')
                    Foulso = None if len(teamStats) < 39 else teamStats[38].text.encode('ascii', 'ignore')
                    # append to dataframe
                    df.loc[len(df)] = [Date, col, homeStatus, Opp, oppHasLink, winner, PF, PA, FG, FGA, FG_pc,
                                       ThreeP, ThreePA, ThreeP_pc, FT, FTA, FT_pc, ORB, TRB, AST,
                                       STL, BLK, TOV, Fouls, FGo, FGAo, FG_pco, ThreePo,
                                       ThreePAo, ThreeP_pco, FTo, FTAo, FT_pco, ORBo, TRBo,
                                       ASTo, STLo, BLKo, TOVo, Foulso, isTourneyGame]
                    if game_link is not None:
                        new_data = parseBRboxscore(game_link, Date)
                        if new_data is not None:
                            player_df = player_df.append(new_data)
    df.to_csv(output_file)
    player_df.to_csv(player_file)
    print("** saved to '%s'" % output_file)
    
os.chdir('C:\\Users\\Morris\\Desktop\\March Madness\\2019\\Output')
parseBRstats('All Games 2012.csv', 'Players 2012.csv', 2012)
parseBRstats('All Games 2013.csv', 'Players 2013.csv', 2013)
parseBRstats('All Games 2014.csv', 'Players 2014.csv', 2014)
parseBRstats('All Games 2015.csv', 'Players 2015.csv', 2015)
parseBRstats('All Games 2016.csv', 'Players 2016.csv', 2016)
parseBRstats('All Games 2017.csv', 'Players 2017.csv', 2017)
parseBRstats('All Games 2018.csv', 'Players 2018.csv', 2018)
parseBRstats('All Games 2019.csv', 'Players 2019.csv', 2019)
parseBRstats('All Games 2011.csv', 'Players 2011.csv', 2011)
