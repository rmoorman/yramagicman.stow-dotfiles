#!/usr/bin/python3

import requests
import json
import socket

hostname = socket.gethostname()

j = json.JSONDecoder()
m = requests.get('http://api.openweathermap.org/data/2.5/weather?zip=45342,us&units=metric&appid=8c7a3eda698041f3d194181a7b1da8c5')
i = requests.get('http://api.openweathermap.org/data/2.5/weather?zip=45342,us&units=imperial&appid=8c7a3eda698041f3d194181a7b1da8c5')

weather = m.json()
imperial = i.json()

wkeys = [k for k in weather.keys()]
wkeys_weather = list(filter((lambda x: x == 'weather'), wkeys))
wkeys_main = list(filter((lambda x: x == 'main'), wkeys))

ikeys = [k for k in imperial.keys()]
ikeys_imperial = list(filter((lambda x: x == 'imperial'), ikeys))
ikeys_main = list(filter((lambda x: x == 'main'), ikeys))
if weather['cod'] != 200:
    display = 'error'
    print(display)
display = ''
display = display + str(weather[wkeys_weather[0]][0]['main'])
display = display + ' ' + str(weather[wkeys_main[0]]['temp']) + 'C'
display = display + ' ' + str(imperial[ikeys_main[0]]['temp']) + 'F'
print(display)
