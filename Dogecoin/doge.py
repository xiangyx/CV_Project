import requests as re
import pandas as pd
from datetime import datetime, timedelta
import time 

def formatURL(coin: str='DOGE') -> str:
    # The url of coindesk's API about price info
    url = "https://production.api.coindesk.com/v2/price/values/"
    # Price info for the past 15 minutes
    start_time = (datetime.now() - timedelta(minutes=15)).isoformat(timespec="minutes")
    end_time = datetime.now().isoformat(timespec="minutes")
    params = f"?start_date={start_time}&end_date={end_time}&ohlc=false"
    return url + coin + params

def getData(coin: str='DOGE') -> pd.DataFrame:
    prices = re.get(formatURL(coin))
    prices = prices.json()['data']['entries']
    data = pd.DataFrame(prices, columns=['time', 'price'])
    return data

def detectDip(data, threshold = 10):
    '''
    If the minimum price is less than 90% of the maximum price
    in this time window, raise a flag indicating there is dip
    '''
    peak = data['price'].max()
    bottom = data['price'].min()
    dip = 100 - (bottom/peak)*100
    if dip > threshold:
        return True
    else:
        return False

print(formatURL( ))
while True:
    data = getData()
    print(detectDip(data))
    time.sleep(300)