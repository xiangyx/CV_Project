import requests as re
import pandas as pd
from datetime import datetime, timedelta

from prefect.tasks.control_flow.case import case
from prefect import task, Flow, Parameter



def formatURL(coin: str='DOGE') -> str:
    # The url of coindesk's API about price info
    url = "https://production.api.coindesk.com/v2/price/values/"
    # Price info for the past 15 minutes
    start_time = (datetime.now() - timedelta(minutes=15)).isoformat(timespec="minutes")
    end_time = datetime.now().isoformat(timespec="minutes")
    params = f"?start_date={start_time}&end_date={end_time}&ohlc=false"
    return url + coin + params

def notifyDip():
    return 'There has been a dip in DOGE price'

@task(max_retries=3, retry_delay=timedelta(minutes=1))
def getData(coin: str='DOGE') -> pd.DataFrame:
    prices = re.get(formatURL(coin))
    prices = prices.json()['data']['entries']
    data = pd.DataFrame(prices, columns=['time', 'price'])
    return data

@task
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


with Flow('Elon-Musk') as flow:
    coin = Parameter('coin', default='DOGE')
    threshold = Parameter('Threshold', default=10)
    data = getData(coin)
    is_dip = detectDip(data, threshold)
    with case(is_dip, True):
        notifyDip()

# flow.run()
flow.register('get-rich')