from flask import Flask, make_response, request
import io
import csv
import numpy as np
import pandas as pd
from statsmodels.tsa.api import ExponentialSmoothing
from flask_jsonpify import jsonpify

def get_data(df):

    data = df[df.city=='sj'].iloc[:,1:].station_precip_mm
    data.index = pd.DatetimeIndex(df[df.city=='sj'].iloc[:,1:].week_start_date)

    return data

def get_model(data):

    fit = ExponentialSmoothing(data.tail(52), trend='add', 
                         damped=True, seasonal='add', 
                         seasonal_periods=4).fit()
    return fit

model = None

app = Flask(__name__)

@app.route('/')
def form():
    return """
        <html>
            <body>
                <h1>Available endpoints</h1>                
                <p>train  
                <br>predict
            </body>
        </html>
    """

@app.route('/train', methods=["POST"])
def train():
    global model
    new_data = request.get_json()

    df = pd.DataFrame(new_data)
    #print(df.head())
    data = get_data(df).fillna(0)
    print(data.dtype)
    model = get_model(data)
    print(model)
    return 'model completed'

@app.route('/predict', methods=["POST"])
def predict():
    if model is not None:
        period = request.get_json()
        #print(model.summary())
        result = model.forecast(period['h'])
        print(result)
        return jsonpify(result.to_dict())
    else:
        print('please run train first')    

if __name__ == "__main__":
    app.run(host='127.0.0.1', port=5001, debug=True)