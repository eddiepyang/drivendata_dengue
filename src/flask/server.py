from flask import Flask, make_response, request
import io
import csv
import numpy as np
import pandas as pd
from statsmodels.tsa.api import ExponentialSmoothing
from flask_jsonpify import jsonpify


# def transform(text_file_contents):
#     return text_file_contents.replace("=", ",")

def get_data(df):

    data = df[df.city=='sj'].iloc[:,1:].station_precip_mm
    data.index = pd.DatetimeIndex(df[df.city=='sj'].iloc[:,1:].week_start_date)

    return data

def get_model(data):

    fit = ExponentialSmoothing(data.tail(52), trend='add', 
                         damped=True, seasonal='add', 
                         seasonal_periods=4).fit()
    return fit

data = None
model = None

app = Flask(__name__)

@app.route('/')
def form():
    return """
        <html>
            <body>
                <h1>Transform a file demo</h1>

                <form action="/transform" method="post" enctype="multipart/form-data">
                    <input type="file" name="data_file" />
                    <input type="submit" />
                </form>
            </body>
        </html>
    """

@app.route('/predict', methods=["POST"])
def predict():
    new_data = request.get_json()
    #print(new_data.keys())

   
    # df = pd.read_csv(f)
    df = pd.DataFrame(new_data)
    #print(df.head())
    data = get_data(df).fillna(0)
    #print(data.dtype)
    model = get_model(data)
    #print(model.summary())
    result = model.forecast(10)
    print(result)
    return jsonpify(result.to_dict())
    
    

if __name__ == "__main__":
    app.run(host='127.0.0.1', port=5001, debug=True)