from flask import Flask, request, session, render_template
#from flask_session import Session
import io
import csv
import numpy as np
import pandas as pd
from statsmodels.tsa.api import ExponentialSmoothing
from statsmodels.api import load
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
    return render_template('includes/main.html')

@app.route('/train', methods=["POST"])
def train():
    #global model
    print(request.args)
    new_data = request.get_json()

    df = pd.DataFrame(new_data)
    #print(df.head())
    data = get_data(df).fillna(0)
    print(data.dtype)
    model = get_model(data)
    model.save('fitted.pkl')
    # session.permanent = True
    # session['user'] = 'ok'
    # session.modified = True
    print(session)
    return 'model completed'
    

@app.route('/predict', methods=["POST"])
def predict():

    model = load('fitted.pkl')
    print('session data: ', session)
    print(request.args)
    period = request.get_json()
    print(period.keys())

    if model is not None:

        #print(model.summary())
        result = model.forecast(period['h'])
        print(result)
        return jsonpify(result.to_dict())
    else:
        return 'please run train first'    


if __name__ == "__main__":

    # app.secret_key = 'super secret key'
    # app.config['SESSION_TYPE'] = 'filesystem'
    app.debug = True
    app.run()
    #app.run(host='0.0.0.1', port=5001, debug=True)