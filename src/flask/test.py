from flask import Flask, make_response, request
import io
import csv
import numpy as np
import pandas as pd
from statsmodels.tsa.api import ExponentialSmoothing

app = Flask(__name__)

def transform(text_file_contents):
    return text_file_contents.replace("=", ",")

def proc_data(df):

    data = df[df.city=='sj'].iloc[:,1:].station_precip_mm
    data.index = pd.DatetimeIndex(df[df.city=='sj'].iloc[:,1:].week_start_date)

    return data

def get_model():
    fit = ExponentialSmoothing(data.tail(104), trend='add', 
                         damped=True, seasonal='add', 
                         seasonal_periods=52).fit()
    return fit

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

@app.route('/transform', methods=["POST"])
def transform_view():
    f = request.files['data_file']
    if not f:
        return "No file"

    stream = io.StringIO(f.stream.read().decode("UTF8"), newline=None)
    csv_input = csv.reader(stream)
    df = pd.DataFrame.from_csv(csv_input)
    data = proc_data(df)
    fit = get_model()

    #print("file contents: ", file_contents)
    #print(type(file_contents))
    print(csv_input)
    for row in csv_input:
        print(row)

    stream.seek(0)
    result = transform(fit.forecast(10))

    response = make_response(result)
    response.headers["Content-Disposition"] = "attachment; filename=result.csv"
    return response

if __name__ == "__main__":
    app.run(host='0.0.0.0', port=5001, debug=True)