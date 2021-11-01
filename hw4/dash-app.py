# Run this app with `python app.py` and
# visit http://127.0.0.1:8050/ in your web browser.

import dash
import flask
from dash import dcc
from dash import html
import numpy as np
import plotly.express as px
import pandas as pd
from dash.dependencies import Input, Output

# disable warning about changes
pd.options.mode.chained_assignment = None  # default='warn'

f_app = flask.Flask(__name__)
app = dash.Dash(__name__)
app.config.suppress_callback_exceptions = True

# read the data 
url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,count(tree_id), health, steward' +\
        '&$where=spc_common=\'ginkgo\'' +\
        '&$group=spc_common, health, steward').replace(' ', '%20')
df = pd.read_json(url)

dNames = pd.read_json('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?')

def get_options(list_species):
    dict_list = []
    for i in list_species:
        if i == i:
            dict_list.append({'label': i, 'value': i})

    return dict_list

def get_boro(list_boros):
    dict_list = []
    for i in list_boros:
        if i == i:
            dict_list.append({'label': i, 'value': i})

    return dict_list



app.layout = html.Div(children=[
    html.Div(className='row',  # Define the row element
     children=[
        html.Div(className='four columns div-user-controls',
            children=[
                html.H2("HW4 - Tree's Health Tracker"),
                html.P('''Visualising NYC Trees health with Plotly - Dash'''),
                html.P('''Pick any tree species from the dropdown below.'''),
                html.Div(
                    className='div-for-dropdown',
                    children=[
                        dcc.Dropdown(id='specieselector', options=get_options(dNames['spc_common'].unique()),
                            value=dNames['spc_common'].sort_values()[0],
                            style={'backgroundColor': '#1E1E1E'},
                            className='specieselector'
                        )],
                    style={'color': '#1E1E1E'}),
                html.P('''Pick a boro from the dropdown below.'''),
                html.Div(
                    className='div-for-dropdown',
                    children=[
                        dcc.Dropdown(id='boroselector', options=get_boro(dNames['boroname'].unique()),
                            value=dNames['boroname'].sort_values()[0],
                            style={'backgroundColor': '#1E1E1E'},
                        )],
                    style={'color': '#1E1E1E'}),
            ]),
        html.Div(className='eight columns div-for-charts bg-grey',
            children=[
                dcc.Graph(id='bar_graph',config={'displayModeBar': False}, animate=True)
            ])
        ])
    ])

# Callback for timeseries price
@app.callback(Output('bar_graph', 'figure'),
              [Input('specieselector', 'value'),
              Input('boroselector', 'value')])

def update_graph(specie_value, boro_value):
    print(specie_value)
    print(boro_value)
    
    url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,count(tree_id), health, steward' +\
        f'&$where=spc_common=\'{specie_value}\'' +\
        f'&boroname=\'{boro_value}\'' +\
        '&$group=spc_common, health, steward').replace(' ', '%20')
    df = pd.read_json(url)

    df['health'] = df['health'].fillna('Fair')
    df_sub = df
    figure = px.bar(df_sub,
        x="health",
        y="count_tree_id",
        color="steward",
        barmode="stack")
    return figure

if __name__ == '__main__':
    app.run_server(debug=True)