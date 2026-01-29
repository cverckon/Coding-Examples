"""Environmental Data Project"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler



df = pd.read_csv('forest_fires.csv')

df

mo = df['month'].unique()
months = {'jan': 1, 'feb': 2, 'mar': 3, 'apr': 4, 'may': 5, 'jun': 6, 'jul': 7, 'aug': 8, 'sep': 9, 'oct': 10, 'nov': 11, 'dec': 12}

da = df['day'].unique()
days = {'sun': 1, 'mon': 2, 'tue': 3, 'wed': 4, 'thu': 5, 'fri': 6, 'sat': 7}

df['month'] = df['month'].map(months)
df['day'] = df['day'].map(days)

index = []
for i in range(len(df['area'])):
  if df.iloc[i,12] > 600:
    index.append(i)

for i in index:
  df.drop(df.index[i], inplace = True)

index = []
for i in range(len(df['area'])):
  if df.iloc[i,12] > 600:
    index.append(i)
for i in index:
  df.drop(df.index[i], inplace = True)

target = df.columns[12]
predictors = df.columns[0:11]

colors = ['red', 'orange', 'gold', 'lawngreen', 'cyan', 'darkblue', 'magenta', 'gray', 'roaylblue', 'goldenrod', 'olive']
markers = ['o', 'x']

fig, ax = plt.subplots(figsize = (15,15))
for y in [0,1]:
  for x in range(len(predictors)):
    scaler = MinMaxScaler()
    X = scaler.fit_transform(np.array(df[predictors[x]]).reshape(-1,1))
    ax.scatter(X, df[target], marker = markers[y], label = f'{predictors[x]}')
ax.legend()
ax.set_title('Preliminary Plot')
ax.set_xlabel('Predictor Value')
ax.set_ylabel('Area (in ha)')

df_train, df_test = train_test_split(df, test_size = 0.30, random_state = 404)

rfr_y = RandomForestRegressor(n_estimators = 350, oob_score = True, random_state = 404)
rfr_y.fit(df_train[predictors], df_train['area'])

oobs_y = rfr_y.oob_score_
print(f' The oob score for this model is {np.around(oobs_y, 3)}')

fi_str_max = 0
fi_rfr_y = rfr_y.feature_importances_
for i in range(len(fi_rfr_y)):
  pred_str = predictors[i]
  fi_str = str(np.around(fi_rfr_y[i], 3))
  print(f'{pred_str} {fi_str}')

  if fi_rfr_y[i] > fi_str_max:
    fi_str_max = fi_rfr_y[i]
    most_imp_Y = predictors[i]

print(f'The most important feature in the model is {most_imp_Y}')

lm_y = LinearRegression()
lm_y.fit(np.array(df_train[predictors]).reshape(-1,len(predictors)), df_train['area'])
y_pred_lm = lm_y.predict(np.array(df_test[predictors]).reshape(-1,len(predictors)))
y_pred_rf = rfr_y.predict(df_test[predictors])

r2_lm_y = np.around(r2_score(df_test['area'], y_pred_lm), 3)
r2_rf_y = np.around(r2_score(df_test['area'], y_pred_rf), 3)

print(f'The R2 value for the random forest model is {r2_rf_y} and the R2 for the linear regression model is {r2_lm_y}')

fig, ax = plt.subplots(figsize = (10,8))

ax.scatter(y_pred_rf, df_test['area'], c = 'blue', edgecolor = 'gray', alpha = 0.6, label = f'Random Forest Model Predictions, R2 = {r2_rf_y}')
ax.scatter(y_pred_lm, df_test['area'], c = 'firebrick', edgecolor = 'gray', alpha = 0.6, label = f'Linear Regression Model Predictions, R2 = {r2_lm_y}')
ax.plot([0,80], [0,80], c = 'k')

ax.legend()
ax.set_title('Predicted vs. Real Values Both Models')
ax.set_ylabel('Predicted Area (in ha)')
ax.set_xlabel('Real Area (in ha)')
plt.show()