
# python script `am_004_lstm_ubelix`:

# This python-script is run on the high performance computing cluster of the University of Bern, UBELIX.

import sys
import os
import numpy as np
import pandas as pd
from sklearn.preprocessing import MinMaxScaler
from keras import backend as K
from keras.models import Sequential
from keras.layers import Input, Dense, Activation, Dropout, LSTM
from keras.optimizers import Adam, SGD, RMSprop
from keras.losses import mean_squared_error
from keras.metrics import RootMeanSquaredError

# number of array of computing jobs (NEEDS TO BE ADAPTED)
number_job_array = "038"

# number of computing job within array `number_job_array
index = str(sys.argv[1]).strip()

# set working directory on ubelix (NEEDS TO BE ADAPTED)
os.chdir("DIRECTORY/"+number_job_array)


bool_pred_train = True
bool_pred_test = True


directory_parameters = "parameters/"
directory_data = "data/"
directory_results = "results/"


parameters_data_kNp_dates_hyp = pd.read_csv((directory_parameters + "05_job_setup/grid_combinations_data_kNp_dates_hyp_" + number_job_array + ".csv"), converters={"number_xy": str, "number_combination_features": str, "number_combination_kNp": str, "group_dates_train_test": str, "number_dates_train_test": str, "number_hyp_par_grid": str}).iloc[int(index)-1,:]

number_xy = parameters_data_kNp_dates_hyp["number_xy"]
number_combination_features = parameters_data_kNp_dates_hyp["number_combination_features"]
name_data_set = parameters_data_kNp_dates_hyp["name_data_set"]
number_combination_kNp = parameters_data_kNp_dates_hyp["number_combination_kNp"]
group_dates_train_test = parameters_data_kNp_dates_hyp["group_dates_train_test"]
number_dates_train_test = parameters_data_kNp_dates_hyp["number_dates_train_test"]
number_hyp_par_grid_lstm = parameters_data_kNp_dates_hyp["number_hyp_par_grid"]
number_hyp_par_subgrid_lstm = parameters_data_kNp_dates_hyp["number_hyp_par_subgrid"]

parameters_kNp = pd.read_csv((directory_parameters + "03_kNp_dates_train_test/01_table_parameters_kNp.csv"), converters={"number": str, "k": int, "N": int, "p": int}).iloc[int(number_combination_kNp)-1,:]

p = parameters_kNp["p"]


if number_hyp_par_subgrid_lstm == 0:
    
    grid_hyp_params_lstm = pd.read_csv((directory_parameters + "04_hyperparameters/lstm/"+ number_hyp_par_grid_lstm + "/hyp_par_lstm_" + number_hyp_par_grid_lstm + "_param_grid.csv"))
    
else:
    
    grid_hyp_params_lstm = pd.read_csv((directory_parameters + "04_hyperparameters/lstm/"+ number_hyp_par_grid_lstm + "/hyp_par_lstm_" + number_hyp_par_grid_lstm + "_param_grid_" + str(number_hyp_par_subgrid_lstm) + ".csv"))
    

# determine how many different combinations of hyper parameters will be tested
n_par_combs_lstm = grid_hyp_params_lstm.shape[0]
  
# determine part of name of data sets that depends on `number_xy`, `number_combination_features`, `name_data_set`, `number_combination_kNp`, `group_dates_train_test`, and `number_dates_train_test`
full_name_data_set = "XY_" + number_xy + "_" + number_combination_features + "_" + name_data_set + "_P_" + number_combination_kNp + "_D_" + group_dates_train_test + "_" + number_dates_train_test
  
# determine part of name of results data file that depends on `number_xy`, `number_combination_features`, `name_data_set`, `number_combination_kNp`, `group_dates_train_test`, `number_dates_train_test`, `number_hyp_par_grid_lstm` and `number_hyp_par_subgrid_lstm`
full_name_results = full_name_data_set + "_H_" + number_hyp_par_grid_lstm + "_" + str(number_hyp_par_subgrid_lstm)
  
# define pandas data frame to store parameters (`number_xy`, `number_combination_features`, `name_data_set`, `number_combination_kNp`, `group_dates_train_test`, `number_dates_train_test`, `number_hyp_par_grid_lstm` and `number_hyp_par_subgrid_lstm`)
parameters_grid = pd.DataFrame({"number_xy": [number_xy] * n_par_combs_lstm,
"number_combination_features": [number_combination_features] * n_par_combs_lstm,
"name_data_set": [name_data_set] * n_par_combs_lstm,
"number_combination_kNp": [number_combination_kNp] * n_par_combs_lstm,
"group_dates_train_test": [group_dates_train_test] * n_par_combs_lstm,
"number_dates_train_test": [number_dates_train_test] * n_par_combs_lstm, 
"number_hyp_par_grid_lstm": [number_hyp_par_grid_lstm] * n_par_combs_lstm,
"number_hyp_par_subgrid_lstm": [number_hyp_par_subgrid_lstm] * n_par_combs_lstm})

# read training data
data_train = pd.read_csv(directory_data + "features/train/features_train_" + full_name_data_set + ".csv").sort_index(axis = 1)
target_train = pd.read_csv(directory_data + "target/train/target_train_" + full_name_data_set + ".csv")

# read testing data
data_test = pd.read_csv(directory_data + "features/test/features_test_" + full_name_data_set + ".csv").sort_index(axis = 1)
target_test = pd.read_csv(directory_data + "target/test/target_test_" + full_name_data_set + ".csv")



# format data for long short-term memory model
n_training_samples = np.shape(data_train)[0]
n_testing_samples = np.shape(data_test)[0]
n_variables = int(np.shape(data_train)[1] / p)

data_target_train = np.hstack([data_train.to_numpy().reshape((n_training_samples,n_variables * p)),target_train["y"].to_numpy().reshape((n_training_samples,1))])
data_target_test = np.hstack([data_test.to_numpy().reshape((n_testing_samples,n_variables * p)),target_test["y"].to_numpy().reshape((n_testing_samples,1))])

# fit scaler
scaler = MinMaxScaler(feature_range=(0, 1))
scaler = scaler.fit(data_target_train)

# scale `data_target_train`
data_target_train_scaled = scaler.transform(data_target_train)

# scale `data_target_test`
data_target_test_scaled = scaler.transform(data_target_test)

# shape data
X_train, y_train = data_target_train_scaled[:, 0:-1].reshape(n_training_samples, p, n_variables, order = "F"), data_target_train_scaled[:, -1].reshape(n_training_samples, 1, 1)
X_test, y_test = data_target_test_scaled[:, 0:-1].reshape(n_testing_samples, p, n_variables, order = "F"), data_target_test_scaled[:, -1].reshape(n_testing_samples, 1, 1)



# define array where results of parameter estimation with different combinations of hyper parameters will be stored
if bool_pred_train:
  
  predictions_lstm_train = np.zeros([n_par_combs_lstm, n_training_samples])

if bool_pred_test:
  
  predictions_lstm_test = np.zeros([n_par_combs_lstm, n_testing_samples])



# define array where rmse will be stored
rmse_lstm_train_test = pd.DataFrame({"rmse_train": [0.0] * n_par_combs_lstm,
"rmse_test": [0.0] * n_par_combs_lstm})


for ii in range(n_par_combs_lstm):
  
  if int(grid_hyp_params_lstm["nlayers"][ii]) == 1:

    model = Sequential()
    model.add(LSTM(int(grid_hyp_params_lstm["nneurons_one"][ii]), activation=grid_hyp_params_lstm["act_function_one"][ii], input_shape=(p, n_variables)))
    model.add(Dense(1))

  if int(grid_hyp_params_lstm["nlayers"][ii]) == 2:
      
    model = Sequential()
    model.add(LSTM(int(grid_hyp_params_lstm["nneurons_one"][ii]), activation=grid_hyp_params_lstm["act_function_one"][ii], return_sequences=True, input_shape=(p, n_variables)))
    model.add(LSTM(int(grid_hyp_params_lstm["nneurons_two"][ii]), activation=grid_hyp_params_lstm["act_function_two"][ii]))
    model.add(Dense(1))
  
  # determine optimizer  
  if grid_hyp_params_lstm["optimizer"][ii] == "adam":

    opt = Adam(learning_rate=float(grid_hyp_params_lstm["learning_rate"][ii]))
  
  if grid_hyp_params_lstm["optimizer"][ii] == "sgd":
  
    opt = SGD(learning_rate=float(grid_hyp_params_lstm["learning_rate"][ii]))
  
  if grid_hyp_params_lstm["optimizer"][ii] == "rmsprop":
    
    opt = RMSprop(learning_rate=float(grid_hyp_params_lstm["learning_rate"][ii]))

  # determine loss function
  if grid_hyp_params_lstm["loss_function"][ii] == "mse":
    
    def loss_function(y_true, y_pred):
      return mean_squared_error(y_true, y_pred)
  
  # compile model
  model.compile(optimizer=opt, loss=loss_function, metrics=[RootMeanSquaredError()])

  # train model  
  model.fit(X_train, y_train, epochs=int(grid_hyp_params_lstm["nepochs"][ii]), validation_split=0.2, verbose=False)

  # predict on training data
  y_pred_train = model.predict(X_train)
  
  # apply inverse transform
  prediction_train = np.reshape(np.array(scaler.inverse_transform(np.hstack([data_target_train_scaled[:, 0:-1], y_pred_train]))[:,-1]), newshape = n_training_samples)
  
  # calculate rmse
  rmse_lstm_train_test.loc[ii, "rmse_train"] = np.sqrt(np.mean((target_train["y"]-prediction_train)**2))
  
  # store results
  if bool_pred_train:
    
    predictions_lstm_train[ii,:] = prediction_train
    
  # predict on testing data
  y_pred_test = model.predict(X_test)
  
  # apply inverse transform
  prediction_test = np.reshape(np.array(scaler.inverse_transform(np.hstack([data_target_test_scaled[:, 0:-1], y_pred_test]))[:,-1]), newshape = n_testing_samples)
  
  # calculate rmse
  rmse_lstm_train_test.loc[ii, "rmse_test"] = np.sqrt(np.mean((target_test["y"]-prediction_test)**2))
  
  # store results
  if bool_pred_test:
    
    predictions_lstm_test[ii,:] = prediction_test

# define pandas data frame `output` (`parameters_grid`, `grid_hyp_params_lstm`, `rmse_lstm_train_test` and predictions (if stored))
output_0 = pd.concat([parameters_grid, grid_hyp_params_lstm, rmse_lstm_train_test],axis=1)

if bool_pred_train:
  
  output_1 = pd.concat([output_0, pd.DataFrame(data=predictions_lstm_train, columns=["pred_day_train_" + str(jj+1).zfill(3) for jj in range(n_training_samples)])],axis=1)
  
else:
  
  output_1 = output_0

if bool_pred_test:
  
  output = pd.concat([output_1, pd.DataFrame(data=predictions_lstm_test, columns=["pred_day_test_" + str(jj+1).zfill(3) for jj in range(n_testing_samples)])],axis=1)
  
else:
  
  output = output_1

pd.DataFrame(output).to_csv(directory_results + "results_lstm_"+full_name_results+".csv", sep=',', encoding='utf-8', index=False)


