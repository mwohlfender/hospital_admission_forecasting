
# python script `am_series_rnn`:

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
number_job_array = "032"

# number of computing job within array `number_job_array` (NEEDS TO BE ADAPTED)
index = 1

# local directory where files related to computing jobs done on UBELIX, the HPC cluster of the University of Bern, are stored (NEEDS TO BE ADAPTED)
directory_computing_jobs = "C:/Users/mw22f082/Documents_MW/projects/defeat_covid/hospital_admission_forecasting/local_files/hospital_admission_forecasting/jobs/"


bool_pred_train = True
bool_pred_test = True


directory_parameters <- directory_computing_jobs + number_job_array + "/parameters/"
directory_data = directory_computing_jobs + number_job_array + "/data/"
directory_results = directory_computing_jobs + number_job_array + "/results/"


parameters_data_kNp_dates_hyp = pd.read_csv((directory_parameters + "05_job_setup/grid_combinations_data_kNp_dates_hyp_" + number_job_array + ".csv"), converters={"number_xy": str, "number_combination_features": str, "number_combination_kNp": str, "group_dates_train_test": str, "number_dates_train_test": str, "number_hyp_par_grid": str}).iloc[int(index)-1,:]

number_xy = parameters_data_kNp_dates_hyp["number_xy"]
number_combination_features = parameters_data_kNp_dates_hyp["number_combination_features"]
name_data_set = parameters_data_kNp_dates_hyp["name_data_set"]
number_combination_kNp = parameters_data_kNp_dates_hyp["number_combination_kNp"]
group_dates_train_test = parameters_data_kNp_dates_hyp["group_dates_train_test"]
number_dates_train_test = parameters_data_kNp_dates_hyp["number_dates_train_test"]
number_hyp_par_grid_rnn = parameters_data_kNp_dates_hyp["number_hyp_par_grid"]
number_hyp_par_subgrid_rnn = parameters_data_kNp_dates_hyp["number_hyp_par_subgrid"]


if number_hyp_par_subgrid_rnn == 0:
    
    grid_hyp_params_rnn = pd.read_csv((directory_parameters + "04_hyperparameters/rnn/" + number_hyp_par_grid_rnn + "/hyp_par_rnn_" + number_hyp_par_grid_rnn + "_param_grid.csv"))
    
else:
    
    grid_hyp_params_rnn = pd.read_csv((directory_parameters + "04_hyperparameters/rnn/" + number_hyp_par_grid_rnn + "/hyp_par_rnn_" + number_hyp_par_grid_rnn + "_param_grid_" + str(number_hyp_par_subgrid_rnn) + ".csv"))


# determine how many different combinations of hyper parameters will be tested
n_par_combs_rnn = grid_hyp_params_rnn.shape[0]
  
# determine part of name of data sets that depends on `number_xy`, `number_combination_features`, `name_data_set`, `number_combination_kNp`, `group_dates_train_test`, and `number_dates_train_test`
full_name_data_set = "XY_" + number_xy + "_" + number_combination_features + "_" + name_data_set + "_P_" + number_combination_kNp + "_D_" + group_dates_train_test + "_" + number_dates_train_test
  
# determine part of name of results data file that depends on number_combination_features`, `name_data_set`, `number_combination_kNp`, `group_dates_train_test`, `number_dates_train_test`, `number_hyp_par_grid_rnn` and `number_hyp_par_subgrid_rnn`
full_name_results = full_name_data_set + "_H_" + number_hyp_par_grid_rnn + "_" + str(number_hyp_par_subgrid_rnn)
  
# define pandas data frame to store parameters (`number_xy`, `number_combination_features`, `name_data_set`, `number_combination_kNp`, `group_dates_train_test`, `number_dates_train_test`, `number_hyp_par_grid_rnn` and `number_hyp_par_subgrid_rnn`)
parameters_grid = pd.DataFrame({"number_xy": [number_xy] * n_par_combs_rnn,
"number_combination_features": [number_combination_features] * n_par_combs_rnn,
"name_data_set": [name_data_set] * n_par_combs_rnn,
"number_combination_kNp": [number_combination_kNp] * n_par_combs_rnn,
"group_dates_train_test": [group_dates_train_test] * n_par_combs_rnn,
"number_dates_train_test": [number_dates_train_test] * n_par_combs_rnn, 
"number_hyp_par_grid_rnn": [number_hyp_par_grid_rnn] * n_par_combs_rnn,
"number_hyp_par_subgrid_rnn": [number_hyp_par_subgrid_rnn] * n_par_combs_rnn})

# read training data
data_train = pd.read_csv(directory_data + "features/train/features_train_" + full_name_data_set + ".csv").sort_index(axis = 1)
target_train = pd.read_csv(directory_data + "target/train/target_train_" + full_name_data_set + ".csv")

# read testing data
data_test = pd.read_csv(directory_data + "features/test/features_test_" + full_name_data_set + ".csv").sort_index(axis = 1)
target_test = pd.read_csv(directory_data + "target/test/target_test_" + full_name_data_set + ".csv")



# format data for neural network model
n_training_samples = np.shape(data_train)[0]
n_testing_samples = np.shape(data_test)[0]
n_variables = np.shape(data_train)[1]

data_target_train = np.hstack([data_train.to_numpy().reshape((n_training_samples,n_variables)),target_train["y"].to_numpy().reshape((n_training_samples,1))])
data_target_test = np.hstack([data_test.to_numpy().reshape((n_testing_samples,n_variables)),target_test["y"].to_numpy().reshape((n_testing_samples,1))])

# fit scaler
scaler = MinMaxScaler(feature_range=(0, 1))
scaler = scaler.fit(data_target_train)

# scale `data_target_train`
data_target_train_scaled = scaler.transform(data_target_train)

# scale `data_target_test`
data_target_test_scaled = scaler.transform(data_target_test)

# shape data
X_train, y_train = data_target_train_scaled[:, 0:-1], data_target_train_scaled[:, -1]
X_test, y_test = data_target_test_scaled[:, 0:-1], data_target_test_scaled[:, -1]



# define array where results of parameter estimation with different combinations of hyper parameters will be stored
if bool_pred_train:
  
  predictions_rnn_train = np.zeros([n_par_combs_rnn, n_training_samples])

if bool_pred_test:
  
  predictions_rnn_test = np.zeros([n_par_combs_rnn, n_testing_samples])



# define array where rmse will be stored
rmse_rnn_train_test = pd.DataFrame({"rmse_train": [0.0] * n_par_combs_rnn,
"rmse_test": [0.0] * n_par_combs_rnn})


for ii in range(n_par_combs_rnn):
  
  if int(grid_hyp_params_rnn["nlayers"][ii]) == 1:

    model = Sequential([
      Input(shape=(n_variables,)),
      Dense(int(grid_hyp_params_rnn["nneurons_one"][ii]), activation=grid_hyp_params_rnn["act_function_one"][ii]),
      Dense(1)])

  if int(grid_hyp_params_rnn["nlayers"][ii]) == 2:

    model = Sequential([
      Input(shape=(n_variables,)),
      Dense(int(grid_hyp_params_rnn["nneurons_one"][ii]), activation=grid_hyp_params_rnn["act_function_one"][ii]),
      Dense(int(grid_hyp_params_rnn["nneurons_two"][ii]), activation=grid_hyp_params_rnn["act_function_two"][ii]),
      Dense(1)])
  
  # determine optimizer
  if grid_hyp_params_rnn["optimizer"][ii] == "adam":

    opt = Adam(learning_rate=float(grid_hyp_params_rnn["learning_rate"][ii]))

  if grid_hyp_params_rnn["optimizer"][ii] == "sgd":

    opt = SGD(learning_rate=float(grid_hyp_params_rnn["learning_rate"][ii]))

  if grid_hyp_params_rnn["optimizer"][ii] == "rmsprop":
      
    opt = RMSprop(learning_rate=float(grid_hyp_params_rnn["learning_rate"][ii]))
  
  # determine loss function
  if grid_hyp_params_rnn["loss_function"][ii] == "mse":
    
    def loss_function(y_true, y_pred):
      return mean_squared_error(y_true, y_pred)
  
  # compile model
  model.compile(optimizer=opt, loss=loss_function, metrics=[RootMeanSquaredError()])

  # train model  
  history = model.fit(X_train, y_train, epochs=int(grid_hyp_params_rnn["nepochs"][ii]), validation_split=0.2, verbose=False)

  # predict on training data
  y_pred_train = model.predict(X_train)
  
  # apply inverse transform
  prediction_train = np.reshape(np.array(scaler.inverse_transform(np.hstack([data_target_train_scaled[:, 0:-1], y_pred_train]))[:,-1]), newshape = n_training_samples)
  
  # calculate rmse
  rmse_rnn_train_test.loc[ii, "rmse_train"] = np.sqrt(np.mean((target_train["y"]-prediction_train)**2))
  
  # store results
  if bool_pred_train:
    
    predictions_rnn_train[ii,:] = prediction_train
    
  # predict on testing data
  y_pred_test = model.predict(X_test)
  
  # apply inverse transform
  prediction_test = np.reshape(np.array(scaler.inverse_transform(np.hstack([data_target_test_scaled[:, 0:-1], y_pred_test]))[:,-1]), newshape = n_testing_samples)
  
  # calculate rmse
  rmse_rnn_train_test.loc[ii, "rmse_test"] = np.sqrt(np.mean((target_test["y"]-prediction_test)**2))
  
  # store results
  if bool_pred_test:
    
    predictions_rnn_test[ii,:] = prediction_test
    
# define pandas data frame `output` (`parameters_grid`, `grid_hyp_params_rnn`, `rmse_rnn_train_test` and predictions (if stored))
output_0 = pd.concat([parameters_grid, grid_hyp_params_rnn, rmse_rnn_train_test],axis=1)

if bool_pred_train:
  
  output_1 = pd.concat([output_0, pd.DataFrame(data=predictions_rnn_train, columns=["pred_day_train_" + str(i+1).zfill(3) for i in range(n_training_samples)])],axis=1)
  
else:
  
  output_1 = output_0

if bool_pred_test:
  
  output = pd.concat([output_1, pd.DataFrame(data=predictions_rnn_test, columns=["pred_day_test_" + str(i+1).zfill(3) for i in range(n_testing_samples)])],axis=1)
  
else:
  
  output = output_1

pd.DataFrame(output).to_csv(directory_results + "results_rnn_" + full_name_results + ".csv", sep=',', encoding='utf-8', index=False)


