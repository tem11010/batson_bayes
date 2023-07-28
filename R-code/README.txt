This folder contains code and data necessary to: 
1. generate data for simulations in Rstan.
2. Generate plots of simulations for 'OG'/original model, as well simultaneous scenarios.

More details on model specifications are provided in the TAS manuscript. The key code for running the analyses can be found in: data_generation.R, model.R, and simultaneous_sims.R. All other code is used for cleaning and visualizing the data. Note that data is presented in this repo. The extra/ folder contains some anciliary code not necessary for replicating the results in the manuscript. 


Code				Function
data_generation.R		Generates dataset for simulation
model.R				Run RStan models on simulated data from 'data_generation.R'
simulation_plots_full.R		Generate plots for simulated data from 'model.R'
simulation_plots_reduced.R	Generate plots for simulated data used in paper
simultaneous_sims.R		Run simulations for simultaneous strike scenario
simultaneous_plots.R		Generate plot for simulated data from simultaneous strike scenario
				from 'simultaneous_sims.R'
