This folder contains code and data necessary to: 
1. generate data for simulations in Rstan.
2. Generate plots of simlutions for 'OG' and simultaneous scenarios.


Code				Function
data_generation.R		Generates dataset for simulation
model.R				Run RStan models on simulated data from 'data_generation.R'
simulation_plots_full.R		Generate plots for simulated data from 'model.R'
simulation_plots_reduced.R	Generate plots for simulated data used in paper
simultaneous_rcode.R		Run simulations for simultaneous strike scenario
simultaneous_plots.R		Generatae plot for simulated data from simultaneous strike scenario
				from 'simultaneous_rcode.R'