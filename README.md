# ABM_ENDO

**Evolutionary Intertemporal Decision Rules in an Agent Based Macro Model**

Implemented in Julia 1.3.1 https://julialang.org/


To run the simulation / replicate the results.
1.  Load all files into the project directory.
2.	Set parameters in q_set_para_initial() in the file ABM_run.jl.
    Set model specification and display option in ABM_run.jl
3.	Run the program  ABM_run.jl 



Files:

**ABM_run.jl**
Runs the simulation.
Before running this program set the simulation parameters in the function q_set_para_initial().
And set specification and display options

**ABM_func.jl**
Contains functions used for the simulation.
Called while running the simulation.


**support_func.jl**
Contains functions called while running the simulation


**show_functions.jl**
Contains the functions used to create the plots.


