# ABM_ENDO

**Evolutionary Intertemporal Decision Rules in Agenet Based Macro Model**

Implemented in Julia 1.3.1 https://julialang.org/


To run the simulation / replicate the results.
1.  Load all files into the project directory.
2.	Set parameters in q_set_para_initial() in the file Leng_run.jl.
    Set model specification and display option in Leng_run.jl
3.	Run the program  Leng_run.jl 



Files:

**Leng_run.jl**
Runs the simulation.
Before running this program set the simulation parameters in the function q_set_para_initial().
And set specification and display options

**leng_func.jl**
Contains functions used for the simulation.
Called while running the simulation.


**support_func.jl**
Contains functions called while running the simulation


**show_functions.jl**
Containes the functions used to create the plots.


