#chmod +x run_s.sh (Need to run in terminal before you can run the bash file)

#Clears the Commandline for a cleaner output from the code
clear

#Compiles Nate_Solver.f90
gfortran -o Nate3 -g -Wall -O3 -fcheck=all -fmax-errors=1 -std=f2008 Nate3.f90 -llapack -lblas

#Runs Nate_Solver
./Nate3

#Plots the outputted data
python3 ./plotter.py
