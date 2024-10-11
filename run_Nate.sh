#chmod +x run_s.sh (Need to run in terminal before you can run the bash file)

#Clears the Commandline for a cleaner output from the code
clear

#Removes all left over csv files in outputs from previous runs
#-f means no errors get printed if there's no csv files
rm -f outputs/*.csv

#Compiles Nate_Solver.f90
gfortran -o Nate4 -g -Wall -O3 -fcheck=all -fmax-errors=1 -std=f2008 Nate4.f90 -llapack -lblas

#Runs Nate_Solver
./Nate4

#Changes the current directory to the outputs folder
cd outputs

#Combines the numerical_1 and numerical_2 csv files into 1 csv file
#TAC returns the data of double_numerical_2.csv but reversed
#This TAC value is then fed as an input into the CAT command which
#returns the merged double_numerical_1 and double_numerical_2 file
#The file is then saved as double_numerical.csv
cat double_numerical_1.csv <(tac double_numerical_2.csv) > double_numerical.csv

#Removes the now superfluous double_numerical_1.csv and double_numerical_2.csv
rm double_numerical_1.csv double_numerical_2.csv


#Does the same as the above code but for double_numerical_3-5 CSV files
if [ -e "double_numerical_3.csv" ]; then
	cat double_numerical.csv double_numerical_3.csv > temp.csv
	mv temp.csv double_numerical.csv
	rm double_numerical_3.csv
fi

if [ -e "double_numerical_4.csv" ]; then
	cat double_numerical.csv <(tac double_numerical_4.csv) > temp.csv
	mv temp.csv double_numerical.csv
	rm double_numerical_4.csv
fi

if [ -e "double_numerical_5.csv" ]; then
	cat double_numerical.csv double_numerical_5.csv > temp.csv
	mv temp.csv double_numerical.csv
	rm double_numerical_5.csv
fi

#Returns the current directory back to the original
cd ..

#Plots the outputted data
python3 ./plotter.py
