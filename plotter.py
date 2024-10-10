#Imports libraries
import matplotlib.pyplot as plt
from os import getcwd, path, chdir, listdir, mkdir

#Gets the inputs stored in inputs.txt and returns them as a dictionary
def get_inputs():
    #Opens the inputs.txt file in read-only mode
    with open("inputs.txt", "r") as f:
        # Defines the outputted dictionary
        inputs = {}
        
        #Loops through the lines in the file:
        for line in f:
            #separates the variables names from their values
            values = line.split("=")

            #This if statement was added after writing the pseudocode as
            #   we realised that if the file had no equals,
            #   then it would cause the program to crash
            #   due to there being no values in the values array
            if len(values) == 2:
                #Gets rid of any whitespace in the name of the constants
                values[0] = values[0].strip()

                #Skips over the line being read if it's been commented out
                if values[0][0] in ("#", "!"):
                    continue

                #Switch-Case statment which looks at the name of the constants defined in the inputs.txt file
                match values[0]:
                    # Stores the variable called N as an integer
                    case "N":
                        #(Needs to be converted to a float first before becoming a int due to python weirdness)
                        values[1] = int(float(values[1]))
                    #Default Case (by default defines the constants as floats)
                    case _:
                        values[1] = float(values[1])

                #Adds the value of the constant to the dictionary under the name of the variable
                inputs[values[0]] = values[1]
    return inputs

#Gets all the files in the outputs folder, plots them, and saves the plots in the graphs folder
def plot(inputs, plotting_mode="separate"):
    #Saves the current dirrectory
    cwd = getcwd()
    #Changes the directory to be the outputs folder
    chdir(path.join(cwd, "outputs"))
    #Gets the name of all the files in the dirrectory in alphabetical order
    files = listdir()
    
    #Loops through the files (in alphabetical order)
    for file in files:
        #Opens the current file in read-only mode
        with open(file, "r") as f:
            #Creating 2 arrays to hold
            k = []
            E = []
            n = 0
            for line in f:
                n += 1
                #separates the string by commas
                values = line.split(",")
                #Gets the r values for the different axis
                k.append(float(values[0]))
                E.append(float(values[1]))
        
        #Gets rid of the case where you double plot the single for no reason as we don't do the numerical for the single
        #(Only realised upon implementation)
        if not (plotting_mode=="overlap" and "single" in file):
            #Plots k vs E (labelling whether it's plotting the analytical or numerical solution)
            if "analytical" in file:
                plt.plot(k,E,label="Analytical")
            elif "numerical" in file:
                plt.plot(k,E,label="Numerical")
            else:
                plt.plot(k,E)
            
            #Only saves the file either if it's saving each graph individually or if the analytical and numerical have been graphed
            ## New Line Below ##
            if (plotting_mode=="separate") or ("numerical" in file):
                #Labels the axes
                plt.xlabel('k (1/a)')
                plt.ylabel('E(k) (eV)')

                #Sets the title and subtitle
                plt.suptitle(f'Band Structure (1st Brillouin Zone)') #The title
                #(The subtitle shows the values of the constants in the input file for the corresponding graph)
                subtitle = ""
                for key, value in inputs.items():
                    subtitle += f"{key}={value} "
                plt.title(subtitle) #The subtitle

                #Adds a grid behind the data on the graph
                plt.grid()

                #Adds a legend if you're plotting the analytical and numerical on the same graph
                if plotting_mode == "overlap":
                    plt.legend()

                try:
                    #Saves the plot in the graphs folder
                    chdir(path.join(cwd, "graphs"))
                except FileNotFoundError:
                    #Creates the graphs folder if it doesn't already exist
                    mkdir(path.join(cwd, "graphs"))
                    chdir(path.join(cwd, "graphs"))
                
                ## 3 New Lines Below ##
                #Saves the graph as the name of the original csv file but as a png instead and also contains the input variables and plotting_mode in the name                
                elog_name = subtitle.replace("=", "_")
                plt.savefig(f"{file[:-4]} - {elog_name} ({plotting_mode}).png")

                #Making sure the directory returns to the outputs folder
                chdir(path.join(cwd, "outputs"))

                #Makes the program have to plot on a new graph rather than on the same one
                plt.clf()


    #Changes the directory back to the original one
    #This line has also been changed from the pseudocode,
    #   as we accidentally put it in the if statement,
    #   which meant the program was no longer reading,
    #   the files in the outputs folder.
    chdir(cwd)

print("Started Plotting...")
plot(inputs:=get_inputs())
plot(inputs, "overlap")
print("Finished Plotting")
