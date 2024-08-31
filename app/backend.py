import subprocess
import os
import tkinter as tk 

def get_script_path(script_name):
    # Get the directory where the script is running
    app_dir = os.path.dirname(os.path.abspath(__file__))
    # Construct the path to the R script
    script_path = os.path.join(app_dir, "Scripts", script_name)
    return script_path

def run_r_script(script_name, input_file, directory=None, param1=None, param2=None, param3=None):
    try:
        # Get the script path dynamically
        script_path = get_script_path(script_name)
        print(script_path)
        #script_path = script_path.get() if isinstance(script_path, tk.StringVar) else script_path

        # Extract absolute paths for input files and directory
        input_file_path = os.path.abspath(input_file.get())
        print('exp_path::::', input_file_path)
        directory_path = os.path.abspath(directory.get() if isinstance(directory, tk.StringVar) else directory) if directory else None
        
        param1 = param1.get() if isinstance(param1, tk.StringVar) else param1
        param2 = param2.get() if isinstance(param2, tk.StringVar) else param2
        param3 = param3.get() if isinstance(param3, tk.StringVar) else param3

        # Build the command to run the R script
        command = ["Rscript", script_path, input_file_path]
        if directory_path:
            command.append(directory_path)
        if param1 is not None:
            command.append(param1)
        if param2 is not None:
            command.append(param2)
        if param3 is not None:
            command.append(param3)

        # Execute the R script
       # print(script_path)
       # print(command)
        
        result = subprocess.run(command, check=True, text=True, capture_output=True)
        return f"Model run completed successfully!\nOutput:\n{result.stdout}"
    except subprocess.CalledProcessError as e:
        return f"An error occurred:\n{e.stderr}"

