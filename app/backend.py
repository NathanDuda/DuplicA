import subprocess
import os
import tkinter as tk 

# installed: https://cloud.r-project.org/bin/windows/base/R-4.4.1-win.exe
# .\R-4.4.1-win.exe /SILENT /DIR="C:\Program Files\R\R-4.4.1"
# agreed to make changes
# setx PATH "%PATH%;C:\Program Files\R\R-4.4.1\bin"
#  & "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" --version


def get_script_path(script_name):
    # Get the directory where the script is running
    app_dir = os.path.dirname(os.path.abspath(__file__))
    # Construct the path to the R script
    script_path = os.path.join(app_dir, "Scripts", script_name)
    return script_path

def run_r_script(script_name, input_file, directory_path=None, param1=None, param2=None, param3=None):
    R_path = "C:\\Program Files\\R\\R-4.4.1\\bin\\Rscript.exe"
    try:
        # Get the script path dynamically
        script_path = get_script_path(script_name)
        #script_path = script_path.get() if isinstance(script_path, tk.StringVar) else script_path

        # Extract absolute paths for input files and directory
        param1 = param1.get() if isinstance(param1, tk.StringVar) else param1
        param2 = param2.get() if isinstance(param2, tk.StringVar) else param2
        param3 = param3.get() if isinstance(param3, tk.StringVar) else param3

        PC = False
        command = [R_path, script_path, input_file, directory_path, str(param1), str(param2), param3, str(PC)]
        
        # Execute the R script        
        result = subprocess.run(command, check=True, text=True, capture_output=True)
        return f"Model run completed successfully!\nOutput:\n{result.stdout}"
    except subprocess.CalledProcessError as e:
        return f"An error occurred:\n{e.stderr}"

