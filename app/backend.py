import subprocess
import os

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
        
        # Build the command to run the R script
        command = ["Rscript", script_path, input_file]
        if directory is not None:
            command.append(directory)
        if param1 is not None:
            command.append(param1)
        if param2 is not None:
            command.append(param2)
        if param3 is not None:
            command.append(param3)
        
        # Execute the R script
        result = subprocess.run(command, check=True, text=True, capture_output=True)
        return f"Model run completed successfully!\nOutput:\n{result.stdout}"
    except subprocess.CalledProcessError as e:
        return f"An error occurred:\n{e.stderr}"


