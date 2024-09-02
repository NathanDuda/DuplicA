import subprocess
import os
#import customtkinter as ctk

# installed: https://cloud.r-project.org/bin/windows/base/R-4.4.1-win.exe
# .\R-4.4.1-win.exe /SILENT /DIR="C:\Program Files\R\R-4.4.1"
# agreed to make changes
# setx PATH "%PATH%;C:\Program Files\R\R-4.4.1\bin"
#  & "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" --version



def get_script_path(script_name):
    app_dir = os.path.dirname(os.path.abspath(__file__))
    script_path = os.path.join(app_dir, "Scripts", script_name)
    return script_path

def run_r_script(run_type, script_name, expression_file, directory_path, dups_file, exp_dir, add_pseudofunc, missing_expr_is_pseudo, rm_exp_lower_than, PC, min_dups_per_species_pair):
    R_path = "C:\\Program Files\\R\\R-4.4.1\\bin\\Rscript.exe"
    try:
        script_path = get_script_path(script_name)
        
        
        
        if run_type == 'OF':
            command = [R_path, script_path, expression_file, directory_path, str(add_pseudofunc), str(missing_expr_is_pseudo), rm_exp_lower_than, str(PC), min_dups_per_species_pair]

                
        if run_type == 'custom':
            
            command = [R_path, script_path, dups_file, exp_dir, str(add_pseudofunc), str(missing_expr_is_pseudo), rm_exp_lower_than, str(PC), min_dups_per_species_pair]

        
        
        
        print('COMMAND:', command)
        
        result = subprocess.run(command, check=True, text=True, capture_output=True)
        return f"Model run completed successfully!\nOutput:\n{result.stdout}"
    except subprocess.CalledProcessError as e:
        return f"An error occurred:\n{e.stderr}"
