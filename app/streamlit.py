
import subprocess
import os

def get_script_path(script_name):
    app_dir = os.path.dirname(os.path.abspath(__file__))
    script_path = os.path.join(app_dir, "Scripts", script_name)
    return script_path

def run_r_script(run_type, script_name, expression_file, directory_path=None, dups_file=None, exp_dir=None, add_pseudofunc=False, missing_expr_is_pseudo=False, rm_exp_lower_than=None, PC=False, min_dups_per_species_pair=None, use_absolute_exp=False):
    print(f"Running script: {script_name}")
    print(f"Expression file: {expression_file}")
    print(f"Directory path: {directory_path}")
    print(f"Dups file: {dups_file}")
    print(f"Exp dir: {exp_dir}")

    R_path = "C:\\Program Files\\R\\R-4.4.1\\bin\\Rscript.exe"
    try:
        script_path = get_script_path(script_name)
        command = [R_path, script_path, expression_file]

        if run_type == 'OF':
            command += [directory_path, str(add_pseudofunc), str(missing_expr_is_pseudo), rm_exp_lower_than, str(PC), str(min_dups_per_species_pair), str(use_absolute_exp)]
        elif run_type == 'custom':
            command += [dups_file, exp_dir, str(add_pseudofunc), str(missing_expr_is_pseudo), rm_exp_lower_than, str(PC), str(min_dups_per_species_pair), str(use_absolute_exp)]

        print('COMMAND:', command)
        result = subprocess.run(command, check=True, text=True, capture_output=True)
        return f"Model run completed successfully!\nOutput:\n{result.stdout}"
    except subprocess.CalledProcessError as e:
        return f"An error occurred:\n{e.stderr}"

import streamlit as st

# Sidebar for selecting the model
st.sidebar.title("Select Model")
model = st.sidebar.radio("Model:", ["CDROM", "Model 2", "Model 3"])

# Shared variables for all models
expression_file = ""
ortho_dir = ""
dups_file = ""
exp_dir = ""
add_pseudofunc = False
missing_expr_is_pseudo = False
exp_cutoff = 1
PC = False
min_dups_per_species_pair = 10
use_absolute_exp = False

# Function to run CDROM model
def run_CDROM(run_type, script_name, expression_file, ortho_dir, dups_file, exp_dir, add_pseudofunc, missing_expr_is_pseudo, exp_cutoff, PC, min_dups_per_species_pair, use_absolute_exp):
    script_name = "model_CDROM.R"

    print(f'Expression file: {expression_file}')
    print(f'OrthoFinder output directory: {ortho_dir}')
        
    result = run_r_script(
        run_type,
        script_name,
        expression_file,
        directory_path=ortho_dir if run_type == 'OF' else None,
        dups_file=dups_file if run_type == 'custom' else None,
        exp_dir=exp_dir if run_type == 'custom' else None,
        add_pseudofunc=add_pseudofunc,
        missing_expr_is_pseudo=missing_expr_is_pseudo,
        rm_exp_lower_than=exp_cutoff,
        PC=PC,
        min_dups_per_species_pair=min_dups_per_species_pair,
        use_absolute_exp=use_absolute_exp
    )
    
    st.success(f"Result: {result}")

# Function to run Model 2
def run_model2():
    script_name = "model2.R"
    
    if expression_file:
        result = run_r_script('custom', script_name, expression_file)  # Adjusted to use 'custom' for consistency
        st.success(f"Result: {result}")
    else:
        st.error("Expression file not selected.")

# Function to run Model 3
def run_model3():
    script_name = "model3.R"
    
    if expression_file:
        result = run_r_script('custom', script_name, expression_file)  # Adjusted to use 'custom' for consistency
        st.success(f"Result: {result}")
    else:
        st.error("Expression file not selected.")

# CDROM model page
if model == "CDROM":
    st.title("CDROM Model")
    st.markdown("Inferring mechanisms of duplicate gene preservation using asymmetry of gene expression divergence.")

    run_type = st.selectbox("Run Type:", ["OrthoFinder", "Custom"])
    
    if run_type == "OrthoFinder":
        expression_file = st.file_uploader("Expression File:")
        ortho_dir = st.file_uploader("OrthoFinder Output Directory:", accept_multiple_files=True)
    elif run_type == "Custom":
        dups_file = st.file_uploader("Duplicate Genes File:")
        exp_dir = st.file_uploader("Expression Folder:")

    # Additional options
    st.markdown("### Additional Options")
    
    exp_cutoff = st.number_input("Expression cutoff:", value=1)
    min_dups_per_species_pair = st.number_input("Min dups per species pair:", value=10)
    
    add_pseudofunc = st.checkbox("Add pseudofunctionalization?")
    missing_expr_is_pseudo = st.checkbox("Consider missing expression as pseudofunctionalization?")
    use_absolute_exp = st.checkbox("Use absolute expression?")
    
    if run_type == "Custom":
        PC = st.checkbox("Differentiate parent/child copies?")

    # Run button
    if st.button("Run CDROM Model"):
        run_CDROM(
            run_type,
            'model_CDROM.R',
            expression_file,
            ortho_dir,
            dups_file,
            exp_dir,
            add_pseudofunc,
            missing_expr_is_pseudo,
            exp_cutoff,
            PC,
            min_dups_per_species_pair,
            use_absolute_exp
        )

# Model 2 page
elif model == "Model 2":
    st.title("Model 2")
    expression_file = st.text_input("Expression File:")
    
    # Run button
    if st.button("Run Model 2"):
        run_model2()

# Model 3 page
elif model == "Model 3":
    st.title("Model 3")
    expression_file = st.text_input("Expression File:")
    
    # Run button
    if st.button("Run Model 3"):
        run_model3()
