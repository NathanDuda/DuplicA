
import tkinter as tk
from tkinter import messagebox
from backend import run_r_script
from frontend import create_model_page, create_model_listbox

def run_CDROM():
    script_name = "model_CDROM.R"
    
    input_file = input_file_var.get()
    ortho_dir = ortho_dir_var.get()
    
    param1 = param1_var.get()
    param2 = param2_var.get()
    param3 = param3_var.get()
    
    if script_name and input_file and ortho_dir:
        result = run_r_script(script_name, input_file, ortho_dir, param1, param2, param3)
        messagebox.showinfo("Result", result)
    else:
        messagebox.showerror("Error", "Input file or parameters not selected.")



def run_model(model_name):
    script_name = model_scripts.get(model_name)
#    input_file = selected_files[model_name].get("file")

    if script_name and input_file_var:
        result = run_r_script(script_name, input_file_var)
        messagebox.showinfo("Result", result)
    else:
        messagebox.showerror("Error", "Input file or script not selected.")

def show_model_page(model_name):
    for page in model_pages.values():
        page.pack_forget()
    model_pages[model_name].pack(fill=tk.BOTH, expand=True)

def on_model_select(event):
    selected_model = model_listbox.get(model_listbox.curselection())
    show_model_page(selected_model)

# Initialize the main window
root = tk.Tk()
root.geometry('800x500')
root.title("DuplicA")

# Model script paths
model_scripts = {
    "CDROM": "model_CDROM.R",
    "Model 2": "model2.R",
    "Model 3": "model3.R"
}

# Model descriptions
model_descriptions = {
    "CDROM": "Description for running CDROM. stuff stuff stuff and stuff.",
    "Model 2": "Description for Model 2: This model focuses on ABC processing for better insights.",
    "Model 3": "Description for Model 3: This model handles DEF operations for data refinement."
}

# Variables to store selected file paths for each model
#selected_files = {
#    "CDROM": {
#        "OrthoFinder Output Directory": tk.StringVar(),
#        "Expression Data": tk.StringVar()
#    },
#    "Model 2": {"file": tk.StringVar()},
#    "Model 3": {"file": tk.StringVar()}
#}

input_file_var = tk.StringVar()
ortho_dir_var = tk.StringVar()

# Additional parameters for the CDROM model
param1_var = tk.BooleanVar(value=False)
param2_var = tk.BooleanVar(value=False)
param3_var = tk.StringVar()

# Create the side pane and model listbox
side_pane = tk.Frame(root)
side_pane.pack(side=tk.LEFT, fill=tk.Y, padx=10, pady=10)

# Create the listbox with model names
model_listbox = create_model_listbox(side_pane, model_scripts.keys(), on_model_select)

# Create the container frame for model pages
content_frame = tk.Frame(root)
content_frame.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True, padx=10, pady=10)

# Create pages for models
model_pages = {}

# Page for CDROM model with additional input options
model_pages["CDROM"] = create_model_page(
    content_frame,
    "CDROM",
    model_descriptions["CDROM"],
    {
        "OrthoFinder Output Directory": ("directory", ortho_dir_var),
        "Expression Data": ("file", input_file_var),
        "add_pseudofunc": ("boolean", param1_var),
        "missing_expr_is_pseudo": ("boolean", param2_var),
        "rm_exp_lower_than": ("numeric", param3_var)
    },
    run_CDROM
)

# Pages for other models
#for model_name in ["Model 2", "Model 3"]:
#    model_pages[model_name] = create_model_page(
#        content_frame,
#        model_name,
#        model_descriptions[model_name],
    #    {"file": ("file", selected_files[model_name]["file"])},
#        lambda name=model_name: run_model(name)
#    )

# Start with the first model page visible
show_model_page("CDROM")

# Run the main loop
root.mainloop()