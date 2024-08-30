

import tkinter as tk
from tkinter import filedialog



def create_model_page(parent_frame, model_name, description, param_vars, run_command):
    page_frame = tk.Frame(parent_frame)
    
    tk.Label(page_frame, text=f"File selection for {model_name}:").pack(pady=5)
    
    # Modify button to select files or directories based on parameter type
    for param_name, (param_type, var) in param_vars.items():
        if param_type == "file":
            tk.Button(page_frame, text=f"Browse {param_name}", command=lambda p=param_name: select_file(var, p)).pack(pady=5)
            tk.Label(page_frame, textvariable=var, text="No file selected").pack(pady=5)
        elif param_type == "directory":
            tk.Button(page_frame, text=f"Browse {param_name}", command=lambda p=param_name: select_directory(var, p)).pack(pady=5)
            tk.Label(page_frame, textvariable=var, text="No directory selected").pack(pady=5)
    
    tk.Label(page_frame, text=description, wraplength=400, justify="left").pack(pady=10)

    # Add parameter inputs
    for param_name, (param_type, options) in param_vars.items():
        if param_type == "numeric":
            tk.Label(page_frame, text=f"{param_name}:").pack(pady=5)
            tk.Entry(page_frame, textvariable=options).pack(pady=5)
        elif param_type == "option":
            tk.Label(page_frame, text=f"{param_name}:").pack(pady=5)
            tk.OptionMenu(page_frame, options[0], *options[1]).pack(pady=5)
        elif param_type == "boolean":
            tk.Label(page_frame, text=f"{param_name}:").pack(pady=5)
            tk.Checkbutton(page_frame, text="Enable", variable=options, onvalue="True", offvalue="False").pack(pady=5)
    
    tk.Button(page_frame, text="Run Model", command=run_command).pack(pady=20)
    
    return page_frame



def create_model_listbox(side_pane, model_names, on_select):
    tk.Label(side_pane, text="Select Model:").pack(anchor="w")
    model_listbox = tk.Listbox(side_pane)
    for model in model_names:
        model_listbox.insert(tk.END, model)
    model_listbox.pack(fill=tk.BOTH, expand=True, pady=10)
    model_listbox.bind("<<ListboxSelect>>", on_select)
    return model_listbox

def select_file(var, param_name):
    file_path = filedialog.askopenfilename(title=f"Select {param_name}")
    if file_path:
        var.set(file_path)
        
def select_directory(var, param_name):
    directory_path = filedialog.askdirectory(title=f"Select {param_name}")
    if directory_path:
        var.set(directory_path)