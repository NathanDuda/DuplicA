
import tkinter as tk
from tkinter import filedialog, messagebox
import subprocess

# Function to run the selected R script with multiple parameters
def run_r_script(script_path, input_file, param1, param2, param3):
    try:
        # Assuming the R script can accept additional parameters
        subprocess.run(["Rscript", script_path, input_file, param1, param2, param3], check=True)
        messagebox.showinfo("Success", "Model run completed successfully!")
    except subprocess.CalledProcessError as e:
        messagebox.showerror("Error", f"An error occurred:\n{e}")

# Function to handle file selection for each model
def select_file(var):
    file_path = filedialog.askopenfilename()
    if file_path:
        var.set(file_path)

# Function to switch between model pages
def show_model_page(model_name):
    for page in model_pages.values():
        page.pack_forget()
    model_pages[model_name].pack(fill=tk.BOTH, expand=True)

# Function to run Model 1 with multiple parameters
def run_model_1():
    script_path = model_scripts["Model 1"]
    input_file = selected_files["Model 1"].get()
    param1 = param1_var.get()
    param2 = param2_var.get()
    param3 = param3_var.get()
    if script_path and input_file:
        run_r_script(script_path, input_file, param1, param2, param3)
    else:
        messagebox.showerror("Error", "Input file or parameters not selected.")

# Initialize the main window
root = tk.Tk()
root.title("Data Analysis App")

# Model script paths
model_scripts = {
    "Model 1": "/path/to/model1.R",
    "Model 2": "/path/to/model2.R",
    "Model 3": "/path/to/model3.R"
}

# Model descriptions
model_descriptions = {
    "Model 1": "Description for Model 1: This model performs XYZ analysis on the provided dataset.",
    "Model 2": "Description for Model 2: This model focuses on ABC processing for better insights.",
    "Model 3": "Description for Model 3: This model handles DEF operations for data refinement."
}

# Variables to store selected file paths for each model
selected_files = {
    "Model 1": tk.StringVar(),
    "Model 2": tk.StringVar(),
    "Model 3": tk.StringVar()
}

# Additional parameters for Model 1
param1_var = tk.StringVar()
param2_var = tk.StringVar()
param3_var = tk.StringVar()

# Create a frame for the side pane
side_pane = tk.Frame(root)
side_pane.pack(side=tk.LEFT, fill=tk.Y, padx=10, pady=10)

# Listbox to display available models
tk.Label(side_pane, text="Select Model:").pack(anchor="w")
model_listbox = tk.Listbox(side_pane)
for model in model_scripts:
    model_listbox.insert(tk.END, model)
model_listbox.pack(fill=tk.BOTH, expand=True, pady=10)

# Function to handle model selection from listbox
def on_model_select(event):
    selected_model = model_listbox.get(model_listbox.curselection())
    show_model_page(selected_model)

model_listbox.bind("<<ListboxSelect>>", on_model_select)

# Create a container frame for model pages
content_frame = tk.Frame(root)
content_frame.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True, padx=10, pady=10)

# Dictionary to hold model pages
model_pages = {}

# Create a page for Model 1 with multiple input options
page_frame_1 = tk.Frame(content_frame)

tk.Label(page_frame_1, text=f"File selection for Model 1:").pack(pady=5)
tk.Button(page_frame_1, text="Browse", command=lambda var=selected_files["Model 1"]: select_file(var)).pack(pady=5)
tk.Label(page_frame_1, textvariable=selected_files["Model 1"], text="No file selected").pack(pady=5)

tk.Label(page_frame_1, text=model_descriptions["Model 1"], wraplength=400, justify="left").pack(pady=10)

# Additional parameters for Model 1
tk.Label(page_frame_1, text="Parameter 1 (Numeric):").pack(pady=5)
tk.Entry(page_frame_1, textvariable=param1_var).pack(pady=5)

tk.Label(page_frame_1, text="Parameter 2 (Option):").pack(pady=5)
options = ["Option A", "Option B", "Option C"]
param2_var.set(options[0])
tk.OptionMenu(page_frame_1, param2_var, *options).pack(pady=5)

tk.Label(page_frame_1, text="Parameter 3 (Boolean):").pack(pady=5)
tk.Checkbutton(page_frame_1, text="Enable", variable=param3_var, onvalue="True", offvalue="False").pack(pady=5)

tk.Button(page_frame_1, text="Run Model", command=run_model_1).pack(pady=20)

model_pages["Model 1"] = page_frame_1

# Create pages for other models without additional parameters
for model_name in ["Model 2", "Model 3"]:
    page_frame = tk.Frame(content_frame)
    
    tk.Label(page_frame, text=f"File selection for {model_name}:").pack(pady=5)
    tk.Button(page_frame, text="Browse", command=lambda var=selected_files[model_name]: select_file(var)).pack(pady=5)
    tk.Label(page_frame, textvariable=selected_files[model_name], text="No file selected").pack(pady=5)
    
    tk.Label(page_frame, text=model_descriptions[model_name], wraplength=400, justify="left").pack(pady=10)
    
    tk.Button(page_frame, text="Run Model", command=lambda name=model_name: run_selected_model(name)).pack(pady=20)
    
    model_pages[model_name] = page_frame

# Start with the first model page visible
show_model_page("Model 1")

# Run the main loop
root.mainloop()
