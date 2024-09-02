import customtkinter as ctk
from tkinter import messagebox
from backend import run_r_script
from frontend import create_CDROM_page

def run_CDROM(run_type):
    script_name = "model_CDROM.R"
    
    # OrthoFinder run_type 
    expression_file = expression_file_var.get()
    ortho_dir = ortho_dir_var.get()
    
    # custom run_type
    dups_file = dups_file_var.get()
    exp_dir = exp_dir_var.get()
    PC = PC_var.get()
    
    # other parameters 
    add_pseudofunc = add_pseudofunc_var.get()
    missing_expr_is_pseudo = missing_expr_is_pseudo_var.get()
    rm_exp_lower_than = rm_exp_lower_than_var.get()
    min_dups_per_species_pair = min_dups_per_species_pair_var.get()
    
    result = run_r_script(run_type, script_name, expression_file, ortho_dir, dups_file, exp_dir, add_pseudofunc, missing_expr_is_pseudo, rm_exp_lower_than, PC, min_dups_per_species_pair)

    
    messagebox.showinfo("Result", result)
  
def run_model2():
    script_name = "model2.R"
    
    expression_file = expression_file_var.get()
    
    if expression_file:
        result = run_r_script(script_name, expression_file)
        messagebox.showinfo("Result", result)
    else:
        messagebox.showerror("Error", "Expression file not selected.")

def run_model3():
    script_name = "model3.R"
    
    expression_file = expression_file_var.get()
    
    if expression_file:
        result = run_r_script(script_name, expression_file)
        messagebox.showinfo("Result", result)
    else:
        messagebox.showerror("Error", "Expression file not selected.")

# Initialize the main window
root = ctk.CTk()
root.title("DuplicA")

# Variables for model CDROM
expression_file_var = ctk.StringVar()
ortho_dir_var = ctk.StringVar()
dups_file_var = ctk.StringVar()
exp_dir_var = ctk.StringVar()
add_pseudofunc_var = ctk.BooleanVar(value=False)
missing_expr_is_pseudo_var = ctk.BooleanVar(value=False)
rm_exp_lower_than_var = ctk.StringVar(value=1)
PC_var = ctk.BooleanVar(value=False)
min_dups_per_species_pair_var = ctk.StringVar(value=10)

# Side pane with model selection
side_pane = ctk.CTkFrame(root)
side_pane.pack(side=ctk.LEFT, fill=ctk.Y, padx=10, pady=10)

ctk.CTkLabel(side_pane, text="Select Model:").pack(anchor="w")

ctk.CTkButton(side_pane, text="CDROM", command=lambda: show_model_page("CDROM")).pack(fill=ctk.BOTH, expand=True)
ctk.CTkButton(side_pane, text="Model 2", command=lambda: show_model_page("Model 2")).pack(fill=ctk.BOTH, expand=True)
ctk.CTkButton(side_pane, text="Model 3", command=lambda: show_model_page("Model 3")).pack(fill=ctk.BOTH, expand=True)

# Content frame for displaying model pages
content_frame = ctk.CTkFrame(root)
content_frame.pack(side=ctk.RIGHT, fill=ctk.BOTH, expand=True, padx=10, pady=10)

# Model pages
def show_model_page(model_name):
    for page in model_pages.values():
        page.pack_forget()
    model_pages[model_name].pack(fill=ctk.BOTH, expand=True)

model_pages = {
    "CDROM": create_CDROM_page(
        content_frame,
        "Description: Inferring mechanisms of duplicate gene preservation using asymmetry of gene expression divergence.",
        {
            "expression_file_var": expression_file_var,
            "ortho_dir_var": ortho_dir_var,
            "dups_file_var": dups_file_var,
            "exp_dir_var": exp_dir_var,
            "add_pseudofunc_var": add_pseudofunc_var,
            "missing_expr_is_pseudo_var": missing_expr_is_pseudo_var,
            "rm_exp_lower_than_var": rm_exp_lower_than_var,
            "PC_var": PC_var,
            "min_dups_per_species_pair_var": min_dups_per_species_pair_var
        },
        run_CDROM
    )
}

# Show the first model page by default
show_model_page("CDROM")

root.mainloop()








