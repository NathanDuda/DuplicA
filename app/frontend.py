import customtkinter as ctk
from tkinter import filedialog
def create_CDROM_page(parent_frame, description, param_vars, run_command):
    page_frame = ctk.CTkFrame(parent_frame)
    
    # OF_input Frame
    OF_input_frame = ctk.CTkFrame(page_frame)
    OF_input_frame.grid(row=0, column=0, sticky="nsew")
    
    # swtich between frames
    ctk.CTkButton(OF_input_frame, text="Use custom input", command=lambda: show_page('custom_input_page')).grid(row=0, column=0, columnspan=2, pady=20)
    
    # title and description
    ctk.CTkLabel(OF_input_frame, text="CDROM", font=('', 20)).grid(row=0, column=1, columnspan=3, pady=5)
    ctk.CTkLabel(OF_input_frame, text=description).grid(row=1, column=1, columnspan=3, pady=10)
    
    # input file and folder
    ctk.CTkButton(OF_input_frame, text="Browse: Expression File", command=lambda: param_vars['expression_file_var'].set(select_file('Expression Data'))).grid(row=2, column=1, pady=5, padx=10, sticky="w")
    ctk.CTkButton(OF_input_frame, text="Browse: OrthoFinder Folder", command=lambda: param_vars['ortho_dir_var'].set(select_directory('OrthoFinder Output Directory'))).grid(row=3, column=1, pady=5, padx=10, sticky="w")
    
    # run button
    ctk.CTkButton(OF_input_frame, text="Run Model", font = ('', 18), width = 130, height = 50, fg_color = ('green'), hover_color=('darkgreen'), command=run_command(run_type = 'OF')).grid(row=4, column=1, columnspan=2, pady=20)

    # Additional parameters:
    ctk.CTkLabel(OF_input_frame, text='').grid(row=5, column=1, columnspan=3, pady=30)
    ctk.CTkLabel(OF_input_frame, text='Additional options', font = ('', 10)).grid(row=6, column=0, columnspan=3, pady=5, padx = 3, sticky = 'w')

    # numeric
    ctk.CTkLabel(OF_input_frame, text="min dups per species pair:").grid(row=7, column=0, pady=5, padx=10, sticky="w")
    ctk.CTkEntry(OF_input_frame, textvariable=param_vars['min_dups_per_species_pair_var']).grid(row=7, column=1, pady=5, padx=10, sticky="w") 
    
    ctk.CTkLabel(OF_input_frame, text="Set expression values lower than _ to 0:").grid(row=8, column=0, pady=5, padx=10, sticky="w")
    ctk.CTkEntry(OF_input_frame, textvariable=param_vars['rm_exp_lower_than_var']).grid(row=8, column=1, pady=5, padx=10, sticky="w")
    
    
    # logical 
    ctk.CTkLabel(OF_input_frame, text="Add pseudofunctionalization?").grid(row=9, column=0, pady=5, padx=10, sticky="w")
    ctk.CTkCheckBox(OF_input_frame, text='', variable=param_vars['add_pseudofunc_var'], onvalue="True", offvalue="False").grid(row=9, column=1, pady=5, padx=10, sticky="w")
    
    ctk.CTkLabel(OF_input_frame, text="Should genes with missing expression data be considered pseudofunctionalized?").grid(row=10, column=0, pady=5, padx=10, sticky="w")
    ctk.CTkCheckBox(OF_input_frame, text='', variable=param_vars['missing_expr_is_pseudo_var'], onvalue="True", offvalue="False").grid(row=10, column=1, pady=5, padx=10, sticky="w")
    
    
    

    
    # custom_input_page Frame
    custom_input_frame = ctk.CTkFrame(page_frame)
    custom_input_frame.grid(row=0, column=0, sticky="nsew")
    
    # switch between frames
    ctk.CTkButton(custom_input_frame, text="Use OrthoFinder as input", command=lambda: show_page('OF_input_page')).grid(row=0, column=0, columnspan=2, pady=20)

    # title and description
    ctk.CTkLabel(custom_input_frame, text="CDROM", font=('', 20)).grid(row=0, column=1, columnspan=3, pady=5)
    ctk.CTkLabel(custom_input_frame, text=description).grid(row=1, column=1, columnspan=3, pady=10)
    
    # input file and folder
    ctk.CTkButton(custom_input_frame, text="Browse: Duplicate Genes file", command=lambda: param_vars['dups_file_var'].set(select_file('Expression Data'))).grid(row=2, column=1, pady=5, padx=10, sticky="w")
    ctk.CTkButton(custom_input_frame, text="Browse: Expression Folder", command=lambda: param_vars['exp_dir_var'].set(select_directory('OrthoFinder Output Directory'))).grid(row=3, column=1, pady=5, padx=10, sticky="w")
    
    # run button
    ctk.CTkButton(custom_input_frame, text="Run Model", font = ('', 18), width = 130, height = 50, fg_color = ('green'), hover_color=('darkgreen'), command=run_command(run_type = 'custom')).grid(row=4, column=1, columnspan=2, pady=20)
    
    # Additional parameters:
    ctk.CTkLabel(custom_input_frame, text='').grid(row=5, column=1, columnspan=3, pady=30)
    ctk.CTkLabel(custom_input_frame, text='Additional options', font = ('', 10)).grid(row=6, column=0, columnspan=3, pady=5, padx = 3, sticky = 'w')

    # numeric 
    ctk.CTkLabel(custom_input_frame, text="min dups per species pair:").grid(row=7, column=0, pady=5, padx=10, sticky="w")
    ctk.CTkEntry(custom_input_frame, textvariable=param_vars['min_dups_per_species_pair_var']).grid(row=7, column=1, pady=5, padx=10, sticky="w") 
    
    ctk.CTkLabel(custom_input_frame, text="Set expression values lower than _ to 0:").grid(row=8, column=0, pady=5, padx=10, sticky="w")
    ctk.CTkEntry(custom_input_frame, textvariable=param_vars['rm_exp_lower_than_var']).grid(row=8, column=1, pady=5, padx=10, sticky="w")
    
    # logical 
    ctk.CTkLabel(custom_input_frame, text="Are parent/child copies differentiated?").grid(row=9, column=0, pady=5, padx=10, sticky="w")
    ctk.CTkCheckBox(custom_input_frame, text='', variable=param_vars['PC_var'], onvalue="True", offvalue="False").grid(row=9, column=1, pady=5, padx=10, sticky="w")
    
    ctk.CTkLabel(custom_input_frame, text="Add pseudofunctionalization?").grid(row=10, column=0, pady=5, padx=10, sticky="w")
    ctk.CTkCheckBox(custom_input_frame, text='', variable=param_vars['add_pseudofunc_var'], onvalue="True", offvalue="False").grid(row=10, column=1, pady=5, padx=10, sticky="w")
    
    ctk.CTkLabel(custom_input_frame, text="Should genes with missing expression data be considered pseudofunctionalized?").grid(row=11, column=0, pady=5, padx=10, sticky="w")
    ctk.CTkCheckBox(custom_input_frame, text='', variable=param_vars['missing_expr_is_pseudo_var'], onvalue="True", offvalue="False").grid(row=11, column=1, pady=5, padx=10, sticky="w")
    
    
    # Page toggling function
    def show_page(page):
        if page == 'custom_input_page':
            custom_input_frame.grid(row=0, column=0, sticky="nsew")
            OF_input_frame.grid_forget()
        elif page == 'OF_input_page':
            OF_input_frame.grid(row=0, column=0, sticky="nsew")
            custom_input_frame.grid_forget()
    
    # Show OF_input_page by default
    show_page('OF_input_page')
    
    return page_frame

def select_file(param_name):
    file_path = filedialog.askopenfilename(title=f"Select {param_name}")
    return file_path

def select_directory(param_name):
    directory_path = filedialog.askdirectory(title=f"Select {param_name}")
    return directory_path
