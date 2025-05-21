<a id="readme-top"></a>
<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/NathanDuda/DuplicA">
    <img src="app/Static/Images/DuplicA_logo.png" alt="Logo">
  </a>
    <h3 align="center">A phylogenomic toolkit for duplicate gene analysis.</h3>
</div>


# DuplicA `README`

## About

DuplicA is an interactive web-based platform for running models of gene duplication, expression, selection, and function across multiple species. It integrates genomic datasets, public databases, and statistical modeling into a seamless analysis and visualization workflow.

&nbsp;
## Requirements

- Docker (install from: https://docs.docker.com/engine/install/)

&nbsp;
## Installation

**1. Clone the repository:**

```sh
git clone https://github.com/NathanDuda/DuplicA.git
```

**2. Build the Docker image:**

```sh
cd DuplicA
sudo docker build -t duplic-a .
```

This may take 10-15 minutes the first time you use the app. 

**3. Run the app:**

```sh
./run_duplica
```

**5. Open the app**

Now that the app is running locally, go to http://localhost:8000 


&nbsp;
## Using your own data

To analyze your own data with the app, place all files in a single folder and start the app by specifying the path to that folder.

```sh
./run_duplica /path/to/your/data_folder
```

Change `/path/to/your/data_folder` to the actual path to your data. 



&nbsp;
## Tutorial

### Home Page
**Browse and filter available models**<br>
<img src="app/Static/Tutorial/Home_Demo_Filter.gif" width="60%" height="60%">

**View information about a selected model**<br>
<img src="app/Static/Tutorial/Home_Demo_Model_Page.gif" width="60%" height="60%">

---

### Run a Workflow 
**Select models to include in your workflow**<br>
<img src="app/Static/Tutorial/Workflow_Demo_Model_Selection.gif" width="60%" height="60%">

**Exclude models and required inputs update automatically**<br>
<img src="app/Static/Tutorial/Workflow_Demo_Model_Deselection.gif" width="60%" height="60%">

**Upload your data**<br>
<img src="app/Static/Tutorial/Workflow_Demo_Input.gif" width="60%" height="60%">

**Execute the workflow**<br>
<img src="app/Static/Tutorial/Run_Workflow_Demo.gif" width="60%" height="60%">

**Monitor progress and status updates**<br>
<img src="app/Static/Tutorial/Workflow_Progress.gif" width="60%" height="60%">

---

### Analyze Workflow Results
**Easily generate custom figures from your results**<br>
<img src="app/Static/Tutorial/Example_Analysis_Boxplot.png" width="60%" height="60%">

**Choose from multiple figure types**<br>
<img src="app/Static/Tutorial/Example_Analysis_Scatterplot.png" width="60%" height="60%">



&nbsp;
## Troubleshooting

### **Error: `Cannot connect to the Docker daemon at unix:///var/run/docker.sock`**

**Solution:**

Run docker daemon:

```sh
sudo service docker start 
```


### **Error: `permission denied while trying to connect to the Docker daemon`**

**Solution:**

Run the app as super user:

```sh
sudo ./run_duplica
```

**Or:**

Add yourself to the Docker group:

```sh
sudo usermod -aG docker $USER
new_group docker
```

&nbsp;
## Contact

**Nathan Duda**
- GitHub: [@NathanDuda](https://github.com/NathanDuda)
- Email: nathanduda02@gmail.com


