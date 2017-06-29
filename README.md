# FAST (Fast Automated Spike Tracker)
----

FAST is an automated, unsupervised algorithm for spike-sorting of continuously acquired (24/7) extracellular data recorded on arrays of single or grouped electrodes.

For more details on the algorithm, please refer to our preprint paper.
Dhawale AK, Poddar R, Kopelowitz A, Normand V, Wolff SBE, Ölveckzy BP (2016) Automated long-term recording and analysis of neural activity in behaving animals. *bioRxiv*.  [https://doi.org/10.1101/033266](https://doi.org/10.1101/033266).

We are currently developing a user-interface that would make FAST easier to run on a variety of data-sets and platforms, but for now we provide the source code and executables as is. For any details not covered in this documentation, please [contact us](https://olveczkylab.oeb.harvard.edu/about).

## Computer requirements

The FAST algorithm is deployed on a scalable master-worker architecture. 

You will need to set up the following machine(s).

**Workstation**
A workstation is required to generate the properly formatted snippeting and clustering commands to be run on the distributed computing platform. 
You will need...
- F# development environment with ability to run F# scripts (.fsx) interactively. 
	Examples include...
	- Visual Studio 2013 or later.
	- Visual Code with Ionide plugin.
	
**Master and worker node(s)**
- Windows 7 (64 bit) or above
- At least 8 GB RAM

Note that master and worker nodes are not required to be distinct machines - they could instead be different processes running in the same Windows environment (even, for example, on the workstation computer).	

## Installation

- Download the latest [release](https://github.com/Olveczky-Lab/FAST/tree/master/Releases) from the FAST github repository on your workstation PC.

- Set up master and worker nodes.
	- Install [.NET Framework 4.5](https://www.microsoft.com/en-us/download/details.aspx?id=30653) on all nodes.
	- Deploy FAST on master and worker nodes.
		- Identify an unused port number on the worker nodes that will be used for communication with the master node (for example, port 8000).
		- Modify the following parameters in the **DeployFAST.ps1** script
			- *ReleasePath*
			- list of *computerNames* (can be names or IP addresses)
			- *port*
			- *username* and *password* for all nodes.
		- If required, update the port number in the *WorkerNodes/ProcessRunnerService/**ProcessRunnerService.exe.config*** file.
		- Run **DeployFAST.ps1** in a powershell window on any PC with access to the nodes.
	- Copy the following powershell scripts from the *MasterNode* folder to the appropriate directories on the master node.
		- **SnippetAll.ps1**, **SnippetAllAmp.ps1**, **StartTTL.ps1** to the _C:\Titanic\Snippeter_ folder.
		- **StartClustering.ps1**, **StopClustering.ps1**, **StartTerminating.ps1**, **StartLevel2.ps1**, **StartAutoSorting.ps1** to the _C:\Titanic\Clusterer_ folder.
	

- Install REST service (blobserver) on your data server. 
	In our current implementation of the FAST algorithm, the worker nodes send web requests to send and receive data from the data server. To allow this communication you will have to install a simple RESTful web service that we call a blobserver on your data server. 
	- If your data server runs Windows 7 (x64) or above, you can make a local copy of the **blobserver.exe** file, then open a command prompt terminal and start the blobserver by typing the following command. 
	`blobserver.exe 8001` 
	or 
	`blobserver.exe 8001 >nul` to suppress program output.
	Here `8001` represents an open TCP port - you could use any available TCP port.
	- If your data server runs a different OS, then you will need to build the included **blobserver.go** source code with the go compiler before running the executable.
	
- Modify paths in **StartSnippeting.fsx** and **StartClustering.fsx** to point to the IP address and location of data on the data server (see comments in scripts for more details).

## Data file format

FAST is currently limited to processing data files recorded on 64 channel electrode arrays. If your data is recorded on fewer channels you will need to pad it with zeros up to 64 channels. We have provided a utility that can help with this.

- [RHD file format](https://github.com/Olveczky-Lab/FAST/blob/master/RHDFormat.txt): This is similar to IntanTech's native [RHD2000 format](http://intantech.com/files/Intan_RHD2000_data_file_formats.pdf), and includes data recorded from auxillary channels on the RHD2000 Intan chips, chip supply voltages and FPGA board TTL inputs, in addition to the voltage recordings from the electrode array.

- [AMP file format](https://github.com/Olveczky-Lab/FAST/blob/master/AMPFormat.txt): This contains only the voltage recordings.


## Snippeting

Follow these steps to snippet your data.

## Clustering

