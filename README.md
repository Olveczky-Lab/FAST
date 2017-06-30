# FAST (Fast Automated Spike Tracker)
----

FAST is a largely unsupervised algorithm for spike-sorting of continuously acquired (24/7) extracellular data recorded on arrays of single or grouped electrodes.

For more details on the algorithm, please refer to our preprint paper.
Dhawale AK, Poddar R, Kopelowitz A, Normand V, Wolff SBE, Ölveckzy BP (2016) Automated long-term recording and analysis of neural activity in behaving animals. *bioRxiv*.  [https://doi.org/10.1101/033266](https://doi.org/10.1101/033266).

We are currently developing a user-interface that would make FAST easier to run on a variety of data-sets and platforms, but for now we provide the source code and executables as is. For any details not covered in this documentation, please [contact us](https://olveczkylab.oeb.harvard.edu/about).

## Computer requirements

The FAST algorithm is deployed on a scalable master-worker architecture. 

You will need to set up the following machine(s).

**Workstation**
A workstation is required to generate the properly formatted snippeting and clustering commands to be run on the distributed computing platform. 
- Windows 7 (64 bit) or later.
- F# development environment with ability to run F# scripts (.fsx) interactively. 
	Examples include...
	- Visual Studio 2013 or later.
	- [Visual Studio Code](https://code.visualstudio.com/) with [Ionide](http://ionide.io/) plugin and [Visual F# Tools](https://www.microsoft.com/en-us/download/details.aspx?id=48179).
	
**Master and worker node(s)**
- Windows 7 (64 bit) or later.
- At least 8 GB RAM.

Note that master and worker nodes are not required to be distinct machines - they could instead be different processes running in the same Windows environment (even on the workstation computer).	

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
	- Set up Master node.
		- Update the following powershell scripts (*.ps1) in the *MasterNode* Release folder with the list of worker-nodes, their port numbers, and login (username + pwd) details -  **StartSnippeting.ps1**, **StartSnippetingAMP.ps1**, **StartTTL.ps1**, **StartClustering.ps1**, **StopClustering.ps1**, **StartTerminating.ps1**, **StartLevel2.ps1**, **StartAutoSorting.ps1**. Also, if necessary, update the number of snippeting/clustering processes to run on each worker node - a good rule of thumb is 4 (default) for every 8 GB of RAM.
		- Copy **SnippetAll.ps1**, **SnippetAllAmp.ps1**, **StartSnippeting.ps1**, **StartSnippetingAMP.ps1**, **StartTTL.ps1** to the _C:\Titanic\Snippeter_ folder on the designated Master node.
		- Copy **StartClustering.ps1**, **StopClustering.ps1**, **StartTerminating.ps1**, **StartLevel2.ps1**, **StartAutoSorting.ps1** to the _C:\Titanic\Clusterer_ folder on the designated Master node.
	

- Install REST service (blobserver) on your data server. 
	In our current implementation of the FAST algorithm, the worker nodes send web requests to send and receive data from the data server. To allow this communication you will have to install a simple RESTful web service that we call a blobserver on your data server. 
	- If your data server runs Windows 7 (x64) or above, you can make a local copy of the **blobserver.exe** file, then open a command prompt terminal and start the blobserver by typing the following command. 
	`blobserver.exe 8001` 
	or 
	`blobserver.exe 8001 >nul` to suppress program output.
	Here `8001` represents an open TCP port - you could use any available TCP port.
	- If your data server runs a different OS, then you will need to build the included **blobserver.go** source code with the go compiler before running the executable.
	
- Modify paths in **StartSnippeting.fsx** and **StartClustering.fsx** to point to the IP address and location of data on the data server (see comments in scripts for more details).

## Recording file format

FAST is currently limited to processing data files recorded on 64 channel electrode arrays. If your data is recorded on fewer channels you will need to pad it with zeros up to 64 channels. 

- [RHD file format](https://github.com/Olveczky-Lab/FAST/blob/master/RHDFormat.txt): This is similar to IntanTech's native [RHD2000 format](http://intantech.com/files/Intan_RHD2000_data_file_formats.pdf), and includes data recorded from auxillary channels on the RHD2000 Intan chips, chip supply voltages and FPGA board TTL inputs, in addition to the voltage recordings from the electrode array.

- [AMP file format](https://github.com/Olveczky-Lab/FAST/blob/master/AMPFormat.txt): This contains only the voltage recordings. 

We have provided a Matlab function **[convertToAMP.m](https://github.com/Olveczky-Lab/FAST/Utilities/convertToAMP.m)** that you can use to convert your data files to our AMP file format.


## Snippeting

You will need to run **StartSnippeting.fsx** in an F# interactive window within your preferred development environment. If using Visual Studio, you should enable running F# interactive as a 64-bit process in *Options > F# Tools*.

- Update paths and addresses in **StartSnippeting.fsx**.

- Specify the grouping of channels in your electrode array. We have included options for single electrodes and tetrodes (channels grouped by default as `[0,1,2,3], [4,5,6,7], ... , [60,61,62,63]`), but you can also specify your own custom grouping by editing the .fsx script.

- Specify whether the files are in RHD or AMP format, in the .fsx script.

- Specify the grouping of channels for median subtraction. By default there are two groups comprising channels `[0..31]` and `[32..63]` which correspond to the chip grouping of channels on our custom 2X RHD2132 Intan headstage.

- List the files you want to snippet, either in the .fsx script or in an external text file (see **snippeting_list_example.txt**). For each file, you can specify an optional list of channels to exclude as well as the number of frames to snippet (optional, in case the end of the recording is corrupted).

- Now run the **StartSnippeting.fsx** script in F# interactive. Then enter the command `init all();;`. This will do the following...
	- Create the required directory structure for each recording file.
	- Create a *SnippeterSettings.xml* definition for each recording that contains all information about the file to be snippeted. You can modify specific attributes in this XML file if you would like to change particular snippeting parameters.
	- Generate a list of snippeting 'in' and 'out' paths in the F# interactive window. 
	
-  Copy and paste the snippeting 'inpaths' and 'outpaths' from the F# interactive window on your workstation to either **SnippetAll.ps1** or **SnippetAllAMP.ps1** scripts on the Master node (depending on whether you are snippeting RHD or AMP files).

- To launch the snippeting process on the Worker nodes, open a powershell window on the Master node and run the command 
`C:\Titanic\Snippeter\SnippetAll.ps1 IPaddress:port` 
or 
`C:\Titanic\Snippeter\SnippetAllAMP.ps1 IPaddress:port`
where the argument "IPaddress:port" corresponds to the address of the RESTful service (blobserver) that is running on your data server (e.g. `C:\Titanic\Snippeter\SnippetAll.ps1 192.168.0.1:8001`). 

The snippeting process should generate two files for each channel grouping.
- A SpikeTimes file with a list of sample numbers for detected events at *uint64* precision.
- A Spikes file with the waveforms of the detected events in *int16* precision. Each event waveform comprises *nchannels X 64 samples* 16-bit words arranged in the order `[Ch0-Sample0, Ch1-Sample0, Ch2-Sample0, Ch3-Sample0, Ch0-Sample1, etc.]`. To convert to units of voltage, change type to double precision and multiply by 1.95e-7.


## Clustering

- Update paths and addresses in **StartClustering.fsx**.

- Specify 'rat' name - this is just the name of the folder containing the collection of data files that you would like to cluster.

- Add a list of file names you would like to cluster. Determine whether you would like to cluster all channel groups specified in *SnippeterSettings.xml* or a select set of channel groups by commenting/uncommenting appropriate sections of the .fsx script.

- Uncomment and run the code sections at the bottom of the .fsx script (Steps 1-9) one by one in the F# interactive window. Steps 2-8 will generate a list of clustering commands which have to be copied and pasted into a powershell window on the Master node. Begin each step only once the previous step has finished running.

If run successfully, the clustering steps should generate an ensemble of *MergedChains* within the ChGroup folders of every data file. These merged chains are the putative clusters generated by the fully automated steps 1 and 2 of FAST. For each merged chain cluster, the *\*.stimes* and *\*.snums* files contain the spike times and event numbers (*uint64* precision) in the parent *Spikes* or *SpikeTimes* files for that channel group.


## Manual inspection
To inspect and peform manual corrections of these clusters, we recommend using our [ChainViewer GUI](https://github.com/Olveczky-Lab/FAST-ChainViewer) written in Matlab.


