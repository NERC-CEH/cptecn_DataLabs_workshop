# cptecn_workshop
Repo of the online project workshop for a Constructing a Digital Environment Pilot Study using ECN data 
(NE/T006102/1, Methodologically Enhanced Virtual Labs for Early Warning of Significant or Catastrophic Change in Ecosystems: Changepoints for a Changing Planet)

# Coverage
Youtube:

<iframe width="560" height="315" src="https://www.youtube.com/embed/WDBXpkzLRM8" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

[Blog post (Data Science for the Natural Environment)](https://www.lancaster.ac.uk/data-science-of-the-natural-environment/blogs/methodologically-enhanced-virtual-labs-the-next-crucial-step-to-support-collaborative-environmental-data-science)

# Project participants (UKCEH and Lancaster University)
    Lindsay Banin
    Gordon Blair
    Idris Eckley
    Peter Henrys 
    Michael Hollaway 
    Rebecca Killick
    AaronLowther
    Don Monteith
    Susannah Rennie
    Will Simms
    Michael Tso
    John Watkins 

# Description of the files in the lab

    EDA-ALL-Moth.ipynb: This is the most recent notebook looking at the moth data. We rigorously consider how best to combine the data, highlight any problems/issues with the data and apply the changepoint.msr approach in a preliminary example.
    
    changepoint.msr_0.0.1.tar.gz: version 1 of the changepoint.msr package
    
    changepoint.msr_0.0.2.tar.gz: version 2 of the changepoint.msr package. Improved plotting functionality. Look out for updates on CRAN (https://cran.r-project.org/).
    
    Demo-Perch.ipynb: Applies the changepoint.msr approach to the Windermere Perch abundance and water chemistry data suggested by Pete.
    
    install-changepoint.msr.ipynb: A notebook to store the functions used to install the changepoint.msr package on datalabs.
    
    long-tem-workflow.ipynb: A notebook Michael Tso put together in preparation for the workshop.
    
    moth_sum_species.R:
    
    archive/: A directory containing old notebooks that have been superseded
    
    cptMF/: The (old) python version of the changepoint.msr package. The changepoint.msr approach was initially developed in python. The algorithm protoype was written in python and then converted to C. It was, at the time, much more straightforward to release an R package instead of a python package publicly hence development of this package ceased. 
    
    csv/: Contains the Windermere Perch and water chemistry data.
    
    R/: Contains some (possibly old) R functions. 
    
    resources/: Contains some documentation for the moth data. 
    
    tidy/: Contains a notebook that Michael T created to test the python package. 
    


# License
![image](https://user-images.githubusercontent.com/37928699/158212013-9e86c2cc-8f6a-4228-9968-bbea1efbdb35.png)
CC-BY
