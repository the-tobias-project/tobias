# Tobias
Tobias (_Tests of bias_) is a suite of exploratory statistical tests for detecting and untangling the sources of bias that can influence genetic test interpretation. 

Our initial release (version 1) focuses on the marker of genetic ancestry. Through a variety of hypotheses and models, we ask whether this marker - when ignored - can confound the clinical interpretation of a genetic lesion observed in a patient. Asking questions like these is made possible by data that is painstakingly aggregated and freely published by two resources: ClinVar and ExAC. 

ClinVar solicits, curates and disseminates the clinical interpretations assigned to genetic variants discovered by hundreds of clinical test providers. ExAC collects, curates and disseminates petabytes of data from large population genomic sequencing projects, allowing us to precisely estimate the allele frequency of variants in different human populations. Most clinical laboratory processes as well as variant classification guidelines rely on these two resources (albeit to varying extents) in making their determination about the effect of a variant.

#### GUI:
Tobias comes with a web-based graphical user interface to help clinicians and other non-technical researchers quickly visualize and explore a variety of hypotheses (under development by Arturo Lopez Pineda <arturolp@stanford.edu>).

#### Funding acknowledgements:
Tobias was conceived and developed thanks to support from  
1. The UCSF/Stanford CERSI grant, awarded by the FDA (U01 FD004979)
2. The Stanford/Baylor Clinical Genome Resource grant, awarded by the NHGRI (U01 HG007436-04)


#### Installation (for development):
Install "direnv" first. Then run "make install" within this folder.


#### Connection to Databricks:
This are the commands I run locally in the tobias folder. I am using archlinux, so this should be changed
for other distributions (eg, Ubuntu)


-  install pyenv and virtualenv-pyenv

pip install pyenv virtualenv-pyenv

pyenv install 3.10.6
pyenv virtualenv 3.10.6 tobias
pyenvl local tobias

-  Set linux environment, distribution dependent (example for Archlinux here)

unset SPARK_HOME
sudo pacman -S jdk8-openjdk
sudo pacman -S jre8-openjdk
sudo archlinux-java set java-8-openjdk # set java8 as default java, with other versions didn't work

Just set as default java 8 in Ubuntu and you are good to go

- Configure python environment

pip uninstall pyspark # be sure that pyspark is not in the current environment
pip install databricks-connect
databricks-connect configure # use the url of databricks workspace to extract the following elements:

databricks host: https://adb-587466035000722.2.azuredatabricks.net/
Databricks Token: <GENERATE A PERSONAL ACCESS TOKER HERE IN DATABRICKS SETTINGS>
Cluster ID: 1025-235731-iplxjnlf
Org ID: 587466035000722
PORT: 15001

- In .Renviron, set SPARK_HOME, eg:
SPARK_HOME=/home/leandro/.pyenv/versions/tobias/lib/python3.10/site-packages/pyspark





