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


#### Installation
Install "direnv" first. Then run the following commands:

```console
apt-get install direnv

pip install pyenv virtualenv-pyenv databricks-connect
pyenv install 3.10.6
pyenv virtualenv 3.10.6 tobias
pyenv local tobias

pip uninstall pyspark

make install
```

Check that the java version is java-8! Otherwise databricks-connect does not work


Go to databricks > compute and select your cluster, you can see an url like this one:

https://DATABRICKS_HOST/login.html?o=ORGANIZATION#setting/clusters/CLUSTER_ID/configuration

Extract the following fields

```console
{
databricks host: DATABRICKS_HOST

Databricks Token: GENERATE A PERSONAL ACCESS TOKER HERE IN DATABRICKS SETTINGS

Cluster ID: CLUSTER_ID

Org ID: ORGANIZATION

PORT: 15001
}
```

Then copy .envrc_template to .envrc and set the corresponding values in .envrc





