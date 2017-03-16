# HOW TO 
Here, we describe the process by which the (ExAC x ClinVar) collapsed flat-file was created on the Stanford SCG cluster. YMMV.

1. Start by downloading the ExAC and ClinVar repos (GRCh37) from:
* ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/vcf_GRCh37/archive/2016/
* ftp://ftp.broadinstitute.org/pub/ExAC_release/release0.3.1/
respectively

2. Then, unzip them

3. Then, load libraries
```
module load tabix/0.2.6
module load samtools/1.3.1
module loadbcftools/1.3.1
```

4. Then, after installing samtools:
```
bgzip clinvar_[date].vcf
bcftools index clinvar_[date].vcf.gz
bgzip ExAC.r0.3.1.sites.vep.vcf
bcftools index ExAC.r0.3.1.sites.vep.vcf.gz
```

5. Then, intersect
```
nohup bcftools isec $1 $2 -p `pwd` -c none -n=2 /srv/gsfs0/projects/bustamante/reference_panels/ClinVar/GRCh37/clinvar_20160104.vcf.gz /srv/gsfs0/projects/bustamante/reference_panels/ExAC/r0.3.1/ExAC.r0.3.1.sites.vep.vcf.gz
```

6. Then, confirm that:
```
cat 0001.vcf | awk -F '\t' 'NF==8' | wc -l 
cat 0000.vcf | awk -F '\t' 'NF==8' | wc -l
```
are the same

7. Then, create a joint VCF and pull out fields of use into a tab delimited flat file
```
./intersect_vcfs.sh 0000.vcf 0001.vcf > clinvar.exac.variants.tab
```
8. Then, denormalize by gene (col 6), and by submission (col 7, 8 and 9).
```
cat clinvar.exac.variants.tab | awk '{
  split($6,a,"|"); 
  if(length(a)!=1) 
    for(i=1;i<=length(a);i++) {
        for(j=1;j<6;j++) 
          printf $j"\t"; 
        printf a[i]"\t"; 
        for(j=7;j<NF;j++) 
          printf $j"\t"; 
        print $NF;
    } 
    else print;
 }' > clinvar.exac.variants.gene.tab
```

9. Then, denormalize by submission (col 7, 8 and 9), after confirming that these are equal length fields (col separated by |)
cat clinvar.exac.variants.tab | awk '{split($7,a,"|"); split($8,b,"|"); split($9,c,"|"); if(length(a)!=1) for(i=1;i<=length(a);i++) {for(j=1;j<7;j++) printf $j"\t"; printf a[i]"\t"b[i]"\t"c[i]"\t"; for(j=10;j<NF;j++) printf $j"\t"; print $NF;} else print;}' > clinvar.exac.variants.submission.tab

Then, denormalize by both
cat clinvar.exac.variants.gene.tab | awk '{split($7,a,"|"); split($8,b,"|"); split($9,c,"|"); if(length(a)!=1) for(i=1;i<=length(a);i++) {for(j=1;j<7;j++) printf $j"\t"; printf a[i]"\t"b[i]"\t"c[i]"\t"; for(j=10;j<NF;j++) printf $j"\t"; print $NF;} else print;}' > clinvar.exac.variants.gene.submission.tab

Then, denormalize multiple submissions per lab (asserting different diseases for the same variant).
cat clinvar.exac.variants.gene.submission.tab | awk '{split($7,a,","); split($8,b,","); split($9,c,","); if(length(a)!=1) {for(i=1;i<=length(a);i++) {for(j=1;j<7;j++) printf $j"\t"; printf a[i]"\t"b[i]"\t"c[i]"\t"; for(j=10;j<NF;j++) printf $j"\t"; print $NF;}} else print;}' > clinvar.exac.variants.gene.submission.diseases.tab

Then, denormalize multiple alleles per variant (different frequencies and assertions per variant).
cat clinvar.exac.variants.gene.submission.diseases.tab | awk '{split($5,a,","); if(length(a)!=1) {split($10,nfe,","); split($12,afr,","); split($14,amr,","); split($16,eas,","); split($18,sas,","); split($20,fin,","); split($22,oth,","); split($24,adj,","); for(i=1;i<=length(a);i++) print $1"\t"$2"\t"$3"\t"$4"\t"a[i]"\t"$6"\t"$7"\t"$8"\t"$9"\t"nfe[i]"\t"$11"\t"afr[i]"\t"$13"\t"amr[i]"\t"$15"\t"eas[i]"\t"$17"\t"sas[i]"\t"$19"\t"fin[i]"\t"$21"\t"oth[i]"\t"$23"\t"adj[i]"\t"$25;} else print; }'| awk 'NF==25' > clinvar.exac.variants.gene.submission.diseases.alleles.tab

Something is seriously wrong here. You end up with lines that are messy (NF!=25). Go over carefully from beginning, and find out which group of fields is wrong to parse together.
