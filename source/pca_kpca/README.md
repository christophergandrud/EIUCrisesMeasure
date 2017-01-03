# Conduct PCA and KPCA Analysis of Economist Intelligence Unit (EIU) Corpus

The files in this directory conduct a variety of Principal Components Analysis and Kernel Principal Components Analysis on a corpus or EIU texts.

The preprocessed EIU corpus is stored in *source/preprocessed_data/eiu_texts_from_2003.rda*. These texts were (1) gathered using source code in   *source/gather_eiu.R*. The raw texts were (2) parsed using *source/parse_extract_eiu.R*. Finally, they were preprocessed (3) using *source/preprocess_eiu.R*.

Use GNU make run the PCA and KPCA analyses in this directory:

```
make -j 2
```

Note that `-2` informs make to run the two R scripts in parallel at a time. Simply omit this if you do not wish to run the scripts simultaneously or increase the number following `-j` to increase the number of jobs run simultaneously.

Note also that you will need to manually add your working directories to the R source files.
