# Conduct PCA and KPCA Analysis of Economist Intelligence Unit (EIU) Corpus

[INCOMPLETE]

The files in this directory conduct a variety of Principal Components Analysis and Kernel Principal Components Analysis on a corpus or EIU texts.

The preprocessed EIU corpus is stored in *source/preprocessed_data/eiu_texts_from_2003.rda*. These texts were (1) gathered using source code in   *source/gather_eiu.R*. The raw texts were (2) parsed using *source/parse_extract_eiu.R*. Finally, they were preprocessed (3) using *source/preprocess_eiu.R*.

Use GNU make run the PCA and KPCA analyses in this directory:

```
make --jobs
```

Note that `--jobs` informs make to run the R scripts in parallel. Simply omit this if you do not wish to run the scripts simultaneously.

Note also that you will need to manually add your working directories to the R source files.
