# Creating FinStress

This directory contains the full source code used to create the FinStress indicator of real-time perceived financial market stress.

In the top level directory there are three R source code files used to gather the documents from the Economist Intelligence Unit, extract the texts, and preprocess them. They were run in the following order:

1. gather_eiu.R: Note: requires authentication credentials

2. parse_extract_eiu.R

3. preprocess_eiu.R

This produces a preprocessed an R list of preprocessed documents stored in pca_kpca/preprocessed_data/eiu_texts_from_2003.rda.

This data set is used by a numher of R source files in the pca_kpca subdirectory:

- kpca_*.R files run the kernel principal component analysis for various string lengths. kpca_5.R is the version used to create FinStress.

- kpca_pre_2008_reanalysis.R

- pca_bag_of_words.R

- kernel_random_forest.R

Finally, clean_save_finstress.R does some cleaning of the KPCA 5 character length string kernel output to create the public facing FinStress data set.
