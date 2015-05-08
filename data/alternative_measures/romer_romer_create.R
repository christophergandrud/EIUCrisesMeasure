###########################
# Create data set for Romer and Romer (2015) measure
# Christopher Gandrud
# MIT License
# Based on April 2015 version
# http://eml.berkeley.edu//~cromer/RomerandRomerFinancialCrises.pdf
###########################

countries <- c('Finland', 'Finland', 'Finland', 'Finland', 'Finland',
               'France', 'France', 'France', 'France', 'France', 
               'France',
               'Germany', 'Germany',
               'Iceland', 'Iceland',
               'Italy',
               'Japan', 'Japan', 'Japan', 'Japan', 'Japan',
               'Japan', 'Japan', 'Japan', 'Japan', 'Japan',
               'Japan', 'Japan', 'Japan', 'Japan', 'Japan',
               'Japan', 'Japan', 'Japan', 'Japan', 'Japan',
               'Japan', 'Japan', 'Japan', 'Japan', 'Japan',
               'Japan', 'Japan', 'Japan', 'Japan', 'Japan',
               'Norway', 'Norway', 'Norway', 'Norway', 'Norway', 
               'Norway',
               'Sweden', 'Sweden',
               'Turkey', 'Turkey', 'Turkey', 'Turkey', 'Turkey', 
               'Turkey',
               'United States', 'United States', 'United States', 'United States', 'United States', 
               'United States', 'United States', 'United States'
               )

month <- c('1992-03-01', '1992-09-01', '1993-03-01', '1993-09-01', '1994-03-01',
           '1991-09-01', '1995-03-01', '1995-09-01', '1996-03-01', '1996-09-01',
           '1997-03-01',
           '1974-09-01', '2003-03-01',
           '2006-09-01', '2007-03-01',
           '1997-03-01',
           '1990-09-01', '1991-03-01', '1991-09-01', '1992-03-01', '1992-09-01',
           '1993-03-01', '1993-09-01', '1994-03-01', '1994-09-01', '1995-03-01',
           '1995-09-01', '1996-03-01', '1996-09-01', '1997-03-01', '1997-09-01',
           '1998-03-01', '1998-09-01', '1999-03-01', '1999-09-01', '2000-03-01',
           '2000-09-01', '2001-03-01', '2001-09-01', '2002-03-01', '2002-09-01',
           '2003-03-01', '2003-09-01', '2004-03-01', '2004-09-01', '2005-03-01',
           '1991-09-01', '1992-03-01', '1992-09-01', '1993-03-01', '1993-09-01',
           '1994-03-01',
           '1992-09-01', '1993-03-01',
           '2001-03-01', '2001-09-01', '2002-03-01', '2002-09-01', '2003-03-01',
           '2003-09-01',
           '1986-03-01', '1990-03-01', '1990-09-01', '1991-03-01', '1991-09-01',
           '1992-03-01', '1998-09-01', '2007-03-01'
           )

distress <- c(2, 6, 8, 5, 3,
              1, 2, 4, 5, 5, 
              3,
              2, 1,
              5, 2,
              1,
              3, 1, 5, 3, 4, 
              4, 5, 3, 3, 4, 
              5, 6, 4, 5, 7,
              11, 13, 9, 6, 4,
              3, 6, 6, 8, 7, 
              6, 5, 4, 3, 2,
              9, 5, 8, 6, 3, 
              2,
              5, 8,
              8, 8, 6, 4, 4, 3,
              1, 5, 8, 4, 3, 
              2, 3, 1
              )


romer_romer <- data.frame(country = countries, month = month, 
                          rr_distress = distress)
