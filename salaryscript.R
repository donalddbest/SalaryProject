# Script: salaryscript.R
# Author: Donald DiJacklin

# Intent: To take the 2014 San Francisco pay records freely available at 
# http://transparentcalifornia.com/salaries/san-francisco/
# and turn them into distribution graphs for the different reported job titles, so that they are easily investigable.


# Clear previous garbage.
rm(list = ls())

#Checks whether I have the package. If not, it installs it.
EnsurePackage <- function(x)
{
  
  x <- as.character( x )
  
  if (!require( x, character.only = TRUE))
  {
  
    install.packages( pkgs = x, repos = "http://cran.r-project.org")
    require( x, character.only = TRUE)
    
  }
  
  
}

# I make sure I have the sqlite package and load the library.
EnsurePackage( "RSQLite" )
library( "RSQLite" )

# Getting the data from my database.
connect <- dbConnect( RSQLite::SQLite(), "salarydb" )
salframe <- dbGetQuery( connect, "select * from salary" )

# Gets all the unique job titles.
uniques <- unique( salframe$JobTitle )

# Tells the computer what file to output to.
pdf( file = "TotalPayDistributions" )

#find unique values for job and make a subset of salaries for each unique.
for(unique in uniques)
{
  
  # Subsets the data by the current unique
  x <- subset( salframe, JobTitle == unique, JobTitle:TotalPay )
  
  
  # Only makes a graph of the histogram if there are "enough" observations
  if( length( x$TotalPay ) > 12)
  {
    
    hist( x$TotalPay, main = paste( "Histogram of ", x$JobTitle[1] ), xlab = paste( "Annual Total Pay of ", x$JobTitle[1] ), col = "goldenrod2")

  }

}

# Closes the file.
dev.off()
