# bio.lobster
Scripts and Functions for lobster stock assessments. Interoperable with github projects under Beothuk and PEDLibrary.  
To install this package via devtools:

# to enable inter-operability with github
require( devtools ) # or install.packages( "devtools", dependencies=TRUE )

# this is to bootstrap the bio.* suite of tools
install_github( "LobsterScience/bio.lobster" ) 

# to use some of the functionality:
require( bio.lobster ) # this should ideally be placed into your .Rprofile
