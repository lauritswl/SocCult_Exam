/*===============================================================================

                            Stata Setup File
							
Description: This program is used for install stata packages

*==============================================================================*/

clear all
set more off

capture ssc install outreg2

// install moulton
** moulton.rar is provided in the file

capture ssc install randcmd

// install nwqap

capture ssc install rwolf
