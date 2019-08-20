!!        Execute last commandline again
!{n}      Execute history commandline number {n}
!-1       Execute last command
!-2       Execite second last command
!!:p      Print last commandline
!{n}:p    Print nth commandline

!^        Last commands first argument
!$        Last commands last argument
!*        Last command's arguments
!:{n}     Last command's nth argument 
!{n}:{N}  History commandline number {n}'s {N}th argument

!{glob}   History commandline that matches glob

!^:h      First argument of last command (:h upto last segment of path)
!^:t      First argument of last command (:t the last segment of path only)
!^:r      First argument of last command (:r remove the file extension)
!#        The current commandline
!#^       The first argument of the current commandline

^old^new       Make changes to the previous commandline
!!:s/old/new   Make changes to the previous commandline
!!:gs/old/new  Make global changes to the previous commandline

