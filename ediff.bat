@echo off
set command=%1
set original=%2
set original=%original:\=\\%
set original=%original:"=\"%
set modified=%3
set modified=%modified:\=\\%
set modified=%modified:"=\"%

:compare
c:\emacs25\bin\emacsclient -c -n -e "(ediff-files %original% %modified%)"
goto exit

:merge
set ancestor=%4
set ancestor=%ancestor:\=\\%
set ancestor=%ancestor:"=\"%
set output=%5
set output=%output:\=\\%
set output=%output:"=\"%
c:\emacs25\bin\emacsclient -c -n -e "(ediff-merge-with-ancestor %original% %modified% %ancestor% nil %output%)"
goto exit

:exit
