#if not exist ebin md ebin
FOR %%f in (*.erl) DO  erlc -W0 -o . %%f
pause