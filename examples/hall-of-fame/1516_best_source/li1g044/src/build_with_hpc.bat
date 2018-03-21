
ghc -fhpc %1.hs -main-is %1 -o %1.exe && ^
for %%f in (..\tests\%1\*.in) do (%1 < %%f)

hpc markup %1 && ^
hpc report %1 && ^
explorer hpc_index.html
