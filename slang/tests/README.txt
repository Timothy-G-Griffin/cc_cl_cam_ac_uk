
The programs in this directory are used by ../test.ml. 
None of these programs take input. 
For each file foo.slang there is an entry in manifest.txt of the form 

foo output

where output is the correct result of evaluating foo.slang. 

Running slang.byte with the -t flag will read the manifest and 
test that the output is correct (for the selected interpreters). 
