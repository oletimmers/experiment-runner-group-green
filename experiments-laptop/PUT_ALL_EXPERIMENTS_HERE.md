# Explanation of experiments-layout

**IMPORTANT**:

Put all the experiments we're doing ON THE LAPTOP, in this folder!!!

Folder must be `llm_language_problemtype`, like `ChatGPT_haskell_PS_OG`

Possible values:
```
        "llm": "ChatGPT","Gemini","Claude"
        "language": "cpp","haskell"
        "problem": "PS","SR"
        "prompt": "OG", "EE"
```

Inside you put a `code.cpp` or `code.hs` file according to the programming language.


Instructions to run the String manipulation code :

The command line input is in this format :  <filename> <target_string> <replacement_string>
eg : example.txt model network

NOTE : The example.txt file should be in the same directory of the String code so that the code can access it.
