#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <cctype>

// Function to convert a string to lowercase
std::string toLower(std::string str) {
    std::transform(str.begin(), str.end(), str.begin(),
                   [](unsigned char c){ return std::tolower(c); });
    return str;
}

int main(int argc, char* argv[]) {
    // Check for correct number of arguments
    if (argc != 4) {
        std::cerr << "Usage: ./replace <filename> <target_string> <replacement_string>" << std::endl;
        return 1;
    }

    // Get filename, target, and replacement strings from arguments
    std::string filename = argv[1];
    std::string target = argv[2];
    std::string replacement = argv[3];

    // Open the input file
    std::ifstream infile(filename);

    // Check if the file is open
    if (!infile.is_open()) {
        std::cerr << "Error opening file: " << filename << std::endl;
        return 1;
    }

    // Read the file content line by line
    std::string line;
    std::string output; 
    while (std::getline(infile, line)) {
        // Find the target string in the line (case-insensitive)
        size_t pos = toLower(line).find(toLower(target));

        // Replace all occurrences of the target string in the line
        while (pos != std::string::npos) {
            line.replace(pos, target.length(), replacement);
            pos = toLower(line).find(toLower(target), pos + replacement.length());
        }

        // Append the modified line to the output string
        output += line + "\n";
    }

    // Close the input file
    infile.close();

    // Print the modified content
    std::cout << output;

    return 0;
}