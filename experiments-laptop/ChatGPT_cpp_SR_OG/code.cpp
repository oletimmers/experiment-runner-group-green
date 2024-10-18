#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>
#include <cctype>

// Function to convert a string to lowercase (for case-insensitive comparison)
std::string toLowerCase(const std::string& str) {
    std::string result = str;
    std::transform(result.begin(), result.end(), result.begin(), ::tolower);
    return result;
}

// Function to replace all occurrences of the target string with the replacement string (case-insensitive)
std::string replaceAllOccurrences(const std::string& content, const std::string& target, const std::string& replacement) {
    std::string lowerContent = toLowerCase(content);
    std::string lowerTarget = toLowerCase(target);
    
    std::string result = content;
    size_t pos = 0;
    
    while ((pos = lowerContent.find(lowerTarget, pos)) != std::string::npos) {
        result.replace(pos, target.length(), replacement);
        lowerContent.replace(pos, target.length(), std::string(replacement.length(), ' ')); // update lowerContent to avoid shifting
        pos += replacement.length(); // move past the last replacement
    }
    
    return result;
}

int main(int argc, char* argv[]) {
    // Checking if the correct number of arguments is passed
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <filename> <target_string> <replacement_string>\n";
        return 1;
    }

    std::string filename = argv[1];
    std::string target = argv[2];
    std::string replacement = argv[3];

    // Open the file
    std::ifstream file(filename);
    if (!file) {
        std::cerr << "Error: File '" << filename << "' could not be opened.\n";
        return 1;
    }

    // Read the file content into a string
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string content = buffer.str();

    // Check if the file is empty
    if (content.empty()) {
        std::cerr << "Error: File '" << filename << "' is empty.\n";
        return 1;
    }

    // Perform the replacement
    std::string modifiedContent = replaceAllOccurrences(content, target, replacement);

    // Check if the target string was found and replaced
    if (modifiedContent == content) {
        std::cerr << "Warning: Target string '" << target << "' not found in the file.\n";
    }

    // Print the modified content to the console
    std::cout << "Modified content:\n";
    std::cout << modifiedContent << "\n";

    return 0;
}