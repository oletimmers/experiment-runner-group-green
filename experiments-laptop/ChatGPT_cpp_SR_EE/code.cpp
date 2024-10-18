#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <cctype> // for std::tolower
#include <cstdlib> // for exit()

// Function to convert a string to lowercase (for case-insensitive comparison)
std::string toLowerCase(const std::string& str) {
    std::string lower_str = str;
    std::transform(lower_str.begin(), lower_str.end(), lower_str.begin(), [](unsigned char c) {
        return std::tolower(c);
    });
    return lower_str;
}

// Function to perform case-insensitive string replacement
std::string replaceAllCaseInsensitive(const std::string& text, const std::string& target, const std::string& replacement) {
    std::string lower_text = toLowerCase(text);
    std::string lower_target = toLowerCase(target);
    std::string result;
    size_t pos = 0;
    size_t found;

    while ((found = lower_text.find(lower_target, pos)) != std::string::npos) {
        // Append the part before the match
        result.append(text, pos, found - pos);
        // Append the replacement
        result.append(replacement);
        // Move the position forward
        pos = found + target.size();
    }
    // Append the remaining part of the text
    result.append(text, pos, std::string::npos);

    return result;
}

// Main function
int main(int argc, char* argv[]) {
    // Check if the correct number of arguments are provided
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <filename> <target_string> <replacement_string>\n";
        return 1;
    }

    // Extract command line arguments
    std::string filename = argv[1];
    std::string target = argv[2];
    std::string replacement = argv[3];

    // Open the file for reading
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error: Could not open file '" << filename << "'\n";
        return 1;
    }

    // Read the file line by line and perform the replacement
    std::string line;
    bool target_found = false;
    std::string modified_content;

    while (std::getline(file, line)) {
        std::string modified_line = replaceAllCaseInsensitive(line, target, replacement);
        // If any change is made, mark that the target was found
        if (toLowerCase(line).find(toLowerCase(target)) != std::string::npos) {
            target_found = true;
        }
        modified_content += modified_line + "\n"; // Append the modified line
    }

    // Close the file after reading
    file.close();

    // Check if target string was found
    if (!target_found) {
        std::cerr << "Warning: Target string '" << target << "' not found in the file.\n";
    }

    // Output the modified content
    std::cout << "Modified Content:\n";
    std::cout << modified_content;

    return 0;
}