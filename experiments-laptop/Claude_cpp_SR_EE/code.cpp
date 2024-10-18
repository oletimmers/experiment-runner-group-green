#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <cctype>

// Function to read the file content into a vector of strings
std::vector<std::string> readFileLines(const std::string& filename) {
    std::vector<std::string> lines;
    std::ifstream file(filename);
    if (file.is_open()) {
        std::string line;
        while (std::getline(file, line)) {
            lines.push_back(line);
        }
        file.close();
    } else {
        std::cerr << "Unable to open file: " << filename << std::endl;
    }
    return lines;
}

// Function to perform case-insensitive string replacement
std::string replaceStringCaseInsensitive(const std::string& str, const std::string& target, const std::string& replacement) {
    std::string result = str;
    std::string::size_type pos = 0;
    while ((pos = std::tolower(result.find(std::string(target.begin(), target.end()), pos))) != std::string::npos) {
        result.replace(pos, target.length(), replacement);
        pos += replacement.length();
    }
    return result;
}

int main(int argc, char* argv[]) {
    if (argc < 4) {
        std::cerr << "Usage: " << argv[0] << " <filename> <target_string> <replacement_string>" << std::endl;
        return 1;
    }

    const std::string filename = argv[1];
    const std::string target = argv[2];
    const std::string replacement = argv[3];

    std::vector<std::string> lines = readFileLines(filename);

    if (lines.empty()) {
        std::cout << "File is empty." << std::endl;
        return 0;
    }

    bool targetFound = false;
    for (auto& line : lines) {
        std::string modifiedLine = replaceStringCaseInsensitive(line, target, replacement);
        if (modifiedLine != line) {
            targetFound = true;
        }
        std::cout << modifiedLine << std::endl;
    }

    if (!targetFound) {
        std::cout << "Target string not found." << std::endl;
    }

    return 0;
}