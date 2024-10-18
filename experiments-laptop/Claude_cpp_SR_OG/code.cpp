#include <iostream>
#include <fstream>
#include <string>
#include <cctype>
#include <algorithm>

std::string readFileContents(const std::string& filename) {
    std::ifstream file(filename);
    if (!file) {
        std::cerr << "Error: Unable to open file '" << filename << "'" << std::endl;
        return "";
    }

    std::string contents((std::istreambuf_iterator<char>(file)),
                         std::istreambuf_iterator<char>());
    return contents;
}

std::string replaceStringInsensitive(const std::string& input, const std::string& target, const std::string& replacement) {
    std::string result = input;
    std::string::size_type pos = 0;
    std::string loweredInput = input;
    std::transform(loweredInput.begin(), loweredInput.end(), loweredInput.begin(), ::tolower);
    std::string loweredTarget = target;
    std::transform(loweredTarget.begin(), loweredTarget.end(), loweredTarget.begin(), ::tolower);

    while ((pos = loweredInput.find(loweredTarget, pos)) != std::string::npos) {
        result.replace(pos, target.length(), replacement);
        pos += replacement.length();
        loweredInput.replace(pos, target.length(), replacement);
    }

    return result;
}

int main(int argc, char* argv[]) {
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <filename> <target_string> <replacement_string>" << std::endl;
        return 1;
    }

    const std::string filename = argv[1];
    const std::string target = argv[2];
    const std::string replacement = argv[3];

    std::string fileContents = readFileContents(filename);
    if (fileContents.empty()) {
        return 1;
    }

    std::string modifiedContents = replaceStringInsensitive(fileContents, target, replacement);

    std::cout << "Modified contents:\n" << modifiedContents << std::endl;

    return 0;
}