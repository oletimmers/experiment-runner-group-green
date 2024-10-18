#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <cctype>

using namespace std;

// Function to convert a string to lowercase
string toLower(string str) {
    transform(str.begin(), str.end(), str.begin(),
              [](unsigned char c){ return tolower(c); });
    return str;
}

int main(int argc, char* argv[]) {
    // Check if the correct number of arguments is provided
    if (argc != 4) {
        cerr << "Usage: " << argv[0] << " <filename> <target_string> <replacement_string>" << endl;
        return 1;
    }

    // Get filename, target string, and replacement string from command line arguments
    string filename = argv[1];
    string target = argv[2];
    string replacement = argv[3];

    // Open the file for reading
    ifstream file(filename);

    // Check if the file is open
    if (!file.is_open()) {
        cerr << "Error opening file: " << filename << endl;
        return 1;
    }

    // Read the file content into a string
    string content((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());

    // Check if the file is empty
    if (content.empty()) {
        cout << "File is empty." << endl;
        return 0;
    }

    // Convert target string to lowercase for case-insensitive search
    string targetLower = toLower(target);

    // Find the first occurrence of the target string (case-insensitive)
    size_t pos = toLower(content).find(targetLower);

    // Replace all occurrences of the target string
    while (pos != string::npos) {
        content.replace(pos, target.length(), replacement);
        pos = toLower(content).find(targetLower, pos + replacement.length());
    }

    // Print the modified content
    cout << content << endl;

    // Close the file
    file.close();

    return 0;
}