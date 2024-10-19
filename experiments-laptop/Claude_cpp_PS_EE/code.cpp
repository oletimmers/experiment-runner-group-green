#include <iostream>
#include <vector>
#include <algorithm>
#include <fstream>
#include <sstream>

using namespace std;

class Solution {
public:
    vector<int> twoSum(vector<int>& nums, int target) {
        int n = nums.size();
        for (int i = 0; i < n - 1; ++i) {
            for (int j = i + 1; j < n; ++j) {
                if (nums[i] + nums[j] == target) {
                    return {i, j};
                }
            }
        }
        return {}; // This line should never be reached given the problem constraints
    }
};

int main() {
    std::ifstream infile("input.txt");  // Read from input.txt
    std::string line;
    
    // Read the target
    std::getline(infile, line);
    int target = std::stoi(line);
    
    // Read the vector elements
    std::vector<int> nums;
    std::getline(infile, line);
    std::istringstream iss(line);
    int num;
    while (iss >> num) {
        nums.push_back(num);
    }

    // Your Two Sum logic
    Solution solution;
    std::vector<int> result = solution.twoSum(nums, target);
    
    if (!result.empty()) {
        std::cout << result[0] << ", " << result[1] << std::endl;
    } else {
        std::cout << "No solution found." << std::endl;
    }

    return 0;
}