#include <vector>
#include <unordered_map>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;

class Solution {
public:
    vector<int> twoSum(vector<int>& nums, int target) {
        unordered_map<int, int> numMap; // Value, Index

        for (int i = 0; i < nums.size(); ++i) {
            int complement = target - nums[i];

            // If complement found, return current index and its index
            if (numMap.count(complement)) {
                return {numMap[complement], i};
            }
            // Otherwise, store the current value and its index
            numMap[nums[i]] = i; 
        }

        return {}; // No solution found 
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