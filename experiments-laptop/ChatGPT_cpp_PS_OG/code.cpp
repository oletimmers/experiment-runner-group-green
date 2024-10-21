#include <vector>
#include <unordered_map>
#include <iostream>
#include <fstream>
#include <sstream>

class Solution {
public:
    std::vector<int> twoSum(std::vector<int>& nums, int target) {
        std::unordered_map<int, int> num_map;  // To store the number and its index
        for (int i = 0; i < nums.size(); ++i) {
            int complement = target - nums[i];
            if (num_map.find(complement) != num_map.end()) {
                // If complement is found in the map, return the pair of indices
                return {num_map[complement], i};
            }
            // Otherwise, add the current number and its index to the map
            num_map[nums[i]] = i;
        }
        // If no solution is found (although the problem guarantees one solution exists)
        return {};
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