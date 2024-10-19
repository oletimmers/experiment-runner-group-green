#include <iostream>
#include <vector>
#include <algorithm>
#include <fstream>
#include <sstream>

using namespace std;

class Solution {
public:
    vector<int> twoSum(vector<int>& nums, int target) {
        int left = 0;
        int right = nums.size() - 1;

        // If not sorted, you might need to sort it first:
        sort(nums.begin(), nums.end());

        while (left < right) {
            int currentSum = nums[left] + nums[right];

            if (currentSum == target) {
                return {left, right};
            } else if (currentSum < target) {
                left++; // Need a larger sum, move left pointer
            } else {
                right--; // Need a smaller sum, move right pointer
            }
        }

        return {}; // No solution found
    }
};

int main() {
    std::ifstream infile("input_1.txt");  // Read from input.txt
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