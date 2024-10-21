#include <iostream>
#include <vector>
#include <algorithm>
#include <fstream>
#include <sstream>

using namespace std;

class Solution {
public:
    vector<int> twoSum(vector<int>& nums, int target) {
        // 1. Create a copy of the input array to avoid side effects
        vector<int> sortedNums = nums; 

        // 2. Sort the copied array
        sort(sortedNums.begin(), sortedNums.end());

        int left = 0;
        int right = sortedNums.size() - 1;

        while (left < right) {
            int currentSum = sortedNums[left] + sortedNums[right];

            if (currentSum == target) {
                // 3. Find the original indices from the sorted indices
                int index1 = -1, index2 = -1;
                for (int i = 0; i < nums.size(); ++i) {
                    if (nums[i] == sortedNums[left] && index1 == -1) {
                        index1 = i;
                    } else if (nums[i] == sortedNums[right]) {
                        index2 = i;
                    }
                }
                return {index1, index2}; 
            } else if (currentSum < target) {
                left++;
            } else {
                right--;
            }
        }

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