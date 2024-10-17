#include <vector>
#include <unordered_map>
#include <iostream>

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
    Solution solution;
    std::vector<int> nums = {2, 7, 11, 15};
    int target = 9;

    // Call the twoSum function
    std::vector<int> result = solution.twoSum(nums, target);

    // Print the result
    if (!result.empty()) {
        std::cout << result[0] << ", " << result[1] << std::endl;
    } else {
        std::cout << "No solution found." << std::endl;
    }

    return 0;
}
