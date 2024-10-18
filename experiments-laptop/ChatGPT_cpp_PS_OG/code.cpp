#include <vector>
#include <unordered_map>
#include <iostream>

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
