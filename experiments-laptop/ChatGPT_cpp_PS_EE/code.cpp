#include <iostream>
#include <unordered_map>
#include <vector>

using namespace std;

class Solution {
public:
    vector<int> twoSum(vector<int>& nums, int target) {
        unordered_map<int, int> num_map; // stores number and its index
        for (int i = 0; i < nums.size(); ++i) {
            int complement = target - nums[i];
            auto it = num_map.find(complement);
            if (it != num_map.end()) {
                return {it->second, i};
            }
            num_map[nums[i]] = i;
        }
        return {}; // This line should theoretically never be reached due to problem constraints
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
