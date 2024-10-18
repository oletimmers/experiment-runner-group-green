#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

class Solution {
public:
    vector<int> twoSum(vector<int>& nums, int target) {
        int left = 0;
        int right = nums.size() - 1;

        // If not sorted, you might need to sort it first:
        // sort(nums.begin(), nums.end());

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
