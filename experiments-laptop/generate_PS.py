import random

# Generate 1 million random integers
nums = [str(random.randint(1, 100000)) for _ in range(1000000)]
target = str(random.randint(1,50)) 

with open('input.txt', 'w') as f:
    f.write(target + '\n')
    f.write(' '.join(nums))
