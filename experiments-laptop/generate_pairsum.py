import random
import os
nums = [str(random.randint(1, 10000)) for _ in range(10000)]
target = str(random.randint(1,50)) 

#modify the path before running!
base_dir = 'D:\\GreenLab\\experiment-runner-group-green\\experiments-laptop'
for folder_name in os.listdir(base_dir):
    folder_path = os.path.join(base_dir, folder_name)
    if 'PS' in folder_name:
        cd_command = f"cd {folder_name}"
        compile_command = f"{cd_command}"

        input_file_path = os.path.join(folder_path, 'input.txt')

        with open(input_file_path, 'w') as f:
            f.write(target + '\n')
            f.write(' '.join(nums))

        if 'cpp' in folder_name:
            compile_command += f"&& g++ code.cpp -o code"
        else:
            compile_command += f"&& stack ghc -- code.hs"
        os.system(compile_command)
        run_command = f"{cd_command}"
        run_command += f"&& .\\code"
        print(folder_name)
        os.system(run_command)

