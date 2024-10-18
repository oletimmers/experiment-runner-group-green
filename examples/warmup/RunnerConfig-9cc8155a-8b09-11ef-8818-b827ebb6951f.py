from EventManager.Models.RunnerEvents import RunnerEvents
from EventManager.EventSubscriptionController import EventSubscriptionController
from ConfigValidator.Config.Models.RunTableModel import RunTableModel
from ConfigValidator.Config.Models.FactorModel import FactorModel
from ConfigValidator.Config.Models.RunnerContext import RunnerContext
from ConfigValidator.Config.Models.OperationType import OperationType
from ExtendedTyping.Typing import SupportsStr
from ProgressManager.Output.OutputProcedure import OutputProcedure as output

from typing import Dict, List, Any, Optional
from pathlib import Path
from os.path import dirname, realpath
from io import StringIO

import paramiko
import pandas as pd 
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("paramiko")
logger.setLevel(logging.INFO)  # Set to WARNING to ignore INFO messages

class RunnerConfig:
    ROOT_DIR = Path(dirname(realpath(__file__)))

    # ================================ USER SPECIFIC CONFIG ================================
    """The name of the experiment."""
    name:                       str             = "warmuptests"

    """The path in which Experiment Runner will create a folder with the name `self.name`, in order to store the
    results from this experiment. (Path does not need to exist - it will be created if necessary.)
    Output path defaults to the config file's path, inside the folder 'experiments'"""
    results_output_path:        Path            = ROOT_DIR / 'experiments'

    """This has to be the direct path ON the laptop, since from ssh it will get navigated to directly."""
    source_path:                str             = "/home/ubuntu-ole/green-lab/main-repo/experiment-runner-group-green/experiments-laptop"


    # Change these LOCALLY
    hostname:                   str             = "192.168.178.207"
    username:                   str             = "ubuntu-ole"
    password:                   str             = "john123pw"

    """Experiment operation type. Unless you manually want to initiate each run, use `OperationType.AUTO`."""
    operation_type:             OperationType   = OperationType.AUTO

    """The time Experiment Runner will wait after a run completes.
    This can be essential to accommodate for cooldown periods on some systems."""
    time_between_runs_in_ms:    int             = 1000

    # Dynamic configurations can be one-time satisfied here before the program takes the config as-is
    # e.g. Setting some variable based on some criteria
    def __init__(self):
        """Executes immediately after program start, on config load"""

        EventSubscriptionController.subscribe_to_multiple_events([
            (RunnerEvents.BEFORE_EXPERIMENT, self.before_experiment),
            (RunnerEvents.BEFORE_RUN       , self.before_run       ),
            (RunnerEvents.START_RUN        , self.start_run        ),
            (RunnerEvents.START_MEASUREMENT, self.start_measurement),
            (RunnerEvents.INTERACT         , self.interact         ),
            (RunnerEvents.STOP_MEASUREMENT , self.stop_measurement ),
            (RunnerEvents.STOP_RUN         , self.stop_run         ),
            (RunnerEvents.POPULATE_RUN_DATA, self.populate_run_data),
            (RunnerEvents.AFTER_EXPERIMENT , self.after_experiment )
        ])
        self.run_table_model = None  # Initialized later
        

        output.console_log("Custom config loaded")

    def create_run_table_model(self) -> RunTableModel:
        """Create and return the run_table model here. A run_table is a List (rows) of tuples (columns),
        representing each run performed"""
        llm_factor = FactorModel("llm", ["ChatGPT","Gemini","Claude"])
        language_factor = FactorModel("language", ["cpp","haskell"])
        problem_type= FactorModel("problem", ["PS","SR"])
        prompt_factor = FactorModel("prompt", ["OG", "EE"])
        self.run_table_model = RunTableModel(
            factors=[llm_factor, language_factor, problem_type, prompt_factor],
            repetitions = 3,
            data_columns=['energy_usage_dram',
                          'energy_usage_package',
                          'energy_usage_pp0',
                          'energy_usage_pp1', 
                          'cpu_usage_0', 
                          'cpu_usage_1',
                          'cpu_usage_2', 
                          'cpu_usage_3',
                          'cpu_usage_4',
                          'cpu_usage_5',
                          'cpu_usage_6',
                          'cpu_usage_7',
                          'cpu_usage_8',
                          'cpu_usage_9',
                          'cpu_usage_10',
                          'cpu_usage_11',
                          'memory_usage', 'execution_time', 'machine_code_size']
        )
        return self.run_table_model

    def before_experiment(self) -> None:
        """Perform any activity required before starting the experiment here
        Invoked only once during the lifetime of the program."""
        pass

    def before_run(self) -> None:
        """Perform any activity required before starting a run.
        No context is available here as the run is not yet active (BEFORE RUN)"""
        pass

    def start_run(self, context: RunnerContext) -> None:
        ssh_client = self.create_new_ssh_client() # open connection once
        llm = context.run_variation['llm']
        language = context.run_variation['language']
        problem = context.run_variation['problem']
        prompt = context.run_variation['prompt']
        folder_id = f"{llm}_{language}_{problem}_{prompt}"
        output.console_log(f"Running {folder_id}")

        # Cd into the run folder
        cd_command = f"cd {self.source_path}/{folder_id} " 
        compile_command = f"{cd_command}"

        """
        Here we first compile and then measure the lines of machine code
        """
        if language == "cpp":
            #compile c++
            compile_command += f"&& g++ code.cpp -o code"
        else:
            compile_command += f"&& ghc -o code code.hs"

        compile_command += f" && chmod +x code"

        stdin, stdout, stderr = ssh_client.exec_command(f"{compile_command}")
        output.console_log(stdout.read().decode())
        error_output = stderr.read().decode()
        if error_output:
            output.console_log(error_output)
        else:
            output.console_log("Code compiled successfully")

        objdump_command = f"{cd_command}&& objdump -d code | wc -l"
        stdin, stdout, stderr = ssh_client.exec_command(objdump_command)
        instruction_count = int(stdout.read().decode().strip())
        write_command = f"{cd_command}&& echo {instruction_count} > instruction_count.txt"
        stdin, stdout, stderr = ssh_client.exec_command(write_command)
        output.console_log("Instruction count written to instruction_count.txt")
        output.console_log(f"Instruction count: {instruction_count}")

        output.console_log("Compiled and ready to run!")
        self.close_ssh_client(ssh_client)

    def start_measurement(self, context: RunnerContext) -> None:
        ssh_client = self.create_new_ssh_client() # open connection once
        llm = context.run_variation['llm']
        language = context.run_variation['language']
        problem = context.run_variation['problem']
        prompt = context.run_variation['prompt']
        folder_id = f"{llm}_{language}_{problem}_{prompt}"
        # Cd into the run folder
        cd_command = f"cd {self.source_path}/{folder_id} "

        """Perform any activity required for starting measurements."""

        run_command = f"./code"
        energibridge_command = f'{cd_command}&& sudo -S energibridge --output "energibridge.csv" --summary {run_command}'

        stdin, stdout, stderr = ssh_client.exec_command(energibridge_command)
        stdin.write(self.password + '\n')
        stdin.flush()

        error_output = stderr.read().decode()
        if error_output:
            output.console_log(error_output)
        else:
            output.console_log(f"Code run: Energibridge output written to energibridge.csv\n{stdout.read().decode()}")
        self.close_ssh_client(ssh_client)

    def interact(self, context: RunnerContext) -> None:
        """Perform any interaction with the running target system here, or block here until the target finishes."""
        pass

    def stop_measurement(self, context: RunnerContext) -> None:
        """Perform any activity here required for stopping measurements."""
        pass

    def stop_run(self, context: RunnerContext) -> None:
        """Perform any activity here required for stopping the run.
        Activities after stopping the run should also be performed here."""
        pass

    def populate_run_data(self, context: RunnerContext) -> Optional[Dict[str, SupportsStr]]:
        ssh_client = self.create_new_ssh_client()
        llm = context.run_variation['llm']
        language = context.run_variation['language']
        problem = context.run_variation['problem']
        prompt = context.run_variation['prompt']
        folder_id = f"{llm}_{language}_{problem}_{prompt}"
        # Cd into the run folder
        cd_command = f"cd {self.source_path}/{folder_id} " 
        """Parse and process any measurement data here.
        You can also store the raw measurement data under `context.run_dir`
        Returns a dictionary with keys `self.run_table_model.data_columns` and their values populated"""
        def read_file_from_ssh(ssh_client, remote_path):
            sftp_client = ssh_client.open_sftp()
            with sftp_client.file(remote_path, 'r') as remote_file:
                csv_content = remote_file.read().decode('utf-8')
            sftp_client.close()
            return csv_content

        # Path to the CSV file on the remote server
        remote_csv_path = f"{self.source_path}/{folder_id}/energibridge.csv"
        instruction_count_file = f"{self.source_path}/{folder_id}/instruction_count.txt"

        # Read the CSV content from the remote server
        csv_content = read_file_from_ssh(ssh_client, remote_csv_path)

        # Put the CSV content into a pandas DataFrame
        df = pd.read_csv(StringIO(csv_content))
    
        num_rows = len(df)

        # Calculate energy usage (sum of relevant energy columns divided by number of rows)
        energy_usage_dram = round((df['DRAM_ENERGY (J)'].sum() / num_rows, 3))
        energy_usage_package = round((df['PACKAGE_ENERGY (J)'].sum() / num_rows, 3))
        energy_usage_pp0 = round((df['PP0_ENERGY (J)'].sum() / num_rows, 3))
        energy_usage_pp1 = round((df['PP1_ENERGY (J)'].sum() / num_rows, 3))
                                 
        # Cpu usage                                 
        cpu_usage_0 = round(df['CPU_USAGE_0'].sum() / num_rows, 3)
        cpu_usage_1 = round(df['CPU_USAGE_1'].sum() / num_rows, 3)
        cpu_usage_2 = round(df['CPU_USAGE_2'].sum() / num_rows, 3)
        cpu_usage_3 = round(df['CPU_USAGE_3'].sum() / num_rows, 3)
        cpu_usage_4 = round(df['CPU_USAGE_4'].sum() / num_rows, 3)
        cpu_usage_5 = round(df['CPU_USAGE_5'].sum() / num_rows, 3)
        cpu_usage_6 = round(df['CPU_USAGE_6'].sum() / num_rows, 3)
        cpu_usage_7 = round(df['CPU_USAGE_7'].sum() / num_rows, 3)
        cpu_usage_8 = round(df['CPU_USAGE_8'].sum() / num_rows, 3)
        cpu_usage_9 = round(df['CPU_USAGE_9'].sum() / num_rows, 3)
        cpu_usage_10 = round(df['CPU_USAGE_10'].sum() / num_rows, 3)
        cpu_usage_11 = round(df['CPU_USAGE_11'].sum() / num_rows, 3)

        # Calculate average memory usage (average over the rows)
        memory_usage = round(df['USED_MEMORY'].mean(), 3)

        # Calculate execution time (difference between first and last Time value)
        execution_time = df['Time'].max() - df['Time'].min()

        machine_code_size = read_file_from_ssh(ssh_client, instruction_count_file)
        
        # Calculate execution time
        output.console_log("Config.populate_run_data() called!")
        run_data = {
            'energy_usage_dram': energy_usage_dram,
            'energy_usage_package': energy_usage_package,
            'energy_usage_pp0': energy_usage_pp0,
            'energy_usage_pp1': energy_usage_pp1, 
            'cpu_usage_0': cpu_usage_0, 
            'cpu_usage_1': cpu_usage_1,
            'cpu_usage_2': cpu_usage_2, 
            'cpu_usage_3': cpu_usage_3,
            'cpu_usage_4': cpu_usage_4,
            'cpu_usage_5': cpu_usage_5,
            'cpu_usage_6': cpu_usage_6,
            'cpu_usage_7': cpu_usage_7,
            'cpu_usage_8': cpu_usage_8,
            'cpu_usage_9': cpu_usage_9,
            'cpu_usage_10': cpu_usage_10,
            'cpu_usage_11': cpu_usage_11,
            'memory_usage': memory_usage,
            'execution_time': execution_time,
            'machine_code_size': machine_code_size
        }
        self.close_ssh_client(ssh_client)
        return run_data

    def after_experiment(self) -> None:
        """Perform any activity required after stopping the experiment here
        Invoked only once during the lifetime of the program."""
        pass

    """
    Create a new ssh client
    """
    def create_new_ssh_client(self):
        ssh_client = paramiko.SSHClient()
        ssh_client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        ssh_client.connect(hostname=self.hostname, username=self.username, password=self.password)
        output.console_log("SSH connection established")
        return ssh_client
    
    def close_ssh_client(self, ssh_client):
        if ssh_client:
            ssh_client.close()
            output.console_log("SSH connection closed")

    # ================================ DO NOT ALTER BELOW THIS LINE ================================
    experiment_path:            Path             = None
