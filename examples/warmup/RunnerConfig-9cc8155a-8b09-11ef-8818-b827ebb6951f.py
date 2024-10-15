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

    source_path:                str             = "~/Greenlab/main-repo/experiment-runner-group-green/experiments-laptop"


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
        self.ssh_client = None

        output.console_log("Custom config loaded")

    def create_run_table_model(self) -> RunTableModel:
        """Create and return the run_table model here. A run_table is a List (rows) of tuples (columns),
        representing each run performed"""
        llm_factor = FactorModel("llm", ["ChatGPT","Gemini","Claude"])
        language_factor = FactorModel("language", ["cpp","haskell"])
        problem_type= FactorModel("problem", ["PS","SR"])
        self.run_table_model = RunTableModel(
            factors=[llm_factor, language_factor, problem_type],
            exclude_variations=[ # To have now only ONE treatment
                {llm_factor: ['Gemini', 'Claude']}, 
                {llm_factor: ['haskell']}, 
                {llm_factor: ['SR']}
                # {language_factor: ['example_treatment2'], factor2: [True]},  # all runs having the combination ("example_treatment2", True) will be excluded
            ],
            repetitions = 3,
            data_columns=['energy_usage', 'cpu_usage', 'memory_usage', 'execution_time', 'machine_code_size']
        )
        return self.run_table_model

    def before_experiment(self) -> None:
        """Perform any activity required before starting the experiment here
        Invoked only once during the lifetime of the program."""
        self.ssh_client = paramiko.SSHClient()
        self.ssh_client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        self.ssh_client.connect(hostname=self.hostname, username=self.username, password=self.password)
        output.console_log("SSH connection established")

    def before_run(self) -> None:
        """Perform any activity required before starting a run.
        No context is available here as the run is not yet active (BEFORE RUN)"""
        stdin, stdout, stderr = self.ssh_client.exec_command(f"cd {self.source_path}")
        output.console_log(stdout.read().decode())
        # output.console_log(stdout.read().decode())
        output.console_log("Config.before_run() called!")

    def start_run(self, context: RunnerContext) -> None:
        """Perform any activity required for starting the run here.
        For example, starting the target system to measure.
        Activities after starting the run should also be performed here."""
        llm = context.run_variation['llm']
        language = context.run_variation['language']
        problem = context.run_variation['problem']
        folder_id = f"{llm}_{language}_{problem}"
        """
        Here we first compile and then measure the lines of machine code
        """
        stdin, stdout, stderr = self.ssh_client.exec_command(f"cd {folder_id}")
        output.console_log(stdout.read().decode())
        output.console_log(stderr.read().decode())

        if language == "cpp":
            #compile c++
            compile_command = f"g++ code.cpp -o code"
            stdin, stdout, stderr = self.ssh_client.exec_command(compile_command)
            output.console_log(stdout.read().decode())
            error_output = stderr.read().decode()
            if error_output:
                output.console_log(error_output)
            else:
                output.console_log("C++ file compiled successfully")
        else:
            compile_command = f"ghc -o code code.hs"
            stdin, stdout, stderr = self.ssh_client.exec_command(compile_command)
            output.console_log(stdout.read().decode())
            error_output = stderr.read().decode()
            if error_output:
                output.console_log(error_output)
            else:
                output.console_log("Haskell file compiled successfully")

        objdump_command = f"objdump -d code | wc -l"
        stdin, stdout, stderr = self.ssh_client.exec_command(objdump_command)
        instruction_count = int(stdout.read().decode().strip())
        write_command = f"echo {instruction_count} > instruction_count.txt"
        stdin, stdout, stderr = self.ssh_client.exec_command(write_command)
        output.console_log("Instruction count written to instruction_count.txt")
        output.console_log(f"Instruction count: {instruction_count}")

        output.console_log("Compiled and ready to run!")

    def start_measurement(self, context: RunnerContext) -> None:
        """Perform any activity required for starting measurements."""
        """We are already in the right directory I believe"""
        # llm = context.run_variation['llm']
        # language = context.run_variation['language']
        # problem = context.run_variation['problem']
        # folder_id = f"{llm}_{language}_{problem}"

        run_command = f"./code"
        energibridge_command = f'energibridge --output "energibridge.csv" --summary {run_command}'

        stdin, stdout, stderr = self.ssh_client.exec_command(energibridge_command)
        output.console_log(stdout.read().decode())
        error_output = stderr.read().decode()
        output.console_log("Config.start_measurement() called!")

    def interact(self, context: RunnerContext) -> None:
        """Perform any interaction with the running target system here, or block here until the target finishes."""

        output.console_log("Config.interact() called!")

    def stop_measurement(self, context: RunnerContext) -> None:
        """Perform any activity here required for stopping measurements."""

        output.console_log("Config.stop_measurement called!")

    def stop_run(self, context: RunnerContext) -> None:
        """Perform any activity here required for stopping the run.
        Activities after stopping the run should also be performed here."""

        output.console_log("Config.stop_run() called!")

    def populate_run_data(self, context: RunnerContext) -> Optional[Dict[str, SupportsStr]]:
        """Parse and process any measurement data here.
        You can also store the raw measurement data under `context.run_dir`
        Returns a dictionary with keys `self.run_table_model.data_columns` and their values populated"""
        def read_csv_from_ssh(ssh_client, remote_path):
            sftp_client = ssh_client.open_sftp()
            with sftp_client.file(remote_path, 'r') as remote_file:
                csv_content = remote_file.read().decode('utf-8')
            sftp_client.close()
            return csv_content

        # Path to the CSV file on the remote server
        remote_csv_path = 'energibridge.csv'

        # Read the CSV content from the remote server
        csv_content = read_csv_from_ssh(self.ssh_client, remote_csv_path)

        # Read the CSV content into a pandas DataFrame
        df = pd.read_csv(StringIO(csv_content))
    
        # Calculate energy usage
        energy_usage = round(df['SYSTEM_POWER (Watts)'].sum() * df['Delta'].sum(), 3)
        
        # Calculate average CPU usage
        cpu_usage_columns = [col for col in df.columns if col.startswith('CPU_USAGE_')]
        cpu_usage = round(df[cpu_usage_columns].mean().mean(), 3)
        
        # Calculate average memory usage
        memory_usage = round(df['USED_MEMORY'].mean(), 3)

        instruction_count_file = context.run_dir / "instruction_count.txt"
        if instruction_count_file.exists():
            with open(instruction_count_file, 'r') as file:
                machine_code_size = int(file.read().strip())
        else:
            machine_code_size = 0
        
        # Calculate execution time
        execution_time = round(df['Delta'].sum(), 3)
        output.console_log("Config.populate_run_data() called!")
        run_data = {
            'energy_usage': energy_usage,
            'cpu_usage': cpu_usage,
            'memory_usage': memory_usage,
            'execution_time': execution_time,
            'machine_code_size': machine_code_size
        }
        
        return run_data

    def after_experiment(self) -> None:
        """Perform any activity required after stopping the experiment here
        Invoked only once during the lifetime of the program."""
        if self.ssh_client:
            self.ssh_client.close()
            output.console_log("SSH connection closed")
        output.console_log("Config.after_experiment() called!")

    # ================================ DO NOT ALTER BELOW THIS LINE ================================
    experiment_path:            Path             = None
