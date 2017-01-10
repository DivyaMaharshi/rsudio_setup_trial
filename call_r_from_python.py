# run_max.py
import subprocess

# Define command and arguments
command = 'Rscript'
path2script = '/home/igp/rsudio_setup_trial/testing.R'

# Variable number of args in a list
args = ['121','216968']

# Build subprocess command
cmd = [command, path2script] + args

# check_output will run the command and store to result
x = subprocess.check_output(cmd, universal_newlines=True)

print(x)
