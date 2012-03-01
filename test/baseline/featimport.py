import os

def check_path(path_value):
    return os.path.isfile(str(path_value))
