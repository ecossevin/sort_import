import os
import sys
import re

"""
This scripts looks for all SUBROUTINE defined in each MODULE. Then, 
it adds the USE MODULE, ONLY : SUB1, SUB2...

subroutines_per_mod = {MODULE1 : [SUBROUTINE1,...], MODULE2 : [SUBROUTINE1,...] }
mod_per_subroutines= {SUBROUTINE1 : MODULE1, SUBROUTINE2: MODULE2 }

The use_to_add contains the module to USE and the ONLY associated : 
use_to_add = {MODULE1 : [CALL2], MODULE2 : [CALL3]}   

1) read all the mode_XX.F90 files and build subroutines_per_mod 
2) read all the mode_XX.F90 files and add the USE


TODO : if a USE statement imports some SUBROUTINE and other type 
than subroutine, the code will be broken. Indeed, the subroutines will
be added, but the other wont be recovered in the import.
"""

#==========================
#   Helper functions  
#==========================
def is_comment(line):
    #ignore commented lines
    return re.match(r'^\s*\!', line)

def open_module(file_path, file_action):

    file_name = os.path.basename(file_path)

    with open(file_path, file_action, encoding='utf-8') as file:
        file_content = file.readlines()
    return(file_name, file_content)

def save_module(lines, file_path):
    with open(file_path, "w") as module_file:
        for line in lines:
            module_file.write(line)
#==========================
#    1)
#==========================
def find_subroutine_declarations(module_name, file_content, sub_map):
# Reads the file and creates a dict { name : subroutine1_name, subroutine2_name ... }
    sub_pattern = rf'SUBROUTINE\s*([A-Za-z0-9_]*)'
    ignore_sub = ["DR_HOOK"] #ignore CALL and SUBROUTINE
    for line in file_content:
        if not is_comment(line):  #ignore commented lines
            if 'SUBROUTINE' in line: 
                sub_name = re.findall(sub_pattern, line)[0]
                if sub_name not in ignore_sub:
                    if sub_name not in sub_map[module_name]:
                        sub_map[module_name].append(sub_name)
    return sub_map  

def find_subroutine_in_module(folder):
# Inspect recursively the folder, find all the files name starting with module_prefix, then find all the SUBROUTINE defined in that module

    node_type = "SUBROUTINE"
    sub_per_mod = {}
    mod_per_sub= {}
    map_module_name = {}
    module_prefix = "mode_"
    for root, folders, files in os.walk(folder):
        for file in files:
            if file.startswith(module_prefix):
                file_path = os.path.join(root, file)
                file_name, lines = open_module(file_path, "r")
                module_name = get_module_name(lines)
                map_module_name[module_name] = file_name #MODULE1 : mode_module1.F90
                sub_per_mod[module_name] = []
                find_subroutine_declarations(module_name, lines, sub_per_mod)
                for subroutine in sub_per_mod[module_name]:
                    mod_per_sub[subroutine] = module_name
    return sub_per_mod, mod_per_sub, map_module_name
#                print(file_path)

#==========================
#    2)
#==========================
def update_use_statements(folder, subroutines_per_mod, mod_per_subroutines):
# Inspect recursively the folder, find all the files name starting with module_prefix, then add the USE statement to each SUBROUTINE of the file

    map_module_name = {}
    module_prefix = "mode_"
    for root, folders, files in os.walk(folder):
        for file in files:
            if file.startswith(module_prefix):
                file_path = os.path.join(root, file)
                file_name, lines = open_module(file_path, "r")
                print("file_name = ", file_name)
#                if file_name == "mode_turb_ver_thermo_corr.F90":
#                    breakpoint()
                module_name = get_module_name(lines)
                process_module(lines, subroutines_per_mod, mod_per_subroutines)
                save_module(lines, file_path)


def process_module(lines, subroutines_per_mod, mod_per_subroutines):
    """
    i)     looks for SUBROUTINE begining idx
    ii)    looks for first USE idx 1) one line, 2) several lines
    iii)   if USE are in subroutines_per_mod : clean them
    iv)    build use_to_add 
    v)     add USE
    use_to_add = {MODULE1 : [CALL2], MODULE2 : [CALL3]}   
    """
    ignore_call = ["DR_HOOK"]
    call_pattern = rf'CALL\s*([A-Za-z0-9_]*)'
    idx = 0
    use_pattern = rf'USE\s*([A-Za-z0-9_]*)'
    in_subroutine = False
    calls_name = []
    while idx<len(lines):
        line = lines[idx]
        if not is_comment(line):
            if "SUBROUTINE" in line:
                if "END SUBROUTINE" not in line:
                    in_subroutine = True
                    has_a_start_index = False 
                    use_to_add = {} #local to each subroutine
                    calls_name = []
                    idx_use = -1
    
        #add all the calls to use_to_add 
        if not is_comment(line):
            if "CALL" in line:
                call_name = re.findall(call_pattern, line)[0]
                if call_name not in calls_name:
                    if call_name in mod_per_subroutines:
                        module_name = mod_per_subroutines[call_name]
                        if module_name not in use_to_add:
                            use_to_add[module_name] = [call_name]
                        else:
                            if call_name not in use_to_add[module_name]:
                                use_to_add[module_name].append(call_name)
        
        #clean the imports
        if not is_comment(line):
            if "USE" in line:
                module_name = re.findall(use_pattern, line)[0]
                if module_name in subroutines_per_mod:
                    if in_subroutine : 
                        if not has_a_start_index:
                            idx_use = idx
                            has_a_start_index = True #we want the first idx of a cleaned USE
                    lines.pop(idx)
                    line = lines[idx]
                    while re.match(r'^\s*&', line): #if line starts with &, we are still in the USE
                        lines.pop(idx)
                        line = lines[idx]
                    idx -= 1
        #at the end of the routine, we insert the new use statements at idx_use
        if not is_comment(line):
            if "END SUBROUTINE" in line:
                in_subroutine = False
                if idx_use != -1:
                    use_lst = insert_use(idx_use, lines, use_to_add)
                    lines[idx_use:idx_use] = use_lst
#                    lines.insert(idx_use, use_lst)
                    idx += len(use_lst)
        idx += 1
    print("***********************")
    print("       NEW  FILE       ")
    print("***********************")
    #for line in lines:
    #    print(line.strip())

def insert_use(idx_use, lines, use_to_add):
    use_lst = []
    for use in use_to_add:
        generate_use(use, use_to_add[use], use_lst)
    return use_lst

def generate_use(use_name, calls, use_lst):
    """
    from use = {MODULE1 : [CALL2, CALL4, CALL7, CALL8]} 
    returns :
    USE MODULE1, ONLY : CALL2, CALL4, CALL7, &
    & CALL8
    with only three CALL per line
    """
    use_string = f"USE {use_name}, ONLY : " 
    #only three subroutine name per line
    for i in range(0, len(calls), 3):
        less_calls = calls[i:i+3]
        less_calls_str = ', '.join(less_calls) + ', & \n'
        if i == 0:
            use_string += less_calls_str
            use_lst.append(use_string)
        else:
            less_calls_str = f"& {less_calls_str}"
            use_lst.append(less_calls_str)
    use_lst[-1] = use_lst[-1][:-5] #remove the lst , & \n
    use_lst[-1] = use_lst[-1] + " \n" #remove the lst , & \n
def get_module_name(lines):
    pattern_module = r'MODULE\s*([A-Za-z0-9_]*)'
    for line in lines:
        if "MODULE" in line:
            if not is_comment(line):
                module_name = re.findall(pattern_module, line)[0]
                return module_name

#==========================
#    1)
#==========================
folder_name = sys.argv[1]
subroutines_per_mod, mod_per_subroutines, map_module_name = find_subroutine_in_module(folder_name)
#print(subroutines_per_mod)

#==========================
#    2)
#==========================
update_use_statements(folder_name, subroutines_per_mod, mod_per_subroutines)

