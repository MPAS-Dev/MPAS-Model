#!/usr/bin/env python
"""
This script is used to manage regression suites. A regression suite is a set of
test cases that ensure one or more features in a model meet certain criteria.

Using this script one can setup or clean a regression suite.

When setting up a regression suite, this script will generate a script to run
all tests in the suite, and additionally setup each individual test case.

When cleaning a regression suite, this script will remove any generated files
for each individual test case, and the run script that runs all test cases.
"""
# -----import libraries-----
# {{{
from __future__ import absolute_import, division, print_function, \
    unicode_literals

import sys
import os
import fnmatch
import argparse
import xml.etree.ElementTree as ET
import subprocess

# }}}


def process_test_setup(test_tag, config_file, work_dir, model_runtime,
                       regression_script_code, baseline_dir, verbose):
    # {{{
    if verbose:
        stdout = open(work_dir + '/manage_regression_suite.py.out', 'a')
        stderr = stdout
        print('     Script setup outputs to {}'.format(
            work_dir + '/manage_regression_suite.py.out'))
    else:
        dev_null = open('/dev/null', 'a')
        stderr = dev_null
        stdout = dev_null

    # Process test attributes
    try:
        test_name = test_tag.attrib['name']
    except KeyError:
        print("ERROR: <test> tag is missing 'name' attribute.")
        print("Exiting...")
        sys.exit(1)

    try:
        test_core = test_tag.attrib['core']
    except KeyError:
        print("ERROR: <test> tag with name '{}' is missing 'core' "
              "attribute.".format(test_name))
        print("Exiting...")
        sys.exit(1)

    try:
        test_configuration = test_tag.attrib['configuration']
    except KeyError:
        print("ERROR: <test> tag with name '{}' is missing 'configuration' "
              "attribute.".format(test_name))
        print("Exiting...")
        sys.exit(1)

    try:
        test_resolution = test_tag.attrib['resolution']
    except KeyError:
        print("ERROR: <test> tag with name '{}' is missing 'resolution' "
              "attribute.".format(test_name))
        print("Exiting...")
        sys.exit(1)

    try:
        test_test = test_tag.attrib['test']
    except KeyError:
        print("ERROR: <test> tag with name '{}' is missing 'test' "
              "attribute.".format(test_name))
        print("Exiting...")
        sys.exit(1)

    # Determine the file name for the test case output
    case_output_name = test_name.replace(' ', '_')

    # Setup test case

    if baseline_dir == 'NONE':
        subprocess.check_call(
            ['./setup_testcase.py', '-q', '-f', config_file,
             '--work_dir', work_dir, '-o', test_core, '-c', test_configuration,
             '-r', test_resolution, '-t', test_test, '-m', model_runtime],
            stdout=stdout, stderr=stderr)
    else:
        subprocess.check_call(
            ['./setup_testcase.py', '-q', '-f', config_file,
             '--work_dir', work_dir, '-o', test_core, '-c', test_configuration,
             '-r', test_resolution, '-t', test_test, '-m', model_runtime,
             '-b', baseline_dir], stdout=stdout, stderr=stderr)

    print("   -- Setup case '{}': -o {} -c {} -r {} -t {}".format(
        test_name, test_core, test_configuration, test_resolution, test_test))

    # Write step into suite script to cd into the base of the regression suite
    regression_script_code += "os.chdir(base_path)\n"

    # Write the step to define the output file
    regression_script_code += "case_output = open('case_outputs/{}', 'w')\n".format(
        case_output_name)

    # Write step to cd into test case directory
    regression_script_code += "os.chdir('{}/{}/{}/{}')\n".format(
        test_core, test_configuration, test_resolution, test_test)

    for script in test_tag:
        # Process test case script
        if script.tag == 'script':
            try:
                script_name = script.attrib['name']
            except KeyError:
                print("ERROR: <script> tag is missing 'name' attribute.")
                print('Exiting...')
                sys.exit(1)

            command = "subprocess.check_call(['time', '-p', " \
                      "'{}/{}/{}/{}/{}/{}']".format(
                          work_dir, test_core, test_configuration,
                          test_resolution, test_test, script_name)
            command = '{}, stdout=case_output, stderr=case_output)'.format(
                command)
            # Write test case run step
            regression_script_code += "print(' ** Running case {}')\n".format(
                test_name)
            regression_script_code += 'try:\n'
            regression_script_code += '    {}\n'.format(command)
            regression_script_code += "    print('      PASS')\n"
            regression_script_code += 'except subprocess.CalledProcessError:\n'
            regression_script_code += "    print('   ** FAIL (See case_outputs/{} for more information)')\n".format(
                case_output_name)
            regression_script_code += "    test_failed = True\n"

    # Finish writing test case output
    regression_script_code += "case_output.close()\n"
    regression_script_code += "\n"
    if verbose:
        stdout.close()
    else:
        dev_null.close()

    return regression_script_code
    # }}}


def process_test_clean(test_tag, work_dir):
    # {{{
    dev_null = open('/dev/null', 'a')
    # Process test attributes
    try:
        test_name = test_tag.attrib['name']
    except KeyError:
        print("ERROR: <test> tag is missing 'name' attribute.")
        print("Exiting...")
        sys.exit(1)

    try:
        test_core = test_tag.attrib['core']
    except KeyError:
        print("ERROR: <test> tag with name '{}' is missing 'core' "
              "attribute.".format(test_name))
        print("Exiting...")
        sys.exit(1)

    try:
        test_configuration = test_tag.attrib['configuration']
    except KeyError:
        print("ERROR: <test> tag with name '{}' is missing 'configuration' "
              "attribute.".format(test_name))
        print("Exiting...")
        sys.exit(1)

    try:
        test_resolution = test_tag.attrib['resolution']
    except KeyError:
        print("ERROR: <test> tag with name '{}' is missing 'resolution' "
              "attribute.".format(test_name))
        print("Exiting...")
        sys.exit(1)

    try:
        test_test = test_tag.attrib['test']
    except KeyError:
        print("ERROR: <test> tag with name '{}' is missing 'test' "
              "attribute.".format(test_name))
        print("Exiting...")
        sys.exit(1)

    # Clean test case
    subprocess.check_call(
        ['./clean_testcase.py', '-q', '--work_dir', work_dir, '-o', test_core,
         '-c', test_configuration, '-r', test_resolution, '-t', test_test],
        stdout=dev_null, stderr=dev_null)

    print("   -- Cleaned case '{}': -o {} -c {} -r {} -t {}".format(
        test_name, test_core, test_configuration, test_resolution, test_test))

    dev_null.close()
    # }}}


def local_parallel_setup_script(
    args, suite_tag, testcase_data, testcase_data_prereq, data_dic):
    # {{{
    work_dir = args.work_dir
    verbose = args.verbose
    suite_root = suite_tag
    try:
        suite_name = suite_tag.attrib['name']
    except KeyError:
        print("ERROR: <regression_suite> tag is missing 'name' attribute.")
        print('Exiting...')
        sys.exit(1)

    if not os.path.exists('{}'.format(work_dir)):
        os.makedirs('{}'.format(work_dir))

    if verbose:
        # flush existing regression suite output file
        open(work_dir + '/manage_regression_suite.py.out', 'w').close()

    regression_script_name = '{}/parallel_{}.py'.format(work_dir, suite_name)
    local_parallel_script = open('{}'.format(regression_script_name), 'w')
    local_parallel_code = write_regression_local_parallel_top(
        work_dir, suite_tag, args.nodes, data_dic)


    dev_null = open('/dev/null', 'a')
    subprocess.check_call(['chmod',
                           'a+x',
                           '{}'.format(regression_script_name)],
                           stdout=dev_null,
                           stderr=dev_null)
    dev_null.close()


    setup_suite(
        suite_root,
        args.work_dir,
        args.model_runtime,
        args.config_file,
        args.baseline_dir,
        args.verbose,
        args,
        data_dic)

    local_parallel_code = write_regression_local_parallel_testcase_data(
        local_parallel_code, work_dir, testcase_data,  testcase_data_prereq, data_dic)

    local_parallel_code = write_regression_local_parallel_bottom(
        local_parallel_code)

    return local_parallel_script, local_parallel_code
    # }}}


def get_prereq(testcase_data, testcase_data_prereq, data_dic):
    # {{{
    names = [i for i in testcase_data.keys() if i in testcase_data_prereq]
    cwd = os.getcwd() + "/"
    for i in names:
        command = ['time', '-p', str(cwd)+str(testcase_data[i]['path'])]
        data_dic['procs'].insert(0, testcase_data[i]['procs'])
        data_dic['prereq_commands'].insert(0,[testcase_data[i]['path'], command])

    return data_dic
    # }}}


def get_testcase_data(testcase_data, testcase_data_prereq, data_dic):
    # {{{
    names = [i for i in testcase_data.keys() if i not in testcase_data_prereq]
    cwd = os.getcwd() + "/"
    for i in names:
        command = ['time', '-p', str(cwd)+str(testcase_data[i]['path'])]
        data_dic['procs'].append(testcase_data[i]['procs'])
        data_dic['allocation_commands'].append([testcase_data[i]['path'], command])
    return data_dic 
    # }}}


def write_regression_local_parallel_testcase_data(local_parallel_code, work_dir, testcase_data, testcase_data_prereq, data_dic):
        # {{{
    index = 0
    data_dic = get_testcase_data(  testcase_data , testcase_data_prereq, data_dic)
    data_dic = get_prereq(testcase_data , testcase_data_prereq, data_dic)

    local_parallel_code += "data_dic = {}\n"
    for key in data_dic.keys():
        local_parallel_code += "data_dic['"+str(key)+"'] = []\n"

    for key in data_dic.keys():
        for value in data_dic[key]:
            if type(value) == list:
                local_parallel_code += "data_dic['"+str(key)+"'].append(" + str(value) + ")\n"
            else:
                local_parallel_code += "data_dic['"+str(key)+"'].append('"+ str(value) +"')\n"
        local_parallel_code += "\n\n"

    return local_parallel_code
    # }}}


def write_regression_local_parallel_bottom(local_parallel_code):
    # {{{
    local_parallel_code += "if max(procs) > number_of_procs:\n"
    local_parallel_code += "  print('more processors are used than allocated')\n"
    local_parallel_code += "  quit()\n"
    local_parallel_code += "start_time = time.time()\n"
    local_parallel_code += "def stream_queue(command, number_of_procs):\n"
    local_parallel_code += "  if command == []:\n"
    local_parallel_code += "    return \n"
    local_parallel_code += "  base = os.getcwd()\n"
    local_parallel_code += "  Queue_running = []\n"
    local_parallel_code += "  index = 0\n"
    local_parallel_code += "  continue_add = True\n"
    local_parallel_code += "  Done = False\n"
    local_parallel_code += "  while True:\n"
    local_parallel_code += "     if continue_add and not Done:\n"
    local_parallel_code += "      if number_of_procs >= procs[index]:\n"
    local_parallel_code += "        try:\n"
    local_parallel_code += "          if index < len(command):\n"
    local_parallel_code += "#            print('-----IN ADDING PHASE-----')\n"
    local_parallel_code += "#            print('we have {} number_of_procs, current task uses {} Adding to queue'.format(number_of_procs, procs[index]))\n"
    local_parallel_code += "            case_output = open('case_outputs/'+command[index][0].replace('/', '_'), 'w+')\n"
    local_parallel_code += "#            print('processing command: {}'.format(command[index][1]))\n"
    local_parallel_code += "            os.chdir(command[index][0])\n"
    local_parallel_code += "            open_proc = subprocess.Popen(command[index][1], stdout=case_output, stderr=case_output)\n"
    local_parallel_code += "            os.chdir(base)\n"
    local_parallel_code += "            Queue_running.append([open_proc, procs[index],command[index][1]]) \n"
    local_parallel_code += "            number_of_procs = number_of_procs - procs[index]\n"
    local_parallel_code += "#            print('       New number_of_procs: {}'.format(number_of_procs)) \n"
    local_parallel_code += "        except subprocess.CalledProcessError: \n"
    local_parallel_code += "          print('error for : {}'.format(str(command[index][1]))) \n"
    local_parallel_code += "      elif number_of_procs < procs[index]: \n"
    local_parallel_code += "#        print('NOT ENOUGH WAIT FOR PROCESSES') \n"
    local_parallel_code += "        continue_add = False \n"
    local_parallel_code += "      index = index + 1 \n"
    local_parallel_code += "      if index > len(procs) -1: \n"
    local_parallel_code += "#        print('No more to add moving to processing phase') \n"
    local_parallel_code += "        continue_add = False \n"
    local_parallel_code += "     elif not continue_add and not Done: \n"
    local_parallel_code += "       for background_process in Queue_running: \n"
    local_parallel_code += "         background_process[0].wait() \n"
    local_parallel_code += "         pid =background_process[0].pid \n"
    local_parallel_code += "         if not psutil.pid_exists(pid): \n"
    local_parallel_code += "           Queue_running.remove(background_process) \n"
    local_parallel_code += "           name = background_process[-1][-1]\n"
    local_parallel_code += "           name = name[name.find('compass')+len('compass')+1:-3].replace('/','_')\n" 
    local_parallel_code += "           print('compleate\t' + str(name))\n"
    local_parallel_code += "           number_of_procs += background_process[1] \n"
    local_parallel_code += "           if index < len(procs)-1: \n"
    local_parallel_code += "#             print('STILL HAVE MORE TO PROCESS') \n"
    local_parallel_code += "             if number_of_procs >= procs[index]: \n"
    local_parallel_code += "               continue_add = True \n"
    local_parallel_code += "           else: \n"
    local_parallel_code += "             if len(Queue_running) == 0: \n"
    local_parallel_code += "               Done = True \n"
    local_parallel_code += "               continue_add = False \n"
    local_parallel_code += "     elif Done and not continue_add: \n"
    local_parallel_code += "      return \n"
    local_parallel_code += " \n"
    local_parallel_code += "  return\n"
    local_parallel_code += "print('--- PROCESSING PREREQS ---')\n"
    local_parallel_code += "stream_queue(prereq_commands, number_of_procs)\n"
    local_parallel_code += "print('--- PROCESSING commands ---')\n"
    local_parallel_code += "stream_queue(allocation_commands, number_of_procs)\n"
    local_parallel_code += "end_time = time.time()\n"
    local_parallel_code += "print('parallel run time: {} min'.format((end_time - start_time) / 60))\n"
    local_parallel_code = write_regression_script_testcase_data_bottom(
        local_parallel_code)

    return local_parallel_code
    # }}}


def write_regression_local_parallel_top(work_dir, suite_tag, nodes, data_dic):
    # {{{
    local_parallel_code = "#!/usr/bin/env python\n\n\n"
    local_parallel_code += "import time\n"
    local_parallel_code += "import psutil\n"
    local_parallel_code += "import sys\n"
    local_parallel_code += "import os\n"
    local_parallel_code += "import subprocess\n"
    local_parallel_code += "import numpy as np\n"
    local_parallel_code += "import multiprocessing as mp\n"
    local_parallel_code += "out, err = subprocess.Popen(['nproc'],stdout=subprocess.PIPE, stderr=subprocess.STDOUT).communicate()\n"
    local_parallel_code += "nodes = {}\n".format(nodes)
    local_parallel_code += "number_of_procs = nodes * int(out.split()[0])\n"
    local_parallel_code += "print('Number of usable Processors: {}'.format(number_of_procs))\n"
    local_parallel_code += "os.environ['PYTHONUNBUFFERED'] = '1'\n"
    local_parallel_code += "test_failed=False\n"
    local_parallel_code += "if not os.path.exists('case_outputs'):\n"
    local_parallel_code += "    os.makedirs('case_outputs')\n"
    local_parallel_code += "base_path = '" + work_dir + "'\n"
    local_parallel_code += "os.chdir(base_path)\n"
    local_parallel_code += "locations = []\n"
    local_parallel_code += "procs = []\n"
    local_parallel_code += "run_file_names = []\n"
    local_parallel_code += "prereq_commands = []\n"
    local_parallel_code += "allocation_commands = []\n"
    local_parallel_code += "commands = []\n\n\n"

    for child in suite_tag:
        for script in child:
            script_name = script.attrib['name']
            data_dic['run_file_names'].append(script_name)

    local_parallel_code += "\n\n\n"
    return local_parallel_code
    # }}}


def write_regression_script_testcase_data_bottom(regression_script_code):
    # {{{
    regression_script_code += "print('\\n\\n\\nTEST RUNTIMES & ERROR:')\n"
    regression_script_code += "case_output = '/case_outputs/'\n"
    regression_script_code += "totaltime = 0\n"
    regression_script_code += "for _, _, files in os.walk(base_path + case_output):\n"
    regression_script_code += "    for afile in sorted(files):\n"
    regression_script_code += "        outputfile = base_path + case_output + afile\n"
    regression_script_code += "        runtime = np.ceil(float(subprocess. check_output(\n  ['grep', 'real', outputfile])."
    regression_script_code += "decode('utf-8').split('\\n')[-2].split()[1]))\n"
    regression_script_code += "        totaltime += runtime\n"
    regression_script_code += "        mins = int(np.floor(runtime/60.0))\n"
    regression_script_code += "        secs = int(np.ceil(runtime - mins*60))\n"
    regression_script_code += "        print('{:02d}:{:02d} {}'.format(mins, secs, afile))\n"
    regression_script_code += "        error_out = subprocess.call(['grep', 'Error', outputfile], stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)\n"
    regression_script_code += "        if error_out != 1:\n"
    regression_script_code += "            print('\terror found in: {} '.format(afile))\n"
    regression_script_code += "mins = int(np.floor(totaltime/60.0))\n"
    regression_script_code += "secs = int(np.ceil(totaltime - mins*60))\n"
    regression_script_code += "print('Total runtime {:02d}:{:02d}'.format(mins, secs))\n"
    regression_script_code += "\n"
    regression_script_code += "if test_failed:\n"
    regression_script_code += "    sys.exit(1)\n"
    regression_script_code += "else:\n"
    regression_script_code += "    sys.exit(0)\n"

    return regression_script_code
    # }}}


def write_regression_script_testcase_data_top(work_dir):
    # {{{
    regression_script_code = ""
    regression_script_code += '#!/usr/bin/env python\n'
    regression_script_code += '\n'
    regression_script_code += '# This script was written by '
    regression_script_code += 'manage_regression_suite.py as part of a\n'
    regression_script_code += '# regression_suite file\n'
    regression_script_code += '\n'
    regression_script_code += 'import sys\n'
    regression_script_code += 'import os\n'
    regression_script_code += 'import subprocess\n'
    regression_script_code += 'import numpy as np\n'
    regression_script_code += '\n'
    regression_script_code += "os.environ['PYTHONUNBUFFERED'] = '1'\n"
    regression_script_code += "test_failed = False\n"
    regression_script_code += '\n'
    regression_script_code += "if not os.path.exists('case_outputs'):\n"
    regression_script_code += "    os.makedirs('case_outputs')\n"
    regression_script_code += '\n'
    regression_script_code += "base_path = '{}'\n".format(work_dir)

    return regression_script_code
    # }}}


def setup_suite(suite_tag, work_dir, model_runtime, config_file, baseline_dir,
                verbose, args, data_dic):
    # {{{
    try:
        suite_name = suite_tag.attrib['name']
    except KeyError:
        print("ERROR: <regression_suite> tag is missing 'name' attribute.")
        print('Exiting...')
        sys.exit(1)

    if not os.path.exists('{}'.format(work_dir)):
        os.makedirs('{}'.format(work_dir))

    if verbose:
        # flush existing regression suite output file
        open(work_dir + '/manage_regression_suite.py.out', 'w').close()

    regression_script_code = ""

    # Create regression suite run script
    regression_script_name = '{}/{}.py'.format(work_dir, suite_name)
    regression_script = open('{}'.format(regression_script_name), 'w')

    regression_script_code = write_regression_script_testcase_data_top(work_dir)

    for child in suite_tag:
        # Process <test> tags within the test suite
        if child.tag == 'test':
            regression_script_code = process_test_setup(
                child,
                config_file,
                work_dir,
                model_runtime,
                regression_script_code,
                baseline_dir,
                verbose)

    dev_null = open('/dev/null', 'a')
    subprocess.check_call(['chmod',
                           'a+x',
                           '{}'.format(regression_script_name)],
                           stdout=dev_null,
                           stderr=dev_null)
    dev_null.close()
    if not args.local_parallel:

        regression_script_code = write_regression_script_testcase_data_bottom(
            regression_script_code)

        return regression_script, regression_script_code
    # }}}


def clean_suite(suite_tag, work_dir):
    # {{{
    try:
        suite_name = suite_tag.attrib['name']
    except KeyError:
        print("ERROR: <regression_suite> tag is missing 'name' attribute.")
        print('Exiting...')
        sys.exit(1)

    # Remove the regression suite script, if it exists
    regression_script = '{}/{}.py'.format(work_dir, suite_name)
    if os.path.exists(regression_script):
        os.remove(regression_script)

    for child in suite_tag:
        # Process <test> children within the <regression_suite>
        if child.tag == 'test':
            process_test_clean(child, work_dir)
    # }}}


def get_test_case_procs(suite_tag, testcase_data, testcase_data_prereq):
    # {{{
    for child in suite_tag:
        if child.tag == 'test':
            try:
                test_name = child.attrib['name']
            except KeyError:
                print("<test> tag is missing a 'name' attribute")
                print("Exiting...")
                sys.exit(1)

            try:
                test_core = child.attrib['core']
            except KeyError:
                print("<test> tag named '{}' is missing a 'core' "
                      "attribute".format(test_name))
                print("Exiting...")
                sys.exit(1)

            try:
                test_configuration = child.attrib['configuration']
            except KeyError:
                print("<test> tag named '{}' is missing a 'configuration' "
                      "attribute".format(test_name))
                print("Exiting...")
                sys.exit(1)

            try:
                test_resolution = child.attrib['resolution']
            except KeyError:
                print("<test> tag named '{}' is missing a 'resolution' "
                      "attribute".format(test_name))
                print("Exiting...")
                sys.exit(1)

            try:
                test_test = child.attrib['test']
            except KeyError:
                print("<test> tag named '{}' is missing a 'test' "
                      "attribute".format(test_name))
                print("Exiting...")
                sys.exit(1)

            test_path = '{}/{}/{}/{}'.format(test_core, test_configuration,
                                             test_resolution, test_test)
            driver_path = '{}/config_driver.xml'.format(test_path)
            config_tree = ET.parse(driver_path)
            config_root = config_tree.getroot()

            cases = []
            assert(config_root.tag == 'driver_script')
            for case in config_root.iter('case'):
                name = case.attrib['name']
                cases.append(name)

            prereqs = list()
            for config_prereq in config_root.iter('prerequisite'):
                prereq = dict()
                for tag in ['core', 'configuration', 'resolution', 'test']:
                    prereq[tag] = config_prereq.attrib[tag]

                # Make sure the prerequisite is already in the test suite
                found = False
                for other_name, other_test in testcase_data.items():
                    match = [prereq[tag] == other_test[tag] for tag in
                             ['core', 'configuration', 'resolution', 'test']]
                    if all(match):
                        found = True
                        prereq['name'] = other_name
                        break

                if not found:
                    raise ValueError(
                        'Prerequisite of {} does not precede it in the test '
                        'suite: {} {} {} {}'.format(
                            test_name, prereq['core'], prereq['configuration'],
                            prereq['resolution'], prereq['test']))

                prereqs.append(prereq)

            del config_root
            del config_tree

            procs = 1
            threads = 1
            # Loop over all files in test_path that have the .xml extension.
            for file in os.listdir('{}'.format(test_path)):
                if fnmatch.fnmatch(file, '*.xml'):
                    # Build full file name
                    config_file = '{}/{}'.format(test_path, file)
                    config_tree = ET.parse(config_file)
                    config_root = config_tree.getroot()
                    if config_root.tag == 'config':
                        case = config_root.attrib['case']
                        if case in cases:
                            for model_run in config_root.iter('model_run'):
                                try:
                                    procs_str = model_run.attrib['procs']
                                    procs = int(procs_str)
                                except (KeyError, ValueError):
                                    procs = 1

                                try:
                                    threads_str = model_run.attrib['threads']
                                    threads = int(threads_str)
                                except (KeyError, ValueError):
                                    threads = 1
                    del config_root
                    del config_tree
            testcase_data[test_name] = {'core': test_core,
                               'configuration': test_configuration,
                               'resolution': test_resolution,
                               'test': test_test,
                               'path': test_path,
                               'procs': procs,
                               'threads': threads,
                               'prereqs': prereqs}
            for sub in testcase_data[test_name]['prereqs']:
                name = sub['name']
                if name not in testcase_data_prereq:
                    testcase_data_prereq.append(name)
    return testcase_data , testcase_data_prereq
    # }}}


def summarize_suite(testcase_data):
    # {{{

    max_procs = 1
    max_threads = 1
    max_cores = 1
    for name in testcase_data:
        procs = testcase_data[name]['procs']
        threads = testcase_data[name]['threads']
        cores = threads * procs

        max_procs = max(max_procs, procs)
        max_threads = max(max_threads, threads)
        max_cores = max(max_cores, cores)

    print("\n")
    print(" Summary of test cases:")
    print("      Maximum MPI tasks used: {:d}".format(max_procs))
    print("      Maximum OpenMP threads used: {:d}".format(max_threads))
    print("      Maximum Total Cores used: {:d}".format(max_cores))
    return testcase_data
    # }}}


def main():
    # {{{
    # Define and process input arguments
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("-t", "--test_suite", dest="test_suite",
                        help="Path to file containing a test suite to setup",
                        metavar="FILE", required=True)
    parser.add_argument("-f", "--config_file", dest="config_file",
                        help="Configuration file for test case setup",
                        metavar="FILE")
    parser.add_argument("-s", "--setup", dest="setup",
                        help="Option to determine if regression suite should "
                             "be setup or not.", action="store_true")
    parser.add_argument("-c", "--clean", dest="clean",
                        help="Option to determine if regression suite should "
                             "be cleaned or not.", action="store_true")
    parser.add_argument("-v", "--verbose", dest="verbose",
                        help="Use verbose output from setup_testcase.py",
                        action="store_true")
    parser.add_argument("-m", "--model_runtime", dest="model_runtime",
                        help="Definition of how to build model run commands "
                             "on this machine", metavar="FILE")
    parser.add_argument("-b", "--baseline_dir", dest="baseline_dir",
                        help="Location of baseslines that can be compared to",
                        metavar="PATH")
    parser.add_argument("--work_dir", dest="work_dir",
                        help="If set, script will setup the test suite in "
                        "work_dir rather in this script's location.",
                        metavar="PATH")
    parser.add_argument(
        "-lp",
        "--local_parallel",
        help="If set, script will setup the test suite to "
        "run in parallel on an interactive allocated node",
        action="store_true")
    parser.add_argument('-N', '--nodes', type=int, default=1,
                        help="Number of allocated Nodes; default to one")

    args = parser.parse_args()

    if not args.config_file:
        print("WARNING: Not configuration file specified. Using the default "
              "of 'local.config'")
        args.config_file = 'local.config'

    if not os.path.exists(args.config_file):
        parser.error("Configuration file '{}' does not exist. Please create "
                     "and setup before running again.".format(
                         args.config_file))

    if not args.work_dir:
        args.work_dir = os.path.dirname(os.path.realpath(__file__))

    args.work_dir = os.path.abspath(args.work_dir)

    if not args.model_runtime:
        args.model_runtime = '{}/runtime_definitions/mpirun.xml'.format(
            os.path.dirname(os.path.realpath(__file__)))
        print('WARNING: No model runtime specified. Using the default of '
              '{}'.format(args.model_runtime))

    if not args.baseline_dir:
        args.baseline_dir = 'NONE'

    if not args.setup and not args.clean:
        print('WARNING: Neither the setup (-s/--setup) nor the clean '
              '(-c/--clean) flags were provided. Script will perform no '
              'actions.')

    write_history = False

    # Parse regression_suite file
    suite_tree = ET.parse(args.test_suite)
    suite_root = suite_tree.getroot()

    # If the file was a <regression_suite> file, process it
    if suite_root.tag == 'regression_suite':
        testcase_data = {}
        # If cleaning, clean the suite
        if args.clean:
            print("Cleaning Test Cases:")
            clean_suite(suite_root, args.work_dir)
            write_history = True
        # If setting up, set up the suite
        if args.setup:
            print("\n")
            testcase_data_prereq = []
            data_dic = {}
            testcase_data, testcase_data_prereq = get_test_case_procs(suite_root, testcase_data, testcase_data_prereq)
            testcase_data = summarize_suite(testcase_data)
            print("\n\nSetting Up Test Cases:")
            if args.local_parallel:
                data_dic["procs"] = []
                data_dic["run_file_names"] = []
                data_dic["prereq_commands"] = []
                data_dic["allocation_commands"] =[]
                print("--------")
                local_parallel_script, local_parallel_code = local_parallel_setup_script(args, suite_root, testcase_data, testcase_data_prereq, data_dic) 
                print("not here")
                local_parallel_script.write(local_parallel_code)
            else:
                regression_script, regression_script_code = setup_suite(
                    suite_root, args.work_dir, args.model_runtime,
                    args.config_file, args.baseline_dir, args.verbose, args)
                regression_script.write(regression_script_code)
            if args.verbose:
                cmd = ['cat',
                       args.work_dir + '/manage_regression_suite.py.out']
                print('\nCase setup output:')
                print(subprocess.check_output(cmd).decode('utf-8'))
            write_history = True

    # Write the history of this command to the command_history file, for
    # provenance.
    if write_history:
        # Build variables for history output
        old_dir = os.getcwd()
        os.chdir(os.path.dirname(os.path.realpath(__file__)))
        git_version = subprocess.check_output(
            ['git', 'describe', '--tags', '--dirty'])
        git_version = git_version.decode('utf-8').strip('\n')
        os.chdir(old_dir)
        calling_command = ""
        for arg in sys.argv:
            calling_command = "{}{} ".format(calling_command, arg)

        history_file_path = '{}/command_history'.format(args.work_dir)
        if os.path.exists(history_file_path):
            history_file = open(history_file_path, 'a')
            history_file.write('\n')
        else:
            history_file = open(history_file_path, 'w')

        history_file.write('**************************************************'
                           '*********************\n')
        history_file.write('git_version: {}\n'.format(git_version))
        history_file.write('command: {}\n'.format(calling_command))
        history_file.write('**************************************************'
                           '*********************\n')
        history_file.close()
    # }}}


if __name__ == "__main__":
    main()


# vim: foldmethod=marker ai ts=4 sts=4 et sw=4 ft=python
