import os
import subprocess
from termcolor import colored

print('\n ------ Testing ------ \n')

total, correct = 0, 0
for in_f in os.listdir('input'):
    if not in_f[6:9].startswith(''): 
        continue

    out_f = in_f.replace('input', 'output')
    in_f = f'.\input\{in_f}'
    exp_f = in_f.replace('input', 'expected')

    print(f'File: {in_f}')

    # execute the file
    subprocess.run(['runghc', 'ws.hs', in_f])

    if os.path.isfile(out_f):
        with open(out_f, 'r') as output, open(exp_f, 'r') as expected:
            expd = expected.readlines()
            recv = output.readlines()

            res = expd == recv
            correct += 1 if res else 0

            res_s = (colored('PASSED', 'green') if res else colored('FAILED', 'red'))
            print(f'{expd}\n{recv}\nEqual? {res_s}\n')
        os.remove(out_f)
    else:
        print('Error encountered! Output file not created.')
    total +=1

print(f'\n --- Results {correct}/{total} ---- \n')
