#! /usr/bin/env python3
from subprocess import check_output

def get_pass(account):
    return check_output("pass show " + account, shell=True).splitlines()[0].decode()

if __name__ == "__main__":
    import sys
    print(get_pass(sys.argv[1]))
