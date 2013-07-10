#!/usr/bin/env python
# encoding: utf-8

"""Run the fits many times with random parameters and plot a summary of the
differences.

"""

from __future__ import division, print_function

from numpy import pi
import numpy as np
import matplotlib.pyplot as plt
import random
from commandline_parser import commandline_parser as fit_parser
from phase_stepping_fit import main as fit_main
from progress_bar import progress_bar


def main(args):
    tries = args.n
    differences = []
    for i in xrange(tries):
        print(progress_bar((i + 1) / tries), end="\r")
        constant = random.uniform(100, 100000)
        phase = random.uniform(-pi / 2, pi/2)
        visibility = random.uniform(0, 1)
        steps = random.randint(4, 24)
        fit_args = fit_parser.parse_args(
                """-c{0} -p{1} -v {2} -n{3}""".format(
                    constant, phase,
                    visibility, steps).split())
        original_pars, least_squares_pars, fourier_pars = fit_main(fit_args)
        differences.append(np.sum(
            np.sqrt((np.array(least_squares_pars) -
                np.array(fourier_pars))**2)))
    print()
    plt.figure()
    plt.plot(np.arange(tries), differences)
    plt.ylabel("lst - fourier")
    plt.xlabel("run number")
    plt.ion()
    plt.show()
    try:
        raw_input("Press ENTER to quit")
    except KeyboardInterrupt:
        pass

if __name__ == '__main__':
    import argparse
    commandline_parser = argparse.ArgumentParser(description=__doc__,
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    commandline_parser.add_argument('-n',
            type=int, nargs='?', default=10000,
            help='number of comparisons')
    args = commandline_parser.parse_args()
    main(args)
