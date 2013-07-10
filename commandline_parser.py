"""Options for the phase stepping curve analysis."""

import argparse

commandline_parser = argparse.ArgumentParser(
        description='''Compare the least squares fit with the fourier
        component analysis.
        ''',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
commandline_parser.add_argument('--constant', '-c', 
        type=float, nargs='?', default=10000,
        help='Average number of photons in the phase stepping curve.')
commandline_parser.add_argument('--phase', '-p', 
        type=float, nargs='?', default=1,
        help='Phase shift.')
commandline_parser.add_argument('--visibility', '-v',
        type=float, nargs='?', default=0.5,
        help='Visibility of the system (must be between 0 and 1).')
commandline_parser.add_argument('--steps', '-n',
        type=int, nargs='?', default=10,
        help='Number of phase steps.')
commandline_parser.add_argument('--noise',
        nargs='?', default='zero',
        help='Noise added to the phase steps.')
