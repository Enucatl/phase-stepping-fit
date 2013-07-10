#!/usr/bin/env python
# encoding: utf-8

"""Compare the least squares fit of synusoidal functions with the fourier
component analysis.

"""

from __future__ import division, print_function

import numpy as np
from numpy import sin, cos, pi

from commandline_parser import commandline_parser

def cvphi_to_cab(c, v, phi):
    """convert the
    intensity, visibility, phase shift parameters to intensity and two
    angular parameters a, b such that:

    c[1 + v cos(px - phi)] = c + a cos(px) + b sin(px)

    the transformation gives:

    c = c
    a = vc cos(phi)
    b = vc sin(phi)

        >>> from math import pi
        >>> cvphi_to_cab(0, 0, 0)
        (0, 0.0, 0.0)
        >>> cvphi_to_cab(0.5, 1, 0)
        (0.5, 0.5, 0.0)
    """
    a = v * c * cos(phi)
    b = v * c * sin(phi)
    return c, a, b

def phase_stepping_curve(c, v, phi, n):
    """Return the phase stepping curve sampled over one period
    with average c, visibility v, shift phi and n steps.

        >>> phase_stepping_curve(0.5, 1, 0, 4)
        array([ 1. ,  0.5,  0. ,  0.5])
    """
    p = 2 * pi / n #period
    xs = np.arange(n)
    angles = p * xs - phi
    return c * (1 + v * np.cos(angles))

def least_squares_fit(phase_stepping_curve):
    """Fit y = c + a cos(px) + b sin(px) using
    the least squares analytical method.
    see Statistical Methods in Experimental Physics, p. 182, 8.4

    """
    n = phase_stepping_curve.shape[0]
    xs = np.arange(n)
    p = 2 * pi / n #period
    angles = p * xs
    A = np.vstack([1, np.cos(angles), np.sin(angles)]).T
    c, a, b = np.linalg.lstsq(A, phase_stepping_curve)[0]
    return c, a, b

def fourier_fit(phase_stepping_curve):
    """Fit y = c + a cos(px) + b sin(px) using
    the fourier transform.

    """
    transformed = np.fft.rfft(phase_stepping_curve)
    c = transformed[0]
    a = transformed[1].real
    b = transformed[1].imag
    return c, a, b

def main(args):
    pass

if __name__ == '__main__':
    args = commandline_parser.parse_args()
    main(args)