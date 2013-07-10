========================================
Phase Stepping Fit
========================================

Test if the least squares fit and the fourier component analysis behave
consistently.

Usage
----------------------------------------
to run the comparison 100 times and display the difference between the least squares
and the fourier analysis type:

.. code:: bash

    python comparison.py -n 100`.

Requirements
----------------------------------------

- Python 2.7
- Numpy
- Matplotlib

Test
----------------------------------------
The modules include doctests that can also be run with py.test.
