========================================
Phase Stepping Fit
========================================

Test if the least squares fit and the fourier component analysis behave
consistently.

Usage
----------------------------------------
To run the comparison 100 times with random input (visibility, average intensity, phase and phase steps)
and display the difference between the least squares
and the fourier analysis type:

.. code:: bash

    python comparison.py -n 100

More details can be shown with

.. code:: bash

    python comparison.py -h

The code for the fits is in the ``phase_stepping_fit.py`` file. This can run
one comparison and print the result. See the help:

.. code:: bash

    python phase_stepping_fit.py -h

Requirements
----------------------------------------

- Python 2.7
- Numpy
- Matplotlib

Test
----------------------------------------
The modules include doctests that can also be run with py.test.
