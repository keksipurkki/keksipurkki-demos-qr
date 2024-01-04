#!/usr/bin/env python

import numpy as np
from sys import argv

actual = np.loadtxt("lambda.txt")
size = np.size(actual)
print(size)

m = -2*np.ones([size,size])
m[np.diag_indices(size)] = range(1, size + 1)

expected, vecs = np.linalg.eig(m)
expected = sorted(expected)

max_err = max(np.abs(expected - actual))

if (max_err > 1e-5):
    print(max_err)
    print("Buu!")
    print(*(expected - actual), sep="\n")
else:
    print("Yeah!")
