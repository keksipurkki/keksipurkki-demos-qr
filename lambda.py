#!/usr/bin/env python

import numpy as np

size = 50

m = -2*np.ones([size,size])
m[np.diag_indices(size)] = range(1, size + 1)

expected, vecs = np.linalg.eig(m)
expected = sorted(expected)

actual = np.loadtxt("lambda.txt")

max_err = max(np.abs(expected - actual))

if (max_err > 1e-5):
    print(max_err)
    print("Buu!")
    print(*(expected - actual), sep="\n")
else:
    print("Yeah!")
