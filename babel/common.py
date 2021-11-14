import numpy as np
import matplotlib.pyplot as plt

def plot_2d(X, xlim=3, ylim=3):
    fig, ax = plt.subplots(figsize=(6,6))
    fig.patch.set_facecolor("#add8e6")

    plt.scatter(X[0,:], X[1,:])
    ax.set_xlim([-xlim, xlim])
    ax.set_ylim([-ylim, ylim])

    plt.show()


def lol():
    return "xd"
