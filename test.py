import numpy as np
import matplotlib.pyplot as plt

from ann import *
from metrics import *


def main():
	D = 2
	K = 3
	N = int(K*1.5e4)

	X0 = np.random.randn((N//K),D) + np.array([3,2])
	X1 = np.random.randn((N//K),D) + np.array([1,-3])
	X2 = np.random.randn((N//K),D) + np.array([-1,2])
	X = np.vstack((X0, X1, X2))

	y = np.array([0]*(N//K) + [1]*(N//K) + [2]*(N//K))

	plt.scatter(X[:,0], X[:,1], c = y, alpha = 0.5)
	plt.show()

	nn = ClassificationANN([50])
	nn.fit(X,y, eta = 1e-5, epochs = 750, mu = 0, show_curve = True)
	y_hat = nn.predict(X)

	print("Accuracy: {}".format(accuracy(y,y_hat)))


if __name__ == "__main__":
	main()