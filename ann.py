import numpy as np
import matplotlib.pyplot as plt

from objectives import cross_entropy
from metrics import accuracy
from utils import *


def sigmoid(H):
	return 1/(1 + np.exp(-H))


def softmax(H):
	eH = np.exp(H)
	return eH/eH.sum(axis = 1, keepdims = True)


def ReLU(H):
	return H*(H > 0)

def Leaky_ReLU(H):
	return H*(H > 0) + 0.01*H*(H < 0)


def derivative(Z, a):
	if a is sigmoid:
		return Z*(1 - Z)
	elif a is np.tanh:
		return 1 - Z*Z
	elif a is ReLU:
		return Z > 0
	elif a is Leaky_ReLU:
		return (Z > 0) + 0.01*(Z < 0)
	else:
		raise exception("No known activation provided.")


class ClassificationANN():
	def __init__(self, hidden_layer_sizes, hidden_activations = None):
		self.hidden_layer_sizes = hidden_layer_sizes
		self.hidden_activations = hidden_activations
		self.L = len(hidden_layer_sizes) + 1

	def forward(self, X):
		self.Z = {0: X}
		for l in sorted(self.a.keys()):
			self.Z[l] = self.a[l](np.matmul(self.Z[l - 1],self.W[l]) + self.b[l])


	def fit(self, X, y, eta = 1e-3, lambda1 = 0, lambda2 = 0, epochs = 1000, batch_sz = 0, mu = 0, show_curve = False):
		N, D = X.shape
		K = len(set(y))

		Y = one_hot_encode(y)

		X, Y, y = shuffle(X, Y, y)


		self.layer_sizes = [D] + self.hidden_layer_sizes + [K]

		self.W = {l + 1: np.random.randn(M[0],M[1]) for l, M in enumerate(zip(self.layer_sizes, self.layer_sizes[1:]))}
		self.b = {l + 1: np.random.randn(M) for l, M in enumerate(self.layer_sizes[1:])}
		self.vW = 0
		self.vb = 0

		if self.hidden_activations is None:
			self.a = {l+1: Leaky_ReLU for l in range(self.L - 1)}
		else:
			self.a = {l+1: act for l, act in enumerate(self.hidden_activations)}

		self.a[self.L] = softmax

		J = []

		for epoch in range(int(epochs)):  # stochastic gradient descent
			if batch_sz == 1:
				for i in range(N):
					x_i = X[[i],:]
					y_i = Y[[i],:]
					self.forward(x_i)
					self.z_i = {0: x_i}
					for l in sorted(self.a.keys()):
						self.z_i[l] = self.a[l](np.matmul(self.z_i[l - 1], self.W[l]) + self.b[l])
					J.append(cross_entropy(y_i, self.z_i[self.L]) + (lambda1)*sum(np.sum(np.abs(W)) for W in self.W.values()) +
							(lambda2/2)*sum(np.sum(W*W) for W in self.W.values()))

					dH = self.z_i[self.L] - y_i

					for l in sorted(self.W.keys(), reverse = True):
						dW = np.matmul(self.z_i[l -1].T, dH) + lambda1*np.sign(self.W[l]) + lambda2*self.W[l]
						self.W[l] -= eta*dW
						self.b[l] -= eta*dH.sum(axis = 0)

						if l > 1:
							dZ = np.matmul(dH, self.W[l].T)
							dH = dZ*derivative(self.Z[l - 1], self.a[l - 1])

			elif batch_sz > 1:  # batch gradient descent
				n_batches = N//batch_sz
				for i in range(n_batches):
					x_b = X[(i*batch_sz):((i + 1)*batch_sz), :]
					y_b = Y[(i*batch_sz):((i + 1)*batch_sz), :]
					self.forward(x_b)
					self.z_b = {0: x_b}
					for l in sorted(self.a.keys()):
						self.z_b[l] = self.a[l](np.matmul(self.z_b[l - 1], self.W[l]) + self.b[l])
					J.append(cross_entropy(y_b, self.z_b[self.L]) + (lambda1)*sum(np.sum(np.abs(W)) for W in self.W.values()) +
							(lambda2/2)*sum(np.sum(W*W) for W in self.W.values()))

					dH = self.z_b[self.L] - y_b

					for l in sorted(self.W.keys(), reverse = True):
						dW = np.matmul(self.z_b[l - 1].T, dH) + lambda1*np.sign(self.W[l]) + lambda2*self.W[l]
						self.W[l] -= eta*dW
						self.b[l] -= eta*dH.sum(axis = 0)

						if l > 1:
							dZ = np.matmul(dH, self.W[l].T)
							dH = dZ*derivative(self.Z[l - 1], self.a[l - 1])

			else: # regular gradient descent
				self.forward(X)
				J.append(cross_entropy(Y,self.Z[self.L]) + (lambda1)*sum(np.sum(np.abs(W)) for W in self.W.values()) +
						(lambda2/2)*sum(np.sum(W*W) for W in self.W.values()))

				dH = self.Z[self.L] - Y

				for l in sorted(self.W.keys(), reverse = True):
					dW = np.matmul(self.Z[l - 1].T,dH) + lambda1*np.sign(self.W[l]) + lambda2*self.W[l]
					#self.vW = mu*self.vW - eta*dW
					#self.vb = mu*self.vb - eta*dH.sum(axis = 0)
					self.W[l] += mu*self.vW - eta*dW
					self.b[l] += mu*self.vb - eta*dH.sum(axis = 0)

					if l > 1:
						dZ = np.matmul(dH, self.W[l].T)
						dH = dZ*derivative(self.Z[l - 1], self.a[l - 1])

		if show_curve:
			plt.plot(J[::100])
			plt.title("Training Curve")
			plt.xlabel("epochs")
			plt.ylabel("J")
			plt.show()


	def predict(self, X):
		self.forward(X)
		return self.Z[self.L].argmax(axis = 1)
