import matplotlib.pyplot as plt
from sklearn.datasets import load_iris
from sklearn.tree import DecisionTreeClassifier, plot_tree

# Load the IRIS dataset
iris = load_iris()
X, y = iris.data, iris.target

# Create and fit the decision tree classifier
clf = DecisionTreeClassifier(random_state=1,max_depth=5)
clf.fit(X, y)

# Plotting the decision tree
plt.figure(figsize=(20,10))
plot_tree(clf, filled=True, feature_names=iris['feature_names'], class_names=iris['target_names'])
plt.title("Decision Tree for IRIS Dataset")

# Save the plot as a PNG file
plt.savefig('iris_decision_tree.png')

# Display the plot
plt.show()
