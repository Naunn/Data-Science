from tensorflow.keras import layers, models
from sklearn.preprocessing import LabelEncoder
from keras.utils import np_utils
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.metrics import confusion_matrix, classification_report
from sklearn.model_selection import KFold, cross_val_score
import numpy as np
import pandas as pd

# Load Data ===============================================
raw_data = pd.read_excel("data/some_data.xls")
print(raw_data)

df = pd.read_excel("data/df_splited.xlsx", sheet_name=None)
# In each iteration of the loop, we create a variable with a name corresponding to the current key (sheet name) using locals()
for frame in df.keys():
    locals()[frame] = df[frame]
print(df.keys())

# Prep data =============================================
X, Y = train_data.iloc[:, :-1], train_data.iloc[:, -1].astype(
    "str"
)  # train_data.iloc[:,-1].map({4: 'High', 3: 'Middle', 2: 'Low', 1: 'very_low'})
# encode class values as integers
encoder = LabelEncoder()
encoded_Y = encoder.fit_transform(Y)
encoded_test_Y = encoder.fit_transform(test_data.iloc[:, -1].astype("str"))
# convert integers to dummy variables (i.e. one hot encoded)
dummy_y = np_utils.to_categorical(encoded_Y)
dummy_test_y = np_utils.to_categorical(encoded_test_Y)

# Build model (without cross validation) =============================================
# 5 inputs -> [10 hidden nodes] -> 4 outputs
model = models.Sequential(
    [
        layers.Dense(10, input_dim=X.shape[1], activation="relu"),
        layers.Dense(dummy_y.shape[1], activation="softmax"),
    ]
)

model.compile(
    optimizer="adam",
    loss="categorical_crossentropy",  # "sparse_categorical_crossentropy",
    metrics=["accuracy"],
)

# train
model.fit(train_data.iloc[:, :-1], dummy_y, epochs=250, batch_size=10)

model.evaluate(x=test_data.iloc[:, :-1], y=dummy_test_y)

pred = np.array([np.argmax(_) + 1 for _ in model.predict(test_data.iloc[:, :-1])])
print(confusion_matrix(np.array(test_data.iloc[:, -1]), pred))
print(classification_report(np.array(test_data.iloc[:, -1]), pred))


# Check model with cross validation =============================================
# 5 inputs -> [10 hidden nodes] -> 4 outputs
def baseline_model():
    model_cv = models.Sequential(
        [
            layers.Dense(10, input_dim=X.shape[1], activation="relu"),
            layers.Dense(dummy_y.shape[1], activation="softmax"),
        ]
    )

    model_cv.compile(
        optimizer="adam",
        loss="categorical_crossentropy",  # "sparse_categorical_crossentropy",
        metrics=["accuracy"],
    )
    return model_cv


estimator = KerasClassifier(
    build_fn=baseline_model, epochs=250, batch_size=10, verbose=0
)
kfold = KFold(n_splits=10, shuffle=True)

results = cross_val_score(estimator, X, dummy_y, cv=kfold)
print("Baseline: %.2f%% (%.2f%%)" % (results.mean() * 100, results.std() * 100))
