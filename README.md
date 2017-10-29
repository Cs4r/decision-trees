# decision-trees

In this repo I use decision trees to solve several well-known machine learning problems.


## Titanic

### Model 1
![Titanic model 1](titanic-model-1.png)

**Rules**:

Rule 1: (981/146, lift 1.3)
	AGE = Adulto
	SEX = Hombre
	->  class Muere  [0.850]

Rule 2: (681/153, lift 1.1)
	CLASS = 3aa
	->  class Muere  [0.775]

Rule 3: (30, lift 3.0)
	CLASS in {1a, 2a}
	AGE = Ninio
	->  class Sobrevive  [0.969]

Rule 4: (251/17, lift 2.9)
	CLASS in {1a, 2a}
	SEX = Mujer
	->  class Sobrevive  [0.929]


### Model 2
![Titanic model 2](titanic-model-2.png)

**Rules:**

Rule 1: (1150/244, lift 1.2)
	SEX = Hombre
	->  class Muere  [0.787]

Rule 2: (457/113, lift 1.1)
	CLASS = 3aa
	->  class Muere  [0.752]

Rule 3: (188/17, lift 2.8)
	CLASS in {crew, 1a, 2a}
	SEX = Mujer
	->  class Sobrevive  [0.905]

### Model 3
![Titanic model 3](titanic-model-3.png)


*Rules:*

Rule 1: (1163/244, lift 1.2)
	SEX = Hombre
	->  class Muere  [0.790]

Rule 2: (483/127, lift 1.1)
	CLASS = 3aa
	->  class Muere  [0.736]

Rule 3: (178/13, lift 2.9)
	CLASS in {2a, crew, 1a}
	SEX = Mujer
	->  class Sobrevive  [0.922]
 
 
**Note**: *Model 2 and 3 are effectively the same.* 



## Iris

### Model 1
![Iris model 3](iris-model-1.png)

*Rules:*

Rule 1: (33, lift 2.9)
	PetalLengthCm <= 1.9
	->  class Iris-setosa  [0.971]

Rule 2: (66/33, lift 1.5)
	PetalLengthCm > 1.9
	->  class Iris-versicolor  [0.500]

Rule 3: (26, lift 2.9)
	PetalLengthCm > 5
	->  class Iris-virginica  [0.964]

Rule 4: (16, lift 2.8)
	SepalWidthCm <= 2.9
	PetalWidthCm > 1.5
	->  class Iris-virginica  [0.944]

### Model 7
![Iris model 7](iris-model-7.png)


*Rules:*

Rules:

Rule 1: (33, lift 2.9)
	PetalLengthCm <= 1.9
	->  class Iris-setosa  [0.971]

Rule 2: (32/1, lift 2.8)
	PetalLengthCm > 1.9
	PetalLengthCm <= 4.9
	PetalWidthCm <= 1.7
	->  class Iris-versicolor  [0.941]

Rule 3: (28, lift 2.9)
	PetalWidthCm > 1.7
	->  class Iris-virginica  [0.967]

Rule 4: (31/2, lift 2.7)
	PetalLengthCm > 4.9
	->  class Iris-virginica  [0.909]
 

 
