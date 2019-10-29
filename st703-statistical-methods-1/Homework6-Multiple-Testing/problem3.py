plant_data = [
	(1, 32.94),
	(1, 35.98),
	(1, 34.76),
	(1, 32.40),
	(2, 30.55),
	(2, 32.64),
	(2, 32.37),
	(2, 32.04),
	(3, 31.23),
	(3, 31.09),
	(3, 30.62),
	(3, 30.42),
	(4, 34.41),
	(4, 34.88),
	(4, 34.07),
	(4, 33.87),
	(5, 35.61),
	(5, 35.00),
	(5, 33.65),
	(5, 32.91)
]


treatment_means = {
	1: 34.02,
	2: 31.90,
	3: 30.84,
	4: 34.31,
	5: 34.29
}

y_total = 0

for i in plant_data:
	y_total = y_total + i[1]

mean = y_total / len(plant_data)

print("The total mean is: " + str(mean))


treatment_ss = 0

for i in plant_data:
	print((treatment_means[i[0]] - mean)**2)
	treatment_ss = treatment_ss + (treatment_means[i[0]] - mean)**2

print("The treatment sum of squares is: " + str(treatment_ss))
