DEFAULT_TREATMENT = ["AB", "BA", "BA" , "AB"]
RESPONSES = [0.3, 0.9, 1.0, 0.2, 0.5, 0.7, 0.1, 0.2]

def split(string):
	return [char for char in string]


def which_treatment(switch, index):
	if switch==0:
		return DEFAULT_TREATMENT[index]
	if switch==1:
		return "AB" if DEFAULT_TREATMENT[index] == "BA" else "BA"


total_range = range(0,16)




for i in total_range:

	binary_list = split(bin(i)[2:].zfill(4))

	treatment_list = []
	for j in range(len(binary_list)):
		treatment_list.append( which_treatment(int(binary_list[j]), j))

	# converting list of strings to list of characters
	treatment_list = [i for ele in treatment_list for i in ele]

	A_sum = 0
	B_sum = 0

	for treatment_index in range(len(treatment_list)):
		if treatment_list[treatment_index] == "A":
			A_sum += RESPONSES[treatment_index]
		else:
			B_sum += RESPONSES[treatment_index]

	A_avg = round(A_sum / 4, 3)
	B_avg = round(B_sum / 4, 3)
	diff = round(B_avg - A_avg, 3)

	for i in treatment_list:
		print(i + " & ", end='')

	print(" & " + str(B_avg) + ' & ' + str(A_avg) + ' & ' + str(diff) + ' \\\\')

