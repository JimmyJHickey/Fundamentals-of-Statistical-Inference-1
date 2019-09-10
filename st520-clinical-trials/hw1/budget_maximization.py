PROPORTION_N11 = 0.006
PROPORTION_N12 = 0.495
PROPORTION_N21 = 0.004
PROPORTION_N22 = 0.495

COST_CASE = 4
COST_CONTROL = 1
BUDGET = 400


def calculate_variance_case_control(num_case, num_control):
	return 1/( PROPORTION_N11/(PROPORTION_N11 + PROPORTION_N21) * num_case) + 1/( PROPORTION_N21/(PROPORTION_N11 + PROPORTION_N21) * num_case) + 1/( PROPORTION_N12/(PROPORTION_N12 + PROPORTION_N22) * num_control) + 1/(PROPORTION_N22/(PROPORTION_N12 + PROPORTION_N22) * num_control)


def contraint(num_case, num_control):
	return (COST_CASE * num_case + COST_CONTROL * num_control <= BUDGET)

def optimize(case_max, constraint, optimization_function):
	variance_estimates = {}

	for num_case in range(1, case_max):
		num_control = 1
		while constraint(num_case, num_control):
			variance_estimates[(num_case, num_control)] = calculate_variance_case_control(num_case, num_control)

	return sorted(variance_estimates.items(), key = lambda x : x[1])


case_max = int(BUDGET / COST_CASE)

print( optimize(case_max, contraint, calculate_variance_case_control))
