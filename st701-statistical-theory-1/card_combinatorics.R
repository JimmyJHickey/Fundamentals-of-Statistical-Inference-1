###
# Jimmy Hickey
# 2019-09-03
# Card Combinatorics problems
###

# P(Full House | 2 ranks)

P_Full_given_ranks = (2 * choose(4,3) * 1 * choose(4,2)) / 
                      ( choose(13, 2) * choose(8,5) )
P_Full_given_ranks
# 0.010989

# P(Full House | all face cards & 3 suits)
P_Full_give_face_3suits = ( 3 * choose(3,3) * 2 * choose(3,2) ) / (choose(9,5) * choose(4,3))
P_Full_give_face_3suits

# P(Full House | all # cards (A-10))
P_Full_given_no_face = (10 * choose(4,3) * 9 * choose(4,2) ) / (choose(40,5))
P_Full_given_no_face
