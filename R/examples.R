# ffa_meta(goal, dimensions, params) function usage examples

# quadratic function
# 1-dimensional
#
# result <- ffa(
#     goal = function (x) { x * x },
#         dimensions = 1,
#         params = list(
#             fflies_count = 20,
#             iterations = 20,
#             coefficients = list(
#             absorption = 1,
#             randomness = 0.1,
#             attraction_base = 1,
#             attraction_min = 0.05
#         ),
#         ranges = list(
#             list(min = -10, max = 10),
#             list(min = -10, max = 10)
#         ),
#         rand_scaling = c(1)
#     )
# )

# ackley function
# 2-dimensional
#
# source("R/functions.R")
# result <- ffa(
#     goal = ackley,
#         dimensions = 2,
#         params = list(
#             fflies_count = 20,
#             iterations = 20,
#             coefficients = list(
#             absorption = 1,
#             randomness = 0.1,
#             attraction_base = 1,
#             attraction_min = 0.05
#         ),
#         ranges = list(
#             list(min = -10, max = 10),
#             list(min = -10, max = 10)
#         ),
#         rand_scaling = c(1, 1)
#     )
# )
