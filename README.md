# Project-Evaluation-of-Hedging-Strategies
Evaluation  of hedging strategies in R for the purchase of currency, specifically the U.S. dollar, through the use of financial options.

This project seeks to determine the best hedging strategy for the purchase of currency, specifically the U.S. dollar, through the use of financial options. The strategies to be analyzed are the following: traditional Long Call strategy, Participative Forward strategy, Collar or Bull Call Spread strategy and finally Gaviota strategy.

In addition, the spot market prices for the one-month hedging horizon were found through a Monte Carlo simulation with 50,000 iterations in R. This simulation was also used in the valuation of European-type financial options with the use of the Geometric Brownian Motion valuation model that assumes risk neutrality. For the above reason, the implicit devaluation (r - rf) in continuous time is used instead of μ whose calculation was obtained from the Colombian Risk Free Rate (IBR: 4.251% E.A at one month) and the United States Risk Free Rate (Treasury: 1.43% E.A at one month) both as of February 21, 2020 and with a continuous monthly composition. 
