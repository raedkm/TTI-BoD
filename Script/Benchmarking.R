#---------------------------------------------#
#Project : Benchmark - 2019
#Sub     : Benchmark computer
#Purpose : To compare the computer with other users
#Created by Raed Alotaibi
#Date Created: 1-July-2019
#Last Updated: 1-July-2019
#---------------------------------------------#


# Load the benchmarkme package
library(benchmarkme)

# Assign the variable ram to the amount of RAM on this machine
ram <- get_ram()
ram

# Assign the variable cpu to the cpu specs
cpu <- get_cpu()
cpu

# Run the io benchmark
res <- benchmark_io(runs = 1, size = 5)

# Plot the results
plot(res)
