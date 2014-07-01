# THIS SCRIPT IS MEANT TO TEST THE FUNCTIONS IN THE PACKAGE 'dsmodelling'

# LOAD REQUIRED LIBRARIES AND FUNCTIONS AND LOGIN TO SERVERS 
library('opal')
server1 <- opal.login('******', '*****', 'http://54.242.140.255')
server2 <- opal.login('******', '*****', 'http://54.242.46.59')
opals <- list(server1,server2)

# ASSIGN DATA FROM OPAL SOURCE TO R
datashield.assign(server1, 'D', 'HOPsim.hopsim1ob')
datashield.assign(server2, 'D', 'HOPsim.hopsim2ob')

# DATASHIELD LOADS DATA AS PARLISTS SO HERE WE CHANGE IT INTO DATAFRAMES AND ORDER BY INDIVIDUAL IDs
datashield.assign(opals, "T", quote(data.frame(as.list(D))))
datashield.assign(opals, "D", quote(order.frame(T, T$ind.id)))

# For the function 'glm.ds'run the tests of the package 'dsmodellingclient'.
# This functions is called by the equivalent client function.
