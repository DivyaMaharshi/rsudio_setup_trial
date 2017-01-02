library(logging)
logReset()
basicConfig(level='FINEST')
addHandler(writeToFile, file="~/testing.log", level='DEBUG')

# loginfo('test %d', 1)
# logdebug('test %d', 2)
# logwarn('test %d', 3)