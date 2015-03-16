def f():
    """ Read a binary string from stdin
        and convert it to an integer
    """
    return int(raw_input(), 2)

# read the number of integers to sum
n = f()

rsum = 0
for i in range(n):
    rsum += f()

result        = bin(rsum)[2:]
result_padded = result.zfill(32)

print result_padded
