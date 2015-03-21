n  = int(raw_input())
ns = map(int, raw_input().split())
lsum = 0
rsum = sum(ns)
for i, v in enumerate(ns):
    rsum -= v
    if lsum == rsum:
        print i,
    lsum += v
print ""
