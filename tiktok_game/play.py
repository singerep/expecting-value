# import numpy as np
import random

def place(slots, rv):
    eligible_slots = []
    min_bound = 0
    max_bound = n - 1

    for i, s in enumerate(slots):
        if s is None:
            eligible_slots += [i]
        else:
            if rv < s:
                max_bound = s - 1
                break
            elif rv > s:
                min_bound = s + 1
                eligible_slots = []

    # print('---')
    # print(slots)
    # print(rv)
    # print(eligible_slots)

    if len(eligible_slots) == 0:
        return False

    eligible_range_len = 1 + max_bound - min_bound
    # print(eligible_range_len)
    bin_size = 1 + (eligible_range_len)//len(eligible_slots)
    # print(bin_size)
    slot = eligible_slots[(rv - min_bound)//bin_size]
    # print(slot)
    slots[slot] = rv
    return slots

def play(n, k):
    rvs = random.sample(population=range(n), k=k)
    slots = [None]*k
    print('-- START GAME --')
    print('random draws:', rvs)
    for rv in rvs:
        slots = place(slots, rv)
        print(slots)
        if slots is False:
            return False
    print()
    return True

if __name__ == '__main__':
    n = 4
    k = 2

    trials = 10
    ws = 0
    ls = 0

    for i in range(trials):
        if play(n, k):
            ws += 1
        else:
            ls += 1
    
    # print()
    print(ws)
    print(ws/trials)

# [None, None, None, None]
# 95 -> [None, None, None, 95]
# [(0-31), (32-63), (64-94)]