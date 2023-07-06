from math import comb

def pr_n_k_x_i(M, n, k, x, i):
    return (comb(x - 1, i - 1)*comb(n - x, k - i)/comb(n - 1, k - 1))*pr_n_k(M, x - 1, i - 1)*pr_n_k(M, n - x, k - i)

def prs_n_k_x(M, n, k, x):
    return [(i, pr_n_k_x_i(M, n, k, x, i)) for i in range(1, k + 1)]

def pr_n_k_x(M, n, k, x):
    prs = prs_n_k_x(M, n, k, x)
    return max(prs, key=lambda x : x[1])[1]

def pr_n_k(M, n, k):
    if k == 1 or k == 0:
        return 1
    if n < k:
        return 0
    if n == k:
        return 1
    if M[n][k] != None:
        return M[n][k]

    probs = 0
    for x in range(1, n + 1):
        probs += pr_n_k_x(M, n, k, x)

    M[n][k] = probs/n
    return probs/n

def theory_odds(n, k):
    M = [[None for j in range(k + 1)] for i in range(n + 1)]
    opt = pr_n_k(M, n, k)
    return opt

def theory_odds_grid(ns, ks):
    prs = []
    max_n = max(ns)
    max_k = max(ks)
    M = [[None for j in range(max_k + 1)] for i in range(max_n + 1)]
    for n in ns:
        for k in ks:
            opt = pr_n_k(M, n, k)
            prs.append({'n': n, 'k': k, 'pr': opt})

    return prs

def board_odds(n, board):
    num_placed = len([x for x in board if x != None])
    num_unplaced = len(board) - num_placed

    segments = []
    segment = []
    min_bound = 0
    max_bound = n + 1

    for i, x in enumerate(board, start=1):
        if x is None:
            segment.append(x)
            if i == len(board):
                segments.append((min_bound, n + 1, segment))
        else:
            max_bound = x
            if len(segment) > 0:
                segments.append((min_bound, max_bound, segment))
                segment = []
            min_bound = x

    pr = 1
    M = [[None for j in range(len(board) + 1)] for i in range(n + 1)]
    for s in segments:
        s_min = s[0] + 1
        s_max = s[1] - 1
        s_len = len(s[2])
        pr_s = pr_n_k(M, s_max - s_min + 1, s_len)
        pr *= pr_s * comb(s_max - s_min + 1, s_len)
    pr /= comb(n - num_placed, num_unplaced)
    return pr