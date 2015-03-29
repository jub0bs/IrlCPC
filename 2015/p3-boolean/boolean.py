def getinputs():
    return raw_input().split()[1:]

def pop2(stack):
    b2 = stack.pop()
    b1 = stack.pop()
    return b1, b2

def calc(tokens):
    stack = []
    for token in tokens:
        if   token == '0':
            stack.append(False)
        elif token == '1':
            stack.append(True)
        elif token == 'A':
            b1, b2 = pop2(stack)
            stack.append(b1 and b2)
        elif token == 'R':
            b1, b2 = pop2(stack)
            stack.append(b1 or b2)
        elif token == 'X':
            b1, b2 = pop2(stack)
            stack.append(b1 != b2)
        elif token == 'N':
            stack.append(not stack.pop())
    return {
        False : 0,
        True  : 1,
        }[stack.pop()]

# main
n = int(raw_input())
for i in range(n):
    tokens = getinputs()
    print calc(tokens)

