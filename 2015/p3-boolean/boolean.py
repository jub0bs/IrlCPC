def getinputs():
    return raw_input().split()[1:]

def calc(tokens):
    stack = []
    for token in tokens:
        processToken(stack, token)
    return 1 if stack.pop() else 0

def processToken(stack, token):
    if   token == '0':
        stack.append(False)
    elif token == '1':
        stack.append(True)
    elif token == 'A':
        stack.append(stack.pop() & stack.pop())
    elif token == 'R':
        stack.append(stack.pop() | stack.pop())
    elif token == 'X':
        stack.append(stack.pop() ^ stack.pop())
    elif token == 'N':
        stack.append(not stack.pop())
    return stack

n = int(raw_input())
for i in range(n):
    tokens = getinputs()
    print calc(tokens)
