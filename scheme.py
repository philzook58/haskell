

#Takes in parentheses delimitted code and returns listy structure
def lex(string):
	current = 0
	tokens = []
	while(current < len(string)):
		if string[current] == ' ' or string[current] == '\n' or  string[current] == '\t'  :
			current += 1
		elif string[current] == '(' or string[current] == ')':
			tokens.append(string[current])
			current += 1
		else:
			word = [string[current]]




def parse(tokens):
	tree = []
	i = 0
	while(i < len(tokens)):
		if tokens[i] == '(':
			subtree, j = parse(tokens[i+1:])
			tree.append(subtree)
			i += j
		if tokens[i] == ')':
			return tree, i+1
		else:
			tree.append(tokens[i])
		i += 1
	return "ERROR"

print parse([ "(", "a", "b","(","(", "(", "c", "d", ")", ")"])

