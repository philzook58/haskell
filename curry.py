from pymonad.Reader import curry

@curry
def add(x, y):
    return x + y

print add(7)(4)
