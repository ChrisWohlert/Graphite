module AnimEnv where

top = 4.5
bottom = -4.5
left = -8.0
right = 8.0
topLeft = (left, top)
topRight = (right, top)
bottomLeft = (left, bottom)
bottomRight = (right, bottom)
height = abs bottom + top
width = abs left + right

moveUp v (a, b) = (a, b + v)
moveRight v (a, b) = (a + v, b)

smallMargin = 0.5
mediumMargin = 1.5
