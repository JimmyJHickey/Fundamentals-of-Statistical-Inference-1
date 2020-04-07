from math import sqrt

x = [1294, 1279, 1274, 1264, 1263, 1254, 1251, 1251, 1248, 1240, 1232, 1220, 1218, 1210]
n = len(x)

y = [1284, 1272, 1256, 1254, 1242, 1274, 1264, 1256, 1250]
m = len(y)

xbar = sum(x) / n
ybar = sum(y) / m

xmxbarsq=sum( [ (i - xbar)**2 for i in x] )
ymybarsq=sum( [ (i - ybar)**2 for i in y] )

sp = 1/(n+m-2) * (xmxbarsq + ymybarsq)
t = (xbar - ybar) / sqrt( sp * (1/n + 1/m))

print("xbar:\t" + str(xbar))
print("ybar:\t" + str(ybar))
print("sp:\t" + str(sp))
print("t:\t" + str(t))
