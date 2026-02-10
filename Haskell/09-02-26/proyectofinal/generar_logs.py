import random

ips = [
    "192.168.1.1", "192.168.1.50", "10.0.0.5", "10.0.0.12",
    "172.16.0.2", "172.16.0.99", "192.168.0.10", "10.10.10.1",
    "203.0.113.5", "198.51.100.42"
]

paths = [
    "/index.html", "/about", "/contact", "/api/data", "/api/users",
    "/admin", "/login", "/logout", "/dashboard", "/products",
    "/api/orders", "/search", "/profile", "/settings", "/health"
]

codes = [200, 200, 200, 200, 200, 201, 301, 304, 400, 401, 403, 404, 404, 500, 502, 503]

with open("09-02-26/server.log", "w") as f:
    for _ in range(10000):
        ip = random.choice(ips)
        path = random.choice(paths)
        code = random.choice(codes)
        f.write(f"{ip} {path} {code}\n")

print("server.log generado con 10,000 entradas")
