import subprocess
import json
import os

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

def main():
    exe = os.path.join(SCRIPT_DIR, "leerLogs")
    log_file = os.path.join(SCRIPT_DIR, "server.log")

    # 1. Llamar al ejecutable de Haskell
    print("")
    print("Logs con Haskell")
    print("")
    result = subprocess.run([exe, log_file], capture_output=True, text=True)

    if result.returncode != 0:
        print("Error")
        print(result.stderr)
        return

    # 2. Parsear el JSON que devuelve Haskell
    data = json.loads(result.stdout)

    # 3. Resultados
    print(f"  Total de peticiones: {data['total_peticiones']}")
    print(f"  Errores (código >= 400): {data['errores']}")
    print(f"  IPs únicas: {len(data['ips_unicas'])}")
    print()
    print("  IPs unicas:")
    for ip in data["ips_unicas"]:
        print(f"    - {ip}")
    print("=" * 40)

if __name__ == "__main__":
    main()
