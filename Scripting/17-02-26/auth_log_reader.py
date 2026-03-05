#!/usr/bin/env python3
"""
Auth Log Reader
Analiza el archivo de logs de Spring Boot y reporta eventos de autenticacion.
Uso: python auth_log_reader.py <ruta_al_log>
"""

import sys
import time


LOG_PATH_DEFAULT = (
    "/Users/andreidiazrosario/Documents/School/Paradigmas"
    "/Aspectos/aspectos/logs/tienda.log"
)


def analizar_log(log_path):
    print(f"[*] Leyendo log: {log_path}")
    print(f"[*] Buscando eventos de autenticacion...\n")

    start_time = time.time()

    exitosos  = []
    fallidos  = []
    intentos  = []
    lineas    = 0

    try:
        with open(log_path, "r", encoding="utf-8", errors="ignore") as f:
            for linea in f:
                lineas += 1
                linea = linea.strip()

                if "[AUTH]" not in linea:
                    continue

                if "Intento de login" in linea:
                    intentos.append(linea)

                elif "LOGIN EXITOSO" in linea:
                    exitosos.append(linea)

                elif "LOGIN FALLIDO" in linea:
                    fallidos.append(linea)

    except FileNotFoundError:
        print(f"[!] Error: No se encontro el archivo '{log_path}'")
        print(f"[!] Asegurate de haber corrido el proyecto Spring Boot primero.")
        sys.exit(1)

    elapsed = time.time() - start_time

    # ── Reporte ──────────────────────────────────────────────────────────────
    print("=" * 60)
    print("  REPORTE DE AUTENTICACION")
    print("=" * 60)
    print(f"  Lineas analizadas : {lineas:,}")
    print(f"  Tiempo            : {elapsed:.4f} segundos")
    print(f"  Total intentos    : {len(intentos)}")
    print(f"  Exitosos          : {len(exitosos)}")
    print(f"  Fallidos          : {len(fallidos)}")
    print("=" * 60)

    if exitosos:
        print("\n[+] LOGINS EXITOSOS:")
        for e in exitosos:
            print(f"    {e}")

    if fallidos:
        print("\n[-] LOGINS FALLIDOS:")
        for f in fallidos:
            print(f"    {f}")

        # Usuarios que fallaron mas de una vez
        usuarios_fallidos = {}
        for entrada in fallidos:
            if "usuario:" in entrada:
                usuario = entrada.split("usuario:")[1].split("|")[0].strip().strip("'")
                usuarios_fallidos[usuario] = usuarios_fallidos.get(usuario, 0) + 1

        sospechosos = {u: c for u, c in usuarios_fallidos.items() if c > 1}
        if sospechosos:
            print("\n[!] POSIBLE FUERZA BRUTA - usuarios con multiples fallos:")
            for usuario, count in sospechosos.items():
                print(f"    '{usuario}' -> {count} intentos fallidos")

    print()


if __name__ == "__main__":
    ruta = sys.argv[1] if len(sys.argv) == 2 else LOG_PATH_DEFAULT
    analizar_log(ruta)
