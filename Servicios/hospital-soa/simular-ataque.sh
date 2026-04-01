#!/bin/bash

# =============================================================
# Simulacion de ataque de fuerza bruta contra el hospital-gateway
# El aspecto BruteForceAspect detecta y bloquea automaticamente
# despues de 5 intentos fallidos desde la misma IP.
# =============================================================

GATEWAY="http://localhost:8080"
USUARIO="admin"

# Lista de contrasenas que un atacante podria intentar
PASSWORDS=(
  "password"
  "123456"
  "admin"
  "hospital123"
  "qwerty"
  "letmein"
  "admin123"   # esta es la correcta, pero ya estara bloqueado
)

echo "================================================"
echo " SIMULACION DE ATAQUE DE FUERZA BRUTA"
echo " Target: $GATEWAY/api/auth/login"
echo " Usuario objetivo: $USUARIO"
echo "================================================"
echo ""

for i in "${!PASSWORDS[@]}"; do
  INTENTO=$((i + 1))
  PASSWORD="${PASSWORDS[$i]}"

  echo -n "Intento $INTENTO | usuario: $USUARIO | password: $PASSWORD  -->  "

  RESPONSE=$(curl -s -o /tmp/respuesta.json -w "%{http_code}" -X POST "$GATEWAY/api/auth/login" \
    -H "Content-Type: application/json" \
    -d "{\"username\":\"$USUARIO\",\"password\":\"$PASSWORD\"}")

  BODY=$(cat /tmp/respuesta.json)

  if [ "$RESPONSE" == "200" ]; then
    echo "EXITO (200) - Token obtenido"
    echo ""
    echo ">>> Login exitoso con password: $PASSWORD"
    break
  elif [ "$RESPONSE" == "429" ]; then
    echo "BLOQUEADO (429)"
    echo ""
    echo ">>> El aspecto detecto el ataque y bloqueo la IP."
    echo ">>> Para desbloquear: curl -X DELETE $GATEWAY/api/brute-force/reset"
    break
  else
    echo "Fallido ($RESPONSE)"
  fi

  sleep 0.5
done

echo ""
echo "================================================"
echo " Revisa los logs en:"
echo "   hospital-gateway/logs/brute-force.log"
echo "   hospital-gateway/logs/gateway-requests.log"
echo "================================================"
