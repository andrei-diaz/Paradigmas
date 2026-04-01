# Hospital SOA - Sistema Hospitalario con Microservicios

Sistema hospitalario implementado con arquitectura SOA usando 4 proyectos Spring Boot que se comunican entre sí.

## Arquitectura

```
                        ┌─────────────────────┐
                        │   hospital-gateway  │
                        │     Puerto 8080     │
                        │   (Orquestador)     │
                        └──────────┬──────────┘
               ┌───────────────────┼──────────────────┐
               ▼                   ▼                  ▼
    ┌──────────────────┐  ┌───────────────┐  ┌─────────────────────┐
    │   auth-service   │  │patient-service│  │ notification-service│
    │   Puerto 8081    │  │  Puerto 8082  │  │     Puerto 8083     │
    └──────────────────┘  └───────────────┘  └─────────────────────┘
```

## Servicios

| Servicio | Puerto | Descripcion |
|---|---|---|
| hospital-gateway | 8080 | API Gateway - orquesta los 3 servicios |
| auth-service | 8081 | Autenticacion JWT |
| patient-service | 8082 | CRUD de pacientes |
| notification-service | 8083 | Envio de emails y alertas |

## Requisitos

- Java 17
- Maven 3.8+

## Instrucciones de Ejecucion

Abrir **4 terminales** y ejecutar cada servicio en orden:

**Terminal 1 - Auth Service:**
```bash
cd auth-service
mvn spring-boot:run
```

**Terminal 2 - Patient Service:**
```bash
cd patient-service
mvn spring-boot:run
```

**Terminal 3 - Notification Service:**
```bash
cd notification-service
mvn spring-boot:run
```

**Terminal 4 - Hospital Gateway:**
```bash
cd hospital-gateway
mvn spring-boot:run
```

Verificar que los 4 servicios estan corriendo:
```bash
curl http://localhost:8080/api/health
```

---

## Usuarios Predefinidos

| Usuario | Contraseña | Rol |
|---|---|---|
| admin | admin123 | ADMIN |
| doctor | doctor123 | DOCTOR |
| enfermera | enfermera123 | ENFERMERA |

---

## Pruebas con curl

### 1. Health Check del Gateway
```bash
curl http://localhost:8080/api/health
```

---

### 2. Login (auth-service)
```bash
curl -X POST http://localhost:8080/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username":"admin","password":"admin123"}'
```
Respuesta:
```json
{
  "token": "eyJhbGciOiJIUzI1NiJ9...",
  "username": "admin",
  "role": "ADMIN",
  "mensaje": "Login exitoso"
}
```
Guardar el token para los siguientes requests:
```bash
TOKEN="eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJhZG1pbiIsInJvbGUiOiJBRE1JTiIsImlhdCI6MTc3NDM5MTM5NywiZXhwIjoxNzc0NDc3Nzk3fQ.9tYbsbahzBWg5AgslvtEWusPa64tgGtF31YenHdSlaQ"
```

---

### 3. Listar Pacientes
```bash
curl http://localhost:8080/api/patients \
  -H "Authorization: Bearer $TOKEN"
```

### 4. Obtener Paciente por ID
```bash
curl http://localhost:8080/api/patients/1 \
  -H "Authorization: Bearer $TOKEN"
```

### 5. Crear Paciente
```bash
curl -X POST http://localhost:8080/api/patients \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{
    "nombre": "Ana",
    "apellido": "Gomez",
    "cedula": "004-1112223-4",
    "edad": 45,
    "diagnostico": "Asma",
    "doctor": "Dr. Martinez"
  }'
```

### 6. Actualizar Paciente
```bash
curl -X PUT http://localhost:8080/api/patients/1 \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{
    "nombre": "Juan",
    "apellido": "Perez",
    "cedula": "001-1234567-8",
    "edad": 36,
    "diagnostico": "Hipertension controlada",
    "doctor": "Dr. Garcia"
  }'
```

### 7. Eliminar Paciente
```bash
curl -X DELETE http://localhost:8080/api/patients/1 \
  -H "Authorization: Bearer $TOKEN"
```

---

### 8. Enviar Email de Notificacion
```bash
curl -X POST http://localhost:8080/api/notifications/email \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{
    "destinatario": "doctor@hospital.com",
    "asunto": "Cita programada",
    "mensaje": "El paciente Juan Perez tiene cita manana a las 9am."
  }'
```

### 9. Enviar Alerta del Sistema
```bash
curl -X POST http://localhost:8080/api/notifications/alert \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{
    "destinatario": "admin@hospital.com",
    "asunto": "Alerta critica",
    "mensaje": "Capacidad de UCI al 90%."
  }'
```

### 10. Listar Notificaciones
```bash
curl http://localhost:8080/api/notifications \
  -H "Authorization: Bearer $TOKEN"
```

---

## Endpoints Compuestos (Composicion de Servicios)

### 11. COMPUESTO 1: Autenticar + Registrar Paciente + Notificar
Realiza en una sola llamada:
1. Autentica al usuario en **auth-service**
2. Registra el paciente en **patient-service**
3. Envia email de confirmacion via **notification-service**

```bash
curl -X POST http://localhost:8080/api/compuesto/registrar-paciente \
  -H "Content-Type: application/json" \
  -d '{
    "username": "admin",
    "password": "admin123",
    "paciente": {
      "nombre": "Luis",
      "apellido": "Fernandez",
      "cedula": "005-9876543-2",
      "edad": 62,
      "diagnostico": "Diabetes tipo 1",
      "doctor": "Dr. Garcia"
    }
  }'
```

Respuesta incluye: token JWT + paciente creado + notificacion enviada.

---

### 12. COMPUESTO 2: Login + Dashboard (pacientes + notificaciones)
Realiza en una sola llamada:
1. Autentica al usuario en **auth-service**
2. Obtiene todos los pacientes de **patient-service**
3. Obtiene todas las notificaciones de **notification-service**

```bash
curl -X POST http://localhost:8080/api/compuesto/login-dashboard \
  -H "Content-Type: application/json" \
  -d '{"username":"doctor","password":"doctor123"}'
```

Respuesta incluye: token JWT + lista de pacientes + lista de notificaciones.


---

## Aspectos AOP en el Gateway

El gateway implementa 3 aspectos que se ejecutan de forma transparente sin modificar los controladores.

> **Por que en el gateway?** En SOA, los microservicios son cajas negras que no se pueden modificar.
> El gateway es el unico punto de entrada, por eso es donde se aplican los aspectos de seguridad y monitoreo.

### Aspectos implementados

| Aspecto | Archivo | Que hace |
|---|---|---|
| `BruteForceAspect` | `aspect/BruteForceAspect.java` | Detecta y bloquea ataques de fuerza bruta en el login |
| `LoggingAspect` | `aspect/LoggingAspect.java` | Loguea cada request/response que pasa por el gateway |
| `PerformanceAspect` | `aspect/PerformanceAspect.java` | Mide el tiempo de las operaciones compuestas |

---

## Prueba de Fuerza Bruta (BruteForceAspect)

Simula un atacante que intenta adivinar la contrasena con intentos repetidos.

**Paso 1 — Hacer 3 intentos fallidos con contrasena incorrecta:**
```bash
curl -X POST http://localhost:8080/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username":"admin","password":"INCORRECTA"}'
```
Repetir este comando 3 veces. Cada vez responde `401 Unauthorized`.

**Lo que se ve en los logs del gateway:**
```
[LOGIN]       Intento de login desde IP: 127.0.0.1
[BRUTE FORCE] Intento fallido 1/3 - IP: 127.0.0.1
[LOGIN]       Intento de login desde IP: 127.0.0.1
[BRUTE FORCE] Intento fallido 2/3 - IP: 127.0.0.1
[LOGIN]       Intento de login desde IP: 127.0.0.1
[BRUTE FORCE] Intento fallido 3/3 - IP: 127.0.0.1
[BRUTE FORCE] *** ALERTA: IP 127.0.0.1 BLOQUEADA por fuerza bruta ***
```

**Paso 2 — Intentar de nuevo (con cualquier contrasena, incluso la correcta):**
```bash
curl -X POST http://localhost:8080/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username":"admin","password":"admin123"}'
```

**Respuesta esperada (429 Too Many Requests):**
```json
{
  "status": 429,
  "error": "Too Many Requests",
  "message": "IP bloqueada por multiples intentos fallidos. Contacte al administrador."
}
```
El auth-service nunca recibe esta peticion. El aspecto la bloquea en el gateway.

**Paso 3 — Resetear el bloqueo para seguir probando:**
```bash
curl -X DELETE http://localhost:8080/api/brute-force/reset
```
Respuesta:
```json
{ "mensaje": "Bloqueos reseteados. Todas las IPs desbloqueadas." }
```
Ahora puedes volver a hacer login normalmente.

---

## Prueba de Performance y Logging (PerformanceAspect + LoggingAspect)

Llamar a cualquier endpoint compuesto:
```bash
curl -X POST http://localhost:8080/api/compuesto/login-dashboard \
  -H "Content-Type: application/json" \
  -d '{"username":"admin","password":"admin123"}'
```

**Lo que se ve en los logs del gateway:**
```
[REQUEST]     POST /api/compuesto/login-dashboard -> CompositeController.loginDashboard()
[PERFORMANCE] Iniciando operacion compuesta: 'loginDashboard' (llama a 3 microservicios)
[PERFORMANCE] 'loginDashboard' completado en 43ms
[RESPONSE]    Status: 200 OK
```

