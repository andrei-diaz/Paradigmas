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

## Estructura del Proyecto

```
hospital-soa/
├── auth-service/
│   └── src/main/java/com/hospital/auth/
│       ├── config/JwtUtil.java           # Generacion y validacion de JWT
│       ├── controller/AuthController.java
│       ├── model/ (User, AuthRequest, AuthResponse)
│       └── service/AuthService.java      # Usuarios en memoria
├── patient-service/
│   └── src/main/java/com/hospital/patient/
│       ├── controller/PatientController.java
│       ├── dto/PatientDTO.java            # DTO para recibir datos
│       ├── model/Patient.java
│       └── service/PatientService.java    # Almacenamiento en memoria
├── notification-service/
│   └── src/main/java/com/hospital/notification/
│       ├── controller/NotificationController.java
│       ├── dto/NotificationRequestDTO.java
│       ├── model/Notification.java
│       └── service/NotificationService.java
└── hospital-gateway/
    └── src/main/java/com/hospital/gateway/
        ├── config/RestTemplateConfig.java
        ├── controller/
        │   ├── GatewayController.java    # Endpoints individuales con proxy
        │   └── CompositeController.java  # Endpoints compuestos
        ├── dto/ (LoginRequestDTO, PatientDTO, RegistrarPacienteRequestDTO,
        │         RegistrarPacienteResponseDTO, LoginDashboardResponseDTO)
        └── service/GatewayService.java   # Comunicacion via RestTemplate
```
