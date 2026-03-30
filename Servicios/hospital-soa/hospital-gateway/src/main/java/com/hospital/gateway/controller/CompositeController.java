package com.hospital.gateway.controller;

import com.hospital.gateway.dto.LoginRequestDTO;
import com.hospital.gateway.dto.LoginDashboardResponseDTO;
import com.hospital.gateway.dto.RegistrarPacienteRequestDTO;
import com.hospital.gateway.dto.RegistrarPacienteResponseDTO;
import com.hospital.gateway.service.GatewayService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/compuesto")
public class CompositeController {

    @Autowired
    private GatewayService gatewayService;

    /**
     * ENDPOINT COMPUESTO 1: Autenticar + Registrar Paciente + Notificar
     *
     * Orquesta 3 servicios en una sola operacion:
     *   1. auth-service   -> valida credenciales y obtiene JWT
     *   2. patient-service -> registra al paciente con el JWT
     *   3. notification-service -> envia email de confirmacion
     *
     * POST /api/compuesto/registrar-paciente
     * Body: { "username": "admin", "password": "admin123",
     *         "paciente": { "nombre": "...", "apellido": "...", ... } }
     */
    @PostMapping("/registrar-paciente")
    public ResponseEntity<?> registrarPaciente(@RequestBody RegistrarPacienteRequestDTO request) {

        // Validacion basica
        if (request.getUsername() == null || request.getPassword() == null) {
            return ResponseEntity.badRequest()
                    .body(Map.of("error", "username y password son requeridos"));
        }
        if (request.getPaciente() == null) {
            return ResponseEntity.badRequest()
                    .body(Map.of("error", "Los datos del paciente son requeridos"));
        }

        // PASO 1: Autenticar en auth-service
        LoginRequestDTO loginDTO = new LoginRequestDTO(request.getUsername(), request.getPassword());
        ResponseEntity<?> loginResponse = gatewayService.login(loginDTO);

        if (!loginResponse.getStatusCode().is2xxSuccessful()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                    .body(Map.of("error", "Autenticacion fallida. Verifique sus credenciales."));
        }

        Map<?, ?> loginBody = (Map<?, ?>) loginResponse.getBody();
        String token = (String) loginBody.get("token");
        String authHeader = "Bearer " + token;

        // PASO 2: Registrar paciente en patient-service
        ResponseEntity<?> patientResponse = gatewayService.createPatient(request.getPaciente(), authHeader);

        if (!patientResponse.getStatusCode().is2xxSuccessful()) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(Map.of("error", "Error al registrar paciente", "detalle", patientResponse.getBody()));
        }

        // PASO 3: Enviar notificacion en notification-service
        Map<String, String> notifBody = Map.of(
                "destinatario", request.getUsername() + "@hospital.com",
                "asunto", "Nuevo paciente registrado",
                "mensaje", "Se registro exitosamente al paciente: "
                        + request.getPaciente().getNombre() + " " + request.getPaciente().getApellido()
        );
        ResponseEntity<?> notifResponse = gatewayService.sendEmail(notifBody, authHeader);

        // Armar respuesta compuesta
        RegistrarPacienteResponseDTO response = new RegistrarPacienteResponseDTO();
        response.setMensaje("Operacion completada: paciente registrado y notificacion enviada");
        response.setToken(token);
        response.setUsuarioAutenticado((String) loginBody.get("username"));
        response.setRol((String) loginBody.get("role"));
        response.setPacienteRegistrado(patientResponse.getBody());
        response.setNotificacionEnviada(notifResponse.getBody());

        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    /**
     * ENDPOINT COMPUESTO 2: Login + Dashboard (pacientes + notificaciones)
     *
     * Orquesta 3 servicios en una sola operacion:
     *   1. auth-service         -> valida credenciales y obtiene JWT
     *   2. patient-service      -> obtiene todos los pacientes
     *   3. notification-service -> obtiene todas las notificaciones
     *
     * POST /api/compuesto/login-dashboard
     * Body: { "username": "admin", "password": "admin123" }
     */
    @PostMapping("/login-dashboard")
    public ResponseEntity<?> loginDashboard(@RequestBody LoginRequestDTO request) {

        if (request.getUsername() == null || request.getPassword() == null) {
            return ResponseEntity.badRequest()
                    .body(Map.of("error", "username y password son requeridos"));
        }

        // PASO 1: Autenticar en auth-service
        ResponseEntity<?> loginResponse = gatewayService.login(request);

        if (!loginResponse.getStatusCode().is2xxSuccessful()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                    .body(Map.of("error", "Autenticacion fallida. Verifique sus credenciales."));
        }

        Map<?, ?> loginBody = (Map<?, ?>) loginResponse.getBody();
        String token = (String) loginBody.get("token");
        String authHeader = "Bearer " + token;

        // PASO 2: Obtener todos los pacientes de patient-service
        ResponseEntity<?> patientsResponse = gatewayService.getAllPatients(authHeader);

        // PASO 3: Obtener todas las notificaciones de notification-service
        ResponseEntity<?> notifsResponse = gatewayService.getAllNotifications(authHeader);

        // Armar respuesta compuesta (dashboard)
        LoginDashboardResponseDTO response = new LoginDashboardResponseDTO();
        response.setMensaje("Dashboard cargado correctamente");
        response.setToken(token);
        response.setUsuarioAutenticado((String) loginBody.get("username"));
        response.setRol((String) loginBody.get("role"));
        response.setPacientes(patientsResponse.getBody());
        response.setNotificaciones(notifsResponse.getBody());

        return ResponseEntity.ok(response);
    }
}
