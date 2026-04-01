package com.hospital.gateway.controller;

import com.hospital.gateway.aspect.BruteForceAspect;
import com.hospital.gateway.dto.LoginRequestDTO;
import com.hospital.gateway.dto.PatientDTO;
import com.hospital.gateway.service.GatewayService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api")
public class GatewayController {

    @Autowired
    private GatewayService gatewayService;

    @Autowired
    private BruteForceAspect bruteForceAspect;

    // =====================
    // AUTH ENDPOINTS
    // =====================

    @PostMapping("/auth/login")
    public ResponseEntity<?> login(@RequestBody LoginRequestDTO dto) {
        return gatewayService.login(dto);
    }

    // =====================
    // PATIENT ENDPOINTS (requieren token)
    // =====================

    @GetMapping("/patients")
    public ResponseEntity<?> getAllPatients(
            @RequestHeader(value = "Authorization", required = false) String authHeader) {
        if (!gatewayService.validarToken(authHeader)) return unauthorized();
        return gatewayService.getAllPatients(authHeader);
    }

    @GetMapping("/patients/{id}")
    public ResponseEntity<?> getPatientById(
            @PathVariable Long id,
            @RequestHeader(value = "Authorization", required = false) String authHeader) {
        if (!gatewayService.validarToken(authHeader)) return unauthorized();
        return gatewayService.getPatientById(id, authHeader);
    }

    @PostMapping("/patients")
    public ResponseEntity<?> createPatient(
            @RequestBody PatientDTO dto,
            @RequestHeader(value = "Authorization", required = false) String authHeader) {
        if (!gatewayService.validarToken(authHeader)) return unauthorized();
        return gatewayService.createPatient(dto, authHeader);
    }

    @PutMapping("/patients/{id}")
    public ResponseEntity<?> updatePatient(
            @PathVariable Long id,
            @RequestBody PatientDTO dto,
            @RequestHeader(value = "Authorization", required = false) String authHeader) {
        if (!gatewayService.validarToken(authHeader)) return unauthorized();
        return gatewayService.updatePatient(id, dto, authHeader);
    }

    @DeleteMapping("/patients/{id}")
    public ResponseEntity<?> deletePatient(
            @PathVariable Long id,
            @RequestHeader(value = "Authorization", required = false) String authHeader) {
        if (!gatewayService.validarToken(authHeader)) return unauthorized();
        return gatewayService.deletePatient(id, authHeader);
    }

    // =====================
    // NOTIFICATION ENDPOINTS (requieren token)
    // =====================

    @GetMapping("/notifications")
    public ResponseEntity<?> getAllNotifications(
            @RequestHeader(value = "Authorization", required = false) String authHeader) {
        if (!gatewayService.validarToken(authHeader)) return unauthorized();
        return gatewayService.getAllNotifications(authHeader);
    }

    @PostMapping("/notifications/email")
    public ResponseEntity<?> sendEmail(
            @RequestBody Map<String, String> body,
            @RequestHeader(value = "Authorization", required = false) String authHeader) {
        if (!gatewayService.validarToken(authHeader)) return unauthorized();
        return gatewayService.sendEmail(body, authHeader);
    }

    @PostMapping("/notifications/alert")
    public ResponseEntity<?> sendAlert(
            @RequestBody Map<String, String> body,
            @RequestHeader(value = "Authorization", required = false) String authHeader) {
        if (!gatewayService.validarToken(authHeader)) return unauthorized();
        return gatewayService.sendAlert(body, authHeader);
    }

    // =====================
    // HEALTH CHECK
    // =====================

    @GetMapping("/health")
    public ResponseEntity<?> health() {
        return ResponseEntity.ok(Map.of(
                "gateway", "Hospital Gateway OK - Puerto 8080",
                "auth-service", "http://localhost:8081/auth/health",
                "patient-service", "http://localhost:8082/patients/health",
                "notification-service", "http://localhost:8083/notifications/health"
        ));
    }

    // =====================
    // BRUTE FORCE RESET (solo para demo)
    // =====================

    @DeleteMapping("/brute-force/reset")
    public ResponseEntity<?> resetBruteForce() {
        bruteForceAspect.resetear();
        return ResponseEntity.ok(Map.of("mensaje", "Bloqueos reseteados. Todas las IPs desbloqueadas."));
    }

    private ResponseEntity<?> unauthorized() {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                .body(Map.of("error", "Token invalido o no proporcionado. Use: Authorization: Bearer <token>"));
    }
}
