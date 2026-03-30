package com.hospital.gateway.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import java.util.Map;

@Service
public class GatewayService {

    @Autowired
    private RestTemplate restTemplate;

    @Value("${services.auth.url}")
    private String authUrl;

    @Value("${services.patient.url}")
    private String patientUrl;

    @Value("${services.notification.url}")
    private String notificationUrl;

    // Valida el token llamando al auth-service
    public boolean validarToken(String authHeader) {
        if (authHeader == null || !authHeader.startsWith("Bearer ")) return false;
        String token = authHeader.substring(7);
        try {
            ResponseEntity<Map> response = restTemplate.getForEntity(
                    authUrl + "/auth/validate?token=" + token, Map.class);
            return response.getStatusCode() == HttpStatus.OK;
        } catch (Exception e) {
            return false;
        }
    }

    // ---- AUTH SERVICE ----

    public ResponseEntity<?> login(Object body) {
        return forward(authUrl + "/auth/login", HttpMethod.POST, body, null);
    }

    // ---- PATIENT SERVICE ----

    public ResponseEntity<?> getAllPatients(String authHeader) {
        return forward(patientUrl + "/patients", HttpMethod.GET, null, authHeader);
    }

    public ResponseEntity<?> getPatientById(Long id, String authHeader) {
        return forward(patientUrl + "/patients/" + id, HttpMethod.GET, null, authHeader);
    }

    public ResponseEntity<?> createPatient(Object body, String authHeader) {
        return forward(patientUrl + "/patients", HttpMethod.POST, body, authHeader);
    }

    public ResponseEntity<?> updatePatient(Long id, Object body, String authHeader) {
        return forward(patientUrl + "/patients/" + id, HttpMethod.PUT, body, authHeader);
    }

    public ResponseEntity<?> deletePatient(Long id, String authHeader) {
        return forward(patientUrl + "/patients/" + id, HttpMethod.DELETE, null, authHeader);
    }

    // ---- NOTIFICATION SERVICE ----

    public ResponseEntity<?> getAllNotifications(String authHeader) {
        return forward(notificationUrl + "/notifications", HttpMethod.GET, null, authHeader);
    }

    public ResponseEntity<?> sendEmail(Object body, String authHeader) {
        return forward(notificationUrl + "/notifications/email", HttpMethod.POST, body, authHeader);
    }

    public ResponseEntity<?> sendAlert(Object body, String authHeader) {
        return forward(notificationUrl + "/notifications/alert", HttpMethod.POST, body, authHeader);
    }

    // ---- Metodo generico de forwarding ----

    private ResponseEntity<?> forward(String url, HttpMethod method, Object body, String authHeader) {
        try {
            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            if (authHeader != null) headers.set("Authorization", authHeader);

            HttpEntity<Object> entity = new HttpEntity<>(body, headers);
            ResponseEntity<Object> response = restTemplate.exchange(url, method, entity, Object.class);
            // Devolver ResponseEntity nuevo sin las cabeceras del servicio upstream
            // (evita que Transfer-Encoding: chunked del upstream llegue al cliente)
            return ResponseEntity.status(response.getStatusCode()).body(response.getBody());
        } catch (HttpClientErrorException e) {
            return ResponseEntity.status(e.getStatusCode()).body(e.getResponseBodyAsString());
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE)
                    .body(Map.of("error", "Servicio no disponible: " + e.getMessage()));
        }
    }
}
