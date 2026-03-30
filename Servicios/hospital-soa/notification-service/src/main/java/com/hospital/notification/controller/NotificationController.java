package com.hospital.notification.controller;

import com.hospital.notification.dto.NotificationRequestDTO;
import com.hospital.notification.model.Notification;
import com.hospital.notification.service.NotificationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/notifications")
public class NotificationController {

    @Autowired
    private NotificationService notificationService;

    // GET /notifications
    @GetMapping
    public ResponseEntity<List<Notification>> getAll() {
        return ResponseEntity.ok(notificationService.getAll());
    }

    // POST /notifications/email - recibe NotificationRequestDTO
    @PostMapping("/email")
    public ResponseEntity<Notification> sendEmail(@RequestBody NotificationRequestDTO dto) {
        Notification notif = notificationService.enviarEmail(
                dto.getDestinatario(),
                dto.getAsunto() != null ? dto.getAsunto() : "Sin asunto",
                dto.getMensaje() != null ? dto.getMensaje() : "");
        return ResponseEntity.status(HttpStatus.CREATED).body(notif);
    }

    // POST /notifications/alert - recibe NotificationRequestDTO
    @PostMapping("/alert")
    public ResponseEntity<Notification> sendAlert(@RequestBody NotificationRequestDTO dto) {
        Notification notif = notificationService.enviarAlerta(
                dto.getDestinatario(),
                dto.getAsunto() != null ? dto.getAsunto() : "Alerta del sistema",
                dto.getMensaje() != null ? dto.getMensaje() : "");
        return ResponseEntity.status(HttpStatus.CREATED).body(notif);
    }

    // GET /notifications/health
    @GetMapping("/health")
    public ResponseEntity<String> health() {
        return ResponseEntity.ok("Notification Service OK - Puerto 8083");
    }
}
