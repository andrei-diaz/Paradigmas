package com.hospital.notification.service;

import com.hospital.notification.model.Notification;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

@Service
public class NotificationService {

    private final List<Notification> notificaciones = new ArrayList<>();
    private final AtomicLong counter = new AtomicLong(0);

    public NotificationService() {
        // Notificaciones de ejemplo ya registradas
        notificaciones.add(new Notification(counter.incrementAndGet(),
                "EMAIL", "doctor@hospital.com",
                "Nuevo paciente asignado",
                "Se le ha asignado al paciente Juan Perez para consulta."));
        notificaciones.add(new Notification(counter.incrementAndGet(),
                "ALERTA", "admin@hospital.com",
                "Alerta de sistema",
                "Capacidad de camas al 85%."));
    }

    public Notification enviarEmail(String destinatario, String asunto, String mensaje) {
        System.out.println("[EMAIL] Para: " + destinatario + " | Asunto: " + asunto);
        Notification notif = new Notification(counter.incrementAndGet(),
                "EMAIL", destinatario, asunto, mensaje);
        notificaciones.add(notif);
        return notif;
    }

    public Notification enviarAlerta(String destinatario, String asunto, String mensaje) {
        System.out.println("[ALERTA] Para: " + destinatario + " | Asunto: " + asunto);
        Notification notif = new Notification(counter.incrementAndGet(),
                "ALERTA", destinatario, asunto, mensaje);
        notificaciones.add(notif);
        return notif;
    }

    public List<Notification> getAll() {
        return new ArrayList<>(notificaciones);
    }
}
