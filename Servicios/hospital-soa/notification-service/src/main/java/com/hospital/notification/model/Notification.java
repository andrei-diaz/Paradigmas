package com.hospital.notification.model;

import java.time.LocalDateTime;

public class Notification {
    private Long id;
    private String tipo;        // EMAIL, ALERTA
    private String destinatario;
    private String asunto;
    private String mensaje;
    private String estado;      // ENVIADO, PENDIENTE, ERROR
    private LocalDateTime fechaEnvio;

    public Notification() {}

    public Notification(Long id, String tipo, String destinatario,
                        String asunto, String mensaje) {
        this.id = id;
        this.tipo = tipo;
        this.destinatario = destinatario;
        this.asunto = asunto;
        this.mensaje = mensaje;
        this.estado = "ENVIADO";
        this.fechaEnvio = LocalDateTime.now();
    }

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public String getTipo() { return tipo; }
    public void setTipo(String tipo) { this.tipo = tipo; }
    public String getDestinatario() { return destinatario; }
    public void setDestinatario(String destinatario) { this.destinatario = destinatario; }
    public String getAsunto() { return asunto; }
    public void setAsunto(String asunto) { this.asunto = asunto; }
    public String getMensaje() { return mensaje; }
    public void setMensaje(String mensaje) { this.mensaje = mensaje; }
    public String getEstado() { return estado; }
    public void setEstado(String estado) { this.estado = estado; }
    public LocalDateTime getFechaEnvio() { return fechaEnvio; }
    public void setFechaEnvio(LocalDateTime fechaEnvio) { this.fechaEnvio = fechaEnvio; }
}
