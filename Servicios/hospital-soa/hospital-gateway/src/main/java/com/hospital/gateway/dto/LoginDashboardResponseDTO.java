package com.hospital.gateway.dto;

// DTO de respuesta: resultado de autenticar + obtener pacientes + obtener notificaciones
public class LoginDashboardResponseDTO {
    private String mensaje;
    private String token;
    private String usuarioAutenticado;
    private String rol;
    private Object pacientes;
    private Object notificaciones;

    public LoginDashboardResponseDTO() {}

    public String getMensaje() { return mensaje; }
    public void setMensaje(String mensaje) { this.mensaje = mensaje; }
    public String getToken() { return token; }
    public void setToken(String token) { this.token = token; }
    public String getUsuarioAutenticado() { return usuarioAutenticado; }
    public void setUsuarioAutenticado(String usuarioAutenticado) { this.usuarioAutenticado = usuarioAutenticado; }
    public String getRol() { return rol; }
    public void setRol(String rol) { this.rol = rol; }
    public Object getPacientes() { return pacientes; }
    public void setPacientes(Object pacientes) { this.pacientes = pacientes; }
    public Object getNotificaciones() { return notificaciones; }
    public void setNotificaciones(Object notificaciones) { this.notificaciones = notificaciones; }
}
