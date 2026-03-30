package com.hospital.gateway.dto;

// DTO de respuesta: resultado de autenticar + registrar paciente + notificar
public class RegistrarPacienteResponseDTO {
    private String mensaje;
    private String token;
    private String usuarioAutenticado;
    private String rol;
    private Object pacienteRegistrado;
    private Object notificacionEnviada;

    public RegistrarPacienteResponseDTO() {}

    public String getMensaje() { return mensaje; }
    public void setMensaje(String mensaje) { this.mensaje = mensaje; }
    public String getToken() { return token; }
    public void setToken(String token) { this.token = token; }
    public String getUsuarioAutenticado() { return usuarioAutenticado; }
    public void setUsuarioAutenticado(String usuarioAutenticado) { this.usuarioAutenticado = usuarioAutenticado; }
    public String getRol() { return rol; }
    public void setRol(String rol) { this.rol = rol; }
    public Object getPacienteRegistrado() { return pacienteRegistrado; }
    public void setPacienteRegistrado(Object pacienteRegistrado) { this.pacienteRegistrado = pacienteRegistrado; }
    public Object getNotificacionEnviada() { return notificacionEnviada; }
    public void setNotificacionEnviada(Object notificacionEnviada) { this.notificacionEnviada = notificacionEnviada; }
}
