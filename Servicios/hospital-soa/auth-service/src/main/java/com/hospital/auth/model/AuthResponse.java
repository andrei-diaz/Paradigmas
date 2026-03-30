package com.hospital.auth.model;

public class AuthResponse {
    private String token;
    private String username;
    private String role;
    private String mensaje;

    public AuthResponse(String token, String username, String role) {
        this.token = token;
        this.username = username;
        this.role = role;
        this.mensaje = "Login exitoso";
    }

    public AuthResponse(String mensaje) {
        this.mensaje = mensaje;
    }

    public String getToken() { return token; }
    public String getUsername() { return username; }
    public String getRole() { return role; }
    public String getMensaje() { return mensaje; }
}
