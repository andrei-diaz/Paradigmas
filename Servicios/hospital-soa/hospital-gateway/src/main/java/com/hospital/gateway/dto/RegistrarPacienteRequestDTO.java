package com.hospital.gateway.dto;

// DTO para el endpoint compuesto: autenticar + registrar paciente + notificar
public class RegistrarPacienteRequestDTO {
    private String username;
    private String password;
    private PatientDTO paciente;

    public RegistrarPacienteRequestDTO() {}

    public String getUsername() { return username; }
    public void setUsername(String username) { this.username = username; }
    public String getPassword() { return password; }
    public void setPassword(String password) { this.password = password; }
    public PatientDTO getPaciente() { return paciente; }
    public void setPaciente(PatientDTO paciente) { this.paciente = paciente; }
}
