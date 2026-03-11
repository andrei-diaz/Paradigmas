package com.paradigmas.service;

import com.paradigmas.security.JwtUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Map;

@Service
public class AuthService {

    @Autowired
    private JwtUtil jwtUtil;

    private static final Map<String, String> USUARIOS = Map.of(
        "admin",  "admin123",
        "andrei", "dragon",
        "user",   "1234"
    );

    public String login(String username, String password) {
        if (username == null || password == null) {
            throw new IllegalArgumentException("Usuario y contraseña requeridos");
        }

        String passwordCorrecta = USUARIOS.get(username);

        if (passwordCorrecta == null) {
            throw new SecurityException("Usuario no encontrado: " + username);
        }

        if (!passwordCorrecta.equals(password)) {
            throw new SecurityException("Contraseña incorrecta para: " + username);
        }

        return jwtUtil.generateToken(username);
    }
}
