package com.hospital.auth.service;

import com.hospital.auth.config.JwtUtil;
import com.hospital.auth.model.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.Map;

@Service
public class AuthService {

    @Autowired
    private JwtUtil jwtUtil;

    // Usuarios predefinidos (simulando base de datos)
    private static final Map<String, User> USUARIOS = new HashMap<>();

    static {
        USUARIOS.put("admin",   new User("admin",   "admin123",   "ADMIN"));
        USUARIOS.put("doctor",  new User("doctor",  "doctor123",  "DOCTOR"));
        USUARIOS.put("enfermera", new User("enfermera", "enfermera123", "ENFERMERA"));
    }

    public String login(String username, String password) {
        User user = USUARIOS.get(username);
        if (user != null && user.getPassword().equals(password)) {
            return jwtUtil.generateToken(user.getUsername(), user.getRole());
        }
        return null;
    }

    public User getUserByUsername(String username) {
        return USUARIOS.get(username);
    }

    public boolean validarToken(String token) {
        return jwtUtil.isValid(token);
    }

    public Map<String, String> getTokenInfo(String token) {
        try {
            var claims = jwtUtil.validateToken(token);
            Map<String, String> info = new HashMap<>();
            info.put("username", claims.getSubject());
            info.put("role", claims.get("role", String.class));
            info.put("valido", "true");
            return info;
        } catch (Exception e) {
            Map<String, String> error = new HashMap<>();
            error.put("valido", "false");
            error.put("error", e.getMessage());
            return error;
        }
    }
}
