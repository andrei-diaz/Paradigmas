package com.example.sentinelauth;

import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/auth")
public class AuthController {

    @PostMapping("/login")
    public String login(@RequestBody LoginRequest request) {
        // Lógica de negocio pura
        if ("admin".equals(request.username()) && "root123".equals(request.password())) {
            return "Token-JWT-Exitoso";
        }
        throw new RuntimeException("Credenciales Inválidas");
    }
}
