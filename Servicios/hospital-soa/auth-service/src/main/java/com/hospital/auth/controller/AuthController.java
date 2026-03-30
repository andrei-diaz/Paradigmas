package com.hospital.auth.controller;

import com.hospital.auth.model.AuthRequest;
import com.hospital.auth.model.AuthResponse;
import com.hospital.auth.model.User;
import com.hospital.auth.service.AuthService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/auth")
public class AuthController {

    @Autowired
    private AuthService authService;

    // POST /auth/login
    @PostMapping("/login")
    public ResponseEntity<?> login(@RequestBody AuthRequest request) {
        String token = authService.login(request.getUsername(), request.getPassword());
        if (token != null) {
            User user = authService.getUserByUsername(request.getUsername());
            return ResponseEntity.ok(new AuthResponse(token, user.getUsername(), user.getRole()));
        }
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                .body(new AuthResponse("Credenciales invalidas"));
    }

    // GET /auth/validate?token=xxx
    @GetMapping("/validate")
    public ResponseEntity<Map<String, String>> validateToken(@RequestParam String token) {
        Map<String, String> info = authService.getTokenInfo(token);
        if ("true".equals(info.get("valido"))) {
            return ResponseEntity.ok(info);
        }
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(info);
    }

    // GET /auth/health
    @GetMapping("/health")
    public ResponseEntity<String> health() {
        return ResponseEntity.ok("Auth Service OK - Puerto 8081");
    }
}
