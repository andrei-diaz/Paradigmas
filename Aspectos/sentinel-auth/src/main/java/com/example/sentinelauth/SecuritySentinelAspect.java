package com.example.sentinelauth;

import jakarta.servlet.http.HttpServletRequest;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.server.ResponseStatusException;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Aspect
@Component
public class SecuritySentinelAspect {

    private static final Logger auditLog = LoggerFactory.getLogger("SecuritySentinel");
    private final Map<String, Integer> failedAttempts = new ConcurrentHashMap<>();
    private final Map<String, Long> lastRequestTime = new ConcurrentHashMap<>();
    private final AlertService alertService;

    public SecuritySentinelAspect(AlertService alertService) {
        this.alertService = alertService;
    }

    // PUNTO DE CORTE (Pointcut): Vigilamos el método login
    @Pointcut("execution(* com.example.sentinelauth.AuthController.login(..))")
    public void loginPointcut() {
    }

    // CONSEJO BEFORE (Advice): Detección de DoS
    @Before("loginPointcut()")
    public void detectDoS() {
        ServletRequestAttributes attr = (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes();
        String ip = attr.getRequest().getRemoteAddr();
        long now = System.currentTimeMillis();

        if (lastRequestTime.containsKey(ip) && (now - lastRequestTime.get(ip) < 200)) { // 200ms entre peticiones
            logAttack("DENIAL_OF_SERVICE", ip, "Frecuencia excesiva", attr.getRequest());
            throw new ResponseStatusException(HttpStatus.TOO_MANY_REQUESTS, "Ataque detectado");
        }
        lastRequestTime.put(ip, now);
    }

    // CONSEJO AFTER THROWING (Advice): Fuerza Bruta
    @AfterThrowing(pointcut = "loginPointcut()", throwing = "ex")
    public void detectBruteForce(JoinPoint jp, Throwable ex) {
        ServletRequestAttributes attr = (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes();
        String ip = attr.getRequest().getRemoteAddr();
        LoginRequest dto = (LoginRequest) jp.getArgs()[0];

        int attempts = failedAttempts.getOrDefault(ip, 0) + 1;
        failedAttempts.put(ip, attempts);

        if (attempts >= 3) {
            logAttack("BRUTE_FORCE", ip, "Usuario intentado: " + dto.username(), attr.getRequest());
        }
    }

    private void logAttack(String type, String ip, String detail, HttpServletRequest req) {
        String logEntry = String.format(
                "| TIPO: %s | IP: %s | RECURSO: %s | DETALLE: %s | AGENTE: %s |",
                type, ip, req.getRequestURI(), detail, req.getHeader("User-Agent"));
        auditLog.warn(logEntry);

        // Enviar alerta por email (asíncrono, no bloquea)
        alertService.sendAlert(type, ip, detail);
    }
}
