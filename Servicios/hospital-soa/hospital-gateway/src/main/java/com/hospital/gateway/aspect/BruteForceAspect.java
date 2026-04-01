package com.hospital.gateway.aspect;

import jakarta.servlet.http.HttpServletRequest;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.server.ResponseStatusException;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * ASPECTO: Deteccion de Fuerza Bruta
 *
 * Vigila el endpoint de login del gateway.
 * Si una IP falla el login 3 veces seguidas, queda bloqueada.
 * Las siguientes peticiones desde esa IP son rechazadas ANTES de llegar al auth-service.
 *
 * Esto demuestra el patron SOA: el gateway protege los servicios internos
 * sin modificarlos.
 */
@Aspect
@Component
public class BruteForceAspect {

    private static final Logger log = LoggerFactory.getLogger("BruteForce");
    private static final int MAX_INTENTOS = 5;

    // Cuantos intentos fallidos lleva cada IP
    private final Map<String, Integer> intentosFallidos = new ConcurrentHashMap<>();

    // IPs que ya estan bloqueadas
    private final Set<String> ipsBlockeadas = ConcurrentHashMap.newKeySet();

    // Pointcut: apunta exactamente al metodo login del GatewayController
    @Pointcut("execution(* com.hospital.gateway.controller.GatewayController.login(..))")
    public void loginPointcut() {}

    /**
     * BEFORE: Se ejecuta ANTES de procesar el login.
     * Si la IP ya esta bloqueada, lanza excepcion 429 y el auth-service nunca es contactado.
     */
    @Before("loginPointcut()")
    public void verificarBloqueo(JoinPoint jp) {
        String ip = obtenerIP();

        if (ipsBlockeadas.contains(ip)) {
            log.warn("[BRUTE FORCE] IP bloqueada intenta acceder de nuevo: {}", ip);
            throw new ResponseStatusException(
                    HttpStatus.TOO_MANY_REQUESTS,
                    "IP bloqueada por multiples intentos fallidos. Contacte al administrador."
            );
        }

        log.info("[LOGIN] Intento de login desde IP: {}", ip);
    }

    /**
     * AFTER RETURNING: Se ejecuta DESPUES de que login devuelve una respuesta.
     * Si la respuesta es 401 (credenciales incorrectas), suma un fallo a esa IP.
     * Si llega a MAX_INTENTOS, la IP queda bloqueada.
     * Si el login es exitoso, se resetea el contador.
     */
    @AfterReturning(pointcut = "loginPointcut()", returning = "respuesta")
    public void revisarRespuesta(Object respuesta) {
        String ip = obtenerIP();

        if (respuesta instanceof ResponseEntity<?> re) {

            if (re.getStatusCode() == HttpStatus.UNAUTHORIZED) {
                int intentos = intentosFallidos.getOrDefault(ip, 0) + 1;
                intentosFallidos.put(ip, intentos);

                log.warn("[BRUTE FORCE] Intento fallido {}/{} - IP: {}", intentos, MAX_INTENTOS, ip);

                if (intentos >= MAX_INTENTOS) {
                    ipsBlockeadas.add(ip);
                    log.warn("[BRUTE FORCE] *** ALERTA: IP {} BLOQUEADA por fuerza bruta ***", ip);
                }

            } else if (re.getStatusCode().is2xxSuccessful()) {
                // Login exitoso: limpiar el historial de esa IP
                intentosFallidos.remove(ip);
                log.info("[LOGIN] Login exitoso - IP: {} | Contador reseteado", ip);
            }
        }
    }

    /**
     * Resetea todos los bloqueos e intentos fallidos.
     * Util para demos y pruebas.
     */
    public void resetear() {
        intentosFallidos.clear();
        ipsBlockeadas.clear();
        log.info("[BRUTE FORCE] Todos los bloqueos han sido reseteados.");
    }

    private String obtenerIP() {
        ServletRequestAttributes attr =
                (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes();
        return attr.getRequest().getRemoteAddr();
    }
}
