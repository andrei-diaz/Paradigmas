package com.hospital.gateway.aspect;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

/**
 * ASPECTO: Logging de todas las peticiones del gateway
 *
 * Registra automaticamente cada request que llega al gateway:
 * que metodo HTTP, que URL, a que controller/metodo fue, y que status devolvio.
 *
 * El controller no necesita saber que existe este aspecto.
 */
@Aspect
@Component
public class LoggingAspect {

    private static final Logger log = LoggerFactory.getLogger("GatewayLog");

    // Pointcut: todos los metodos de todos los controllers del gateway
    @Pointcut("within(com.hospital.gateway.controller..*)")
    public void todosLosControllers() {}

    /**
     * BEFORE: Loguea el request entrante antes de procesarlo.
     */
    @Before("todosLosControllers()")
    public void logRequest(JoinPoint jp) {
        ServletRequestAttributes attr =
                (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes();

        log.info("[REQUEST]  {} {} -> {}.{}()",
                attr.getRequest().getMethod(),
                attr.getRequest().getRequestURI(),
                jp.getTarget().getClass().getSimpleName(),
                jp.getSignature().getName());
    }

    /**
     * AFTER RETURNING: Loguea el status de la respuesta.
     */
    @AfterReturning(pointcut = "todosLosControllers()", returning = "respuesta")
    public void logResponse(Object respuesta) {
        if (respuesta instanceof ResponseEntity<?> re) {
            log.info("[RESPONSE] Status: {}", re.getStatusCode());
        }
    }
}
