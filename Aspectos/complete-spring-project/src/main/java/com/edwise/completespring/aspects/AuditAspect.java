package com.edwise.completespring.aspects;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import java.util.Arrays;

/**
 * Ejercicio 3 — Auditoría con anotación personalizada (Intermedio-Avanzado)
 * Registra quién realizó operaciones de modificación usando Spring Security.
 */
@Aspect
@Component
public class AuditAspect {

    private static final Logger log = LoggerFactory.getLogger(AuditAspect.class);

    @Around("@annotation(auditable)")
    public Object audit(ProceedingJoinPoint pjp, Auditable auditable) throws Throwable {
        String user = SecurityContextHolder.getContext()
                .getAuthentication().getName();

        log.info("AUDITORÍA | usuario: {} | acción: {} | args: {}",
                user, auditable.action(), Arrays.toString(pjp.getArgs()));

        Object result = pjp.proceed();

        log.info("AUDITORÍA | acción: {} | completada exitosamente", auditable.action());
        return result;
    }
}
