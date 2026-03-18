package com.edwise.completespring.aspects;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * Ejercicio 4 — Manejo centralizado de excepciones con AOP (Avanzado)
 * Intercepta y registra cualquier excepción lanzada en cualquier capa.
 */
@Aspect
@Component
public class ExceptionLoggingAspect {

    private static final Logger log = LoggerFactory.getLogger(ExceptionLoggingAspect.class);

    @AfterThrowing(
            pointcut = "execution(* com.edwise.completespring..*(..))",
            throwing = "ex"
    )
    public void logException(JoinPoint jp, Exception ex) {
        log.error("Excepción en {}.{} → {}: {}",
                jp.getTarget().getClass().getSimpleName(),
                jp.getSignature().getName(),
                ex.getClass().getSimpleName(),
                ex.getMessage());
    }
}
