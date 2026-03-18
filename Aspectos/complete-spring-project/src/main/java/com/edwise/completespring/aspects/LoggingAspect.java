package com.edwise.completespring.aspects;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.Arrays;

/**
 * Ejercicio 1 — Logging de métodos del servicio (Básico)
 * Registra cada llamada a BookService con nombre del método y argumentos.
 */
@Aspect
@Component
public class LoggingAspect {

    private static final Logger log = LoggerFactory.getLogger(LoggingAspect.class);

    @Before("execution(* com.edwise.completespring.services..*(..))")
    public void logBeforeServiceCall(JoinPoint jp) {
        log.info("Llamando a: {} con args: {}",
                jp.getSignature().getName(),
                Arrays.toString(jp.getArgs()));
    }

    @AfterReturning(
            pointcut = "execution(* com.edwise.completespring.services..*(..))",
            returning = "result"
    )
    public void logAfterServiceCall(JoinPoint jp, Object result) {
        log.info("Método {} retornó: {}", jp.getSignature().getName(), result);
    }
}
