package com.edwise.completespring.aspects;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * Ejercicio 2 — Medición de tiempos de ejecución (Intermedio)
 * Mide cuánto tarda cada método del controller. Si supera 200ms, emite advertencia.
 */
@Aspect
@Component
public class PerformanceAspect {

    private static final Logger log = LoggerFactory.getLogger(PerformanceAspect.class);
    private static final long THRESHOLD_MS = 200;

    @Around("execution(* com.edwise.completespring.controllers..*(..))")
    public Object measureTime(ProceedingJoinPoint pjp) throws Throwable {
        long start = System.currentTimeMillis();
        Object result = pjp.proceed();
        long elapsed = System.currentTimeMillis() - start;

        if (elapsed > THRESHOLD_MS) {
            log.warn("Método lento: {} tardó {}ms", pjp.getSignature(), elapsed);
        } else {
            log.info("{} tardó {}ms", pjp.getSignature().getName(), elapsed);
        }

        return result;
    }
}
