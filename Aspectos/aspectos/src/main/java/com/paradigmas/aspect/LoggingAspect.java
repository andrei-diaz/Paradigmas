package com.paradigmas.aspect;

import java.util.Arrays;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class LoggingAspect {

    private static final Logger log = LoggerFactory.getLogger(LoggingAspect.class);

    @Before("execution(* com.paradigmas.service.GreetingService.sayHello(..))")
    public void logBefore() {
        log.info(">>> AOP LOG: Un método de saludo está a punto de ejecutarse...");
    }

    // Definimos el punto de corte: Todos los métodos de TiendaService
    @Pointcut("execution(* com.paradigmas.service.TiendaService.*(..))")
    public void puntoDeControl() {}

    // 1. @Before: Se ejecuta ANTES del método
    @Before("puntoDeControl()")
    public void antes(JoinPoint joinPoint) {
        log.info("-> [@Before]: Intentando ejecutar: {}", joinPoint.getSignature().getName());
        log.info("   Argumentos: {}", Arrays.toString(joinPoint.getArgs()));
    }

    // 2. @AfterReturning: Solo si el método termina EXITOSAMENTE
    @AfterReturning(pointcut = "puntoDeControl()", returning = "resultado")
    public void despuesExito(Object resultado) {
        log.info("-> [@AfterReturning]: ✔ ÉXITO - Respuesta: {}", resultado);
    }

    // 3. @AfterThrowing: Solo si el método lanza una EXCEPCIÓN
    @AfterThrowing(pointcut = "puntoDeControl()", throwing = "error")
    public void despuesError(Exception error) {
        log.error("-> [@AfterThrowing]: ✘ ERROR DETECTADO - Mensaje: {}", error.getMessage());
    }

    // 4. @After: Se ejecuta SIEMPRE al final (como un 'finally')
    @After("puntoDeControl()")
    public void alFinalizar() {
        log.info("-> [@After]: Limpiando recursos o cerrando logs...");
    }

    // 5. @Around: Envuelve al método (Es el más potente)
    @Around("puntoDeControl()")
    public Object cronometro(ProceedingJoinPoint joinPoint) throws Throwable {
        long inicio = System.currentTimeMillis();

        log.info("-> [@Around]: Iniciando cronómetro...");

        Object resultado = joinPoint.proceed();

        long fin = System.currentTimeMillis();
        log.info("-> [@Around]: Tiempo total de ejecución: {}ms", (fin - inicio));

        return resultado;
    }

}


   
