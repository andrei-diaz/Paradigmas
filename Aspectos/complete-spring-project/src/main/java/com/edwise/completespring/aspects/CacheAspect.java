package com.edwise.completespring.aspects;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Ejercicio 5 — Caché manual con AOP (Avanzado)
 * Evita llamadas repetidas a MongoDB para getBook(id) usando un mapa en memoria.
 */
@Aspect
@Component
public class CacheAspect {

    private static final Logger log = LoggerFactory.getLogger(CacheAspect.class);
    private final Map<Object, Object> cache = new ConcurrentHashMap<>();

    @Around("execution(* com.edwise.completespring.services.*.findOne(..))")
    public Object cache(ProceedingJoinPoint pjp) throws Throwable {
        Object key = Arrays.toString(pjp.getArgs());

        if (cache.containsKey(key)) {
            log.info("Cache HIT para key: {}", key);
            return cache.get(key);
        }

        Object result = pjp.proceed();
        cache.put(key, result);
        log.info("Cache MISS -> almacenado key: {}", key);
        return result;
    }
}
