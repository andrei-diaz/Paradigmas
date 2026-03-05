package com.paradigmas.aspect;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class PerformanceAspect {

    @Around("execution(* com.paradigmas.service.GreetingService.performComplexTask(..))")
    public Object measureTime(ProceedingJoinPoint joinPoint) throws Throwable {
        long start = System.currentTimeMillis(); // 1. Acción antes

        // 2. EL MOMENTO CLAVE: Autorizamos al método real a ejecutarse
        Object result = joinPoint.proceed();

        long executionTime = System.currentTimeMillis() - start; // 3. Acción después

        System.out.println(">>> AOP TIMER: El método [" + joinPoint.getSignature().getName() +
                "] tardó " + executionTime + "ms en completarse.");

        return result;
    }
}
