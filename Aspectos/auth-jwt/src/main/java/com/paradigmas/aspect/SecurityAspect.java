package com.paradigmas.aspect;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class SecurityAspect {

    private static final Logger log = LoggerFactory.getLogger(SecurityAspect.class);

    @Before("execution(* com.paradigmas.service.AuthService.login(..))")
    public void antesDeLogin(JoinPoint jp) {
        String username = (String) jp.getArgs()[0];
        log.info("[AUTH] Intento de login - usuario: '{}'", username);
    }

    @AfterReturning(pointcut = "execution(* com.paradigmas.service.AuthService.login(..))", returning = "token")
    public void loginExitoso(JoinPoint jp, Object token) {
        String username = (String) jp.getArgs()[0];
        log.info("[AUTH] LOGIN EXITOSO - usuario: '{}' | token: {}", username, token);
    }

    @AfterThrowing(pointcut = "execution(* com.paradigmas.service.AuthService.login(..))", throwing = "error")
    public void loginFallido(JoinPoint jp, Exception error) {
        String username = (String) jp.getArgs()[0];
        log.warn("[AUTH] LOGIN FALLIDO  - usuario: '{}' | motivo: {}", username, error.getMessage());
    }
}
