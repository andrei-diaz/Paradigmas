package com.edwise.completespring.aspects;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Ejercicio 3 — Anotación personalizada para auditoría.
 * Aplicar en métodos que modifican datos (crear, actualizar, eliminar).
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Auditable {
    String action() default "";
}
