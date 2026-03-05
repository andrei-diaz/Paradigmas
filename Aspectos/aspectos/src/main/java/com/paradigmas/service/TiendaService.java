package com.paradigmas.service;

import org.springframework.stereotype.Service;

@Service
public class TiendaService {

    public String comprarProducto(String producto, int cantidad) {
        System.out.println("   [MÉTODO] Procesando compra de: " + producto);
        
        // Simulamos un pequeño retraso para el @Around
        try { Thread.sleep(500); } catch (InterruptedException e) {}

        if (cantidad <= 0) {
            throw new RuntimeException("La cantidad debe ser mayor a cero");
        }

        return "Éxito: Se compraron " + cantidad + " unidades de " + producto;
    }
}
