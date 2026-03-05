package com.paradigmas;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import com.paradigmas.service.TiendaService;

@SpringBootApplication
public class App implements CommandLineRunner {

     @Autowired
    private TiendaService tiendaService;
    public static void main(String[] args) {
        SpringApplication.run(App.class, args);
    }

    @Override
    public void run(String... args) {
        System.out.println("\n--- PRUEBA 1: COMPRA EXITOSA ---");
        tiendaService.comprarProducto("Laptop", 1);

        System.out.println("\n--- PRUEBA 2: COMPRA CON ERROR ---");
        try {
            tiendaService.comprarProducto("Smartphone", 0);
        } catch (Exception e) {
            // Error controlado para que no se detenga la app
        }
    }
}   
