package com.paradigmas.service;

import org.springframework.stereotype.Service;

@Service
public class GreetingService {

    public String sayHello(String name){
        System.out.println("...Ejecutando lógica interna del servicio...");
        return "Hola, " + name + "!";
    }

    public void performComplexTask() throws InterruptedException {
        // Simulamos que el sistema está trabajando duro por 2 segundos
        System.out.println("...Realizando cálculos matemáticos pesados...");
        Thread.sleep(2000);
    }

}

