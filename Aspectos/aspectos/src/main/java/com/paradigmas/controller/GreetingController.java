package com.paradigmas.controller;

import com.paradigmas.service.GreetingService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class GreetingController {

    private final GreetingService greetingService;

    public GreetingController(GreetingService greetingService) {
        this.greetingService = greetingService;
    }

    @GetMapping("/hello")
    public String hello(@RequestParam String name) {
        return greetingService.sayHello(name);
    }

    @GetMapping("/test-time")
    public String testTime() throws InterruptedException {
        greetingService.performComplexTask();
        return "Tarea finalizada. ¡Revisa tu consola!";
    }
}
