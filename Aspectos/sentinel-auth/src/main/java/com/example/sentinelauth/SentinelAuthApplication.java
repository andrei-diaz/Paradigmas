package com.example.sentinelauth;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableAsync;

@SpringBootApplication
@EnableAsync
public class SentinelAuthApplication {

    public static void main(String[] args) {
        SpringApplication.run(SentinelAuthApplication.class, args);
    }
}
