package com.example.sentinelauth;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;

@Service
public class AlertService {

    private static final Logger log = LoggerFactory.getLogger(AlertService.class);
    private final HttpClient httpClient = HttpClient.newHttpClient();

    @Value("${alert.whatsapp.phone}")
    private String phone;

    @Value("${alert.whatsapp.apikey}")
    private String apiKey;

    @Async
    public void sendAlert(String attackType, String ip, String detail) {
        try {
            String message = String.format(
                    "🚨 *ALERTA SentinelAuth*%n" +
                            "━━━━━━━━━━━━━━━━━━━━%n" +
                            "🛡️ Tipo: *%s*%n" +
                            "📍 IP: %s%n" +
                            "📝 Detalle: %s%n" +
                            "🕐 Hora: %s%n" +
                            "━━━━━━━━━━━━━━━━━━━━",
                    attackType, ip, detail,
                    java.time.LocalDateTime.now().toString());

            String encodedMsg = URLEncoder.encode(message, StandardCharsets.UTF_8);
            String url = String.format(
                    "https://api.callmebot.com/whatsapp.php?phone=%s&text=%s&apikey=%s",
                    phone, encodedMsg, apiKey);

            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .GET()
                    .build();

            HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            log.info("WhatsApp alert sent (status {}): {}", response.statusCode(), attackType);

        } catch (Exception e) {
            log.error("Error enviando alerta WhatsApp: {}", e.getMessage());
        }
    }
}
