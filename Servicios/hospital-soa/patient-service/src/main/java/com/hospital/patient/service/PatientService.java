package com.hospital.patient.service;

import com.hospital.patient.model.Patient;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.atomic.AtomicLong;

@Service
public class PatientService {

    private final Map<Long, Patient> pacientes = new LinkedHashMap<>();
    private final AtomicLong counter = new AtomicLong(3);

    // Pacientes predefinidos
    public PatientService() {
        pacientes.put(1L, new Patient(1L, "Juan",  "Perez",  "001-1234567-8", 35, "Hipertension",   "Dr. Garcia"));
        pacientes.put(2L, new Patient(2L, "Maria", "Lopez",  "002-9876543-1", 28, "Diabetes tipo 2", "Dr. Martinez"));
        pacientes.put(3L, new Patient(3L, "Carlos","Ramirez","003-5551234-0", 52, "Fractura de femur","Dr. Garcia"));
    }

    public List<Patient> getAll() {
        return new ArrayList<>(pacientes.values());
    }

    public Optional<Patient> getById(Long id) {
        return Optional.ofNullable(pacientes.get(id));
    }

    public Patient create(Patient patient) {
        long id = counter.incrementAndGet();
        patient.setId(id);
        pacientes.put(id, patient);
        return patient;
    }

    public Optional<Patient> update(Long id, Patient updated) {
        if (!pacientes.containsKey(id)) return Optional.empty();
        updated.setId(id);
        pacientes.put(id, updated);
        return Optional.of(updated);
    }

    public boolean delete(Long id) {
        return pacientes.remove(id) != null;
    }
}
