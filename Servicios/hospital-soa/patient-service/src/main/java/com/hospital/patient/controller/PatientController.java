package com.hospital.patient.controller;

import com.hospital.patient.dto.PatientDTO;
import com.hospital.patient.model.Patient;
import com.hospital.patient.service.PatientService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/patients")
public class PatientController {

    @Autowired
    private PatientService patientService;

    // GET /patients
    @GetMapping
    public ResponseEntity<List<Patient>> getAll() {
        return ResponseEntity.ok(patientService.getAll());
    }

    // GET /patients/{id}
    @GetMapping("/{id}")
    public ResponseEntity<?> getById(@PathVariable Long id) {
        return patientService.getById(id)
                .<ResponseEntity<?>>map(ResponseEntity::ok)
                .orElse(ResponseEntity.status(HttpStatus.NOT_FOUND)
                        .body(Map.of("error", "Paciente no encontrado con id: " + id)));
    }

    // POST /patients - recibe PatientDTO
    @PostMapping
    public ResponseEntity<Patient> create(@RequestBody PatientDTO dto) {
        Patient patient = new Patient(null, dto.getNombre(), dto.getApellido(),
                dto.getCedula(), dto.getEdad(), dto.getDiagnostico(), dto.getDoctor());
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(patientService.create(patient));
    }

    // PUT /patients/{id} - recibe PatientDTO
    @PutMapping("/{id}")
    public ResponseEntity<?> update(@PathVariable Long id, @RequestBody PatientDTO dto) {
        Patient patient = new Patient(null, dto.getNombre(), dto.getApellido(),
                dto.getCedula(), dto.getEdad(), dto.getDiagnostico(), dto.getDoctor());
        return patientService.update(id, patient)
                .<ResponseEntity<?>>map(ResponseEntity::ok)
                .orElse(ResponseEntity.status(HttpStatus.NOT_FOUND)
                        .body(Map.of("error", "Paciente no encontrado con id: " + id)));
    }

    // DELETE /patients/{id}
    @DeleteMapping("/{id}")
    public ResponseEntity<?> delete(@PathVariable Long id) {
        if (patientService.delete(id)) {
            return ResponseEntity.ok(Map.of("mensaje", "Paciente " + id + " eliminado correctamente"));
        }
        return ResponseEntity.status(HttpStatus.NOT_FOUND)
                .body(Map.of("error", "Paciente no encontrado con id: " + id));
    }

    // GET /patients/health
    @GetMapping("/health")
    public ResponseEntity<String> health() {
        return ResponseEntity.ok("Patient Service OK - Puerto 8082");
    }
}
