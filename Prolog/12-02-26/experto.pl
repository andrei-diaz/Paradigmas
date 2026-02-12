% Sistema Experto Medico
% Paradigmas de Programacion
% Motor de inferencia en Prolog con frontend en Python

:- dynamic tiene/2.

% ----- Base de Conocimientos -----
% Sintomas por enfermedad

% Gripe
sintoma(gripe, fiebre).
sintoma(gripe, tos).

% Alergia
sintoma(alergia, estornudos).
sintoma(alergia, picazon_ojos).

% Resfriado
sintoma(resfriado, congestion_nasal).
sintoma(resfriado, dolor_garganta).

% Migrana
sintoma(migrana, dolor_cabeza).
sintoma(migrana, sensibilidad_luz).
sintoma(migrana, nauseas).

% Gastritis
sintoma(gastritis, dolor_estomago).
sintoma(gastritis, nauseas).
sintoma(gastritis, acidez).

% Conjuntivitis
sintoma(conjuntivitis, ojos_rojos).
sintoma(conjuntivitis, lagrimeo).
sintoma(conjuntivitis, picazon_ojos).

% Bronquitis
sintoma(bronquitis, tos).
sintoma(bronquitis, dificultad_respirar).
sintoma(bronquitis, dolor_pecho).

% Otitis
sintoma(otitis, dolor_oido).
sintoma(otitis, fiebre).
sintoma(otitis, perdida_audicion).

% ----- Reglas de Diagnostico -----

tiene_enfermedad(Paciente, Enfermedad) :-
    sintoma(Enfermedad, Sintoma),
    tiene(Paciente, Sintoma).

% ----- Tratamientos (enfermedad -> medicamento) -----

tratamiento(gripe, medicamento_gripe).
tratamiento(alergia, antihistaminico).
tratamiento(resfriado, descongestionante).
tratamiento(migrana, analgesico).
tratamiento(gastritis, antiacido).
tratamiento(conjuntivitis, gotas_oftalmicas).
tratamiento(bronquitis, broncodilatador).
tratamiento(otitis, antibiotico).

% ----- Reglas de Tratamiento -----

necesita_medicamento(Paciente, Medicamento) :-
    tiene_enfermedad(Paciente, Enfermedad),
    tratamiento(Enfermedad, Medicamento).

% ----- Nombres para Mostrar (mapeo atomo -> texto legible) -----

nombre_mostrar(congestion_nasal, 'Congestion nasal').
nombre_mostrar(dolor_garganta, 'Dolor de garganta').
nombre_mostrar(picazon_ojos, 'Picazon en los ojos').
nombre_mostrar(dolor_cabeza, 'Dolor de cabeza').
nombre_mostrar(sensibilidad_luz, 'Sensibilidad a la luz').
nombre_mostrar(dolor_estomago, 'Dolor de estomago').
nombre_mostrar(ojos_rojos, 'Ojos rojos').
nombre_mostrar(dificultad_respirar, 'Dificultad para respirar').
nombre_mostrar(dolor_pecho, 'Dolor de pecho').
nombre_mostrar(dolor_oido, 'Dolor de oido').
nombre_mostrar(perdida_audicion, 'Perdida de audicion').
nombre_mostrar(medicamento_gripe, 'Medicamento para gripe').
nombre_mostrar(gotas_oftalmicas, 'Gotas oftalmicas').
nombre_mostrar(X, X).

% ----- Predicados Helper para Frontend Python -----

enfermedad(E) :- sintoma(E, _).
sintoma_disponible(S) :- sintoma(_, S).

diagnostico(Paciente, Enfermedad, Medicamento) :-
    tiene_enfermedad(Paciente, Enfermedad),
    tratamiento(Enfermedad, Medicamento).

limpiar_paciente :- retractall(tiene(_, _)).

% ----- Motor de Inferencia (para uso standalone en Prolog) -----

diagnosticar(Paciente) :-
    write('--- Iniciando Diagnostico---'), nl,
    (necesita_medicamento(Paciente, Med) ->
            format('El paciente ~w necesita: ~w.~n', [Paciente, Med])
        ;
            format('No se detecto enfermedad grave para ~w.~n', [Paciente])
    ).
