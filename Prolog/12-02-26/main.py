#!/usr/bin/env python3
"""
Sistema Experto Medico - Frontend Python
Paradigmas de Programacion

Interfaz interactiva con TTS que conecta con motor de inferencia en Prolog.
"""

import os
from pyswip import Prolog
import pyttsx3


# ======================== Colores ANSI ========================

class Color:
    RESET   = "\033[0m"
    BOLD    = "\033[1m"
    RED     = "\033[91m"
    GREEN   = "\033[92m"
    YELLOW  = "\033[93m"
    BLUE    = "\033[94m"
    MAGENTA = "\033[95m"
    CYAN    = "\033[96m"
    WHITE   = "\033[97m"
    BG_BLUE = "\033[44m"
    BG_GREEN = "\033[42m"


# ======================== Sistema Experto ========================

class SistemaExperto:
    def __init__(self):
        self.prolog = Prolog()
        prolog_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "experto.pl")
        self.prolog.consult(prolog_path)
        self.voz_id = self._buscar_voz_espanol()

    def _buscar_voz_espanol(self):
        """Encontrar el ID de la mejor voz en espanol disponible."""
        engine = pyttsx3.init()
        voices = engine.getProperty('voices')
        voz_espanol = None
        for voice in voices:
            vid = voice.id.lower()
            if 'paulina' in vid:
                return voice.id
            elif 'monica' in vid:
                voz_espanol = voice.id
            elif ('es-mx' in vid or 'es-es' in vid) and voz_espanol is None:
                voz_espanol = voice.id
        return voz_espanol

    def hablar(self, texto):
        """Leer texto en voz alta con engine fresco cada vez."""
        try:
            engine = pyttsx3.init()
            engine.setProperty('rate', 150)
            engine.setProperty('volume', 1.0)
            if self.voz_id:
                engine.setProperty('voice', self.voz_id)
            engine.say(texto)
            engine.runAndWait()
            engine.stop()
        except Exception:
            pass

    def obtener_sintomas(self):
        """Obtener lista unica de sintomas desde Prolog."""
        results = list(self.prolog.query("setof(S, sintoma_disponible(S), Lista)"))
        if results:
            return [str(s) for s in results[0]['Lista']]
        return []

    def nombre_display(self, atomo):
        """Obtener nombre legible de un atomo desde Prolog."""
        results = list(self.prolog.query(f"nombre_mostrar({atomo}, Nombre)"))
        if results:
            nombre = str(results[0]['Nombre'])
            return nombre
        return str(atomo).replace('_', ' ').capitalize()

    def registrar_sintoma(self, paciente, sintoma):
        """Registrar un sintoma del paciente en Prolog."""
        self.prolog.assertz(f"tiene({paciente}, {sintoma})")

    def diagnosticar(self, paciente):
        """Consultar Prolog por todos los diagnosticos del paciente."""
        results = list(self.prolog.query(f"diagnostico({paciente}, E, M)"))
        # Deduplicar resultados
        vistos = set()
        unicos = []
        for r in results:
            clave = (str(r['E']), str(r['M']))
            if clave not in vistos:
                vistos.add(clave)
                unicos.append({'E': str(r['E']), 'M': str(r['M'])})
        return unicos

    def limpiar(self):
        """Limpiar datos del paciente en Prolog."""
        list(self.prolog.query("limpiar_paciente"))

    def mostrar_banner(self):
        """Mostrar banner de bienvenida."""
        ancho = 50
        print(f"\n{Color.BG_BLUE}{Color.WHITE}{Color.BOLD}")
        print("=" * ancho)
        print("  SISTEMA EXPERTO MEDICO".center(ancho))
        print("  Paradigmas de Programacion".center(ancho))
        print("=" * ancho)
        print(Color.RESET)

    def mostrar_sintomas(self, sintomas, seleccionados):
        """Mostrar lista numerada de sintomas."""
        print(f"\n{Color.CYAN}{Color.BOLD}--- Sintomas Disponibles ---{Color.RESET}")
        for i, s in enumerate(sintomas, 1):
            nombre = self.nombre_display(s)
            if s in seleccionados:
                marca = f" {Color.GREEN}[registrado]{Color.RESET}"
            else:
                marca = ""
            print(f"  {Color.YELLOW}{i:2d}{Color.RESET}. {nombre}{marca}")
        print(f"\n  {Color.GREEN} 0{Color.RESET}. >>> Diagnosticar <<<")
        print(f"  {Color.RED}-1{Color.RESET}. Salir\n")

    def ejecutar(self):
        """Loop principal interactivo."""
        self.mostrar_banner()
        self.hablar("Bienvenido al sistema experto medico")

        while True:
            paciente = input(f"{Color.BOLD}Nombre del paciente: {Color.RESET}").strip().lower()
            if not paciente:
                paciente = "paciente"
            paciente_prolog = paciente.replace(" ", "_")

            self.hablar(f"Iniciando consulta para {paciente}")
            print(f"\n{Color.MAGENTA}Consulta para: {Color.BOLD}{paciente.title()}{Color.RESET}")

            sintomas = self.obtener_sintomas()
            seleccionados = set()

            while True:
                self.mostrar_sintomas(sintomas, seleccionados)
                try:
                    opcion = int(input(f"{Color.BOLD}Seleccione sintoma: {Color.RESET}"))
                except ValueError:
                    print(f"  {Color.RED}Entrada no valida. Ingrese un numero.{Color.RESET}")
                    continue

                if opcion == -1:
                    self.limpiar()
            
                    print(f"\n{Color.CYAN}Gracias por usar el Sistema Experto Medico.{Color.RESET}\n")
                    return

                if opcion == 0:
                    # Ejecutar diagnostico
                    print(f"\n{Color.BG_GREEN}{Color.WHITE}{Color.BOLD} DIAGNOSTICO {Color.RESET}\n")
                    resultados = self.diagnosticar(paciente_prolog)

                    if resultados:
                        texto_tts = f"Diagnostico para {paciente}. "
                        for r in resultados:
                            enf = r['E']
                            med = r['M']
                            nombre_enf = self.nombre_display(enf)
                            nombre_med = self.nombre_display(med)
                            print(f"  {Color.RED}Enfermedad:{Color.RESET}  {Color.BOLD}{nombre_enf}{Color.RESET}")
                            print(f"  {Color.GREEN}Medicamento:{Color.RESET} {Color.BOLD}{nombre_med}{Color.RESET}")
                            print(f"  {Color.BLUE}{'─' * 30}{Color.RESET}")
                            texto_tts += f"Se detecto {nombre_enf}, se recomienda {nombre_med}. "
                        self.hablar(texto_tts)
                    else:
                        msg = f"No se detecto enfermedad para {paciente}."
                        print(f"  {Color.YELLOW}{msg}{Color.RESET}")
                        self.hablar(msg)

                    print()
                    otra = input(f"{Color.BOLD}Desea otra consulta? (s/n): {Color.RESET}").strip().lower()
                    self.limpiar()
                    if otra != 's':
                        self.hablar("Gracias por usar el sistema experto medico. Hasta luego.")
                        print(f"\n{Color.CYAN}Hasta luego!{Color.RESET}\n")
                        return
                    break

                if 1 <= opcion <= len(sintomas):
                    sintoma = sintomas[opcion - 1]
                    if sintoma not in seleccionados:
                        self.registrar_sintoma(paciente_prolog, sintoma)
                        seleccionados.add(sintoma)
                        nombre = self.nombre_display(sintoma)
                        print(f"  {Color.GREEN}Sintoma registrado: {nombre}{Color.RESET}")
                        self.hablar(f"Sintoma registrado: {nombre}")
                    else:
                        print(f"  {Color.YELLOW}Ese sintoma ya fue registrado.{Color.RESET}")
                else:
                    print(f"  {Color.RED}Opcion fuera de rango.{Color.RESET}")


if __name__ == "__main__":
    SistemaExperto().ejecutar()
