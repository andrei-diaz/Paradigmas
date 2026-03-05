#!/bin/bash

# --- Configuración Editable ---
PROYECTO="mi-app-ingenieria"
GRUPO="com.universidad"
DEPENDENCIAS="web,data-jpa,h2,lombok" # Edita aquí tus dependencias
JAVA_VERSION="17"

echo "🚀 Iniciando proyecto Spring Boot: $PROYECTO"

# 1. Descargar el proyecto base usando la API de Spring
curl https://start.spring.io/starter.zip \
    -d dependencies=$DEPENDENCIAS \
    -d javaVersion=$JAVA_VERSION \
    -d bootVersion=3.2.0 \
    -d groupId=$GRUPO \/.
    -d artifactId=$PROYECTO \
    -d name=$PROYECTO \
    -d type=maven-project \
    -o $PROYECTO.zip
# 2. Descomprimir y limpiar
unzip $PROYECTO.zip -d $PROYECTO
rm $PROYECTO.zip

# 3. Crear estructura de carpetas adicionales de ingeniería
cd $PROYECTO
mkdir -p src/main/resources/static/assets
mkdir -p docs/diagramas
mkdir -p scripts_db

# 4. Mensaje final
echo "✅ Estructura creada. Dependencias instaladas: $DEPENDENCIAS"
echo "📂 Ruta: $(pwd)"
