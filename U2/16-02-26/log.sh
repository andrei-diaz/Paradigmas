#!bin/bash
#Buscando archivos .log modificados hace mas de 30 dias y los borramos
PATH_LOGS="/Users/andreidiazrosario/logs"

find$PATH_LOGS-name "*.log" -mtime +30 -exec rm {} \;
echo "Limpieza de logs completados el $(date)" >> registro_mantenimiento.txt