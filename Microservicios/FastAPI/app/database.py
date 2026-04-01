# app/database.py
# Conexión a datos (placeholder para futuras integraciones)

# Aquí puedes agregar la conexión a tu base de datos.
# Ejemplo con una lista en memoria por ahora:

sensores_db: list[dict] = []


def guardar_lectura(data: dict) -> dict:
    """Guarda una lectura de sensor en la base de datos en memoria."""
    sensores_db.append(data)
    return data


def obtener_lecturas() -> list[dict]:
    """Obtiene todas las lecturas almacenadas."""
    return sensores_db
