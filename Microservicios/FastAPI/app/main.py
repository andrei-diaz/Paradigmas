# app/main.py
from fastapi import FastAPI
from .schemas import SensorData
from .database import guardar_lectura, obtener_lecturas

app = FastAPI(title="Microservicio de Monitoreo")


@app.get("/")
async def root():
    return {"mensaje": "Microservicio de Monitoreo activo 🚀"}


@app.post("/telemetria")
async def recibir_datos(data: SensorData):
    lectura = data.model_dump()
    guardar_lectura(lectura)
    return {"status": "recibido", "sensor": data.sensor_id, "lectura": data.valor}


@app.get("/telemetria")
async def ver_lecturas():
    return {"lecturas": obtener_lecturas()}
