# app/schemas.py
from pydantic import BaseModel, Field


class SensorData(BaseModel):
    sensor_id: str
    valor: float = Field(..., gt=-50, lt=100)  # Validación: entre -50 y 100
    unidad: str = "Celsius"
