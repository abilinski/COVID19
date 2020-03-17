import os

class Config:
    DEBUG = os.environ.get("COVID_DEBUG") or False
