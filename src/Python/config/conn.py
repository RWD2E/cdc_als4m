# connectors/snow.py
from __future__ import annotations
from dataclasses import dataclass
import os
from snowflake.snowpark import Session

@dataclass(frozen=True)
class SnowCfg:
    account: str
    user: str
    password: str
    authenticator: str
    warehouse: str
    role: str

def load_snowcfg(env: str) -> SnowCfg:
    # simplest: env vars (works locally + Prefect + CI)
    prefix = env.upper()
    try:
        return SnowCfg(
            account=os.environ[f"{prefix}_SNOW_ACCOUNT"],
            user=os.environ[f"{prefix}_SNOW_USER"],
            password=os.environ[f"{prefix}_SNOW_PASSWORD"],
            authenticator=os.environ[f"{prefix}_SNOW_AUTHENTICATOR"],
            warehouse=os.environ[f"{prefix}_SNOW_WAREHOUSE"],
            role=os.environ[f"{prefix}_SNOW_ROLE"],
        )
    except KeyError as e:
        raise KeyError(
            f"Missing environment variable: {e}. "
            f"Make sure your .env file exists and contains all required {prefix}_SNOW_* variables."
        )

def get_snow_conn(cfg: SnowCfg):
    return Session.builder.configs(
        {
            "account": cfg.account,
            "user": cfg.user,
            "password": cfg.password,
            "authenticator":cfg.authenticator,
            "warehouse": cfg.warehouse,
            "role": cfg.role
        }
    ).create()
