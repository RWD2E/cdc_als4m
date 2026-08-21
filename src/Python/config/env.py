from pathlib import Path
from dotenv import load_dotenv

def load_env():
    path = Path(__file__).resolve()
    while path != path.parent:
        env_file = path / ".env"
        if env_file.exists():
            load_dotenv(env_file)
            return env_file
        path = path.parent
    return None
