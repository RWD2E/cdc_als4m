import logging
import os
from snowflake.snowpark import Session
from snowflake.snowpark.functions import (
    col,
    count,
    datediff,
    lit,
    year,
    min as s_min,
    row_number,
    when,
    coalesce
)
from snowflake.snowpark.window import Window
from config import env, conn

logger = logging.getLogger(__name__)

