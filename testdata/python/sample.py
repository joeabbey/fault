import os
import sys
import json as j
from pathlib import Path
from typing import Optional, List
from collections.abc import Mapping
from ..models import User, Profile
from .utils import helper

MAX_RETRIES = 3
DEFAULT_TIMEOUT = 30

class UserService:
    """Service for managing users."""

    def __init__(self, base_url: str):
        self.base_url = base_url

    def get_user(self, user_id: int) -> Optional[dict]:
        """Fetch a user by ID."""
        pass

    async def update_user(self, user_id: int, data: dict) -> bool:
        """Update a user."""
        pass

    class _InternalHelper:
        """Nested private class."""
        pass


class DataProcessor:
    """Processes data."""

    def process(self, items: List[str]) -> List[str]:
        return [item.strip() for item in items]


def format_name(first: str, last: str) -> str:
    """Format a full name."""
    return f"{first} {last}"


async def fetch_data(url: str) -> dict:
    """Fetch data from a URL."""
    pass


def _private_helper(x: int) -> int:
    """Private helper function."""
    return x * 2
