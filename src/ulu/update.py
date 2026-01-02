import urllib.request
import json
from . import __version__

def check_for_updates():
    try:
        url = "https://pypi.org/pypi/universal-legacy-updater/json"
        with urllib.request.urlopen(url) as response:
            data = json.loads(response.read().decode())
            latest_version = data['info']['version']
            if latest_version > __version__:
                return f"A new version {latest_version} is available. Current version: {__version__}. Run 'pip install --upgrade universal-legacy-updater' to update."
            else:
                return f"You are using the latest version: {__version__}."
    except Exception as e:
        return f"Unable to check for updates: {e}"