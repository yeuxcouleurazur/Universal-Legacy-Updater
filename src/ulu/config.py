import os
import configparser

CONFIG_FILE = os.path.expanduser('~/.ulu.ini')

def load_config():
    config = configparser.ConfigParser()
    if os.path.exists(CONFIG_FILE):
        config.read(CONFIG_FILE)
    return config

def save_config(config):
    with open(CONFIG_FILE, 'w') as f:
        config.write(f)

def get_config(section, key, default=None):
    config = load_config()
    try:
        return config.get(section, key)
    except (configparser.NoSectionError, configparser.NoOptionError):
        return default

def set_config(section, key, value):
    config = load_config()
    if not config.has_section(section):
        config.add_section(section)
    config.set(section, key, value)
    save_config(config)

def list_config():
    config = load_config()
    output = []
    for section in config.sections():
        output.append(f"[{section}]")
        for key, value in config.items(section):
            output.append(f"{key} = {value}")
    return '\n'.join(output) if output else "No configuration found."