from setuptools import setup

setup (
  name = "nixos_panic_button",
  packages = ["msfocb"],
#  package_data = {
#    "msfocb": ["static/*", "templates/*"],
#  },
  entry_points = {
    "console_scripts": [
      "nixos_panic_button = msfocb.panic_button:main"
    ]
  },
)

