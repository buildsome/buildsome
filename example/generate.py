import importlib

generator = importlib.import_module(''.join(["hidden", "dependency"]))
generator.generate("out/auto.h")
