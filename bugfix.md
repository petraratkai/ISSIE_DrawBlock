# Changes after demo
- Swapped the order of the Sum and Cout ports in adder
- Fixed crashing issue when loading a sheet due to incorrect initialisation of symbols

# Known Bugs
- If a custom component port is renamed (i.e. by deleting the port then replacing it with a port of a different name), the custom component isn't updated correctly in other sheets (despite selecting update on the prompt). We didn't know that this renaming was something we needed to consider (until about 2 hours before the deadline). This renders the port of the old custom component instances un-usable. This can be worked around by manually deleting the old custom component and putting a new one back in, but this obviously isn't ideal ( sorry :( ).