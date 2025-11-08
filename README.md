# fpga2kicad.py

Converts an AMD (Xilinx) pin listing to a kicad schematic symbol. (Note: Not a footprint)

## Usage

Step 0: Install uv: https://docs.astral.sh/uv/

Step 1: Download and extract the CSV "ASCII Pinout Files", e.g.

```
curl -vfsLO https://www.xilinx.com/support/packagefiles/zuppackages/zupall.zip
unzip zupall.zip
```

Step 2: Pick your favorite csv and run it through the tool:

```
<zupall/xczu9egffvb1156pkg.csv uv run fpga2kicad.py
```

