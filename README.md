# fpga2kicad.py

Converts an AMD (Xilinx) pin listing to a kicad schematic symbol. (Note: Not a footprint)

:warning: Very much a WIP, this is currently only being used for/tested with the xczu9egffvb1156 .

## Usage

Step 0: Install uv: https://docs.astral.sh/uv/

Step 1: Download and extract the CSV "ASCII Pinout Files", e.g.

```
curl -vfsLO https://www.xilinx.com/support/packagefiles/zuppackages/zupall.zip
unzip zupall.zip
```

Step 2: Pick your favorite pinout csv and run it through the tool(s):

```
<zupall/xczu9egffvb1156pkg.csv uv run fpga2kicad.py >kipart.csv
uvx kipart kipart.csv -o FPGA.kicad_sym
```

