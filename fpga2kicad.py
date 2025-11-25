# /// script
# requires-python = ">=3.13"
# dependencies = [
#     "kipart",
#     "more-itertools",
# ]
# ///

import csv

from dataclasses import dataclass
import functools
from more_itertools import partition
from itertools import groupby

import os

@functools.total_ordering
@dataclass
class Loc:
    loc: str # e.g. A1 or AD15

    def __eq__(self, o):
        # NB: 'a1' != 'A1'
        return self.loc == o.loc

    def __lt__(self, o):
        # A1 < B1 < B2 < B12 < AA1
        def split(loc: str):
            n = 0
            while not loc[n].isdigit():
                n += 1
            return loc[:n], loc[n:]

        sla, sln = split(self.loc)
        ola, oln = split(o.loc)

        return (len(sla), sla, int(sln)) < (len(ola), ola, int(oln))

    def __str__(self):
        return self.loc



@dataclass
class UsrIOPin:
    loc: Loc
    name: str

    iotype: str # e.g. HP or HD
    bank: str

@dataclass
class PowerPin:
    loc: Loc
    name: str

@dataclass
class UnknownPin:
    loc: Loc
    name: str

    fields: [str]

@dataclass
class GigXcvrPin:
    loc: Loc
    name: str

    iotype: str # e.g. GTH or GTY
    bank: str

@dataclass
class MIOPin:
    loc: Loc
    name: str

    iotype: str # e.g. PSMIO or ... ?
    bank: str

# TODO these are kind of magical; what are they really?
@dataclass
class ConfigPin:
    loc: Loc
    name: str

    iotype: str
    bank: str

@dataclass
class DDRPin:
    loc: Loc
    name: str

    iotype: str
    bank: str

@dataclass
class NCPin:
    loc: Loc
    # name will always be "NC"

Pin = UnknownPin | PowerPin | UsrIOPin | GigXcvrPin | MIOPin | ConfigPin | DDRPin | NCPin

def parse_pins(rows):
    class ParseError(Exception):
        def __str__(self):
            return ": ".join([f"at line {self.args[0]+1}", *[str(arg) for arg in self.args[1:]]])

    def check_empty(r):
        """
            looks for an empty line, e.g.
            ,,,,,
        """
        return all([c == "" for c in r])

    # parser "cursor" (row)
    n = 0

    # consumes the first howevermany single-column rows that look like:
    #     "# some comment"
    for r in rows[n:]:
        if len(r) == 1 and r[0].startswith("#"):
            n += 1
        else:
            break

    if not check_empty(rows[n]):
        raise ParseError(n, f"expected empty line, saw row: {rows[n]}")
    n += 1

    # check for the expected header
    #     Pin,Pin Name,Memory Byte Group,Bank,I/O Type,Super Logic Region
    if rows[n][0] != "Pin":
        raise ParseError(n, f'expected header, saw row: {rows[n]}')

    hdr = rows[n]
    n += 1

    pins = []
    try:
        for r in rows[n:]:
            if r[0] == "":
                break

            loc = r[0]
            name = r[1]
            mem = r[2]
            bank = r[3]
            iotype = r[4]
            reg = r[5]

            # convert from string-ly typed
            pin: Pin = None # None so we can catch any missing pins (see below)

            if name.startswith("IO_"):
                # TODO okay why do io pins have memory groups?
                # if mem != "NA":
                #     raise ParseError(n, f"unexpected memory group (field 3) for io pin: {r}")
                if reg != "NA":
                    raise ParseError(n, f"unexpected logic region (field 6) for io pin: {r}")

                pin = UsrIOPin(Loc(loc), name, iotype, bank)
            elif iotype.startswith("GT") or iotype.startswith("PSGT"):
                # TODO improve error
                if not all([f == "NA" for f in (mem, reg)]):
                    raise ParseError(n, f"unexpected extra fields for power pin: {r}")

                pin = GigXcvrPin(Loc(loc), name, iotype, bank)
            elif name.startswith("PS_MIO"):
                # TODO improve error
                if not all([f == "NA" for f in (mem, reg)]):
                    raise ParseError(n, f"unexpected extra fields for mio pin: {r}")

                pin = MIOPin(Loc(loc), name, iotype, bank)
            elif iotype == "PSCONFIG":
                # TODO improve error
                if not all([f == "NA" for f in (mem, reg)]):
                    raise ParseError(n, f"unexpected extra fields for config pin: {r}")

                pin = ConfigPin(Loc(loc), name, iotype, bank)
            elif iotype == "PSDDR":
                # TODO improve error
                if not all([f == "NA" for f in (mem, reg)]):
                    raise ParseError(n, f"unexpected extra fields for config pin: {r}")

                pin = DDRPin(Loc(loc), name, iotype, bank)
            elif name == "NC":
                # TODO improve error
                if not all([f == "NA" for f in reg[2:]]):
                    raise ParseError(n, f"unexpected extra fields for config pin: {r}")

                pin = NCPin(Loc(loc))
            elif ("GND" in name
                or "VCC" in name
                or "VTT" in name):

                # TODO why do power pins have banks?
                # if not all([f == "NA" for f in r[2:]]):
                #     raise ParseError(n, f"unexpected extra fields (2+) for power pin: {r}")

                # TODO finer classification (esp. in/out ?)
                pin = PowerPin(Loc(loc), name)
            else:
                pin = UnknownPin(Loc(loc), name, r[2:])

            if not pin:
                raise ValueError(f'failed to parse pin')
            pins.append(pin)

            n += 1
    except Exception as exc:
        if isinstance(exc, ParseError):
            raise exc
        row = rows[n]
        raise ParseError(n, f"{row=}") from exc

    # check footer
    if not check_empty(rows[n]):
        raise ParseError(n, f"expected empty line, saw row: {rows[n]}")
    n += 1

    if rows[n][0] != "Total Number of Pins":
        raise ParseError(n, f"expected trailer, saw row: {rows[n]}")

    total = int(rows[n][1])
    if total != len(pins):
        raise ParseError(n, f"expected {total} pins, but saw {len(pins)}")

    return hdr, pins


def dbg_dump(s, fd=3):
    os.write(fd, f'dbg@{fd}  {s}\n'.encode("utf-8"))


def main(ins, outs):
    inp = csv.reader(ins)
    rows = list(inp)
    hdr, pins = parse_pins(rows)

    from collections import OrderedDict
    by_type = OrderedDict()
    for p in pins:
        if type(p) not in by_type:
            by_type[type(p)] = []
        by_type[type(p)].append(p)

    for t, v in by_type.items():
        dbg_dump(f'{t}: {len(v)}')

    dbg_dump(hdr)
    for pin in by_type[UnknownPin]:
        dbg_dump(pin)

    print("""XCZU9EG,
Reference:,U
Value:,XCZU9EG
Footprint:,FFVB1156_XIL
Datasheet:,XCZU9EG-2FFVB1156I
Description:,
ki_locked:,
ki_keywords:,XCZU9EG-2FFVB1156I
ki_fp_filters:,FFVB1156_XIL FFVB1156_XIL-M FFVB1156_XIL-L
pin,name,type,side,unit,style,hidden""")

    out = csv.writer(outs)

    @functools.total_ordering
    @dataclass
    class LastNumKey:
        s: str  #  xxxx_12_yyy < xxxx_13_yyy < xxxx_124_yyy

        def __eq__(self, o):
            return self.s == o.s


        def __lt__(self, o):
            # FIXME could probably memoize this in a constructor
            def splitnum(s: str):
                m = -1
                while not s[m].isdigit():
                    m -= 1
                    if m < -len(s):
                        return (s, '', '')

                n = m
                while s[n-1].isdigit():
                    n -= 1

                return s[:n], int(s[n:len(s)+m+1]), s[(m+1):]

            return splitnum(self.s) < splitnum(o.s)


    class DiffPairKey:
        pn: str # P or N
        num: int

        # accepts xxx01P or xxxP01
        def __init__(self, pin: Pin | str):
            name = pin.name if hasattr(pin, 'name') else pin

            pn = None
            if name.endswith('P') or name.endswith('N'):
                pn = name[-1]
                name = name[:-1]
                n = -2
            else:
                n = -1

            while name[n].isdigit():
                n -= 1
            s = name[n:]
            pn = pn or s[0]
            if not (pn == 'P' or pn == 'N'):
                raise ValueError(f'could not find P/N suffix (not a diff pair?): {pin}')

            try:
                num = int(s[1:])
            except ValueError as e:
                raise ValueError(f'could not parse ordinal from `{pin}`: {e}')

            self.pn = pn
            self.num = int(s[1:])

        def __lt__(self, o):
            # sort P before N
            return (self.num, o.pn) < (o.num, self.pn)


    import re
    def matching(pattern):
        """
        NB: re.match only matches at the beginning of the string!
        """
        # cf. https://docs.python.org/3/library/re.html#search-vs-match
        pat = re.compile(pattern)
        return lambda p: pat.match(p.name)



    # TODO these cry out for a model like
    #       [ unit(s) @ [group1, ..., Spacer, ...] , unit(s) @ [group2, ...]]
    # and then something that maps ^ to "rows"
    # (would ease fuzzy "make me three same-sized units of gnds")

    from operator import itemgetter, attrgetter

    key = attrgetter('iotype')
    by_iotype = groupby(sorted(by_type.pop(GigXcvrPin), key=key), key)

    for iotype, pins in by_iotype:
        key = attrgetter('bank')
        by_bank = groupby(sorted(pins, key=key), key)

        for bank, pins in by_bank:
            unit = f'm{iotype}_{bank}'

            # we want
            # |----------------------|
            # | rx_p0          tx_p0 |
            # | rx_n0          tx_n0 |
            #    ...            ...
            # |                      |
            # | refclk_p0            |
            # | refclk_n0            |
            #    ...
            # |----------------------|

            pins = sorted(pins, key=lambda p: DiffPairKey(p.name[:-len(f'_{bank}')]))

            (pins, clk_pins) = partition(matching(r'(PS_)?MGTREFCLK[0-9][PN]'), pins)
            for p in pins:
                out.writerow([
                    p.loc,
                    p.name,
                    "input" if 'RX' in p.name else 'output',
                    "left" if 'RX' in p.name else 'right',
                    unit,
                    "line", # style (TODO ?)
                    "no",   # hidden
                ])

            out.writerow(["*", "", "", "left", unit, "", ""])
            out.writerow(["*", "", "", "left", unit, "", ""])

            for p in clk_pins:
                out.writerow([
                    p.loc,
                    p.name,
                    "bidirectional",
                    "left",
                    unit,
                    "line", # style (TODO ?)
                    "no",   # hidden
                ])

    # Why is this here? Well, early revisions misclassified e.g. MGTVCC_L pin
    # as a GigXcvrPin, despite lack of an iotype or bank.
    #
    # That meant they showed up in the pin listing (and therefore unit listing)
    # much earlier than the other power pins (unit G for the xczu9cgffvb1156pkg)
    # and in the absence of a good migration path for library symbol unit
    # relabeling, we'll be carrying this forward for the foreseeable future.
    #
    # (Even this positioning represents a swap, becasue previously this block
    # came in between the MGT banks and PS_MGT bank(s), but it's only swapping
    # two adjacent units rather than re-labeling everything going forward, which
    # is an easier manual migration to describe and perform)
    power_pins = by_type.pop(PowerPin)
    (power_pins, mgt_pwr) = partition(lambda p: "MGT" in p.name, power_pins)

    for p in mgt_pwr:
        out.writerow([
            p.loc,
            p.name,
            "pwr",
            "right", # side (left/right/top/bottom)
            "mgt_power",
            "line", # style (TODO ?)
            "no",   # hidden
        ])

    key = attrgetter('iotype')
    by_iotype = groupby(sorted(by_type.pop(UsrIOPin), key=key), key)

    for iotype, pins in by_iotype:
        # sorted is stable, meaning any sort we apply here across all
        # banks will hold within a bank, below
        pins = sorted(pins, key=lambda p: p.name)

        key = attrgetter('bank')
        by_bank = groupby(sorted(pins, key=key), key)

        for bank, pins in by_bank:
            n = 0
            for p in pins:
                out.writerow([
                    p.loc,
                    p.name,
                    "bidirectional",
                    "right", # side (left/right/top/bottom)
                    f'{iotype}_io_{bank}{n//80}', # unit
                    "line", # style (TODO ?)
                    "no",   # hidden
                ])

                n += 1


    # TODO are MIO pins ever not "PSMIO" iotype?
    key = attrgetter('iotype')
    by_iotype = groupby(sorted(by_type.pop(MIOPin), key=key), key)

    for iotype, pins in by_iotype:
        # sorted is stable, meaning any sort we apply here across all
        # banks will hold within a bank, below
        pins = sorted(pins, key=lambda p: LastNumKey(p.name))

        key = attrgetter('bank')
        by_bank = groupby(sorted(pins, key=key), key)

        for bank, pins in by_bank:
            n = 0
            for p in pins:
                out.writerow([
                    p.loc,
                    p.name,
                    "bidirectional",
                    "right", # side (left/right/top/bottom)
                    f'{iotype}_{bank}{n//80}', # unit
                    "line", # style (TODO ?)
                    "no",   # hidden
                ])

                n += 1



    # TODO are these pins ever not "PSConfig" iotype?
    key = attrgetter('iotype')
    by_iotype = groupby(sorted(by_type.pop(ConfigPin), key=key), key)

    for iotype, pins in by_iotype:
        # sorted is stable, meaning any sort we apply here across all
        # banks will hold within a bank, below
        pins = sorted(pins, key=lambda p: p.name)

        key = attrgetter('bank')
        by_bank = groupby(sorted(pins, key=key), key)

        for bank, pins in by_bank:
            n = 0
            for p in pins:
                out.writerow([
                    p.loc,
                    p.name,
                    "bidirectional",
                    "right", # side (left/right/top/bottom)
                    f'{iotype}_{bank}{n//150}', # unit
                    "line", # style (TODO ?)
                    "no",   # hidden
                ])

                n += 1


    pins = by_type.pop(DDRPin)

    def grouped(pins, key):
        it = groupby(sorted(pins, key=key), key)
        # from the docs:
        # "The returned group is itself an iterator that shares the underlying iterable with groupby()."
        # so we need to eagerly evaluate the iterator if we want to be able to do unpacking
        # (otherwise the check for "any more values to unpack?" advances the iterator and throws
        # away the entire group)
        return [(k, list(g)) for k, g in it]

    # DDR pins are only not "PSDDR" iotype (will throw otherwise)
    # TODO better error message than "ValueError: too many values to unpack (expected 1)"
    key = attrgetter('iotype')
    ((iotype, pins),) = grouped(pins, key)

    # effectively asserts that we have exactly one bank of pins
    # TODO better error message than "ValueError: too many values to unpack (expected 1)"
    key = attrgetter('bank')
    ((bank, pins),) = grouped(pins, key)

    # pre-sort by numeric suffix (this order preserved when a later key compares equal)
    # i.e. PS_DDR_A9 < PS_DDR_A10 < ... < PS_DDR_DQ10 < ...
    pins = sorted(pins, key=lambda p: LastNumKey(p.name))

    # TODO: probably should use `search`, or assert that there are only PS_DDR_... pins
    (pins, dq_pins) = partition(matching(
        # NB: explicitly excludes DDR_DQS pins
        r'PS_DDR_DQ[0-9]'
    ), pins)
    (pins, addr_pins) = partition(matching(r'PS_DDR_A[0-9]'), pins)
    (pins, dm_pins) = partition(matching(r'PS_DDR_DM[0-9]'), pins)
    (pins, dqs_pins) = partition(matching(r'PS_DDR_DQS'), pins)
    dqs_pins = sorted(dqs_pins, key=DiffPairKey)

    (pins, numbered_pins) = partition(lambda p: p.name[-1].isdigit(), pins)

    def sided(pins, side="right"):
        return [
            (pin, side) for pin in pins
        ]

    class Spacer:
        pass

    def spaced(it):
        for g in it:
            for i in g:
                yield i
            yield Spacer()

    unit = f'{iotype}_{bank}'
    for (p, side) in [
        *sided(dq_pins, "left"),
        *sided([
            *addr_pins,
            Spacer(),
            *dm_pins,
            Spacer(),
            Spacer(),
            *dqs_pins,
            Spacer(),
            *pins,
            Spacer(),
            # TODO why is this? (why group BA0 and BG0 together?)
            *spaced([pp for _, pp in grouped(numbered_pins, lambda p: p.name[-1])])
        ], "right"),
    ]:
        if isinstance(p, Spacer):
            out.writerow(["*", "", "", side, unit, "", ""])
            continue

        out.writerow([
            p.loc,
            p.name,
            "bidirectional",
            side, # side (left/right/top/bottom)
            unit, # unit
            "line", # style (TODO ?)
            "no",   # hidden
        ])


    # these are for later (after the catch-all)
    pwr, gnds = partition(lambda x: "GND" in x.name, power_pins)

    # catch-all
    n = 0
    for p in [p for p_ty in by_type.values() for p in p_ty]:
        out.writerow([
            p.loc,
            p.name if not isinstance(p, NCPin) else "NC",
            "",     # type TODO
            "left", # side (left/right/top/bottom)
            f'misc{n//200}', # unit TODO
            "line", # style (TODO ?)
            "no",   # hidden
        ])
        n += 1


    @dataclass
    class PowerSortKey:
        p: PowerPin

        def __lt__(self, o):
            return (self.p.name, self.p.loc) < (o.p.name, o.p.loc)


    @dataclass
    class GndSortKey:
        p: PowerPin # only grounds

        def __lt__(self, o):
            # FIXME this list is incomplete (missing ADC_GND, ADC_SUB_GND, DAC_GND, DAC_SUB_GND)
            # FIXME also, likely to change over time
            j, k = (["GND", "RSVDGND", "GNDADC", "GND_PSADC"].index(n) for n in (self.p.name, o.p.name))

            return j < k or self.p.loc < o.p.loc

    n = 0
    for pp in sorted(pwr, key=PowerSortKey):
        out.writerow([
            pp.loc,
            pp.name,
            "pwr",     # type (kicad's "power_input")
            "left" if (n % (80*2)) < 80 else "right", # side (left/right/top/bottom)
            f'pwr{n//(80*2)}', # unit
            "line", # style (TODO ?)
            "no",   # hidden
        ])
        n += 1

    n = 0
    lname = "GND"
    for pp in sorted(gnds, key=GndSortKey):
        if pp.name != lname:
            # TODO this happens to be correct for the example
            #   but assuming unit, side and that there's space left
            #   (e.g. missing n += 1) isn't great
            out.writerow(["*", "", "", "right", "gnd1", "", ""])
            lname = pp.name
        out.writerow([
            pp.loc,
            pp.name,
            "gnd",     # type (kicad's "power_input")
            "left" if (n % (80*2)) < 80 else "right", # side (left/right/top/bottom)
            f'gnd{n//(80*2)}', # unit
            "line", # style (TODO ?)
            "no",   # hidden
        ])
        n += 1



if __name__ == "__main__":
    import sys

    # suppress BrokenPipeError when e.g. used with `| head`
    # (we'll check for a complete input in `read_pins`)
    from signal import signal, SIGPIPE, SIG_DFL
    signal(SIGPIPE,SIG_DFL)

    main(sys.stdin, sys.stdout)
