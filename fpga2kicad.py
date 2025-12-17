# /// script
# requires-python = ">=3.13"
# dependencies = [
#     "kipart",
#     "more-itertools",
# ]
# ///

import csv

from dataclasses import dataclass, field
from enum import StrEnum
import functools
from more_itertools import partition
from itertools import groupby

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
class UsrIOPinAddr:
    # UG571:
    #   Each bank is subdivided into four byte groups, each group containing 13 I/O pins as
    #   shown in Figure 2-1. Each byte group is further sub-divided into two nibble groups,
    #   as shown in Figure 2-2.
    byte_group: int # [0 to 3]
    nibble: str # 'U' for upper or 'L' for lower
    byte_pos: int # [0 to 12]

    def __post_init__(self):
        if self.nibble not in ('U', 'L'):
            raise ValueError(f'Invalid nibble designator `{self.nibble}`: expected `U` or `L` (upper/lower)')
        if self.byte_group < 0 or self.byte_group > 3:
            raise ValueError(f'Byte group out of range: {self.byte_group} (expected [0 to 3])')
        if self.byte_pos < 0 or self.byte_pos > 12:
            raise ValueError(f'Byte position out of range: {self.byte_pos} (expected [0 to 12])')



# TODO split v into hd/hp variants
@dataclass
class UsrIOPin:
    loc: Loc
    name: str

    class IOType(StrEnum):
        HD = "HD"
        HP = "HP"

    iotype: IOType
    bank: str

    # via ug1075 Chapter 1 § "Pin Definitions" (Table 1-4, p.22):
    #
    #  IO_L[1 to 24][P or N]_T[0 to 3][U or L]_N[0 to 12]_[multi-function]_[bank number] or
    #  IO_T[0 to 3][U or L]_N[0 to 12]_[multi-function]_[bank number]
    #
    #  Each user I/O pin name consists of several indicator labels, where:
    #  • IO indicates a user I/O pin.
    #  • L[1 to 24] indicates a unique differential pair with P (positive) and N (negative)
    #    sides. User I/O pins without the L indicator are single-ended.
    #  • T[0 to 3][U or L] indicates the assigned byte group and nibble location (upper or
    #    lower portion) within that group for the pin.
    #  • N[0 to 12] the number of the I/O within its byte group.
    #  • [multi-function] indicates any other functions that the pin can provide. If not used
    #    for this function, the pin can be a user I/O.
    #  • [bank number] indicates the assigned bank for the user I/O pin.
    #
    # Also: some pins are simply named
    #   IO_L[...]_[multi-function]_[bank number]

    diff_pair: str | None = field(init=False) # L[1 to 24][P or N]
    addr: UsrIOPinAddr | None = field(init=False)

    multi_func: str = field(init=False) # might be '' (empty string)

    def __post_init__(self):
        def consume_lit(s, lit):
            if not s.startswith(lit):
                raise ValueError(f'Expected `{s}` to start with {lit}')
            return s[:len(lit)], s[len(lit):]

        def consume_while(s, pred):
            n = 0
            while n < len(s) and pred(s[n]):
                n += 1
            return s[:n], s[n:]

        def consume_int(s):
            v, s = consume_while(s, lambda c: c.isdigit())
            return int(v), s

        s = self.name

        suf = f'{self.bank}'
        if not s.endswith(suf):
            raise ValueError(f'Missing bank suffix for pin `{self.name}`: expected `{suf}')
        s = s[:-len(suf)]

        _, s = consume_lit(s, "IO_")
        if s[0] == 'L':
            # L[1 to 24][P or N]_
            self.diff_pair, s = consume_while(s, lambda c: c != '_')
            s = s[1:] # eat the "_"
        else:
            self.diff_pair = None

        if s[0] == 'T':
            # T[0 to 3][U or L]_N[0 to 12]_
            _, s = consume_lit(s, "T")
            byte_group, s = consume_int(s)
            nibble, s = s[0], s[1:] # checked in addr's __post_init__()
            _, s = consume_lit(s, "_N")
            byte_pos, s = consume_int(s)
            _, s = consume_lit(s, "_")

            self.addr = UsrIOPinAddr(byte_group, nibble, byte_pos)
        else:
            self.addr = None

        if len(s) > 0:
            self.multifunction, s = s[:-1], s[-1:]
            _, s = consume_lit(s, "_")

        if len(s) != 0:
            raise ValueError(f'trailing characters for pin {self.name=}: {s=}')


@dataclass
class PowerPin:
    loc: Loc
    name: str

    bank: str | None

@dataclass
class MiscPin:
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

@dataclass
class GTRTermPin:
    # e.g. MGTRREF_[LRNS]
    # or   MGTAVTTRCAL*
    # or   PS_MGTRAVTT
    # or   PS_MGTRREF*
    #
    # NB: distinct from general analog power supply pins:
    #  MGTAVTT_[LRNS]
    #
    # TODO ugh, but the PS thing is all-in-one PS_MGTRAVTT:
    #   Analog power-supply pin for the transmitter and
    #   receiver termination circuits for the PS-GTR
    #   transceivers.

    loc: Loc
    name: str


Pin = MiscPin | PowerPin | UsrIOPin | GigXcvrPin | MIOPin | ConfigPin | DDRPin | NCPin | GTRTermPin

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
                if reg != "NA":
                    raise ParseError(n, f"unexpected logic region (field 6) for io pin: {r}")

                # parses either HD or HP into its corresponding enum variant, raising
                # AttributeError when we don't recognize it
                iotype = getattr(UsrIOPin.IOType, iotype)
                if iotype == "HD" and  mem != "NA":
                    raise ParseError(n, f"unexpected memory group (field 3) for io pin: {r}")

                pin = UsrIOPin(Loc(loc), name, iotype, bank)
            elif iotype.startswith("GT") or iotype.startswith("PSGT"):
                # TODO improve error
                if not all([f == "NA" for f in (mem, reg)]):
                    raise ParseError(n, f"unexpected extra fields for transceiver pin: {r}")

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
                    raise ParseError(n, f"unexpected extra fields for ddr pin: {r}")

                pin = DDRPin(Loc(loc), name, iotype, bank)
            elif name == "NC":
                # TODO improve error
                if not all([f == "NA" for f in reg[2:]]):
                    raise ParseError(n, f"unexpected extra fields for NC pin: {r}")

                pin = NCPin(Loc(loc))
            elif any(name.startswith(prefix) for prefix in [
                'MGTRREF', 'MGTAVTTRCAL', 'PS_MGTRREF', 'PS_MGTRAVTT']):

                # TODO improve error
                if not all([f == "NA" for f in (bank, iotype, mem, reg)]):
                    raise ParseError(n, f"unexpected extra fields for config pin: {r}")

                pin = GTRTermPin(Loc(loc), name)
            elif ("GND" in name
                or "VCC" in name
                or "VTT" in name):

                # TODO why do power pins have banks?
                # if not all([f == "NA" for f in r[2:]]):
                #     raise ParseError(n, f"unexpected extra fields (2+) for power pin: {r}")

                # TODO finer classification (esp. in/out ?)
                pin = PowerPin(Loc(loc), name, bank or None)
            else:
                pin = MiscPin(Loc(loc), name, r[2:])

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

    for p in by_type.pop(GTRTermPin):
        out.writerow([
            p.loc,
            p.name,
            "input" if "RREF" in p.name else "pwr", # type
            "right", # side (left/right/top/bottom)
            "mgt_power",
            "line", # style (TODO ?)
            "no",   # hidden
        ])

    # TODO we may wish to pluck pins from the pin list in a non-type-based way for these
    # they get used below as
    #       (misc_pins, bank_misc) = partition(lambda p: p.fields[1] == bank, misc_pins)
    # perhaps we want just a big ol' flat list instead of a by-type map, and we can
    # pluck things by time with an appropriate filter. O(N^2) in the number of pins,
    # so we'll be in real trouble when AMD ships their first million pin FPGA.
    misc_pins = by_type.pop(MiscPin)

    key = attrgetter('iotype')
    by_iotype = groupby(sorted(by_type.pop(UsrIOPin), key=key), key)

    # TODO once UsrIOPin is split along hd/hp lines, this can be split into two much simpler keys
    @dataclass
    class UsrIOSortKey:
        p: UsrIOPin

        def __lt__(self, o):
            # looks like all "hp" pins have an addr, but none of the "hd" pins do
            if (self.p.addr is None) != (o.p.addr is None):
                # sort addr-less pins first
                return self.p.addr is None
            elif self.p.addr is None:
                if self.p.diff_pair is not None and o.p.diff_pair is not None:
                    return DiffPairKey(self.p.diff_pair) < DiffPairKey(o.p.diff_pair)
                elif o.p.diff_pair is not None:
                    assert self.p.diff_pair is None
                    return True
                # TODO v
                # so far all of the pins
                raise ValueError(f'TODO sorting {self.p} against {o.p}')

            # making these "match" the order in UG571 (bits in descending order) puts N then P
            # (see p. 149)
            # e.g. L22N
            #      L22P
            #
            # so we prefer ascending order, here
            return (self.p.addr.byte_group, self.p.addr.byte_pos) < (o.p.addr.byte_group, o.p.addr.byte_pos)

    hd_pins = []
    hp_pins = []

    for iotype, pins in by_iotype:
        if iotype == "HD":
            hd_pins.extend(pins)
        elif iotype == "HP":
            hp_pins.extend(pins)
        else:
            raise ValueError(f"unrecognized {iotype=}")

    key = attrgetter('bank')
    # sorted is stable, meaning any sort we apply here across all
    # banks will hold within a bank
    hd_pins = sorted(hd_pins, key=UsrIOSortKey)
    hd_pins = sorted(hd_pins, key=key)
    hd_by_bank = groupby(hd_pins, key)

    hp_pins = sorted(hp_pins, key=UsrIOSortKey)
    hp_pins = sorted(hp_pins, key=key)
    hp_by_bank = groupby(hp_pins, key)

    iotype = UsrIOPin.IOType.HD
    for bank, pins in hd_by_bank:
        # TODO ontology lol
        (power_pins, bank_pwr) = partition(lambda p: p.bank == bank, power_pins)
        (misc_pins, bank_misc) = partition(lambda p: p.fields[1] == bank, misc_pins)

        unit = f'{iotype}_io_{bank}'
        for p in sorted(bank_misc, key=lambda p: p.loc):
            out.writerow([
                p.loc,
                p.name,
                "input",
                "left", # side (left/right/top/bottom)
                unit, # unit
                "line", # style (TODO ?)
                "no",   # hidden
            ])

        for p in sorted(bank_pwr, key=lambda p: p.loc):
            out.writerow([
                p.loc,
                p.name,
                "pwr",
                "top", # side (left/right/top/bottom)
                unit, # unit
                "line", # style (TODO ?)
                "no",   # hidden
            ])

        for p in pins:
            out.writerow([
                p.loc,
                p.name,
                "bidirectional",
                "right", # side (left/right/top/bottom)
                unit, # unit
                "line", # style (TODO ?)
                "no",   # hidden
            ])

    # TODO might be nice to "justify" the pin names so it's clearer what the grouping/order is
    # kicad align the pin name label to the edge of the symbol unit
    # e.g. currently (right sided):
    #     IO_L1P_T0L_N0_DBC_65 - AE10
    #     IO_L1P_T0L_N1_DBC_65 - AF10
    #         IO_L2P_T0L_N2_65 - AH12
    #         IO_L2P_T0L_N3_65 - AH11
    #   IO_L3P_T0L_N4_AD15P_65 - AE12
    #   IO_L3N_T0L_N4_AD15N_65 - AE12
    #
    # would be nice (?)
    #   IO_L1P_T0L_N0_DBC___65 - AE10
    #   IO_L1P_T0L_N1_DBC___65 - AF10
    #   IO_L2P_T0L_N2_______65 - AH12
    #   IO_L2P_T0L_N3_______65 - AH11
    #   IO_L3P_T0L_N4_AD15P_65 - AE12
    #   IO_L3N_T0L_N4_AD15N_65 - AE12
    #
    # (just within a nibble or across nibbles? some secondary function names are looong: SMBALERT, PERSTN0)

    iotype = UsrIOPin.IOType.HP
    for bank, pins in hp_by_bank:
        # TODO ontology lol
        (power_pins, bank_pwr) = partition(lambda p: p.bank == bank, power_pins)
        (misc_pins, bank_misc) = partition(lambda p: p.fields[1] == bank, misc_pins)

        unit = f'{iotype}_io_{bank}'
        for p in sorted(bank_misc, key=lambda p: p.loc):
            out.writerow([
                p.loc,
                p.name,
                "input",
                "left", # side (left/right/top/bottom)
                unit, # unit
                "line", # style (TODO ?)
                "no",   # hidden
            ])


        unit =  f'{iotype}_io_{bank}'
        for p in sorted(bank_pwr, key=lambda p: p.loc):
            out.writerow([
                p.loc,
                p.name,
                "pwr",
                "top", # side (left/right/top/bottom)
                unit, # unit
                "line", # style (TODO ?)
                "no",   # hidden
            ])

        last_bytegroup = None
        last_nibble = None
        for p in pins:
            bg = p.addr.byte_group
            nib = p.addr.nibble
            if last_bytegroup is None:
                last_bytegroup = bg
            if last_nibble is None:
                last_nibble = nib
            if last_bytegroup != bg:
                out.writerow(["*", "", "", "right", unit, "", ""])
                out.writerow(["*", "", "", "right", unit, "", ""])
                last_bytegroup = bg
                last_nibble = nib
            elif last_nibble != nib:
                out.writerow(["*", "", "", "right", unit, "", ""])
                last_nibble = nib

            out.writerow([
                p.loc,
                p.name,
                "bidirectional",
                "right", # side (left/right/top/bottom)
                unit, # unit
                "line", # style (TODO ?)
                "no",   # hidden
            ])


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
                    f'{iotype}_{bank}_{n//80}', # unit
                    "line", # style (TODO ?)
                    "no",   # hidden
                ])

                n += 1


    def grouped(pins, key):
        it = groupby(sorted(pins, key=key), key)
        # from the docs:
        # "The returned group is itself an iterator that shares the underlying iterable with groupby()."
        # so we need to eagerly evaluate the iterator if we want to be able to do unpacking
        # (otherwise the check for "any more values to unpack?" advances the iterator and throws
        # away the entire group)
        return [(k, list(g)) for k, g in it]

    class Spacer:
        pass

    pins = by_type.pop(ConfigPin)

    # Config pins are only "PSCONFIG" iotype (will throw otherwise)
    # TODO better error message than "ValueError: too many values to unpack (expected 1)"
    key = attrgetter('iotype')
    ((iotype, pins),) = grouped(pins, key)

    # effectively asserts that we have exactly one bank of pins
    # TODO better error message than "ValueError: too many values to unpack (expected 1)"
    key = attrgetter('bank')
    ((bank, pins),) = grouped(pins, key)

    pins = sorted(pins, key=lambda p: p.name)

    (pins, jtag_pins) = partition(matching(r'PS_JTAG_'), pins)
    (pins, mode_pins) = partition(matching(r'PS_MODE'), pins)
    (pins, clk_pins) = partition(matching(r'PS_REF_CLK|PS_PAD[IO]'), pins)

    # Put ref clk first, followed by rtc clock
    clk_pins = sorted(clk_pins, key=lambda p: p.name[:len("PS_?")], reverse=True)

    # config pins (note grouping)
    # cf. UG1075 Table 1-4 (p.23-24)
    cfg_pin_types = {
        "PS_REF_CLK": "input",
        "PS_PADI": "input",
        "PS_PADO": "output",

        "PS_DONE": "output",
        "PS_ERROR_OUT": "output",
        "PS_ERROR_STATUS": "output",
        "PS_INIT_B": "bidirectional",
        "PS_POR_B": "input",
        "PS_PROG_B": "input",
        "PS_SRST_B": "input",

        "PS_JTAG_TCK": "input",
        "PS_JTAG_TDI": "input",
        "PS_JTAG_TDO": "output",
        "PS_JTAG_TMS": "input",

        "PS_MODE0": "bidirectional",
        "PS_MODE1": "bidirectional",
        "PS_MODE2": "bidirectional",
        "PS_MODE3": "bidirectional",
    }
    unit = f'{iotype}_{bank}'
    side = "right"
    for p in [
        *clk_pins,
        Spacer(),
        *pins,
        Spacer(),
        *jtag_pins,
        Spacer(),
        *mode_pins
    ]:
        if isinstance(p, Spacer):
            out.writerow(["*", "", "", side, unit, "", ""])
            continue

        name = p.name
        style = "line"
        if name.endswith("_B"):
            name = f'~{{{p.name}}}'
            style = "inv"

        out.writerow([
            p.loc,
            name,
            cfg_pin_types[p.name], # TODO better error message than KeyError
            side, # side (left/right/top/bottom)
            unit,
            style, # style (TODO ?)
            "no",   # hidden
        ])


    pins = by_type.pop(DDRPin)

    # DDR pins are only "PSDDR" iotype (will throw otherwise)
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


    misc_pins = list(misc_pins)
    dbg_dump(hdr)
    for pin in misc_pins:
        dbg_dump(pin)

    n = 0
    for p in [p for p in [*misc_pins, *by_type.pop(NCPin)]]:
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

    pwr, gnds = partition(lambda x: "GND" in x.name, power_pins)

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

    if len(by_type) != 0:
        raise RuntimeError(
            f"Expected all pins to be consumed, but we still have {len(by_type)} types "
            f"left (a total of {sum(len(pp) for pp in by_type.values())} pins)")

import os
def dbg_dump(s, fd=3):
    os.write(fd, f'dbg@{fd}  {s}\n'.encode("utf-8"))


if __name__ == "__main__":
    import sys

    # suppress BrokenPipeError when e.g. used with `| head`
    # (we'll check for a complete input in `read_pins`)
    from signal import signal, SIGPIPE, SIG_DFL
    signal(SIGPIPE,SIG_DFL)

    main(sys.stdin, sys.stdout)
