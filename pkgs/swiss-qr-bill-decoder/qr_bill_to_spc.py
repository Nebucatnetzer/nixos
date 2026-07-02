# Be aware this is AI generated code
"""Convert swiss-qr-bill-decoder JSON into the Swiss Payments Code (SPC) payload.

The decoder emits the decoded bill as JSON on stdin; this reads it and writes the Swiss
QR-bill text payload (the same string that is encoded in the QR code) to stdout,
following the Swiss Implementation Guidelines QR-bill, version 0200.
"""

from __future__ import annotations

import json
import sys
from typing import Any


def split_street_and_number(address_line: str) -> tuple[str, str]:
    """Split e.g. 'Chemin de la Redoute 54' into ('Chemin de la Redoute', '54').

    For a structured address the building number is the trailing token, but only when it
    actually looks like a number. Streets without a number (e.g. a P.O. box) keep the
    whole line as the street name and an empty building number.
    """
    if not address_line:
        return "", ""
    street, separator, number = address_line.rpartition(" ")
    if separator and any(character.isdigit() for character in number):
        return street, number
    return address_line, ""


def split_postal_code_and_town(address_line: str) -> tuple[str, str]:
    """Split e.g. '1260 Nyon' into ('1260', 'Nyon').

    The postal code is the leading token; the remainder is the town. If the first token
    is not numeric the whole line is treated as the town.
    """
    if not address_line:
        return "", ""
    postal_code, separator, town = address_line.partition(" ")
    if separator and any(character.isdigit() for character in postal_code):
        return postal_code, town
    return "", address_line


def address_fields(address: dict[str, Any] | None) -> list[str]:
    """Return the seven SPC address fields for a single party."""
    if not address:
        return ["", "", "", "", "", "", ""]

    address_type = address.get("address_type", "") or ""
    line_1 = address.get("address_line_1", "") or ""
    line_2 = address.get("address_line_2", "") or ""

    if address_type == "S":
        # Structured address: the decoder merged street/number and postcode/town.
        street_or_line_1, building_or_line_2 = split_street_and_number(line_1)
        postal_code, town = split_postal_code_and_town(line_2)
    else:
        # Combined address ("K"): both lines are used verbatim, no postcode/town.
        street_or_line_1, building_or_line_2 = line_1, line_2
        postal_code, town = "", ""

    return [
        address_type,
        address.get("name", "") or "",
        street_or_line_1,
        building_or_line_2,
        postal_code,
        town,
        address.get("country", "") or "",
    ]


def build_spc_payload(bill: dict[str, Any]) -> str:
    """Assemble the 31-line (or more) SPC payload for one decoded bill."""
    reserved_ultimate_creditor = ["", "", "", "", "", "", ""]

    lines = [
        "SPC",
        "0200",
        "1",
        bill.get("iban", "") or "",
        *address_fields(bill.get("recipient_address")),
        *reserved_ultimate_creditor,
        bill.get("amount", "") or "",
        bill.get("currency", "") or "",
        *address_fields(bill.get("sender_address")),
        bill.get("reference_type", "") or "",
        bill.get("reference", "") or "",
        bill.get("message", "") or "",
        "EPD",
    ]

    billing_information = bill.get("billing_information")
    if billing_information:
        lines.append(billing_information)

    return "\n".join(lines)


def main() -> None:
    """Read the decoder JSON from stdin and write the SPC payload to stdout."""
    data = json.load(sys.stdin)
    bill = data[0] if isinstance(data, list) else data
    if not bill:
        sys.exit("No QR bill found in the decoder output.")
    sys.stdout.write(build_spc_payload(bill))


if __name__ == "__main__":
    main()
