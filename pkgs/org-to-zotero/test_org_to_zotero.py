from typing import Any

from org_to_zotero import merge_preferences
from org_to_zotero import read_urls
from org_to_zotero import strip_tracking


def test_strip_tracking_removes_newsletter_parameters() -> None:
    url = (
        "https://www.republik.ch/2026/10/08/heute?utm_source=newsletter"
        "&utm_medium=email&utm_campaign=republik&email=YW5kcmVhc0AybGkuY2g"
    )
    assert strip_tracking(url) == "https://www.republik.ch/2026/10/08/heute"


def test_strip_tracking_removes_ref_cmp_and_mailchimp_ids() -> None:
    assert strip_tracking("https://a.example/x/?ref=selfh.st") == "https://a.example/x/"
    assert strip_tracking("https://a.example/x?CMP=Share") == "https://a.example/x"
    assert strip_tracking("https://a.example/x?mc_cid=65") == "https://a.example/x"


def test_strip_tracking_removes_e_only_on_mailchimp() -> None:
    mailchimp_url = "https://mailchi.mp/zsa/the-ergo-94?e=8d8c6b4f76"
    assert strip_tracking(mailchimp_url) == "https://mailchi.mp/zsa/the-ergo-94"
    other_url = "https://a.example/search?e=1"
    assert strip_tracking(other_url) == other_url


def test_strip_tracking_keeps_other_parameters_and_fragment() -> None:
    url = "https://www.youtube.com/watch?v=C0tQB6BVpc4&utm_source=x#t=10"
    assert strip_tracking(url) == "https://www.youtube.com/watch?v=C0tQB6BVpc4#t=10"


def test_strip_tracking_leaves_clean_url_unchanged() -> None:
    url = "https://www.republik.ch/a?utm=1&q=a%2Fb"
    assert strip_tracking(url) == url


def test_read_urls_strips_tracking_before_skipping_duplicates() -> None:
    text = (
        "* https://selfh.st/weekly/?ref=self-host-weekly-newsletter\n"
        "* [[https://selfh.st/weekly/][Self-Host Weekly]]\n"
    )
    assert read_urls(text) == ["https://selfh.st/weekly/"]


def test_reads_org_link_with_label() -> None:
    text = "* [[https://example.com/a][Some title]] :read:\n"
    assert read_urls(text) == ["https://example.com/a"]


def test_reads_org_link_without_label() -> None:
    text = "- [[https://example.com/a]]\n"
    assert read_urls(text) == ["https://example.com/a"]


def test_reads_bare_url_with_query() -> None:
    text = "Look at https://example.com/a?b=1 later\n"
    assert read_urls(text) == ["https://example.com/a?b=1"]


def test_reads_angle_bracket_url() -> None:
    text = "<https://example.com/a>\n"
    assert read_urls(text) == ["https://example.com/a"]


def test_strips_sentence_punctuation() -> None:
    text = "See https://example.com/a. Also https://example.com/b, too.\n"
    assert read_urls(text) == ["https://example.com/a", "https://example.com/b"]


def test_ignores_non_http_links() -> None:
    text = "* [[file:notes.org][Notes]]\n* [[mailto:someone@example.com][Mail]]\n"
    assert read_urls(text) == []


def test_keeps_file_order_and_skips_duplicates() -> None:
    text = (
        "* [[https://example.com/b][B]]\n"
        "* [[https://example.com/a][A]]\n"
        "https://example.com/b\n"
    )
    assert read_urls(text) == ["https://example.com/b", "https://example.com/a"]


def test_merge_preferences_keeps_other_keys_and_overrides_values() -> None:
    current: dict[str, Any] = {
        "translate": {"enabled": True, "other": 1},
        "profile": {"exit_type": "Crashed", "name": "Person 1"},
    }
    wanted: dict[str, Any] = {
        "translate": {"enabled": False},
        "profile": {"exit_type": "Normal"},
    }
    merge_preferences(current, wanted)
    assert current == {
        "translate": {"enabled": False, "other": 1},
        "profile": {"exit_type": "Normal", "name": "Person 1"},
    }


def test_merge_preferences_does_not_share_nested_dicts() -> None:
    wanted: dict[str, Any] = {"translate": {"enabled": False}}
    current: dict[str, Any] = {}
    merge_preferences(current, wanted)
    current["translate"]["enabled"] = True
    assert wanted == {"translate": {"enabled": False}}
