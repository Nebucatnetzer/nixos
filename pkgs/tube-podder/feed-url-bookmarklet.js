javascript: (() => {
  /* Copy the tube-podder feed URL of the YouTube channel the current page belongs to.

     Install it by making a new bookmark and pasting this whole file, the leading
     "javascript:" included, into its URL field. Then open a channel page or any video of
     the channel and click the bookmark: the feed URL lands in the clipboard and a note
     with the channel name appears in the corner, which is what feeds.toml wants as the
     'description' of the [[feed]] table.

     Only block comments are used and every statement ends in a semicolon, so the code
     still works when a browser collapses the bookmark into a single line. */

  const CHANNEL_ID = /(UC[0-9A-Za-z_-]{22})/;
  const FEED_URL = "https://www.youtube.com/feeds/videos.xml?channel_id=";
  const NOTE_SECONDS = 5;

  /* getAttribute rather than the property: a <link> has no 'content' property, and the
     raw value is enough for a regex that only wants the ID out of it. */
  const attribute = (selector, name) => {
    const element = document.querySelector(selector);
    return element ? element.getAttribute(name) || "" : "";
  };

  const captured = (pattern) => {
    const match = document.documentElement.innerHTML.match(pattern);
    return match ? match[1] : "";
  };

  /* A channel page says it outright, in its feed link and in its own URL. A video page
     says it only in the data the player was handed, where the channel of the video comes
     before every other one. */
  const sources = [
    () => attribute('link[type="application/rss+xml"]', "href"),
    () => location.href,
    () => attribute('meta[itemprop="identifier"]', "content"),
    () => captured(/"externalId":"(UC[0-9A-Za-z_-]{22})"/),
    () => captured(/"channelId":"(UC[0-9A-Za-z_-]{22})"/),
  ];

  const note = (message, ok) => {
    const box = document.createElement("div");
    box.textContent = message;
    box.style.cssText = [
      "position:fixed;top:16px;right:16px;z-index:2147483647",
      "max-width:min(90vw,520px);padding:10px 14px;border-radius:8px",
      "font:13px/1.45 monospace;white-space:pre-wrap;word-break:break-all",
      "color:#fff;box-shadow:0 2px 12px rgba(0,0,0,.45)",
      "background:" + (ok ? "#1c6b39" : "#9b2c2c"),
    ].join(";");
    document.body.appendChild(box);
    setTimeout(() => box.remove(), NOTE_SECONDS * 1000);
  };

  /* Firefox does not always hand a bookmarklet the user gesture that the clipboard API
     asks for, so the old way is tried next and a prompt is the last resort. */
  const copyByHand = (text) => {
    const area = document.createElement("textarea");
    area.value = text;
    area.style.cssText = "position:fixed;top:0;left:0;opacity:0";
    document.body.appendChild(area);
    area.select();
    let copied = false;
    try {
      copied = document.execCommand("copy");
    } catch (error) {
      copied = false;
    }
    area.remove();
    return copied;
  };

  let id = "";
  for (const source of sources) {
    const match = CHANNEL_ID.exec(String(source() || ""));
    if (match) {
      id = match[1];
      break;
    }
  }

  if (!id) {
    note("No YouTube channel ID on this page.\nOpen a channel page or one of its videos.", false);
    return;
  }

  const name =
    attribute('span[itemprop="author"] link[itemprop="name"]', "content") ||
    attribute('meta[itemprop="name"]', "content") ||
    document.title.replace(/ - YouTube$/, "");
  const url = FEED_URL + id;
  const done = () => note("Feed URL copied\n" + name + "\n" + url, true);
  const byHand = () => {
    if (copyByHand(url)) {
      done();
      return;
    }
    prompt("Copy the feed URL:", url);
  };

  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(url).then(done, byHand);
  } else {
    byHand();
  }
})();
