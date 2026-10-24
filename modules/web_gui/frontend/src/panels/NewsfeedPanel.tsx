//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useState } from 'react';
import { rpc } from '../rpc/client';
import type { NewsfeedListResult, NewsfeedItem } from '../rpc/types';
//=============================================================================
const DEFAULT_FEEDS = [
  'https://github.com/nelson-lang/nelson/releases.atom',
  'https://hnrss.org/frontpage',
  'https://feeds.bbci.co.uk/news/rss.xml',
];
//=============================================================================
// Strip markup from a feed summary: feeds carry untrusted HTML, so the web
// panel shows plain text rather than injecting it into the app origin.
function toText(html: string): string {
  if (!html) {
    return '';
  }
  const doc = new DOMParser().parseFromString(html, 'text/html');
  return (doc.body.textContent || '').replace(/\s+/g, ' ').trim();
}
//=============================================================================
export function NewsfeedPanel(): JSX.Element {
  const [url, setUrl] = useState(DEFAULT_FEEDS[0]);
  const [title, setTitle] = useState('');
  const [items, setItems] = useState<NewsfeedItem[]>([]);
  const [status, setStatus] = useState('Loading…');

  const load = (feedUrl: string) => {
    setStatus('Loading…');
    rpc<NewsfeedListResult>('newsfeed.list', { url: feedUrl })
      .then((r) => {
        const feed = r.feed || {};
        setTitle(feed.title || '');
        const list = Array.isArray(feed.items) ? feed.items : [];
        setItems(list);
        setStatus(list.length ? `${list.length} items` : 'No item in this feed.');
      })
      .catch((e) => {
        setItems([]);
        setStatus(`Unable to load the feed: ${e?.message ?? e}`);
      });
  };

  useEffect(() => {
    load(url);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  return (
    <div className="browser newsfeed">
      <div className="newsfeed__toolbar">
        <input
          className="browser__search"
          list="newsfeed-feeds"
          value={url}
          onChange={(e) => setUrl(e.target.value)}
          onKeyDown={(e) => {
            if (e.key === 'Enter') {
              load(url);
            }
          }}
          placeholder="Feed URL…"
        />
        <datalist id="newsfeed-feeds">
          {DEFAULT_FEEDS.map((f) => (
            <option key={f} value={f} />
          ))}
        </datalist>
        <button className="newsfeed__refresh" onClick={() => load(url)}>
          Refresh
        </button>
      </div>
      <div className="newsfeed__meta">
        {title ? <span className="newsfeed__title">{title}</span> : null}
        <span className="newsfeed__status">{status}</span>
      </div>
      <ul className="newsfeed__list">
        {items.map((it, i) => (
          <li key={i} className="newsfeed__item">
            {it.image ? (
              <img className="newsfeed__thumb" src={it.image} alt="" loading="lazy" />
            ) : null}
            <div className="newsfeed__body">
              {it.link ? (
                <a
                  className="newsfeed__headline"
                  href={it.link}
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  {it.title || '(no title)'}
                </a>
              ) : (
                <span className="newsfeed__headline">{it.title || '(no title)'}</span>
              )}
              {it.date ? <div className="newsfeed__date">{it.date}</div> : null}
              {it.summary ? <div className="newsfeed__summary">{toText(it.summary)}</div> : null}
            </div>
          </li>
        ))}
      </ul>
    </div>
  );
}
//=============================================================================
