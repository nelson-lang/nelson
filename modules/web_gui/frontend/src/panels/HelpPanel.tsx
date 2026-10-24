//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef, useState } from 'react';
import { rpc } from '../rpc/client';
import { DesktopIcon } from '../components/DesktopIcon';
//=============================================================================
// Documentation browser: a search box plus the configured remote help page or
// the generated local page for a topic. The text path is retained as a
// fallback when neither rich-document source is available.
export const HELP_TOPIC_EVENT = 'nelson-webview.help-topic';
export function HelpPanel(): JSX.Element {
  const [query, setQuery] = useState('');
  const [topic, setTopic] = useState('');
  const [text, setText] = useState('');
  const [richDocument, setRichDocument] = useState<{
    html?: string;
    url?: string;
    title: string;
  } | null>(null);
  const [loading, setLoading] = useState(false);
  const inputRef = useRef<HTMLInputElement>(null);
  const richPath = useRef('');

  const load = (name: string) => {
    setTopic(name);
    setQuery(name);
    richPath.current = '';
    setRichDocument(null);
    if (!name.trim()) {
      setText('Type a function name and press Enter to see its documentation.');
      return;
    }
    setLoading(true);
    rpc<{ html?: string; path?: string; title?: string; url?: string }>('help.document', {
      name: name.trim(),
    })
      .then((document) => {
        if ((!document.html && !document.url) || !document.path) {
          throw new Error('Rich help is unavailable');
        }
        richPath.current = document.path;
        if (document.url) {
          window.open(document.url, '_blank', 'noopener,noreferrer');
        }
        setRichDocument({
          html: document.html,
          url: document.url,
          title: document.title || name.trim(),
        });
      })
      .catch((error) => {
        console.warn('Nelson rich help unavailable; using text help.', error);
        return rpc<{ text?: string }>('help.text', { name: name.trim() })
          .then((r) =>
            setText(r.text && r.text.trim() ? r.text : `No documentation found for "${name}".`)
          )
          .catch(() => setText(`No documentation found for "${name}".`));
      })
      .finally(() => setLoading(false));
  };

  useEffect(() => {
    load('');
    const onTopic = (e: Event) => {
      const t = (e as CustomEvent<{ topic?: string }>).detail?.topic ?? '';
      if (t) load(t);
      inputRef.current?.focus();
    };
    window.addEventListener(HELP_TOPIC_EVENT, onTopic as EventListener);
    return () => window.removeEventListener(HELP_TOPIC_EVENT, onTopic as EventListener);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  useEffect(() => {
    const onRichLink = (event: MessageEvent<{ source?: string; href?: string }>) => {
      if (event.data?.source !== 'nelson-wasm-help' || !event.data.href || !richPath.current) {
        return;
      }
      const base = richPath.current.slice(0, richPath.current.lastIndexOf('/') + 1);
      const path = new URL(event.data.href, `https://nelson.local${base}`).pathname;
      setLoading(true);
      rpc<{ html?: string; path?: string; title?: string; url?: string }>('help.document', { path })
        .then((document) => {
          if ((!document.html && !document.url) || !document.path) {
            throw new Error('Help page is unavailable');
          }
          richPath.current = document.path;
          setTopic(document.title || path.split('/').pop() || '');
          setQuery(document.title || path.split('/').pop() || '');
          setRichDocument({
            html: document.html,
            url: document.url,
            title: document.title || '',
          });
        })
        .catch(() => undefined)
        .finally(() => setLoading(false));
    };
    window.addEventListener('message', onRichLink);
    return () => window.removeEventListener('message', onRichLink);
  }, []);

  // Split the help text into React nodes, linking the names on a "See also" line.
  const rendered = (() => {
    const lines = text.split('\n');
    return lines.map((line, i) => {
      const m = /^(\s*See also:?\s*)(.+?)(\.?)$/i.exec(line);
      if (m) {
        const names = m[2].split(/,\s*/);
        return (
          <div key={i}>
            {m[1]}
            {names.map((n, j) => {
              const clean = n.trim().replace(/\.$/, '');
              return (
                <span key={j}>
                  <a className="help-link" onClick={() => load(clean)}>
                    {clean}
                  </a>
                  {j < names.length - 1 ? ', ' : ''}
                </span>
              );
            })}
            {m[3]}
          </div>
        );
      }
      return <div key={i}>{line || ' '}</div>;
    });
  })();

  return (
    <div className="help-panel">
      <div className="help-panel__bar">
        <input
          ref={inputRef}
          className="help-panel__search"
          placeholder="Search documentation (e.g. sin)"
          value={query}
          onChange={(e) => setQuery(e.target.value)}
          onKeyDown={(e) => {
            if (e.key === 'Enter') load(query);
          }}
          spellCheck={false}
        />
        <button className="help-panel__go" onClick={() => load(query)} title="Search">
          <DesktopIcon name="search" size={18} />
        </button>
      </div>
      <div className="help-panel__body">
        {loading ? (
          <div className="browser__empty">loading…</div>
        ) : richDocument ? (
          richDocument.url ? (
            <div className="help-panel__text">
              <div className="help-panel__topic">{richDocument.title}</div>
              <p>Documentation opened in a new browser tab.</p>
              <a href={richDocument.url} target="_blank" rel="noreferrer">
                Open documentation
              </a>
            </div>
          ) : (
            <iframe
              className="help-panel__frame"
              title={richDocument.title || 'Nelson Documentation'}
              srcDoc={richDocument.html ?? ''}
            />
          )
        ) : (
          <div className="help-panel__text">
            {topic ? <div className="help-panel__topic">{topic}</div> : null}
            {rendered}
          </div>
        )}
      </div>
    </div>
  );
}
//=============================================================================
