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
import { t, tf } from '../i18n';
//=============================================================================
// Documentation browser. Built-in help is shown through the real self-contained
// viewer (`help/<lang>/index.html`, served same-origin by the help service worker
// from the shipped bundles) so its own search box, TOC tree and inline images all
// work and load lazily per module. A package-qualified topic that is not a built-in
// is an installed toolbox whose pages live only in the engine filesystem; those
// keep rendering through the engine (help.document → inlined page / text).
export const HELP_TOPIC_EVENT = 'nelson-webview.help-topic';

type HelpIndexEntry = { title?: string; url?: string; path?: string };
type Mode =
  | { kind: 'viewer'; src: string }
  | { kind: 'inline'; html?: string; url?: string; title: string }
  | { kind: 'text'; topic: string; text: string };

export function HelpPanel(): JSX.Element {
  const [mode, setMode] = useState<Mode | null>(null);
  const [loading, setLoading] = useState(false);
  const langRef = useRef('en_US');
  const indexRef = useRef<HelpIndexEntry[] | null>(null);
  const richPath = useRef('');
  // The viewer is served by the help service worker; where that worker is not
  // active (e.g. a host that injects isolation headers itself) the panel falls
  // back to the engine-rendered inline/text path.
  const viewerReady = useRef(false);

  const viewerUrl = (lang: string, opts: { open?: string; search?: string }): string => {
    const url = new URL(`help/${lang}/index.html`, window.location.href);
    url.searchParams.set('lang', lang);
    if (opts.open) url.searchParams.set('open', opts.open);
    if (opts.search) url.searchParams.set('search', opts.search);
    return url.toString();
  };

  // The built-in search index (title → page), served by the SW from the `main`
  // bundle. Used to turn a `doc <name>` topic into a viewer page and to tell a
  // built-in topic from an installed toolbox.
  const ensureIndex = async (lang: string): Promise<HelpIndexEntry[]> => {
    if (indexRef.current) return indexRef.current;
    try {
      const response = await fetch(
        new URL(`help/${lang}/index.json`, window.location.href).toString()
      );
      const data = response.ok ? await response.json() : [];
      indexRef.current = Array.isArray(data) ? (data as HelpIndexEntry[]) : [];
    } catch {
      indexRef.current = [];
    }
    return indexRef.current;
  };

  const openInline = (name: string): void => {
    richPath.current = '';
    setLoading(true);
    rpc<{ html?: string; path?: string; title?: string; url?: string }>('help.document', { name })
      .then((document) => {
        if ((!document.html && !document.url) || !document.path) {
          throw new Error('Rich help is unavailable');
        }
        richPath.current = document.path;
        if (document.url) {
          window.open(document.url, '_blank', 'noopener,noreferrer');
        }
        setMode({
          kind: 'inline',
          html: document.html,
          url: document.url,
          title: document.title || name,
        });
      })
      .catch(() =>
        rpc<{ text?: string }>('help.text', { name })
          .then((r) =>
            setMode({
              kind: 'text',
              topic: name,
              text: r.text && r.text.trim() ? r.text : tf('No documentation found for "%1".', name),
            })
          )
          .catch(() =>
            setMode({
              kind: 'text',
              topic: name,
              text: tf('No documentation found for "%1".', name),
            })
          )
      )
      .finally(() => setLoading(false));
  };

  const openTopic = async (rawTopic: string): Promise<void> => {
    const lang = langRef.current;
    const name = (rawTopic || '').trim();
    if (!viewerReady.current) {
      // No service-worker viewer: use the engine-rendered path.
      if (!name) {
        setMode({
          kind: 'text',
          topic: '',
          text: t('Type a function name and press Enter to see its documentation.'),
        });
        return;
      }
      openInline(name);
      return;
    }
    if (!name) {
      setMode({ kind: 'viewer', src: viewerUrl(lang, {}) });
      return;
    }
    const index = await ensureIndex(lang);
    const exact = index.find((entry) => (entry.title || '').toLowerCase() === name.toLowerCase());
    if (exact && (exact.url || exact.path)) {
      setMode({
        kind: 'viewer',
        src: viewerUrl(lang, { open: (exact.url || exact.path) as string }),
      });
      return;
    }
    // Package-qualified and unknown to the built-in index → installed toolbox.
    if (name.includes('.')) {
      openInline(name);
      return;
    }
    // A partial or misspelt name: let the viewer show matching results.
    setMode({ kind: 'viewer', src: viewerUrl(lang, { search: name }) });
  };

  useEffect(() => {
    let cancelled = false;
    rpc<{ lang?: string }>('help.lang', {})
      .then((r) => {
        if (!cancelled && r?.lang === 'fr_FR') langRef.current = 'fr_FR';
      })
      .catch(() => undefined)
      .finally(async () => {
        if (cancelled) return;
        const lang = langRef.current;
        let available = false;
        try {
          const probe = await fetch(
            new URL(`help/${lang}/index.json`, window.location.href).toString()
          );
          available = probe.ok;
        } catch {
          available = false;
        }
        if (cancelled) return;
        viewerReady.current = available;
        if (available) {
          setMode({ kind: 'viewer', src: viewerUrl(lang, {}) });
        } else {
          setMode({
            kind: 'text',
            topic: '',
            text: t('Type a function name and press Enter to see its documentation.'),
          });
        }
      });
    const onTopic = (event: Event) => {
      const name = (event as CustomEvent<{ topic?: string }>).detail?.topic ?? '';
      void openTopic(name);
    };
    window.addEventListener(HELP_TOPIC_EVENT, onTopic as EventListener);
    return () => {
      cancelled = true;
      window.removeEventListener(HELP_TOPIC_EVENT, onTopic as EventListener);
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  // Toolbox-fallback only: the inlined page posts clicked relative links up so the
  // panel can load the next page the same way.
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
          setMode({
            kind: 'inline',
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

  // Text fallback: linkify the names on a "See also" line.
  const renderText = (text: string): JSX.Element[] =>
    text.split('\n').map((line, i) => {
      const m = /^(\s*See also:?\s*)(.+?)(\.?)$/i.exec(line);
      if (!m) return <div key={i}>{line || ' '}</div>;
      const names = m[2].split(/,\s*/);
      return (
        <div key={i}>
          {m[1]}
          {names.map((n, j) => {
            const clean = n.trim().replace(/\.$/, '');
            return (
              <span key={j}>
                <a className="help-link" onClick={() => void openTopic(clean)}>
                  {clean}
                </a>
                {j < names.length - 1 ? ', ' : ''}
              </span>
            );
          })}
          {m[3]}
        </div>
      );
    });

  return (
    <div className="help-panel">
      <div className="help-panel__body">
        {loading ? (
          <div className="browser__empty">{t('loading…')}</div>
        ) : mode?.kind === 'viewer' ? (
          <iframe className="help-panel__frame" title={t('Nelson Documentation')} src={mode.src} />
        ) : mode?.kind === 'inline' ? (
          mode.url ? (
            <div className="help-panel__text">
              <div className="help-panel__topic">{mode.title}</div>
              <p>{t('Documentation opened in a new browser tab.')}</p>
              <a href={mode.url} target="_blank" rel="noreferrer">
                {t('Open documentation')}
              </a>
            </div>
          ) : (
            <iframe
              className="help-panel__frame"
              title={mode.title || t('Nelson Documentation')}
              srcDoc={mode.html ?? ''}
            />
          )
        ) : mode?.kind === 'text' ? (
          <div className="help-panel__text">
            {mode.topic ? <div className="help-panel__topic">{mode.topic}</div> : null}
            {renderText(mode.text)}
          </div>
        ) : null}
      </div>
    </div>
  );
}
//=============================================================================
