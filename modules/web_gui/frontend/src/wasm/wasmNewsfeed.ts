//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { NewsfeedFeed, NewsfeedItem, NewsfeedListResult } from '../rpc/types';
//=============================================================================
const DEFAULT_FEED = 'https://github.com/nelson-lang/nelson/releases.atom';
const RELEASES_API = 'https://api.github.com/repos/nelson-lang/nelson/releases?per_page=30';
const MAX_FEED_BYTES = 4 * 1024 * 1024;
type Fetch = (input: RequestInfo | URL, init?: RequestInit) => Promise<Response>;
interface GithubRelease {
  name?: unknown;
  tag_name?: unknown;
  html_url?: unknown;
  published_at?: unknown;
  body?: unknown;
}
//=============================================================================
function text(value: unknown): string {
  return typeof value === 'string' ? value : '';
}
//=============================================================================
function children(node: Element, name: string): Element[] {
  return Array.from(node.children).filter((child) => child.localName === name);
}
//=============================================================================
function childText(node: Element, names: string[], fallback = ''): string {
  for (const name of names) {
    const value = children(node, name)[0]?.textContent?.trim();
    if (value) return value;
  }
  return fallback;
}
//=============================================================================
function atomLink(node: Element): string {
  const links = children(node, 'link');
  const alternate = links.find((link) => (link.getAttribute('rel') || 'alternate') === 'alternate');
  return (alternate || links[0])?.getAttribute('href') || childText(node, ['link']);
}
//=============================================================================
function imageUrl(node: Element, summary: string): string {
  const media = Array.from(node.getElementsByTagNameNS('*', 'thumbnail'))
    .concat(Array.from(node.getElementsByTagNameNS('*', 'content')))
    .filter((element) => element.hasAttribute('url'))
    .sort(
      (left, right) =>
        Number(right.getAttribute('width') || 0) - Number(left.getAttribute('width') || 0)
    );
  if (media[0]) return media[0].getAttribute('url') || '';
  const enclosure = children(node, 'enclosure').find((element) => {
    const type = element.getAttribute('type') || '';
    return element.hasAttribute('url') && (!type || type.startsWith('image/'));
  });
  if (enclosure) return enclosure.getAttribute('url') || '';
  return /<img[^>]+src\s*=\s*["']([^"']+)["']/i.exec(summary)?.[1] || '';
}
//=============================================================================
function item(node: Element, atom: boolean): NewsfeedItem {
  const summary = childText(node, atom ? ['summary', 'content'] : ['description', 'encoded']);
  return {
    title: childText(node, ['title'], '(no title)'),
    link: atom ? atomLink(node) : childText(node, ['link']),
    date: childText(node, atom ? ['updated', 'published'] : ['pubDate', 'date']),
    summary,
    image: imageUrl(node, summary),
  };
}
//=============================================================================
function parseXml(source: string, url: string): NewsfeedFeed {
  const document = new DOMParser().parseFromString(source, 'application/xml');
  if (document.querySelector('parsererror')) throw new Error('Invalid RSS or Atom document');
  const root = document.documentElement;
  const channel = root.localName === 'rss' ? children(root, 'channel')[0] : null;
  if (channel) {
    return {
      title: childText(channel, ['title'], '(untitled feed)'),
      link: childText(channel, ['link']),
      url,
      items: children(channel, 'item').map((node) => item(node, false)),
    };
  }
  if (root.localName === 'feed') {
    return {
      title: childText(root, ['title'], '(untitled feed)'),
      link: atomLink(root),
      url,
      items: children(root, 'entry').map((node) => item(node, true)),
    };
  }
  throw new Error('Unrecognized feed format');
}
//=============================================================================
export class WasmNewsfeedService {
  constructor(private readonly fetchImpl: Fetch = globalThis.fetch.bind(globalThis)) {}
  async list(value: unknown): Promise<NewsfeedListResult> {
    const url = text(value).trim() || DEFAULT_FEED;
    if (url === DEFAULT_FEED) return this.githubReleases(url);
    if (!/^https?:\/\//i.test(url)) throw new Error('Feed URL must use HTTP or HTTPS');
    try {
      const response = await this.fetchImpl(url, {
        headers: { Accept: 'application/atom+xml, application/rss+xml, application/xml, text/xml' },
      });
      if (!response.ok) throw new Error(`Feed download failed: HTTP ${response.status}`);
      const declared = Number(response.headers.get('content-length') || 0);
      if (declared > MAX_FEED_BYTES) throw new Error('Feed exceeds the browser size limit');
      const source = await response.text();
      if (new TextEncoder().encode(source).byteLength > MAX_FEED_BYTES) {
        throw new Error('Feed exceeds the browser size limit');
      }
      return { feed: parseXml(source, url) };
    } catch (error) {
      if (error instanceof TypeError) {
        throw new Error('Feed download failed: the server must allow cross-origin browser access');
      }
      throw error;
    }
  }
  //=============================================================================
  private async githubReleases(url: string): Promise<NewsfeedListResult> {
    const response = await this.fetchImpl(RELEASES_API);
    if (!response.ok) throw new Error(`Nelson releases download failed: HTTP ${response.status}`);
    const releases = (await response.json()) as unknown;
    if (!Array.isArray(releases)) throw new Error('Invalid Nelson releases response');
    return {
      feed: {
        title: 'Nelson releases',
        link: 'https://github.com/nelson-lang/nelson/releases',
        url,
        items: releases.map((releaseValue): NewsfeedItem => {
          const release = releaseValue as GithubRelease;
          return {
            title: text(release.name) || text(release.tag_name) || '(no title)',
            link: text(release.html_url),
            date: text(release.published_at),
            summary: text(release.body),
            image: '',
          };
        }),
      },
    };
  }
}
//=============================================================================
