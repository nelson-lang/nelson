//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it, vi } from 'vitest';
import { WasmNewsfeedService } from '../../src/wasm/wasmNewsfeed';
//=============================================================================
describe('WebAssembly newsfeed service', () => {
  it('loads the default Nelson feed through the CORS-enabled releases API', async () => {
    const fetch = vi.fn<typeof globalThis.fetch>().mockResolvedValue(
      new Response(
        JSON.stringify([
          {
            name: 'Nelson 2.0',
            tag_name: '2.0.0',
            html_url: 'https://github.com/nelson-lang/nelson/releases/tag/2.0.0',
            published_at: '2026-09-27T10:00:00Z',
            body: 'Release notes',
          },
        ]),
        { status: 200, headers: { 'content-type': 'application/json' } }
      )
    );
    const service = new WasmNewsfeedService(fetch);

    await expect(service.list('')).resolves.toEqual({
      feed: {
        title: 'Nelson releases',
        link: 'https://github.com/nelson-lang/nelson/releases',
        url: 'https://github.com/nelson-lang/nelson/releases.atom',
        items: [
          {
            title: 'Nelson 2.0',
            link: 'https://github.com/nelson-lang/nelson/releases/tag/2.0.0',
            date: '2026-09-27T10:00:00Z',
            summary: 'Release notes',
            image: '',
          },
        ],
      },
    });
    expect(String(fetch.mock.calls[0]?.[0])).toContain('api.github.com/repos/nelson-lang/nelson');
  });

  it('normalizes a CORS-enabled RSS feed', async () => {
    const source = `<?xml version="1.0"?>
      <rss version="2.0" xmlns:media="http://search.yahoo.com/mrss/">
        <channel><title>Portable feed</title><link>https://example.test/</link>
          <item><title>First item</title><link>https://example.test/first</link>
            <pubDate>Sun, 27 Sep 2026 10:00:00 GMT</pubDate>
            <description><![CDATA[<p>Summary</p>]]></description>
            <media:thumbnail url="https://example.test/image.png" width="320"/>
          </item>
        </channel>
      </rss>`;
    const fetch = vi
      .fn<typeof globalThis.fetch>()
      .mockResolvedValue(new Response(source, { status: 200 }));
    const service = new WasmNewsfeedService(fetch);

    const result = await service.list('https://example.test/feed.xml');
    expect(result.feed.title).toBe('Portable feed');
    expect(result.feed.items).toEqual([
      {
        title: 'First item',
        link: 'https://example.test/first',
        date: 'Sun, 27 Sep 2026 10:00:00 GMT',
        summary: '<p>Summary</p>',
        image: 'https://example.test/image.png',
      },
    ]);
  });

  it('reports the browser cross-origin requirement', async () => {
    const fetch = vi
      .fn<typeof globalThis.fetch>()
      .mockRejectedValue(new TypeError('Failed to fetch'));
    const service = new WasmNewsfeedService(fetch);

    await expect(service.list('https://example.test/feed.xml')).rejects.toThrow(
      'server must allow cross-origin browser access'
    );
  });
});
//=============================================================================
