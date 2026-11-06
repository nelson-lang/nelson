import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';
import { describe, expect, it } from 'vitest';

// nelson-help-sw.js is a classic worker script (importScripts'd by the COI service
// worker). Load it in a sandbox so the pure helpers can be exercised in Node.
function loadHelpSw(): {
  nlrpackSinglePayload: (b: Uint8Array) => Uint8Array;
  buildHelpBundleMap: (index: unknown) => Record<string, string>;
  moduleForPath: (rest: string, known: Set<string>) => string;
  targetForEntry: (entry: string) => string;
  helpContentType: (path: string) => string;
} {
  const here = dirname(fileURLToPath(import.meta.url));
  const source = readFileSync(
    resolve(here, '../../../../../tools/wasm/nelson-help-sw.js'),
    'utf8'
  );
  const sandboxModule = { exports: {} as Record<string, unknown> };
  // eslint-disable-next-line @typescript-eslint/no-implied-eval
  new Function('module', 'exports', 'self', source)(sandboxModule, sandboxModule.exports, {});
  return sandboxModule.exports as ReturnType<typeof loadHelpSw>;
}

function nlrpack(fileName: string, payload: Uint8Array): Uint8Array {
  const header = JSON.stringify({ formatVersion: 1, files: [{ file: fileName, offset: 0, bytes: payload.length }] });
  const headerBytes = new TextEncoder().encode(header);
  const out = new Uint8Array(12 + headerBytes.length + payload.length);
  out.set(new TextEncoder().encode('NLRPACK1'), 0);
  new DataView(out.buffer).setUint32(8, headerBytes.length, true);
  out.set(headerBytes, 12);
  out.set(payload, 12 + headerBytes.length);
  return out;
}

describe('nelson-help-sw pure helpers', () => {
  const sw = loadHelpSw();

  it('unwraps an NLRPACK1 single-member container', () => {
    const payload = new Uint8Array([1, 2, 3, 4, 250, 255]);
    const bundle = nlrpack('io.github.nelson_lang.Nelson.modules.main.help.en_US.nhz', payload);
    expect(Array.from(sw.nlrpackSinglePayload(bundle))).toEqual(Array.from(payload));
  });

  it('rejects bytes that are not an NLRPACK1 bundle', () => {
    expect(() => sw.nlrpackSinglePayload(new Uint8Array([0, 1, 2, 3, 4, 5, 6, 7, 8]))).toThrow();
  });

  it('maps help archives to their bundle paths by module and language', () => {
    const index = {
      modules: [
        { module: 'graphics' },
        {
          module: 'help_tools',
          bundles: {
            archives: [
              {
                file: 'runtime-resources/bundles/help_tools-archive-io.github.nelson_lang.Nelson.modules.main.help.en_US.nhz.nlpack',
              },
              {
                file: 'runtime-resources/bundles/help_tools-archive-io.github.nelson_lang.Nelson.modules.graphics.help.en_US.nhz.nlpack',
              },
              {
                file: 'runtime-resources/bundles/help_tools-archive-io.github.nelson_lang.Nelson.modules.graphics.help.fr_FR.nhz.nlpack',
              },
            ],
          },
        },
      ],
    };
    const map = sw.buildHelpBundleMap(index);
    expect(map['main/en_US']).toContain('modules.main.help.en_US.nhz.nlpack');
    expect(map['graphics/en_US']).toContain('modules.graphics.help.en_US.nhz.nlpack');
    expect(map['graphics/fr_FR']).toContain('modules.graphics.help.fr_FR.nhz.nlpack');
    expect(Object.keys(map)).toHaveLength(3);
  });

  it('routes module-prefixed paths to their module and everything else to main', () => {
    const known = new Set(['graphics', 'elementary_functions']);
    expect(sw.moduleForPath('graphics/surf.html', known)).toBe('graphics');
    expect(sw.moduleForPath('elementary_functions/sin.html', known)).toBe('elementary_functions');
    expect(sw.moduleForPath('index.js', known)).toBe('main');
    expect(sw.moduleForPath('toc.html', known)).toBe('main');
    expect(sw.moduleForPath('unknownmodule/foo.html', known)).toBe('main');
  });

  it('strips the main/ prefix from extracted entries, keeps module subfolders', () => {
    expect(sw.targetForEntry('main/index.json')).toBe('index.json');
    expect(sw.targetForEntry('main/index.html')).toBe('index.html');
    expect(sw.targetForEntry('graphics/surf.html')).toBe('graphics/surf.html');
  });

  it('derives content types from the extension', () => {
    expect(sw.helpContentType('index.html')).toContain('text/html');
    expect(sw.helpContentType('index.js')).toContain('javascript');
    expect(sw.helpContentType('surf.svg')).toBe('image/svg+xml');
    expect(sw.helpContentType('plot.png')).toBe('image/png');
    expect(sw.helpContentType('nelson_common.css')).toContain('text/css');
    expect(sw.helpContentType('weird.bin')).toBe('application/octet-stream');
  });
});
