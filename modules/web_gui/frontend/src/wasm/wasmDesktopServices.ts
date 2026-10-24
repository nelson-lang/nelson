//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { NelsonWorkerClientApi } from '@nelson/wasm-client';
import type { WasmHistoryStorage } from './historyStore';
//=============================================================================
const PREFERENCES_KEY = 'nelson-webview.wasm.preferences.v1';
export const WASM_PREFERENCES_PATH = `browser:localStorage/${PREFERENCES_KEY}`;
export class WasmPreferencesService {
  private values: Record<string, string> = {};
  private loaded = false;
  constructor(private readonly storage: WasmHistoryStorage | null) {}
  private normalize(value: unknown): Record<string, string> {
    if (!value || typeof value !== 'object' || Array.isArray(value)) return {};
    return Object.fromEntries(
      Object.entries(value).filter(
        (entry): entry is [string, string] => typeof entry[1] === 'string'
      )
    );
  }
  //=============================================================================
  async load(): Promise<{ schema: 1; values: Record<string, string>; path: string }> {
    if (!this.loaded) {
      this.loaded = true;
      try {
        const stored = this.storage?.getItem(PREFERENCES_KEY) ?? '';
        if (stored) {
          const envelope = JSON.parse(stored) as {
            schema?: unknown;
            values?: unknown;
          };
          if (envelope.schema === 1) this.values = this.normalize(envelope.values);
        }
      } catch {
        this.values = {};
      }
    }
    return { schema: 1, values: { ...this.values }, path: WASM_PREFERENCES_PATH };
  }
  //=============================================================================
  async save(value: unknown): Promise<{ ok: true; path: string }> {
    this.values = this.normalize(value);
    this.loaded = true;
    try {
      if (this.storage) {
        this.storage.setItem(PREFERENCES_KEY, JSON.stringify({ schema: 1, values: this.values }));
      }
    } catch {
      // Keep preferences for the current browser session even if persistent
      // browser storage is unavailable.
    }
    return { ok: true, path: WASM_PREFERENCES_PATH };
  }
}
//=============================================================================
interface ProfileRow {
  function: string;
  file: string;
  line: number;
  calls: number;
  total: number;
  perCall: number;
}
//=============================================================================
export class WasmProfilerService {
  constructor(private readonly client: NelsonWorkerClientApi) {}
  private async evaluateMarkedJson<T>(code: string, begin: string, end: string): Promise<T> {
    let streamedStdout = '';
    const result = await this.client.evaluate(code, (output) => {
      if (output.stream === 'stdout') streamedStdout += output.text;
    });
    if (result.exitCode !== 0) {
      throw new Error(result.errorMessage || result.stderr || 'Nelson request failed');
    }
    const stdout = result.stdout || streamedStdout;
    const start = stdout.indexOf(begin);
    const finish = stdout.lastIndexOf(end);
    if (start < 0 || finish < start) throw new Error('Nelson response is incomplete');
    return JSON.parse(stdout.slice(start + begin.length, finish).trim()) as T;
  }
  //=============================================================================
  async info(): Promise<{ rows: ProfileRow[]; on: boolean }> {
    const begin = '__NELSON_PROFILE_RESULT_BEGIN__';
    const end = '__NELSON_PROFILE_RESULT_END__';
    const payload = await this.evaluateMarkedJson<{
      rows?: Record<string, unknown> | Record<string, unknown>[];
      running?: unknown;
    }>(
      [
        'nelsonWasmProfilePayload = struct();',
        "nelsonWasmProfilePayload.rows = profile('info');",
        "nelsonWasmProfileStatus = profile('status');",
        "nelsonWasmProfilePayload.running = strcmp(nelsonWasmProfileStatus.ProfilerStatus, 'on');",
        `disp(['${begin}', jsonencode(nelsonWasmProfilePayload), '${end}']);`,
        'clear nelsonWasmProfilePayload nelsonWasmProfileStatus;',
      ].join(' '),
      begin,
      end
    );
    const values = Array.isArray(payload.rows)
      ? payload.rows
      : payload.rows && Object.keys(payload.rows).length > 0
      ? [payload.rows]
      : [];
    const number = (value: unknown) => {
      const parsed = Number(value);
      return Number.isFinite(parsed) ? parsed : 0;
    };
    return {
      rows: values.map((row) => ({
        function: String(row.FunctionName ?? ''),
        file: String(row.Filename ?? ''),
        line: number(row.LinePosition),
        calls: number(row.NumCalls),
        total: number(row.TotalTime),
        perCall: number(row.PerCall),
      })),
      on: payload.running === true,
    };
  }
  //=============================================================================
  async control(action: string): Promise<{ ok: true; on: boolean }> {
    if (!['on', 'off', 'clear'].includes(action)) throw new Error('Invalid profiler action');
    const begin = '__NELSON_PROFILE_CONTROL_BEGIN__';
    const end = '__NELSON_PROFILE_CONTROL_END__';
    const on = await this.evaluateMarkedJson<boolean>(
      [
        `profile('${action}');`,
        "nelsonWasmProfileStatus = profile('status');",
        `disp(['${begin}', jsonencode(strcmp(nelsonWasmProfileStatus.ProfilerStatus, 'on')), '${end}']);`,
        'clear nelsonWasmProfileStatus;',
      ].join(' '),
      begin,
      end
    );
    return { ok: true, on: on === true };
  }
  //=============================================================================
  private htmlText(value: unknown): string {
    return String(value)
      .replaceAll('&', '&amp;')
      .replaceAll('<', '&lt;')
      .replaceAll('>', '&gt;')
      .replaceAll('"', '&quot;');
  }
  //=============================================================================
  private html(rows: ProfileRow[]): string {
    const body = rows
      .map(
        (row) =>
          `<tr><td>${this.htmlText(row.function)}</td><td>${this.htmlText(row.file)}</td>` +
          `<td>${row.line}</td><td>${row.calls}</td><td>${row.total}</td>` +
          `<td>${row.perCall}</td></tr>`
      )
      .join('');
    return (
      '<!doctype html><html lang="en"><head><meta charset="utf-8">' +
      '<meta name="viewport" content="width=device-width,initial-scale=1">' +
      '<title>Nelson profile report</title><style>' +
      'body{font:14px system-ui,sans-serif;margin:24px;color:#222}' +
      'table{border-collapse:collapse;width:100%}th,td{border:1px solid #bbb;padding:6px;text-align:left}' +
      'th{background:#eee}td:nth-child(n+3){text-align:right}</style></head><body>' +
      '<h1>Nelson profile report</h1><table><thead><tr><th>Function</th><th>File</th>' +
      '<th>Line</th><th>Calls</th><th>Total (s)</th><th>Per call (s)</th>' +
      `</tr></thead><tbody>${body}</tbody></table></body></html>`
    );
  }
  //=============================================================================
  async exportReport(
    directory: string
  ): Promise<{ ok: true; path: string; index: string; url: string; on: false }> {
    const profile = await this.info();
    if (profile.on) throw new Error('profiler is running');
    if (profile.rows.length === 0) throw new Error('no profile data to export');
    const index = `${directory.replace(/\/$/, '')}/index.html`;
    const html = this.html(profile.rows);
    await this.client.writeFile(index, new TextEncoder().encode(html));
    return {
      ok: true,
      path: directory,
      index,
      url: `data:text/html;charset=utf-8,${encodeURIComponent(html)}`,
      on: false,
    };
  }
}
//=============================================================================
