//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// ANSI stream parser for the DOM console, modelled on xterm.js: the engine
// byte stream (plain text + SGR colour codes + OSC 8 hyperlinks + cursor
// controls) is written into a per-line cell buffer with a cursor, then
// coalesced into styled spans for rendering.
//
// A cell buffer (rather than append-only spans) is what makes carriage return,
// backspace and overwrite behave like a real terminal: CR moves the cursor to
// column 0 without erasing, so "b\r\n" keeps the b while "50%\r100%" overwrites.
export interface Style {
  fg?: string;
  bg?: string;
  bold?: boolean;
}
//=============================================================================
export interface Span {
  text: string;
  style: Style;
  link?: string; // OSC 8 target (nelson-expand:N, or an http url)
}
//=============================================================================
export type Line = Span[];
interface Cell {
  ch: string;
  style: Style;
  link?: string;
}
//=============================================================================
const BASIC: Record<number, string> = {
  30: '#000000',
  31: '#cd3131',
  32: '#0dbc79',
  33: '#e5e510',
  34: '#2472c8',
  35: '#bc3fbc',
  36: '#11a8cd',
  37: '#e5e5e5',
  90: '#767676',
  91: '#f14c4c',
  92: '#23d18b',
  93: '#f5f543',
  94: '#3b8eea',
  95: '#d670d6',
  96: '#29b8db',
  97: '#ffffff',
};
//=============================================================================
const TAB = 8;
export class AnsiParser {
  private style: Style = {};
  private link: string | undefined;
  // Finalized lines (a LF was seen) plus the line currently under the cursor.
  private rows: Cell[][] = [[]];
  private row = 0;
  private col = 0;
  private cur(): Cell[] {
    return this.rows[this.row];
  }
  //=============================================================================
  private putChar(ch: string): void {
    const line = this.cur();
    // Pad with blanks if the cursor is past the end (e.g. after tabs).
    while (line.length < this.col) {
      line.push({ ch: ' ', style: {} });
    }
    line[this.col] = { ch, style: { ...this.style }, link: this.link };
    this.col++;
  }
  //=============================================================================
  private applySgr(codes: number[]): void {
    for (let i = 0; i < codes.length; i++) {
      const c = codes[i];
      if (c === 0) {
        this.style = {};
      } else if (c === 1) {
        this.style.bold = true;
      } else if (c === 22) {
        this.style.bold = false;
      } else if (c === 39) {
        this.style.fg = undefined;
      } else if (c === 49) {
        this.style.bg = undefined;
      } else if (BASIC[c]) {
        this.style.fg = BASIC[c];
      } else if ((c >= 40 && c <= 47) || (c >= 100 && c <= 107)) {
        this.style.bg = BASIC[c - 10];
      } else if (c === 38 || c === 48) {
        const mode = codes[i + 1];
        if (mode === 2) {
          const rgb = `rgb(${codes[i + 2] | 0}, ${codes[i + 3] | 0}, ${codes[i + 4] | 0})`;
          if (c === 38) this.style.fg = rgb;
          else this.style.bg = rgb;
          i += 4;
        } else if (mode === 5) {
          i += 2;
        }
      }
    }
  }
  //=============================================================================
  write(chunk: string): void {
    let i = 0;
    const n = chunk.length;
    while (i < n) {
      const ch = chunk[i];
      if (ch === '\x1b') {
        if (chunk[i + 1] === '[') {
          // CSI: ESC [ ... final-letter. Only SGR ('m') affects the model;
          // erase/cursor CSIs from a full-screen program are skipped.
          let j = i + 2;
          while (j < n && !/[A-Za-z]/.test(chunk[j])) j++;
          if (chunk[j] === 'm') {
            const body = chunk.slice(i + 2, j);
            const codes = body.length ? body.split(';').map((s) => parseInt(s, 10) || 0) : [0];
            this.applySgr(codes);
          }
          i = j + 1;
        } else if (chunk[i + 1] === ']') {
          // OSC 8 hyperlink: ESC ] 8 ; params ; uri ST  (ST = ESC \ or BEL)
          let j = i + 2;
          while (j < n && chunk[j] !== '\x07' && !(chunk[j] === '\x1b' && chunk[j + 1] === '\\')) {
            j++;
          }
          const m = /^8;.*?;(.*)$/.exec(chunk.slice(i + 2, j));
          if (m) this.link = m[1] || undefined;
          i = chunk[j] === '\x07' ? j + 1 : j + 2;
        } else {
          i += 1;
        }
        continue;
      }
      if (ch === '\n') {
        this.rows.push([]);
        this.row = this.rows.length - 1;
        this.col = 0;
        i++;
        continue;
      }
      if (ch === '\r') {
        this.col = 0; // move to column 0, do not erase
        i++;
        continue;
      }
      if (ch === '\t') {
        const next = this.col + (TAB - (this.col % TAB));
        while (this.col < next) this.putChar(' ');
        i++;
        continue;
      }
      if (ch === '\b') {
        if (this.col > 0) this.col--;
        i++;
        continue;
      }
      if (ch < ' ') {
        i++; // other C0 controls (bell, etc.): not printable
        continue;
      }
      this.putChar(ch);
      i++;
    }
  }
  //=============================================================================
  clear(): void {
    this.rows = [[]];
    this.row = 0;
    this.col = 0;
  }
  //=============================================================================
  trimRows(maxRows: number): void {
    if (!Number.isFinite(maxRows) || maxRows < 1) return;
    const limit = Math.floor(maxRows);
    const drop = this.rows.length - limit;
    if (drop <= 0) return;
    this.rows.splice(0, drop);
    this.row = Math.max(0, this.row - drop);
  }
  //=============================================================================
  removeTrailingText(text: string): boolean {
    if (!text) return false;
    const line = this.cur();
    if (line.length < text.length) return false;
    const start = line.length - text.length;
    for (let index = 0; index < text.length; index += 1) {
      if (line[start + index]?.ch !== text[index]) return false;
    }
    line.length = start;
    this.col = Math.min(this.col, line.length);
    return true;
  }
  //=============================================================================
  get lines(): Line[] {
    return this.rows.map((cells) => {
      const spans: Line = [];
      for (const cell of cells) {
        const last = spans[spans.length - 1];
        if (last && last.link === cell.link && sameStyle(last.style, cell.style)) {
          last.text += cell.ch;
        } else {
          spans.push({ text: cell.ch, style: cell.style, link: cell.link });
        }
      }
      return spans;
    });
  }
}
//=============================================================================
function sameStyle(a: Style, b: Style): boolean {
  return a.fg === b.fg && a.bg === b.bg && a.bold === b.bold;
}
//=============================================================================
