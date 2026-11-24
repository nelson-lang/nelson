//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, it, expect } from 'vitest';
import { AnsiParser } from '../../src/console/ansi';
//=============================================================================
function plain(p: AnsiParser): string[] {
  return p.lines.map((line) => line.map((s) => s.text).join(''));
}
//=============================================================================
describe('AnsiParser (xterm-style line model for the DOM console)', () => {
  it('accumulates plain text into one line', () => {
    const p = new AnsiParser();
    p.write('hello world');
    expect(plain(p)).toEqual(['hello world']);
  });

  it('splits on newlines and treats CRLF as one break', () => {
    const p = new AnsiParser();
    p.write('a\nb\r\nc');
    expect(plain(p)).toEqual(['a', 'b', 'c']);
  });

  it('carriage return rewrites the current line (progress bars)', () => {
    const p = new AnsiParser();
    p.write('50%\r100%');
    expect(plain(p)).toEqual(['100%']);
  });

  it('carries SGR state across chunks and applies basic colours', () => {
    const p = new AnsiParser();
    p.write('\x1b[31mred');
    p.write(' still red\x1b[0m plain');
    const line = p.lines[0];
    // The two red chunks coalesce (same style); the reset starts a new span.
    expect(line[0]).toMatchObject({ text: 'red still red', style: { fg: '#cd3131' } });
    expect(line[1].text).toBe(' plain');
    expect(line[1].style.fg).toBeUndefined();
  });

  it('parses truecolor foreground and background', () => {
    const p = new AnsiParser();
    p.write('\x1b[0;38;2;10;20;30;48;2;40;50;60mX\x1b[0m');
    expect(p.lines[0][0].style).toMatchObject({
      fg: 'rgb(10, 20, 30)',
      bg: 'rgb(40, 50, 60)',
    });
  });

  it('handles bold and its reset', () => {
    const p = new AnsiParser();
    p.write('\x1b[1mB\x1b[22mn');
    expect(p.lines[0][0].style.bold).toBe(true);
    expect(p.lines[0][1].style.bold).toBe(false);
  });

  it('extracts OSC 8 hyperlinks (expandable / truncated-matrix labels)', () => {
    const p = new AnsiParser();
    p.write('\x1b]8;;nelson-expand:3\x1b\\Show all\x1b]8;;\x1b\\');
    const spans = p.lines[0];
    expect(spans[0]).toMatchObject({ text: 'Show all', link: 'nelson-expand:3' });
    p.write('after');
    expect(p.lines[0][1].link).toBeUndefined();
  });

  it('accepts the BEL-terminated OSC 8 form too', () => {
    const p = new AnsiParser();
    p.write('\x1b]8;;http://x\x07link\x1b]8;;\x07');
    expect(p.lines[0][0]).toMatchObject({ text: 'link', link: 'http://x' });
  });

  it('expands tabs to 8-column stops', () => {
    const p = new AnsiParser();
    p.write('ab\tc');
    expect(plain(p)).toEqual(['ab      c']); // 2 + 6 spaces = col 8
  });

  it('backspace moves the cursor back so later text overwrites (terminal-style)', () => {
    const p = new AnsiParser();
    p.write('abc\b\bx');
    expect(plain(p)).toEqual(['axc']); // cursor back to col 1, x overwrites the b
  });

  it('drops non-printable C0 controls (bell) but keeps text', () => {
    const p = new AnsiParser();
    p.write('a\x07b');
    expect(plain(p)).toEqual(['ab']);
  });

  it('ignores unsupported CSI sequences without leaking text', () => {
    const p = new AnsiParser();
    p.write('\x1b[2J\x1b[Hclean');
    expect(plain(p)).toEqual(['clean']);
  });

  it('coalesces adjacent spans that share a style', () => {
    const p = new AnsiParser();
    p.write('\x1b[31ma');
    p.write('b');
    expect(p.lines[0]).toHaveLength(1);
    expect(p.lines[0][0].text).toBe('ab');
  });

  it('clear() empties the transcript but keeps the SGR state', () => {
    const p = new AnsiParser();
    p.write('\x1b[31mred');
    p.clear();
    p.write('more');
    expect(plain(p)).toEqual(['more']);
    expect(p.lines[0][0].style.fg).toBe('#cd3131');
  });

  it('removes a fresh prompt suffix before an external command starts', () => {
    const p = new AnsiParser();
    p.write('Nelson\n>> ');
    expect(p.removeTrailingText('>> ')).toBe(true);
    expect(plain(p)).toEqual(['Nelson', '']);
    expect(p.removeTrailingText('>> ')).toBe(false);
  });

  it('trims scrollback rows while keeping the current tail writable', () => {
    const p = new AnsiParser();
    p.write('1\n2\n3\n4');
    p.trimRows(2);
    expect(plain(p)).toEqual(['3', '4']);
    p.write('5');
    expect(plain(p)).toEqual(['3', '45']);
  });
});
//=============================================================================
