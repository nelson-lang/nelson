//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { Json } from '../rpc';
//=============================================================================
// Semver-ish comparison of the first three numeric parts.
export function cmp(a: string, b: string): number {
  const pa = String(a).split('.').map(Number), pb = String(b).split('.').map(Number);
  for (let i = 0; i < 3; i += 1) {
    if ((pa[i] || 0) !== (pb[i] || 0)) return (pa[i] || 0) > (pb[i] || 0) ? 1 : -1;
  }
  return 0;
}
//=============================================================================
export function fmtBytes(value: unknown): string {
  const n = Number(value) || 0;
  if (n < 1024) return n + ' B';
  if (n < 1024 * 1024) return (n / 1024).toFixed(1) + ' KB';
  return (n / (1024 * 1024)).toFixed(1) + ' MB';
}
//=============================================================================
// Licenses known to be compatible with Nelson (LGPL-3.0 / GPL-3.0): the
// permissive and copyleft SPDX identifiers that can be combined with it.
const COMPATIBLE = [
  'MIT', 'MIT-0', 'ISC', 'BSD-2-CLAUSE', 'BSD-3-CLAUSE', 'APACHE-2.0', 'ZLIB', 'BSL-1.0', 'MPL-2.0',
  'CC0-1.0', 'UNLICENSE', 'PSF-2.0', 'LGPL-2.1', 'LGPL-3.0', 'GPL-2.0', 'GPL-3.0',
];
//=============================================================================
function licenseIdCompatible(id: string): boolean {
  const upper = String(id).toUpperCase().replace(/[()]/g, '');
  // GPL-2.0 only (also its deprecated bare form) cannot be combined with
  // (L)GPL-3.0 code; GPL-2.0-or-later can.
  if (upper === 'GPL-2.0' || upper === 'GPL-2.0-ONLY') return false;
  return COMPATIBLE.includes(upper.replace(/-(ONLY|OR-LATER)$/, '').replace(/\+$/, ''));
}
//=============================================================================
// An SPDX expression is compatible when one of its OR alternatives only uses
// compatible licenses (a WITH exception only relaxes the license it follows).
export function licenseStatus(license: unknown): 'ok' | 'unknown' | 'warn' {
  const text = String(license || '').trim();
  if (!text) return 'unknown';
  const alternatives = text.split(/\s+OR\s+/i);
  const ok = alternatives.some(alt => alt.replace(/\s+WITH\s+\S+/gi, '').split(/\s+AND\s+/i).every(licenseIdCompatible));
  return ok ? 'ok' : 'warn';
}
//=============================================================================
export function listOf(value: unknown): string[] {
  if (Array.isArray(value)) return value.map(String).filter(Boolean);
  return value ? [String(value)] : [];
}
//=============================================================================
export function textOf(value: unknown): string {
  if (Array.isArray(value)) return value.join(', ');
  if (value && typeof value === 'object') return Object.keys(value).join(', ');
  return value == null ? '' : String(value);
}
//=============================================================================
// Archive checksum and source for this machine: a flat entry, or the artifact
// of the current architecture of a binary package.
export function artifactOf(p: Json, currentArch: string): { source: string; checksum: string; arch?: string } {
  if (p.source || p.checksum) return { source: p.source || '', checksum: p.checksum || '' };
  const artifacts = p.artifacts && typeof p.artifacts === 'object' ? p.artifacts : {};
  const arch = artifacts[currentArch] ? currentArch : Object.keys(artifacts)[0];
  return arch ? { source: artifacts[arch].source || '', checksum: artifacts[arch].checksum || '', arch } : { source: '', checksum: '' };
}
//=============================================================================
export function shortChecksum(sha: string): string {
  const s = String(sha || '');
  return s.length > 24 ? s.slice(0, 16) + '...' + s.slice(-8) : s;
}
//=============================================================================
export function isNative(p: Json): boolean {
  return p.package_type === 'binary' || p.builtin === true || p.builtin === 'true';
}
//=============================================================================
// Packages an install plan adds besides the requested one (its dependency
// nodes still to install), flattened.
export function planDependencies(plan: Json): { name: string; version: string }[] {
  const out: { name: string; version: string }[] = [];
  const walk = (node: Json) => {
    if (!node || typeof node !== 'object') return;
    const d = node.dependencies;
    const kids = Array.isArray(d) ? d : d && typeof d === 'object' && d.package ? [d] : [];
    for (const kid of kids) {
      if (kid.action === 'install' || kid.action === 'update') out.push({ name: kid.package, version: kid.version || '' });
      walk(kid);
    }
  };
  walk(plan && (plan.install_plan || plan));
  return out;
}
//=============================================================================
function major(version: string): number {
  return Number(String(version || '').replace(/^[^0-9]*/, '').split('.')[0]) || 0;
}
//=============================================================================
export function isMajorUpdate(from: string, to: string): boolean {
  return !!from && !!to && major(to) > major(from);
}
//=============================================================================
// Changelog sections of the versions after `from` up to `to`: a section
// starts at a markdown heading that carries a version number.
export function changesSince(text: string, from: string, to: string): string {
  const lines = String(text || '').replace(/\r\n?/g, '\n').split('\n');
  const out: string[] = [];
  let keep = false;
  for (const line of lines) {
    const heading = /^#{1,6}\s/.test(line) && line.match(/(\d+\.\d+(?:\.\d+)?)/);
    if (heading) keep = cmp(heading[1], from) > 0 && (!to || cmp(heading[1], to) <= 0);
    if (keep) out.push(line);
  }
  return out.join('\n').trim();
}
//=============================================================================
