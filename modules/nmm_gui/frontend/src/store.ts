//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Window state. Actions (actions.ts) update it in place and call update();
// the React root re-renders from it (useSyncExternalStore on the version).
import type { Json } from './rpc';
import { t } from './i18n';
import { cmp, listOf } from './lib/info';
import { searchScore } from './lib/search';
//=============================================================================
export interface Busy {
  name: string;
  action: string;
  version: string;
  label: string;
  step: string;
  index: number;
  total: number;
  trail: string[];
  sub: { name: string; step: string; index: number; total: number } | null;
  cancelling: boolean;
}
//=============================================================================
export interface DocState { name: string; kind: string; text: string; loading: boolean; error: string }
export interface TreeState { name: string; root: Json; error: string; loading: boolean }
export interface VerifyState { name: string; loading: boolean; ok: boolean; issues: string[] }
export interface Dialog { kind: string; props: Json; resolve: (value: Json) => void }
export const state = {
  filter: 'all' as 'all' | 'installed' | 'updates',
  query: '',
  selected: '',
  installed: [] as Json[],
  registry: [] as Json[],
  registryInfo: null as Json,
  registryError: '',
  registryErrorId: '',
  loadError: '',
  busy: null as Busy | null,
  versions: {} as Record<string, string[] | null>,
  chosenVersion: {} as Record<string, string>,
  tree: null as TreeState | null,
  sort: 'name' as 'name' | 'updates' | 'installed',
  doc: null as DocState | null,
  verify: null as VerifyState | null,
  category: '',
  license: '',
  loaded: false,
  arch: '',
  dialog: null as Dialog | null,
};
//=============================================================================
let version = 0;
const listeners = new Set<() => void>();
export function update(): void {
  version += 1;
  listeners.forEach(listener => listener());
}
//=============================================================================
export function subscribe(listener: () => void): () => void {
  listeners.add(listener);
  return () => { listeners.delete(listener); };
}
//=============================================================================
export function getVersion(): number {
  return version;
}
//=============================================================================
export function keywords(p: Json): string[] {
  const k = p.keywords;
  if (Array.isArray(k)) return k;
  if (typeof k === 'string' && k) return [k];
  return [];
}
//=============================================================================
export function srcKind(source: unknown): '' | 'archive' | 'git' | 'local' {
  if (!source || typeof source !== 'string') return '';
  if (source.endsWith('.nmz')) return 'archive';
  if (source.includes('.git')) return 'git';
  return 'local';
}
//=============================================================================
// A binary registry entry carries one prebuilt archive per architecture under
// "artifacts" (no flat source): installed like an archive, no package tests.
export function isBinaryArchive(p: Json): boolean {
  if (p.package_type === 'binary') return true;
  return !!(p.artifacts && typeof p.artifacts === 'object' && Object.keys(p.artifacts).length);
}
//=============================================================================
export function actionLabel(action: string): string {
  return action === 'install' ? t('installing') : action === 'update' ? t('updating') : action === 'remove' ? t('removing') : action;
}
//=============================================================================
export function hasUpdate(p: Json): boolean {
  return !!(p.installed && p.latest && cmp(p.latest, p.installed.version) > 0);
}
//=============================================================================
function pickDescriptive(installedEntry: Json, registryEntry: Json): Json {
  const out: Json = {};
  for (const key of ['summary', 'description', 'keywords', 'platforms', 'nelson', 'homepage', 'license', 'dependencies', 'source', 'repository', 'issues', 'documentation']) {
    if (registryEntry[key] === undefined && installedEntry[key] !== undefined) out[key] = installedEntry[key];
  }
  return out;
}
//=============================================================================
// One merged record per package name: registry entry + installed entry.
export function packages(): Json[] {
  const byName = new Map<string, Json>();
  for (const r of state.registry) byName.set(r.name, { ...r, latest: r.version, installed: null });
  for (const i of state.installed) {
    const cur = byName.get(i.name) || { name: i.name, version: i.version, latest: '' };
    byName.set(i.name, { ...cur, ...pickDescriptive(i, cur), installed: i, version: i.version });
  }
  const s = state.sort;
  return [...byName.values()].sort((a, b) => {
    if (s === 'updates') { const d = (hasUpdate(b) ? 1 : 0) - (hasUpdate(a) ? 1 : 0); if (d) return d; }
    else if (s === 'installed') { const d = (b.installed ? 1 : 0) - (a.installed ? 1 : 0); if (d) return d; }
    return a.name.localeCompare(b.name);
  });
}
//=============================================================================
export function visible(): Json[] {
  const q = state.query.trim();
  const shown = packages().filter(p => {
    if (state.filter === 'installed' && !p.installed) return false;
    if (state.filter === 'updates' && !hasUpdate(p)) return false;
    if (state.category && !listOf(p.categories).includes(state.category)) return false;
    if (state.license && String(p.license || '') !== state.license) return false;
    return true;
  });
  if (!q) return shown;
  return shown.map(p => ({ p, score: searchScore(p, q, keywords(p)) }))
    .filter(x => x.score > 0).sort((a, b) => b.score - a.score).map(x => x.p);
}
//=============================================================================
export function requiredBy(name: string): string[] {
  return state.installed.filter(i => {
    const d = i.dependencies;
    return d && typeof d === 'object' && !Array.isArray(d) && Object.prototype.hasOwnProperty.call(d, name);
  }).map(i => i.name);
}
//=============================================================================
export function distinct(values: string[]): string[] {
  return [...new Set(values.filter(Boolean))].sort((a, b) => a.localeCompare(b));
}
//=============================================================================
// Registry packages missing: no registry configured, or the configured one
// cannot be read (network, checksum, busy interpreter...).
export function registryProblem(): '' | 'missing' | 'unavailable' {
  if (state.registry.length || !state.registryError) return '';
  return state.registryErrorId === 'Nelson:nmm:RegistryNotConfigured' ? 'missing' : 'unavailable';
}
//=============================================================================
