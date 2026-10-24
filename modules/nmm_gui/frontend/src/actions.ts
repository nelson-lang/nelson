//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { rpc, type Json } from './rpc';
import { t, loadTranslations, translationsLoaded } from './i18n';
import { state, update, packages, visible, hasUpdate, actionLabel } from './store';
import { changesSince, fmtBytes, isMajorUpdate, isNative, licenseStatus, planDependencies, cmp } from './lib/info';
//=============================================================================
export function openDialog<T = Json>(kind: string, props: Json = {}): Promise<T> {
  return new Promise(resolve => {
    state.dialog = { kind, props, resolve: (value: Json) => { state.dialog = null; update(); resolve(value); } };
    update();
  });
}
//=============================================================================
// fresh: a user refresh, which downloads the current registry instead of
// reusing one fetched moments ago.
export async function load(fresh = false): Promise<void> {
  // Translations are fetched with the first load, and again by any later
  // load while they are missing (the interpreter may have been busy).
  if (!translationsLoaded()) {
    await loadTranslations();
    if (!state.loaded) update();
  }
  state.loadError = '';
  const failed = (message: string) => ({ list: { ok: false, error: message }, search: { ok: false, error: message }, registry: null });
  let snap: Json = null;
  try {
    snap = await rpc('snapshot', { fresh });
    if (!snap || !snap.ok) snap = failed((snap && snap.error) || t('failed'));
  } catch (e) {
    snap = failed((e as Error).message);
  }
  const list = snap.list || {};
  state.installed = list.ok ? list.installed || [] : [];
  if (!list.ok) state.loadError = list.error || t('failed');
  const found = snap.search || {};
  state.registry = found.ok ? found.packages || [] : [];
  state.registryError = found.ok ? '' : found.error || '';
  state.registryErrorId = found.ok ? '' : found.error_id || '';
  const info = snap.registry;
  state.registryInfo = info && info.ok !== undefined ? info : null;
  state.arch = snap.arch || '';
  state.loaded = true;
  state.busy = null;
  update();
  // Feeds the Package Manager badge of the desktops.
  rpc('updates', { count: packages().filter(hasUpdate).length }).catch(() => {});
}
//=============================================================================
export function select(name: string): void {
  state.selected = name;
  update();
}
//=============================================================================
export function showInAll(name: string, query = ''): void {
  state.filter = 'all';
  state.query = query;
  if (name) state.selected = name;
  update();
}
//=============================================================================
export async function act(action: string, name: string, version: string, tests = false): Promise<void> {
  if (action === 'remove' && !(await openDialog<boolean>('confirm', { message: t('confirmRemove', { name, version }) }))) return;
  if ((action === 'install' || action === 'update') && !(await confirmInstall(action, name, version))) return;
  state.busy = { name, action, version, label: actionLabel(action), step: '', index: 0, total: 0, trail: [], sub: null, cancelling: false };
  state.selected = name;
  update();
  try {
    await rpc(action, { name, version, tests: tests && action !== 'remove' });
  } catch (e) {
    state.busy = null;
    state.loadError = (e as Error).message;
    update();
  }
}
//=============================================================================
// What the install or update will do (dependencies, size, native code,
// license, major version, changelog of the new versions), confirmed first.
async function confirmInstall(action: string, name: string, version: string): Promise<boolean> {
  const p = packages().find(x => x.name === name) || { name };
  const from = p.installed ? p.installed.version : '';
  let preview: Json = null;
  let planError = '';
  try {
    preview = await rpc('preview', { name, version });
    if (!preview || !preview.ok) planError = (preview && preview.error) || t('failed');
  } catch (e) { planError = (e as Error).message; }
  let changes = '';
  if (action === 'update') {
    let text = typeof p.changelog === 'string' ? p.changelog : '';
    if (!text && p.repository) {
      try { const r = await rpc('doc', { kind: 'changelog', path: '', repository: p.repository }); if (r && r.ok) text = r.text || ''; } catch { /* no changelog */ }
    }
    changes = changesSince(text, from, version);
  }
  const entry = { ...p, ...(preview && preview.package) };
  return openDialog<boolean>('preview', {
    title: action === 'update' ? t('previewUpdateTitle', { name, from, to: version }) : t('previewInstallTitle', { name, version }),
    confirm: t(action === 'update' ? 'previewConfirmUpdate' : 'previewConfirmInstall'),
    deps: preview && preview.ok ? planDependencies(preview.plan) : null,
    size: Number(entry.size) > 0 ? fmtBytes(entry.size) : '',
    native: isNative(entry),
    license: licenseStatus(entry.license) === 'warn' ? t('licenseWarn', { license: entry.license }) : '',
    major: action === 'update' && isMajorUpdate(from, version) ? t('previewMajor', { name }) : '',
    changes,
    planError,
  });
}
//=============================================================================
export async function quickAction(method: string, name: string, version: string): Promise<void> {
  try { await rpc(method, { name, version }); } catch (e) { state.loadError = (e as Error).message; update(); }
}
//=============================================================================
export async function updateAll(): Promise<void> {
  for (const p of packages().filter(hasUpdate)) {
    try { await rpc('update', { name: p.name, version: p.latest, tests: false }); } catch { /* keep going */ }
  }
}
//=============================================================================
export async function fetchVersions(name: string): Promise<void> {
  state.versions[name] = null;
  try {
    const r = await rpc('versions', { name });
    state.versions[name] = r && r.ok && Array.isArray(r.versions) ? r.versions.slice().sort((a: string, b: string) => cmp(b, a)) : [];
  } catch { state.versions[name] = []; }
  update();
}
//=============================================================================
export async function installFromSource(): Promise<void> {
  const source = await openDialog<string | null>('prompt', { title: t('addSource'), message: t('addSourcePrompt'), placeholder: t('addSourcePlaceholder'), ok: t('ok') });
  if (!source) return;
  try { await rpc('installSource', { source }); } catch (e) { state.loadError = (e as Error).message; update(); }
}
//=============================================================================
export async function setRegistry(url: string): Promise<void> {
  try { await rpc('setRegistry', { url }); } catch (e) { state.loadError = (e as Error).message; update(); }
}
//=============================================================================
export async function cancelBusy(): Promise<void> {
  if (!state.busy || state.busy.cancelling) return;
  state.busy.cancelling = true;
  update();
  try { await rpc('interrupt'); } catch { /* the command may already be past an interruptible point */ }
}
//=============================================================================
export async function restoreManifest(items: { name: string; version: string }[]): Promise<void> {
  const have = new Set(state.installed.map(i => i.name));
  for (const it of items) {
    if (have.has(it.name)) continue;
    try { await rpc('install', { name: it.name, version: it.version, tests: false }); } catch { /* keep going */ }
  }
}
//=============================================================================
export async function showDoc(name: string, kind: string): Promise<void> {
  if (state.doc && state.doc.name === name && state.doc.kind === kind) { state.doc = null; update(); return; }
  const p = packages().find(x => x.name === name);
  if (!p) return;
  if (typeof p[kind] === 'string' && p[kind].trim()) { state.doc = { name, kind, text: p[kind], loading: false, error: '' }; update(); return; }
  state.doc = { name, kind, text: '', loading: true, error: '' };
  update();
  const path = p.installed ? p.installed.path : '';
  try {
    const r = await rpc('doc', { name, kind, path, repository: p.repository || '' });
    state.doc = r && r.ok ? { name, kind, text: r.text || '', loading: false, error: '' } : { name, kind, text: '', loading: false, error: (r && r.error) || '' };
  } catch (e) { state.doc = { name, kind, text: '', loading: false, error: (e as Error).message }; }
  if (state.doc && state.doc.name === name && state.doc.kind === kind) update();
}
//=============================================================================
export async function showTree(name: string): Promise<void> {
  const p = packages().find(x => x.name === name);
  if (!p) return;
  state.tree = { name, root: null, error: '', loading: true };
  update();
  const version = p.installed ? '' : state.chosenVersion[name] || p.version || '';
  try {
    const r = await rpc('tree', { name, version, installed: !!p.installed });
    state.tree = r && r.ok && r.tree ? { name, root: r.tree, error: '', loading: false } : { name, root: null, error: (r && r.error) || t('depTreeNone'), loading: false };
  } catch (e) { state.tree = { name, root: null, error: (e as Error).message, loading: false }; }
  if (state.tree && state.tree.name === name) update();
}
//=============================================================================
export async function verifyPackage(name: string): Promise<void> {
  state.verify = { name, loading: true, ok: false, issues: [] };
  update();
  try {
    const r = await rpc('verifyInstalled', { name });
    state.verify = r && r.ok !== undefined
      ? { name, loading: false, ok: !!r.ok, issues: r.issues || [] }
      : { name, loading: false, ok: false, issues: [(r && r.error) || t('failed')] };
  } catch (e) { state.verify = { name, loading: false, ok: false, issues: [(e as Error).message] }; }
  if (state.selected === name) update();
}
//=============================================================================
// A finished operation can move its package out of the current view (an
// update leaves "Updates", a removal leaves "Installed"): show it under
// "All", still selected, so the result stays in front of the user.
async function finishOperation(name: string): Promise<void> {
  state.busy = null;
  await load();
  if (!name || visible().some(p => p.name === name)) return;
  if (!packages().some(p => p.name === name)) return;
  showInAll(name);
}
//=============================================================================
// nmm_gui('install', name) parks a deep link on the backend; the page claims
// it exactly once, on startup or when woken by the 'deeplink' notification.
// In the web desktop the deep link comes with the request instead; the window
// fetches the one the builtin left pending.
export async function consumeDeepLink(given: Json = null): Promise<void> {
  let link: Json = given;
  if (!link) {
    try { link = await rpc('pendingDeepLink'); } catch { return; }
  }
  if (!link || !link.name) return;
  showInAll(link.name);
  if (link.action !== 'install' || state.busy) return;
  const p = packages().find(x => x.name === link.name);
  if (!p || p.installed) return;
  act('install', link.name, link.version || p.version || '');
}
//=============================================================================
function onProgress(payload: string): void {
  let ev: Json = null;
  try { ev = JSON.parse(payload); } catch { ev = null; }
  if (!ev || !ev.name) return;
  // The first package of a burst is the root operation; a later event with
  // another name is a dependency being installed, shown as a sub-step
  // without ending the root or moving the selection.
  if (!state.busy) {
    state.busy = { name: ev.name, action: ev.action, version: ev.version || '', label: actionLabel(ev.action), step: '', index: 0, total: 0, trail: [], sub: null, cancelling: false };
    state.selected = ev.name;
  }
  const b = state.busy;
  if (ev.name === b.name) {
    if (ev.step && !b.trail.includes(ev.step)) b.trail.push(ev.step);
    b.step = ev.step;
    b.index = Number(ev.index) || 0;
    b.total = Number(ev.total) || 0;
    if (ev.version) b.version = ev.version;
    b.sub = null;
    update();
    if (ev.step === 'done') finishOperation(b.name);
  } else {
    b.sub = ev.step === 'done' ? null : { name: ev.name, step: ev.step, index: Number(ev.index) || 0, total: Number(ev.total) || 0 };
    update();
  }
}
//=============================================================================
// Backend events (__nmm_gui_notify__): the window receives them through
// window.nmmGuiPush, the web desktop panel through its host subscription.
export function handleBackendEvent(channel: string, payload: string): void {
  if (channel === 'deeplink') {
    let link: Json = null;
    try { link = payload ? JSON.parse(payload) : null; } catch { link = null; }
    consumeDeepLink(link && typeof link === 'object' ? link : null);
    return;
  }
  if (channel === 'progress') { onProgress(payload); return; }
  if (channel === 'failed') {
    const cancelled = state.busy && state.busy.cancelling;
    state.busy = null;
    if (!cancelled) state.loadError = typeof payload === 'string' && payload ? payload : t('failed');
    update();
    return;
  }
  finishOperation(state.busy ? state.busy.name : '');
}
//=============================================================================
export function installBackendHooks(): void {
  window.nmmGuiReload = () => { load(); };
  window.nmmGuiPush = handleBackendEvent;
}
//=============================================================================
