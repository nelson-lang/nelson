//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Offline cache and environment (export / restore of the installed packages).
import { useEffect, useState } from 'react';
import { rpc, type Json } from '../rpc';
import { t, isSingular } from '../i18n';
import { restoreManifest } from '../actions';
import { fmtBytes } from '../lib/info';
import { IconCopy, IconInstall } from './Icons';
import { Modal } from './Modal';
//=============================================================================
function corruptedCount(verify: Json): number {
  const c = verify && verify.corrupted;
  if (Array.isArray(c)) return c.length;
  if (c && typeof c === 'object' && c.name) return 1;
  return 0;
}
//=============================================================================
export function CacheDialog({ done }: { done: (v: null) => void }) {
  const [data, setData] = useState<Json>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const run = async (method: string, params: Record<string, unknown> = {}) => {
    setLoading(true);
    try {
      const r = await rpc(method, params);
      if (r && r.ok) setData(r);
      setError('');
    } catch (e) { setError((e as Error).message); }
    setLoading(false);
  };
  useEffect(() => { run('cache'); }, []);
  const entries: Json[] = data && Array.isArray(data.entries) ? data.entries : [];
  const count = data ? corruptedCount(data.verify) : 0;
  let note: JSX.Element | null = null;
  if (error) note = <div className="cache-note cache-note--err">{error}</div>;
  else if (data && data.verify) {
    note = data.verify.ok
      ? <div className="cache-note cache-note--ok">{t('cacheOk')}</div>
      : <div className="cache-note cache-note--err">{t(isSingular(count) ? 'cacheCorruptedOne' : 'cacheCorruptedMany', { count })}</div>;
  }
  const total = data ? t(isSingular(data.archives || 0) ? 'cacheSummaryOne' : 'cacheSummaryMany', { archives: data.archives || 0, size: fmtBytes(data.bytes) }) : '';
  return (
    <Modal className="modal--cache" label={t('cacheHeading')} onCancel={() => done(null)}>
      <div className="cache-head">
        <div><div className="modal__title">{t('cacheHeading')}</div><div className="mono cache-dir">{data && data.dir ? data.dir : ''}</div></div>
        <button type="button" className="btn btn--ghost" data-c="close" onClick={() => done(null)}>{t('close')}</button>
      </div>
      <div className="cache-sub">{loading && <span className="spinner" />}<span>{total}</span></div>
      {note}
      <div className="cache-list">
        {entries.length ? entries.map(en => (
          <div key={en.name + '@' + en.version} className="cache-row">
            <span className="cache-row__name"><b>{en.name}</b> <span className="mono">{en.version}</span></span>
            <span className="mono cache-row__size">{fmtBytes(en.bytes)}</span>
            <button type="button" className="btn btn--danger" data-c="remove" onClick={() => run('cacheRemove', { name: en.name, version: en.version })}>{t('cacheRemove')}</button>
          </div>
        )) : <div className="cache-empty">{t('cacheEmpty')}</div>}
      </div>
      <div className="modal__actions">
        <button type="button" className="btn" data-c="verify" disabled={loading || !entries.length} onClick={() => run('cacheVerify')}>{t('cacheVerify')}</button>
        <button type="button" className="btn btn--danger" data-c="clear" disabled={loading || !entries.length} onClick={() => run('cacheClear')}>{t('cacheClear')}</button>
      </div>
    </Modal>
  );
}
//=============================================================================
// One "name version" per line; blank lines and # or % comments are skipped.
export function parseManifest(text: string): { name: string; version: string }[] {
  const out: { name: string; version: string }[] = [];
  for (const line of String(text).split('\n')) {
    const s = line.trim();
    if (!s || s.charAt(0) === '#' || s.charAt(0) === '%') continue;
    const parts = s.split(/[\s@]+/);
    if (parts[0]) out.push({ name: parts[0], version: parts[1] || '' });
  }
  return out;
}
//=============================================================================
function saveManifestFile(manifest: string): void {
  const url = URL.createObjectURL(new Blob([manifest + '\n'], { type: 'text/plain' }));
  const link = document.createElement('a');
  link.href = url;
  link.download = 'nelson-packages.txt';
  document.body.appendChild(link);
  link.click();
  link.remove();
  window.setTimeout(() => URL.revokeObjectURL(url), 1000);
}
//=============================================================================
export function EnvironmentDialog({ done }: { done: (v: null) => void }) {
  const [manifest, setManifest] = useState('');
  const [restore, setRestore] = useState('');
  useEffect(() => {
    rpc('freeze').then(r => { if (r && r.ok) setManifest(r.manifest || ''); }).catch(() => {});
  }, []);
  const copy = () => {
    const ta = document.getElementById('env-export') as HTMLTextAreaElement | null;
    if (ta) { ta.select(); try { document.execCommand('copy'); } catch { /* unavailable */ } }
  };
  return (
    <Modal className="modal--env" label={t('env')} onCancel={() => done(null)}>
      <div className="cache-head"><div className="modal__title">{t('env')}</div><button type="button" className="btn btn--ghost" data-e="close" onClick={() => done(null)}>{t('close')}</button></div>
      <label className="env-field"><span>{t('envExport')}</span><textarea id="env-export" readOnly rows={6} value={manifest} /></label>
      <div className="modal__actions">
        <button type="button" className="btn" data-e="save" onClick={() => saveManifestFile(manifest)}>{t('envSave')}</button>
        <button type="button" className="btn" data-e="copy" onClick={copy}><IconCopy />{t('copy')}</button>
      </div>
      <label className="env-field"><span>{t('envImport')}</span>
        <textarea id="env-import" rows={6} placeholder={t('envImportHint')} spellCheck={false} value={restore} onChange={e => setRestore(e.target.value)} /></label>
      <input type="file" id="env-file" accept=".txt,text/plain" hidden
        onChange={e => { const file = e.target.files && e.target.files[0]; if (file) file.text().then(setRestore); }} />
      <div className="modal__actions">
        <button type="button" className="btn" data-e="open" onClick={() => document.getElementById('env-file')?.click()}>{t('envOpen')}</button>
        <button type="button" className="btn btn--primary" data-e="restore" onClick={() => { done(null); restoreManifest(parseManifest(restore)); }}><IconInstall />{t('envRestore')}</button>
      </div>
    </Modal>
  );
}
//=============================================================================
