//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useState, type ReactNode } from 'react';
import type { Json } from '../rpc';
import { t, stepLabel } from '../i18n';
import { state, update, hasUpdate, requiredBy } from '../store';
import { act, cancelBusy, quickAction, showInAll, verifyPackage } from '../actions';
import { artifactOf, fmtBytes, licenseStatus, listOf, shortChecksum, textOf } from '../lib/info';
import { IconCheck, IconCopy, IconExternal, IconInstall, IconRemove, IconUpdate } from './Icons';
//=============================================================================
export function CopyButton({ text, label, className, attrs, children }: {
  text: string; label?: string; className: string; attrs?: Record<string, string>; children: ReactNode;
}) {
  const [copied, setCopied] = useState(false);
  const copy = () => {
    try {
      if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(text);
      else { const ta = document.createElement('textarea'); ta.value = text; document.body.appendChild(ta); ta.select(); document.execCommand('copy'); ta.remove(); }
    } catch { /* clipboard may be unavailable */ }
    setCopied(true);
    window.setTimeout(() => setCopied(false), 1200);
  };
  return <button type="button" className={className} {...attrs} onClick={copy}>{copied ? <><IconCheck />{label}</> : children}</button>;
}
//=============================================================================
function TestsOption({ value, onChange }: { value: boolean; onChange: (v: boolean) => void }) {
  return <label className="opt"><input type="checkbox" id="opt-tests" checked={value} onChange={e => onChange(e.target.checked)} />{t('runTests')}</label>;
}
//=============================================================================
export function ProgressCard({ p }: { p: Json }) {
  const b = state.busy!;
  const determinate = b.total > 0;
  const pct = determinate ? Math.round((Math.max(0, b.index - (b.step === 'done' ? 0 : 1)) / b.total) * 100) : 0;
  const stepText = b.cancelling ? t('cancelling') : b.step ? t('progressStep', { index: b.index, total: b.total, step: stepLabel(b.step) }) : t('progressWaiting');
  return (
    <div className="progress">
      <div className="progress__head"><b>{b.label} {p.name} {b.version || ''}</b><span>{stepText}</span></div>
      <div className="progress__bar"><div className={determinate ? 'is-determinate' : ''} style={determinate ? { width: pct + '%' } : undefined} /></div>
      {b.sub && <div className="progress__sub"><span className="spinner" />{t('dependencyStep', { name: b.sub.name, step: stepLabel(b.sub.step) })}</div>}
      {b.trail.length > 0 && (
        <div className="trail">
          {b.trail.map(s => <span key={s} className={'trail__step' + (s === b.step ? ' is-current' : '')}>{s !== b.step && <IconCheck />}{stepLabel(s)}</span>)}
        </div>
      )}
      <div className="progress__foot">
        <span className="progress__note">{t('progressNote')}</span>
        <button type="button" className="btn btn--danger" data-a="cancel" disabled={b.cancelling} onClick={() => cancelBusy()}>{t('cancel')}</button>
      </div>
    </div>
  );
}
//=============================================================================
export function InstalledActions({ p, kind }: { p: Json; kind: string }) {
  const [tests, setTests] = useState(false);
  const update_ = hasUpdate(p);
  const iv = p.installed.version;
  const quick = (a: string, v = '') => { if (!state.busy) quickAction(a, p.name, v); };
  return (
    <>
      <div className="actions">
        {update_ && (
          <button type="button" className="btn btn--primary" data-a="update" data-n={p.name} data-v={p.latest}
            onClick={() => { if (!state.busy) act('update', p.name, p.latest, tests); }}><IconUpdate />{t('btnUpdateTo', { version: p.latest })}</button>
        )}
        {p.installed.active
          ? <button type="button" className="btn" data-a="unload" data-n={p.name} onClick={() => quick('unload')}>{t('btnUnload')}</button>
          : <button type="button" className="btn" data-a="load" data-n={p.name} data-v={iv} onClick={() => quick('load', iv)}>{t('btnLoad')}</button>}
        {p.installed.pinned
          ? <button type="button" className="btn" data-a="unpin" data-n={p.name} onClick={() => quick('unpin')}>{t('btnUnpin')}</button>
          : <button type="button" className="btn" data-a="pin" data-n={p.name} data-v={iv} onClick={() => quick('pin', iv)}>{t('btnPin')}</button>}
        <button type="button" className="btn" data-a="verify" data-n={p.name} onClick={() => verifyPackage(p.name)}>{t('verify')}</button>
        <button type="button" className="btn btn--danger" data-a="remove" data-n={p.name} data-v={iv}
          onClick={() => { if (!state.busy) act('remove', p.name, iv); }}><IconRemove />{t('btnRemove')}</button>
        <span className="actions__hint">{p.installed.pinned ? t('pinnedTo', { version: p.installed.pinned }) : ''}</span>
      </div>
      {update_ && kind !== 'archive' && <TestsOption value={tests} onChange={setTests} />}
    </>
  );
}
//=============================================================================
export function InstallActions({ p, kind }: { p: Json; kind: string }) {
  const [tests, setTests] = useState(false);
  const hint = kind === 'archive' ? t('hintArchive') : kind === 'git' ? t('hintGit') : t('hintLocal');
  const vers = state.versions[p.name];
  const chosen = state.chosenVersion[p.name] || p.version;
  return (
    <>
      <div className="actions">
        <button type="button" className="btn btn--primary" data-a="install" data-n={p.name} data-v={chosen}
          onClick={() => { if (!state.busy) act('install', p.name, chosen, tests); }}><IconInstall />{t('btnInstall', { version: chosen })}</button>
        {vers && vers.length > 1 && (
          <label className="ver"><span>{t('versionLabel')}</span>
            <select id="ver-select" value={chosen} onChange={e => { state.chosenVersion[p.name] = e.target.value; update(); }}>
              {vers.map(v => <option key={v} value={v}>{v}</option>)}
            </select>
          </label>
        )}
        <span className="actions__hint">{hint}</span>
      </div>
      {kind !== 'archive' && <TestsOption value={tests} onChange={setTests} />}
    </>
  );
}
//=============================================================================
function DepLinks({ names }: { names: string[] }) {
  return <>{names.map(d => <button key={d} type="button" className="deplink" data-dep={d} onClick={() => showInAll(d)}>{d}</button>)}</>;
}
//=============================================================================
export function DetailMeta({ p }: { p: Json }) {
  const depNames = p.dependencies && typeof p.dependencies === 'object' && !Array.isArray(p.dependencies) ? Object.keys(p.dependencies) : [];
  const reqBy = requiredBy(p.name);
  const license = licenseStatus(p.license);
  const meta: [string, string, boolean, ReactNode?][] = ([
    [t('metaNelson'), p.nelson || '', true],
    [t('metaPlatforms'), textOf(p.platforms) === 'all' ? t('metaAllPlatforms') : textOf(p.platforms) || '', false],
    [t('metaDeps'), textOf(p.dependencies) || t('metaNone'), false, depNames.length ? <DepLinks names={depNames} /> : undefined],
    [t('requiredBy'), reqBy.join(', '), false, reqBy.length ? <DepLinks names={reqBy} /> : undefined],
    [t('metaLicense'), p.license || t('licenseNone'), false, license === 'ok' ? undefined : <span className="warn-text">{p.license || t('licenseNone')}</span>],
    [t('metaCategories'), listOf(p.categories).join(', '), false],
    [t('metaDiskSize'), p.installed && p.installed.bytes ? fmtBytes(p.installed.bytes) : '', false],
    [t('metaInstalledOn'), p.installed ? p.installed.installed_at || '' : '', false],
  ] as [string, string, boolean, ReactNode?][]).filter(([, v]) => v !== '');
  const wide: [string, ReactNode, string][] = [];
  if (p.installed && p.installed.path) wide.push([t('metaInstalledIn'), p.installed.path, p.installed.path]);
  const art = artifactOf(p, state.arch);
  if (art.source) {
    const label = t('metaSource') + (art.arch ? ' (' + art.arch + ')' : '');
    const link = /^https?:\/\//i.test(art.source)
      ? <a href={art.source} target="_blank" rel="noopener">{art.source.replace(/^https?:\/\//i, '')}</a> : art.source;
    wide.push([label, link, art.source]);
  }
  if (art.checksum) {
    wide.push([t('metaChecksum'), <>{shortChecksum(art.checksum)} <CopyButton text={art.checksum} className="btn btn--ghost btn--mini"
      attrs={{ 'data-a': 'copyhash', 'data-h': art.checksum, title: t('copyChecksum') }}><IconCopy /></CopyButton></>, art.checksum]);
  }
  if (!meta.length && !wide.length) return null;
  return (
    <div className="meta">
      {meta.map(([l, v, mono, node]) => (
        <div key={l} className="meta__item"><span className="meta__label">{l}</span>
          <span className={'meta__value' + (mono ? ' mono' : '') + (node ? ' meta__value--wrap' : '')}>{node || v}</span></div>
      ))}
      {wide.map(([l, node, full]) => (
        <div key={l} className="meta__item meta__item--wide"><span className="meta__label">{l}</span><span className="meta__value mono" title={full}>{node}</span></div>
      ))}
    </div>
  );
}
//=============================================================================
export function LinksRow({ p }: { p: Json }) {
  const links = ([[p.homepage, t('homepage')], [p.repository, t('linkRepo')], [p.issues, t('linkIssues')], [p.documentation, t('linkDocs')]] as [unknown, string][])
    .filter(([url]) => url && /^https?:\/\//i.test(String(url)));
  const version = p.installed ? p.installed.version : state.chosenVersion[p.name] || p.version || '';
  // Pin the version shown in the detail so the copied command reproduces
  // this exact install (registry packages only: others cannot be installed
  // by name).
  const command = version ? `nmm('install', '${p.name}', '${version}')` : `nmm('install', '${p.name}')`;
  return (
    <div className="linksrow">
      {links.map(([url, label]) => <a key={label} className="linkchip" href={String(url)} target="_blank" rel="noopener">{label} <IconExternal /></a>)}
      {p.latest && (
        <CopyButton text={command} label={t('copied')} className="btn btn--ghost linksrow__copy" attrs={{ 'data-a': 'copy', 'data-n': p.name, 'data-v': version }}>
          <IconCopy />{t('copyInstall')}
        </CopyButton>
      )}
    </div>
  );
}
//=============================================================================
