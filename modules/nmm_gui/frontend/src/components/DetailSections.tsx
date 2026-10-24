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
import { t } from '../i18n';
import { state, update } from '../store';
import { showDoc, showInAll, showTree } from '../actions';
import { renderMarkdown } from '../lib/markdown';
import { IconCheck } from './Icons';
//=============================================================================
export function VerifyBox({ p }: { p: Json }) {
  const v = state.verify;
  if (!v || v.name !== p.name || !p.installed) return null;
  if (v.loading) return <div className="verify"><span className="spinner" />{t('verifying')}</div>;
  if (v.ok) return <div className="verify verify--ok"><IconCheck />{t('verifyOk')}</div>;
  return <div className="verify verify--err"><b>{t('verifyIssues')}</b><ul>{v.issues.map((s, k) => <li key={k}>{s}</li>)}</ul></div>;
}
//=============================================================================
function docAvailable(p: Json, kind: string): boolean {
  if (typeof p[kind] === 'string' && p[kind].trim()) return true;
  // an installed folder, or the repository of a registry package
  return !!(p.installed && p.installed.path) || /^https:\/\/(github|gitlab)\.com\//i.test(String(p.repository || ''));
}
//=============================================================================
function DocButton({ p, kind }: { p: Json; kind: 'readme' | 'changelog' }) {
  if (!docAvailable(p, kind)) return null;
  const active = !!state.doc && state.doc.name === p.name && state.doc.kind === kind;
  const label = kind === 'readme' ? (active ? t('readmeHide') : t('readme')) : active ? t('changelogHide') : t('changelog');
  return (
    <button type="button" className={'btn tree-toggle' + (active ? ' is-active' : '')} data-a="doc" data-n={p.name} data-k={kind}
      onClick={() => showDoc(p.name, kind)}>{label}</button>
  );
}
//=============================================================================
export function DocSections({ p }: { p: Json }) {
  if (!docAvailable(p, 'readme') && !docAvailable(p, 'changelog')) return null;
  const d = state.doc && state.doc.name === p.name ? state.doc : null;
  let body: JSX.Element | null = null;
  if (d) {
    if (d.loading) body = <div className="tree-loading"><span className="spinner" /></div>;
    else if (d.error) body = <div className="tree-error mono">{d.error}</div>;
    // renderMarkdown escapes everything before rebuilding its small subset.
    else if (d.text && d.text.trim()) body = <div className="doc" dangerouslySetInnerHTML={{ __html: renderMarkdown(d.text) }} />;
    else body = <div className="tree-error">{t('docEmpty')}</div>;
  }
  return (
    <div className="tree-sec">
      <div className="doc-toggles"><DocButton p={p} kind="readme" /><DocButton p={p} kind="changelog" /></div>
      {body}
    </div>
  );
}
//=============================================================================
// One dependency comes as an object, several as an array; a leaf has an
// empty object, which is no child.
function treeChildren(node: Json): Json[] {
  const d = node && node.dependencies;
  if (Array.isArray(d)) return d;
  if (d && typeof d === 'object' && d.package) return [d];
  return [];
}
//=============================================================================
function TreeNode({ node, depth }: { node: Json; depth: number }) {
  if (!node) return null;
  const name = node.package || '';
  const ver = node.version || node.requested || '';
  const tag = node.installed ? <span className="tree-tag tree-tag--ok">{t('treeInstalled')}</span>
    : !node.version || node.action === 'missing' ? <span className="tree-tag tree-tag--warn">{t('treeMissing')}</span>
      : <span className="tree-tag">{t('treeToInstall')}</span>;
  const conflicts = Array.isArray(node.conflicts) && node.conflicts.length
    ? <span className="tree-tag tree-tag--warn" title={node.conflicts.join('; ')}>!</span> : null;
  return (
    <>
      <div className="tree-row" style={{ paddingLeft: depth * 18 }}>
        <button type="button" className="deplink" data-dep={name} onClick={() => showInAll(name)}>{name}</button>
        <span className="mono tree-ver">{ver}</span>{tag}{conflicts}
      </div>
      {treeChildren(node).map((c, k) => <TreeNode key={k} node={c} depth={depth + 1} />)}
    </>
  );
}
//=============================================================================
export function TreeSection({ p }: { p: Json }) {
  const tr = state.tree && state.tree.name === p.name ? state.tree : null;
  if (!tr) {
    return <div className="tree-sec"><button type="button" className="btn tree-toggle" data-a="tree" data-n={p.name} onClick={() => showTree(p.name)}>{t('depTree')}</button></div>;
  }
  let body: JSX.Element;
  if (tr.loading) body = <div className="tree-loading"><span className="spinner" />{t('depTreeLoading')}</div>;
  else if (tr.error) body = <div className="tree-error mono">{tr.error}</div>;
  else if (tr.root) body = <div className="tree"><TreeNode node={tr.root} depth={0} /></div>;
  else body = <div className="tree-error">{t('depTreeNone')}</div>;
  return (
    <div className="tree-sec">
      <button type="button" className="btn tree-toggle is-active" data-a="tree-hide" onClick={() => { state.tree = null; update(); }}>{t('depTreeHide')}</button>
      {body}
    </div>
  );
}
//=============================================================================
