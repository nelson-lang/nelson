//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Modal dialogs, opened with openDialog(kind, props) (actions.ts), which
// resolves with the dialog result.
import { useEffect, useRef, useState } from 'react';
import type { Json } from '../rpc';
import { t } from '../i18n';
import { state } from '../store';
import { renderMarkdown } from '../lib/markdown';
import { CacheDialog, EnvironmentDialog } from './ToolDialogs';
import { Modal } from './Modal';
//=============================================================================
function ConfirmDialog({ message, done }: { message: string; done: (v: boolean) => void }) {
  return (
    <Modal className="modal--confirm" label={t('confirmTitle')} onCancel={() => done(false)} onConfirm={() => done(true)}>
      <div className="modal__title">{t('confirmTitle')}</div>
      <div className="modal__msg">{message}</div>
      <div className="modal__actions">
        <button type="button" className="btn" data-r="0" onClick={() => done(false)}>{t('cancel')}</button>
        <button type="button" className="btn btn--danger" data-r="1" onClick={() => done(true)}>{t('btnRemove')}</button>
      </div>
    </Modal>
  );
}
//=============================================================================
function PromptDialog({ props, done }: { props: Json; done: (v: string | null) => void }) {
  const [value, setValue] = useState('');
  const input = useRef<HTMLInputElement>(null);
  useEffect(() => input.current?.focus(), []);
  return (
    <Modal className="modal--prompt" label={props.title} onCancel={() => done(null)} onConfirm={() => done(value.trim())}>
      <div className="modal__title">{props.title}</div>
      <div className="modal__msg">{props.message}</div>
      <input className="modal__input" type="text" placeholder={props.placeholder} spellCheck={false} ref={input} value={value} onChange={e => setValue(e.target.value)} />
      <div className="modal__actions">
        <button type="button" className="btn" data-r="0" onClick={() => done(null)}>{t('cancel')}</button>
        <button type="button" className="btn btn--primary" data-r="1" onClick={() => done(value.trim())}>{props.ok}</button>
      </div>
    </Modal>
  );
}
//=============================================================================
// Install / update confirmation: what the command will do.
function PreviewDialog({ info, done }: { info: Json; done: (v: boolean) => void }) {
  const warn = (text: string) => (text ? <div className="warn-note">{text}</div> : null);
  return (
    <Modal className="modal--preview" label={info.title} onCancel={() => done(false)} onConfirm={() => done(true)}>
      <div className="modal__title">{info.title}</div>
      {info.planError && warn(t('previewPlanError', { error: info.planError }))}
      {warn(info.major)}{warn(info.native ? t('previewNative') : '')}{warn(info.license)}
      {Array.isArray(info.deps) && (info.deps.length
        ? <><div className="preview__label">{t('previewDeps')}</div>
          <ul className="preview__deps">{info.deps.map((d: Json) => <li key={d.name}><b>{d.name}</b> <span className="mono">{d.version}</span></li>)}</ul></>
        : <div className="preview__line">{t('previewNoDeps')}</div>)}
      {info.size && <div className="preview__line">{t('previewSize', { size: info.size })}</div>}
      {info.changes && <><div className="preview__label">{t('previewWhatsNew')}</div>
        <div className="doc preview__changes" dangerouslySetInnerHTML={{ __html: renderMarkdown(info.changes) }} /></>}
      <div className="modal__actions">
        <button type="button" className="btn" data-r="0" onClick={() => done(false)}>{t('cancel')}</button>
        <button type="button" className="btn btn--primary" data-r="1" onClick={() => done(true)}>{info.confirm}</button>
      </div>
    </Modal>
  );
}
//=============================================================================
export function DialogHost() {
  const dialog = state.dialog;
  if (!dialog) return null;
  const done = dialog.resolve;
  switch (dialog.kind) {
    case 'confirm': return <ConfirmDialog message={dialog.props.message} done={done} />;
    case 'prompt': return <PromptDialog props={dialog.props} done={done} />;
    case 'preview': return <PreviewDialog info={dialog.props} done={done} />;
    case 'cache': return <CacheDialog done={done} />;
    case 'env': return <EnvironmentDialog done={done} />;
    default: return null;
  }
}
//=============================================================================
