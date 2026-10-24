//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { t, isSingular } from '../i18n';
import { state, registryProblem } from '../store';
//=============================================================================
export function StatusBar() {
  const info = state.registryInfo;
  const offline = !!(info && info.offline);
  let registry: string;
  let title: string;
  if (info && info.registry && !state.registryError) {
    registry = t('statusRegistry', { registry: info.registry }) + (offline ? t('statusOffline') : '');
    title = offline ? t('statusOfflineTitle', { file: info.cached_file || '' }) : info.registry;
  } else {
    registry = t(registryProblem() === 'unavailable' ? 'statusUnavailable' : 'statusNotConfigured');
    title = state.registryError || '';
  }
  const counts = t('statusCounts', {
    packages: state.registry.length, pkg: t(isSingular(state.registry.length) ? 'pkgOne' : 'pkgMany'),
    installed: state.installed.length, inst: t(isSingular(state.installed.length) ? 'instOne' : 'instMany'),
  });
  return (
    <footer className="status">
      <span className={'status__registry' + (offline ? ' is-warn' : '')} id="status-registry" title={title}>{registry}</span>
      <span className="status__sep">&middot;</span>
      <span id="status-counts">{counts}</span>
      <span className={'status__state' + (state.busy ? ' is-busy' : '')} id="status-state">
        {state.busy ? t('statusRunning', { label: state.busy.label, name: state.busy.name }) : t('statusReady')}
      </span>
    </footer>
  );
}
//=============================================================================
