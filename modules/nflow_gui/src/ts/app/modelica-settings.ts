//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Settings-panel wiring for the OpenModelica path used by the Modelica bridge.
// It reads and writes the preference file the Nelson `modelicaConfigure` function
// uses (<preferences>/nflow_modelica.json, shape {"omcPath": path}), so the
// editor field and the command line share one setting. No new RPC is needed: the
// nflow preferences directory is prefdir(), reachable through getPreferencesPath()
// and nflow.file.readText/writeText.
//=============================================================================
import { nflowRpc, canUseNFlowRpc } from './native-rpc.js';
import { getPreferencesPath } from './host-utils.js';
import { openFilePicker } from './file-dialogs.js';
//=============================================================================
const PREF_FILE = 'nflow_modelica.json';
type ModelicaInfo = {
  available: boolean;
  capable: boolean;
  omc: string;
  version: string;
  reason: string;
};
//=============================================================================
type DetectionState = 'checking' | 'ready' | 'warning' | 'missing' | 'unavailable';
async function prefFilePath(): Promise<string | null> {
  const prefs = await getPreferencesPath();
  if (!prefs) return null;
  return `${prefs.replace(/[\\/]+$/, '')}/${PREF_FILE}`;
}
//=============================================================================
async function readOmcPath(): Promise<string> {
  if (!canUseNFlowRpc('nflow.file.readText')) return '';
  const path = await prefFilePath();
  if (!path) return '';
  try {
    const resp = JSON.parse(await nflowRpc<string>('nflow.file.readText', { path }));
    if (resp && resp.success && typeof resp.content === 'string' && resp.content.length) {
      const cfg = JSON.parse(resp.content);
      if (cfg && typeof cfg.omcPath === 'string') return cfg.omcPath;
    }
  } catch {
    /* no config yet */
  }
  return '';
}
//=============================================================================
async function writeOmcPath(value: string): Promise<boolean> {
  if (!canUseNFlowRpc('nflow.file.writeText')) return false;
  const path = await prefFilePath();
  if (!path) return false;
  try {
    // {omcPath:""} means "cleared" -> the resolver falls back to auto-detect.
    await nflowRpc('nflow.file.writeText', {
      path,
      content: JSON.stringify({ omcPath: value }),
    });
    return true;
  } catch {
    return false;
  }
}
//=============================================================================
function parseModelicaInfo(raw: unknown): ModelicaInfo {
  const value = typeof raw === 'string' ? JSON.parse(raw) : raw;
  const info = value && typeof value === 'object' ? (value as Record<string, unknown>) : {};
  return {
    available: info.available === true,
    capable: info.capable === true,
    omc: typeof info.omc === 'string' ? info.omc : '',
    version: typeof info.version === 'string' ? info.version : '',
    reason: typeof info.reason === 'string' ? info.reason : '',
  };
}
//=============================================================================
async function readModelicaInfo(): Promise<ModelicaInfo> {
  return parseModelicaInfo(await nflowRpc('nflow.modelica.info'));
}
//=============================================================================
export function initModelicaSettings(): void {
  const input = document.getElementById('modelicaPathInput') as HTMLInputElement | null;
  const browse = document.getElementById('modelicaBrowseBtn');
  const status = document.getElementById('modelicaPathStatus');
  const statusLabel = document.getElementById('modelicaPathStatusLabel');
  const statusDetails = document.getElementById('modelicaPathStatusDetails');
  if (!input) return;

  const setStatus = (state: DetectionState, label: string, details = '') => {
    if (status) status.dataset.state = state;
    if (statusLabel) statusLabel.textContent = label;
    if (statusDetails) {
      statusDetails.textContent = details;
      statusDetails.title = details;
    }
  };

  const refreshDetection = async () => {
    if (!canUseNFlowRpc('nflow.modelica.info')) {
      setStatus('unavailable', 'Detection unavailable', 'Open NFlow from Nelson to check it.');
      return;
    }
    setStatus('checking', 'Checking OpenModelica...');
    try {
      const info = await readModelicaInfo();
      if (info.capable) {
        setStatus(
          'ready',
          'Detected and ready',
          [info.version, info.omc].filter(Boolean).join('\n')
        );
      } else if (info.available) {
        setStatus('warning', 'Detected, FMU export unavailable', info.reason || info.omc);
      } else {
        setStatus('missing', 'Not detected', info.reason);
      }
    } catch (error) {
      setStatus('unavailable', 'Detection unavailable', String((error as Error)?.message || error));
    }
  };

  void (async () => {
    input.value = await readOmcPath();
    await refreshDetection();
  })();

  const save = async () => {
    const value = input.value.trim();
    const ok = await writeOmcPath(value);
    if (!ok) {
      setStatus('unavailable', 'Could not save', 'Open this in the NFlow editor window.');
      return;
    }
    await refreshDetection();
  };

  input.addEventListener('change', () => {
    void save();
  });

  browse?.addEventListener('click', async () => {
    const picked = await openFilePicker({
      mode: 'dir',
      title: 'OpenModelica home directory',
      defaultPath: input.value || '.',
    });
    if (picked) {
      input.value = picked;
      await save();
    }
  });
}
//=============================================================================
