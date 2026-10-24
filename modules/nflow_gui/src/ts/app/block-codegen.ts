//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { nflowRpc } from './native-rpc.js';
import { chooseDirectoryViaDialog } from './file-dialogs.js';
import { writeStatus } from './status-service.js';
import { t } from '../utils/i18n.js';
//=============================================================================
export async function generateBlockCode(block: any, statusEl: HTMLElement | null): Promise<void> {
  try {
    const subsystem = block.params?.subsystem;
    if (!subsystem) return;
    const simDtEl = document.getElementById('simDt') as HTMLInputElement | null;
    const sampleTimeVal = simDtEl ? Number(simDtEl.value) || 0.01 : 0.01;
    const logEl: HTMLElement | null = (block as any)._codegenLogEl ?? null;
    try {
      const choose = await chooseDirectoryViaDialog();
      if (!choose?.success) return;
      const destDir = String(choose.path || '');
      const buildAfterGenEl = document.getElementById(
        'codegenBuildAfterGen'
      ) as HTMLInputElement | null;
      const build = (block as any)._codegenBuild ?? buildAfterGenEl?.checked ?? false;
      const buildConfig: string = (block as any)._codegenBuildConfig ?? 'Debug';
      const optionsPayload = {
        sampleTime: sampleTimeVal,
        build,
        buildConfig,
        hostBlock: {
          width: Number(block?.width) || undefined,
          height: Number(block?.height) || undefined,
          externalInputs: (block?.params as any)?.externalInputs || [],
          externalOutputs: (block?.params as any)?.externalOutputs || [],
        },
      } as any;
      if (build) writeStatus(statusEl, t('Building...'));
      if (build && logEl) logEl.textContent = t('Building...');
      const genRaw = await nflowRpc<string>('nflow.codegen.generateBlockToDir', {
        subsystemJson: JSON.stringify(subsystem),
        optionsJson: JSON.stringify(optionsPayload),
        destinationDirectory: destDir,
      });
      const res = JSON.parse(genRaw);
      if (res.success) {
        if (res.buildSuccess === true) {
          writeStatus(statusEl, t('Build succeeded'));
          if (logEl) logEl.textContent = t('Build succeeded');
        } else if (res.buildSuccess === false) {
          const buildMsg = String(res.buildError || 'unknown build error');
          writeStatus(statusEl, t('Build failed: {message}', { message: buildMsg }));
          if (logEl) logEl.textContent = t('Build failed: {message}', { message: buildMsg });
        } else {
          writeStatus(statusEl, t('Saved: {path}', { path: res.dir }));
        }
      } else {
        writeStatus(statusEl, t('Codegen error: {message}', { message: res.error }));
        if (logEl) logEl.textContent = t('Codegen error: {message}', { message: res.error });
      }
    } catch (err: any) {
      writeStatus(statusEl, t('Codegen error: {message}', { message: err?.message ?? err }));
      if (logEl)
        logEl.textContent = t('Codegen error: {message}', { message: err?.message ?? err });
    }
  } catch (err) {
    console.error('[GenerateBlockCode] error', err);
  }
}
//=============================================================================
