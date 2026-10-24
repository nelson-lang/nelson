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
import { renderTeXMath } from '../svg-elements.js';
import { exprToLatex } from '../../utils/expr.js';
import { buildTransferTeX } from '../transfer-fn.js';
import { applySubsystemBlockLabel } from './renderer-block-label-subsystem.js';
import * as FieldNames from '../../FieldNames.js';
//=============================================================================
export function applyBlockTypeLabels(block: any, ctx: any) {
  if (block.type === 'constant') {
    const mathGroup = block.group.querySelector('.nf-constant-formula');
    if (mathGroup) {
      const { tex } = ctx.toLeadingMinusTex(block.params[FieldNames.kValue], '0');
      renderTeXMath(mathGroup, tex, block.width, block.height);
      ctx.scaleMathToFit(mathGroup, block.width, block.height);
      mathGroup.setAttribute('transform', '');
    }
  }
  if (block.type === 'step') {
    const texts = block.group.querySelectorAll('text');
    if (texts[1]) texts[1].textContent = `t=${block.params[FieldNames.kStepTime]}`;
  }
  if (block.type === 'ramp') {
    const texts = block.group.querySelectorAll('text');
    if (texts[1])
      texts[1].textContent = `m=${block.params[FieldNames.kSlope]} t0=${
        block.params[FieldNames.kStart]
      }`;
  }
  if (block.type === 'impulse') {
    const texts = block.group.querySelectorAll('text');
    if (texts[1])
      texts[1].textContent = `t=${block.params[FieldNames.kTime]} A=${
        block.params[FieldNames.kAmp]
      }`;
  }
  if (block.type === 'sine') {
    const texts = block.group.querySelectorAll('text');
    if (texts[1])
      texts[1].textContent = `A=${block.params[FieldNames.kAmp]} f=${
        block.params[FieldNames.kFreq]
      }`;
    if (texts[2]) texts[2].textContent = `ph=${block.params[FieldNames.kPhase]}`;
  }
  if (block.type === 'chirp') {
    const texts = block.group.querySelectorAll('text');
    if (texts[1])
      texts[1].textContent = `A=${block.params[FieldNames.kAmp]} f1=${
        block.params[FieldNames.kF0]
      }`;
    if (texts[2])
      texts[2].textContent = `f2=${block.params[FieldNames.kF1]} T=${block.params[FieldNames.kT1]}`;
  }
  if (block.type === 'noise') {
    const texts = block.group.querySelectorAll('text');
    if (texts[1]) texts[1].textContent = `A=${block.params[FieldNames.kAmp]}`;
  }
  if (block.type === FieldNames.kDelay) {
    const mathGroup = block.group.querySelector('.nf-delay-formula');
    if (mathGroup) renderTeXMath(mathGroup, 'e^{-sT}', block.width, block.height);
  }
  if (block.type === 'comment') ctx.updateCommentLayout(block);
  if (block.type === 'display') {
    if (block.displayTitleEl) {
      block.displayTitleEl.textContent = String(block.params?.[FieldNames.kLabel] ?? 'Display');
    }
  }
  if (block.type === 'expression') {
    const mathGroup = block.group.querySelector('.nf-userfunc-formula');
    if (mathGroup) {
      renderTeXMath(
        mathGroup,
        `\\scriptsize{${exprToLatex(block.params[FieldNames.kExpr] || 'u')}}`,
        block.width,
        block.height
      );
      ctx.resizeUserFuncFromLabel(block);
    }
  }
  if (block.type === 'tf') {
    const mathGroup = block.group.querySelector('.nf-tf-formula');
    if (mathGroup) {
      renderTeXMath(
        mathGroup,
        buildTransferTeX(block.params[FieldNames.kNum], block.params[FieldNames.kDen]),
        block.width,
        block.height
      );
      ctx.resizeUserFuncFromLabel(block);
    }
  }
  if (block.type === 'dtf') {
    const mathGroup = block.group.querySelector('.dtf-math');
    if (mathGroup) {
      renderTeXMath(
        mathGroup,
        buildTransferTeX(block.params[FieldNames.kNum], block.params[FieldNames.kDen], 'z'),
        block.width,
        block.height
      );
      ctx.resizeUserFuncFromLabel(block);
    }
  }
  if (block.type === 'pid') {
    const mathGroup = block.group.querySelector('.nf-pid-formula');
    if (mathGroup) renderTeXMath(mathGroup, '\\mathsf{PID}', block.width, block.height);
  }
  if (block.type === 'switch') {
    const mathGroup = block.group.querySelector('.nf-switch-formula');
    if (mathGroup) {
      const op =
        block.params[FieldNames.kCondition] === FieldNames.kGt
          ? '>'
          : block.params[FieldNames.kCondition] === FieldNames.kNe
          ? '\\ne'
          : '\\geq';
      const rawThreshold = block.params[FieldNames.kThreshold] ?? 0;
      const threshold = String(rawThreshold).trim() || '0';
      const digitCount = (threshold.match(/\d/g) || []).length;
      mathGroup.classList.remove(
        'nf-switch-formula--l',
        'nf-switch-formula--m',
        'nf-switch-formula--s'
      );
      mathGroup.classList.add(
        digitCount > 8
          ? 'nf-switch-formula--s'
          : digitCount >= 3
          ? 'nf-switch-formula--m'
          : 'nf-switch-formula--l'
      );
      mathGroup.setAttribute('transform', 'translate(0 17)');
      renderTeXMath(mathGroup, `${op}\\!\\!${threshold}`, 48, 34);
    }
  }
  if (block.type === 'zoh') {
    const mg = block.group.querySelector('.zoh-math');
    if (mg) renderTeXMath(mg, '\\mathsf{ZOH}', block.width, block.height);
  }
  if (block.type === 'foh') {
    const mg = block.group.querySelector('.foh-math');
    if (mg) renderTeXMath(mg, '\\mathsf{FOH}', block.width, block.height);
  }
  if (block.type === 'fileSource') {
    const texts = block.group.querySelectorAll('text');
    if (texts[1]) texts[1].textContent = `${block.params[FieldNames.kPath]}`;
  }
  if (block.type === FieldNames.kGain) {
    const mathGroup = block.group.querySelector('.nf-gain-formula');
    if (mathGroup) {
      const { tex } = ctx.toLeadingMinusTex(block.params[FieldNames.kGain], '1');
      renderTeXMath(mathGroup, tex, block.width, block.height);
      ctx.scaleMathToFit(mathGroup, block.width * 0.6, block.height);
      ctx.positionGainMath(mathGroup, block, 8);
      const base = mathGroup.getAttribute('transform') || 'translate(0, 0)';
      const match = base.match(/translate\(\s*([-+]?\d*\.?\d+)\s*,?\s*([-+]?\d*\.?\d+)?\s*\)/);
      if (match) {
        const x = Number(match[1]) || 0;
        const y = Number(match[2] || 0) || 0;
        mathGroup.setAttribute('transform', `translate(${x - 4}, ${y})`);
      } else {
        mathGroup.setAttribute('transform', 'translate(-4, 0)');
      }
    }
  }
  if (block.type === 'sum') {
    const signs = String(block.params[FieldNames.kSumInputs] ?? '');
    block.group.querySelectorAll('.sum-sign').forEach((textEl: any) => {
      const idx = Number(textEl.getAttribute('data-sign-index')) || 0;
      textEl.textContent = signs[idx] === '-' ? '-' : '';
    });
  }
  if (block.type === 'derivative') {
    const texts = block.group.querySelectorAll('text');
    if (texts[1]) texts[1].textContent = 'd/dt';
  }
  if (block.type === 'lpf') {
    const texts = block.group.querySelectorAll('text');
    if (texts[1]) texts[1].textContent = `fc=${block.params[FieldNames.kCutoff]}`;
  }
  if (block.type === 'hpf') {
    const texts = block.group.querySelectorAll('text');
    if (texts[1]) texts[1].textContent = `fc=${block.params[FieldNames.kCutoff]}`;
  }
  if (block.type === 'ddelay') {
    const mg = block.group.querySelector('.ddelay-math');
    if (mg) renderTeXMath(mg, 'z^{-1}', block.width, block.height);
  }
  // Refresh the scope top-left label when the inspector changes name/showTitle.
  try {
    if ((block as any).type === 'scope') {
      const typeLabelEl = (block as any).scopeTitle as Element | undefined | null;
      const centeredTitleEl = (block as any).scopeBlockTitle as Element | undefined | null;
      const showTitle = (block as any).params?.showTitle !== false;
      const nameText = String((block as any).params?.name || '').trim();
      const combined = nameText && showTitle ? `Scope - ${nameText}` : 'Scope';
      if (typeLabelEl) {
        try {
          typeLabelEl.setAttribute('display', 'block');
          (typeLabelEl as HTMLElement).textContent = combined;
        } catch (e) {}
      }
      if (centeredTitleEl) {
        try {
          centeredTitleEl.setAttribute('display', 'none');
        } catch (e) {}
      }
    }
  } catch (e) {}

  applySubsystemBlockLabel(block, ctx);
}
//=============================================================================
