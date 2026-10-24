//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { signalDiagramChanged, state } from './state.js';
import { onAppEvent } from './app-events.js';
//=============================================================================
type Binding = {
  kind?: string;
  blockPath?: string | string[];
  paramName?: string;
  varName?: string;
  element?: string;
};
//=============================================================================
type RuntimeBlock = {
  id?: unknown;
  params?: Record<string, unknown>;
};
//=============================================================================
function pathOf(binding: Binding): string[] {
  const raw = binding.blockPath;
  return (Array.isArray(raw) ? raw : raw == null ? [] : [raw]).map(String).filter(Boolean);
}
//=============================================================================
function findRootBlock(id: string): RuntimeBlock | undefined {
  return (state.blocks.get(id) ??
    [...state.blocks.values()].find(
      (candidate) => String((candidate as RuntimeBlock | undefined)?.id ?? '') === id
    )) as RuntimeBlock | undefined;
}
//=============================================================================
function findBlock(path: string[]): RuntimeBlock | undefined {
  let current = path.length ? findRootBlock(path[0]) : undefined;
  for (let index = 1; current && index < path.length; index += 1) {
    const subsystem = current.params?.subsystem as { blocks?: RuntimeBlock[] } | undefined;
    current = subsystem?.blocks?.find((candidate) => String(candidate.id) === path[index]);
  }
  return current;
}
//=============================================================================
function elementIndices(element?: string): number[] | null {
  if (!element) return [];
  const indices = element
    .replace(/[()\s]/g, '')
    .split(',')
    .map((item) => Number(item) - 1);
  return indices.length && indices.every((index) => Number.isInteger(index) && index >= 0)
    ? indices
    : null;
}
//=============================================================================
export function setDashboardElement(current: unknown, element: string | undefined, value: number) {
  const indices = elementIndices(element);
  if (indices === null) return current;
  if (!indices.length) return value;
  if (!Array.isArray(current)) return current;
  const root = structuredClone(current) as unknown[];
  let cursor = root;
  for (let depth = 0; depth < indices.length; depth += 1) {
    const index = indices[depth];
    if (index >= cursor.length) return current;
    if (depth === indices.length - 1) cursor[index] = value;
    else {
      if (!Array.isArray(cursor[index])) return current;
      cursor = cursor[index] as unknown[];
    }
  }
  return root;
}
//=============================================================================
function variableLiteral(value: unknown): string {
  if (typeof value === 'number') return Number.isFinite(value) ? String(value) : '0';
  if (!Array.isArray(value)) return '0';
  if (value.every(Array.isArray))
    return `[${value.map((row) => (row as unknown[]).map(variableLiteral).join(', ')).join('; ')}]`;
  return `[${value.map(variableLiteral).join(', ')}]`;
}
//=============================================================================
function updateVariableText(name: string, value: unknown): void {
  const assignment = `${name} = ${variableLiteral(value)};`;
  state.variablesText = `${state.variablesText.trim()}\n${assignment}`.trim();
  const input = document.getElementById('variablesInput') as HTMLTextAreaElement | null;
  if (input) input.value = state.variablesText;
}
//=============================================================================
function applyBinding(binding: Binding, value: number): boolean {
  if (binding.kind === 'parameter' && binding.paramName) {
    const target = findBlock(pathOf(binding));
    if (!target?.params || !(binding.paramName in target.params)) return false;
    target.params[binding.paramName] = setDashboardElement(
      target.params[binding.paramName],
      binding.element,
      value
    );
    return true;
  }
  if (binding.kind === 'variable' && binding.varName) {
    const current = state.variables[binding.varName];
    if (current === undefined) return false;
    const next = setDashboardElement(current, binding.element, value);
    state.variables[binding.varName] = next;
    updateVariableText(binding.varName, next);
    return true;
  }
  return false;
}
//=============================================================================
let installed = false;
export function installDashboardBindingEvents(): void {
  if (installed) return;
  installed = true;
  onAppEvent('nf-dashboard-control-change', (detail) => {
    const value = Number(detail?.value);
    if (!detail?.binding || !Number.isFinite(value)) return;
    if (applyBinding(detail.binding as Binding, value)) signalDiagramChanged();
  });
}
//=============================================================================
