//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { flushSync } from 'react-dom';
import { createRoot, type Root } from 'react-dom/client';
//=============================================================================
type TooltipViewProps = {
  visible: boolean;
  x: number;
  y: number;
  text: string;
};
//=============================================================================
let tooltipRoot: Root | null = null;
let tooltipHost: HTMLDivElement | null = null;
function TooltipView({ visible, x, y, text }: TooltipViewProps): JSX.Element {
  return (
    <div
      id="nflow-tooltip"
      style={{
        position: 'fixed',
        pointerEvents: 'none',
        padding: '6px 8px',
        background: 'var(--nflow-widget-background)',
        border: '1px solid var(--nflow-widget-border)',
        boxShadow: 'var(--nflow-widget-shadow)',
        color: 'var(--nflow-foreground)',
        fontSize: '12px',
        borderRadius: '4px',
        zIndex: 2000,
        left: `${Math.round(x + 12)}px`,
        top: `${Math.round(y + 12)}px`,
        display: visible ? 'block' : 'none',
      }}
    >
      {text}
    </div>
  );
}
//=============================================================================
function ensureTooltipRoot(): Root {
  if (tooltipRoot && tooltipHost) return tooltipRoot;
  tooltipHost = document.createElement('div');
  tooltipHost.id = 'nflow-tooltip-root';
  document.body.appendChild(tooltipHost);
  tooltipRoot = createRoot(tooltipHost);
  return tooltipRoot;
}
//=============================================================================
export function renderTooltipView(props: TooltipViewProps): void {
  const root = ensureTooltipRoot();
  flushSync(() => {
    root.render(<TooltipView {...props} />);
  });
}
//=============================================================================
export function disposeTooltipView(): void {
  if (tooltipRoot) {
    flushSync(() => {
      tooltipRoot?.unmount();
    });
  }
  tooltipRoot = null;
  tooltipHost?.remove();
  tooltipHost = null;
}
//=============================================================================
