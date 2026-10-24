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
export type WireLabelViewModel = {
  key: string;
  x: number;
  y: number;
  text: string;
};
//=============================================================================
const roots = new WeakMap<Element, Root>();
function WireLabelsView({ labels }: { labels: WireLabelViewModel[] }): JSX.Element {
  return (
    <>
      {labels.map((label) => (
        <text className="wire-label" x={label.x} y={label.y} textAnchor="middle" key={label.key}>
          {label.text}
        </text>
      ))}
    </>
  );
}
//=============================================================================
function ensureWireLabelHost(overlay: Element): SVGElement {
  let host = overlay.querySelector<SVGElement>(':scope > [data-nflow-wire-labels-root="true"]');
  if (!host) {
    host = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    host.setAttribute('data-nflow-wire-labels-root', 'true');
    overlay.appendChild(host);
  }
  return host;
}
//=============================================================================
export function renderWireLabelsView(overlay: Element, labels: WireLabelViewModel[]): void {
  const host = ensureWireLabelHost(overlay);
  let root = roots.get(host);
  if (!root) {
    root = createRoot(host);
    roots.set(host, root);
  }
  flushSync(() => {
    root?.render(<WireLabelsView labels={labels} />);
  });
}
//=============================================================================
