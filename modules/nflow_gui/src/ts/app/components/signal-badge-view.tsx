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
type SignalBadgeViewProps = {
  visible: boolean;
  x: number;
  y: number;
  text: string;
};
//=============================================================================
let badgeRoot: Root | null = null;
let badgeHost: HTMLDivElement | null = null;
function SignalBadgeView({ visible, x, y, text }: SignalBadgeViewProps): JSX.Element {
  return (
    <div
      id="signalBadge"
      hidden={!visible}
      style={{
        left: `${x + 12}px`,
        top: `${y - 24}px`,
      }}
    >
      {text}
    </div>
  );
}
//=============================================================================
function ensureSignalBadgeRoot(): Root {
  if (badgeRoot && badgeHost) return badgeRoot;
  badgeHost = document.createElement('div');
  badgeHost.id = 'signalBadgeRoot';
  document.body.appendChild(badgeHost);
  badgeRoot = createRoot(badgeHost);
  return badgeRoot;
}
//=============================================================================
export function renderSignalBadgeView(props: SignalBadgeViewProps): void {
  const root = ensureSignalBadgeRoot();
  flushSync(() => {
    root.render(<SignalBadgeView {...props} />);
  });
}
//=============================================================================
