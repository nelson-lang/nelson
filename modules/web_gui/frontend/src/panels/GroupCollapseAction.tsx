//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useState } from 'react';
import type { IDockviewHeaderActionsProps } from 'dockview';
import { DesktopIcon } from '../components/DesktopIcon';
import { isGroupCollapsed, onCollapseChange, toggleGroupCollapsed } from '../desktop/collapse';
//=============================================================================
// Chevron at the left of a group's tab strip, like the Qt desktop's dock title
// bar: collapses the group to its tab strip and expands it back. Floating
// groups are already free-standing windows, so they get no chevron (same rule
// as the Qt title bar, which hides it on undocked client windows).
export function GroupCollapseAction(props: IDockviewHeaderActionsProps): JSX.Element | null {
  const { group } = props;
  const [, force] = useState(0);

  useEffect(() => onCollapseChange(() => force((n) => n + 1)), []);

  if (group.api.location.type === 'floating') {
    return null;
  }
  const collapsed = isGroupCollapsed(group);
  return (
    <button
      className="group-collapse"
      title={collapsed ? 'Expand panel' : 'Collapse panel'}
      aria-label={collapsed ? 'Expand panel' : 'Collapse panel'}
      onClick={() => toggleGroupCollapsed(group)}
    >
      <DesktopIcon name={collapsed ? 'expand' : 'collapse'} />
    </button>
  );
}
//=============================================================================
