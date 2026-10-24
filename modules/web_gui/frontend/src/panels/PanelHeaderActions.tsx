//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { IDockviewHeaderActionsProps } from 'dockview';
import { ContextMenu, useContextMenu } from '../components/ContextMenu';
import { DesktopIcon } from '../components/DesktopIcon';
import { panelActionItems } from '../desktop/panelActions';
//=============================================================================
export function PanelHeaderActions(props: IDockviewHeaderActionsProps): JSX.Element | null {
  const { menu, openMenu, closeMenu } = useContextMenu();
  if (!props.activePanel) return null;
  return (
    <>
      <button
        type="button"
        className="panel-actions"
        title="Panel actions"
        aria-label="Panel actions"
        onClick={(event) =>
          openMenu(event, panelActionItems(props.activePanel!, props.containerApi))
        }
      >
        <DesktopIcon name="more" />
      </button>
      <ContextMenu menu={menu} onClose={closeMenu} />
    </>
  );
}
//=============================================================================
