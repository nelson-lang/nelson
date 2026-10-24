//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { type Page, type Locator } from '@playwright/test';
//=============================================================================
/**
 * Page Object for the floating toolbar play/pause button (the sidebar
 * #panel-simulation has its own run/pause/stop buttons, not covered here).
 * `data-action` holds the current state ("run" | "pause").
 */
export class SimulationToolbar {
  readonly page: Page;
  readonly toolbar: Locator;
  readonly playPauseBtn: Locator;
  constructor(page: Page) {
    this.page = page;
    this.toolbar = page.locator('#floatingToolbar');
    this.playPauseBtn = page.locator('#playPauseBtn');
  }
  async ensureVisible(): Promise<void> {
    const visible = await this.toolbar.isVisible();
    if (!visible) {
      // The toolbar is hidden (display:none) until a simulation starts.
    }
  }
  //=============================================================================
  async clickPlayPause(): Promise<void> {
    await this.playPauseBtn.click();
  }
  /** The button carries data-action="pause" while running, "run" otherwise. */
  async isRunning(): Promise<boolean> {
    const action = await this.playPauseBtn.getAttribute('data-action');
    const label = ((await this.playPauseBtn.textContent()) || '').trim();
    if (label === 'Resume') return false;
    return action === 'pause';
  }
  //=============================================================================
  async isStopped(): Promise<boolean> {
    return !(await this.isRunning());
  }
  async statusText(): Promise<string> {
    return (await this.page.locator('.status-bar').textContent()) ?? '';
  }
}
//=============================================================================
