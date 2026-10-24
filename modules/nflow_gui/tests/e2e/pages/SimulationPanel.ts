//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { type Page, type Locator, expect } from '@playwright/test';
//=============================================================================
export class SimulationPanel {
  readonly page: Page;
  readonly panel: Locator;
  readonly diagramName: Locator;
  readonly simDt: Locator;
  readonly runtimeInput: Locator;
  readonly variablesInput: Locator;
  readonly applyBtn: Locator;
  readonly resetBtn: Locator;
  readonly variablesPreview: Locator;
  readonly autoRouteInput: Locator;
  readonly lockedInput: Locator;
  constructor(page: Page) {
    this.page = page;
    this.panel = page.locator('#panel-simulation');
    this.diagramName = page.locator('#diagramName');
    this.simDt = page.locator('#simDt');
    this.runtimeInput = page.locator('#runtimeInput');
    this.variablesInput = page.locator('#variablesInput');
    this.applyBtn = page.locator('#applyVariables');
    this.resetBtn = page.locator('#resetVariables');
    this.variablesPreview = page.locator('#variablesPreview');
    this.autoRouteInput = page.locator('#autoRouteInput');
    this.lockedInput = page.locator('#lockedInput');
  }
  //=============================================================================
  async open(): Promise<void> {
    await this.page.locator('.activity-icon[data-panel="simulation"]').click();
    // If the sidebar is collapsed, the first click may only mark the icon active:
    // click again to expand it.
    try {
      await expect(this.panel).toBeVisible({ timeout: 1_000 });
    } catch (_e) {
      const sidebar = this.page.locator('.sidebar');
      const isCollapsed = await sidebar.evaluate((el) => el.classList.contains('collapsed'));
      if (isCollapsed) {
        await this.page.locator('.activity-icon[data-panel="simulation"]').click();
      }
      await expect(this.panel).toBeVisible({ timeout: 5_000 });
    }
  }
  //=============================================================================
  async setRuntime(seconds: number): Promise<void> {
    await this.runtimeInput.fill(String(seconds));
  }
  //=============================================================================
  async setSampleTime(dt: number): Promise<void> {
    await this.simDt.fill(String(dt));
  }
  //=============================================================================
  async setDiagramName(name: string): Promise<void> {
    await this.diagramName.fill(name);
  }
  //=============================================================================
  async getDiagramName(): Promise<string> {
    return (await this.diagramName.inputValue()) || '';
  }
  //=============================================================================
  async setVariables(text: string): Promise<void> {
    const content = this.page.locator('.sidebar-section-content.variables-editor');
    if ((await content.count()) > 0) {
      const collapsed = await content.evaluate((el) => el.classList.contains('collapsed'));
      if (collapsed) {
        const toggle = this.page.locator('.sidebar-section-title:has-text("Variables") .section-toggle');
        try {
          await toggle.click();
        } catch (_e) {
          await this.page.locator('.sidebar-section-title:has-text("Variables")').click();
        }
      }
    }

    await this.variablesInput.waitFor({ state: 'visible', timeout: 5_000 });
    await this.variablesInput.fill(text);
  }
  //=============================================================================
  async applyVariables(): Promise<void> {
    await this.applyBtn.waitFor({ state: 'visible', timeout: 3_000 });
    await this.applyBtn.click();
  }
  //=============================================================================
  async resetVariables(): Promise<void> {
    await this.resetBtn.click();
  }
  //=============================================================================
  async variablesPreviewText(): Promise<string> {
    return (await this.variablesPreview.textContent())?.trim() ?? '';
  }
  //=============================================================================
  async toggleAutoRoute(enabled: boolean): Promise<void> {
    const isChecked = await this.autoRouteInput.isChecked();
    if (isChecked !== enabled) await this.autoRouteInput.click();
  }
  //=============================================================================
  async toggleLocked(enabled: boolean): Promise<void> {
    const isChecked = await this.lockedInput.isChecked();
    if (isChecked !== enabled) await this.lockedInput.click();
  }
}
//=============================================================================
