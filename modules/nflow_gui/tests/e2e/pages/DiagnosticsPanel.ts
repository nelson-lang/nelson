//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Page Object for the bottom Diagnostics panel and its status-bar counters.
// Selectors are derived from DiagnosticsPanel.tsx, console-host.tsx and
// diagnostics-service.ts so specs read the same DOM the user reads.
//=============================================================================
import { type Page, type Locator } from '@playwright/test';
//=============================================================================
export interface DiagnosticRow {
  severity: 'error' | 'warning' | 'info';
  message: string;
  source: string;
}
//=============================================================================
export class DiagnosticsPanel {
  readonly page: Page;
  readonly tab: Locator;
  readonly panel: Locator;
  readonly list: Locator;
  readonly rows: Locator;
  readonly errorCountEl: Locator;
  readonly warningCountEl: Locator;
  readonly tabCountEl: Locator;
  constructor(page: Page) {
    this.page = page;
    this.tab = page.locator('[data-bottom-tab="diagnostics"]');
    this.panel = page.locator('.nf-diagnostics-panel');
    this.list = page.locator('.nf-diagnostics-list');
    this.rows = page.locator('.nf-diagnostic-row');
    this.errorCountEl = page.locator('#diagnosticErrorCount');
    this.warningCountEl = page.locator('#diagnosticWarningCount');
    this.tabCountEl = page.locator('#diagnosticTabCount');
  }
  //=============================================================================
  async open(): Promise<void> {
    await this.tab.click();
    await this.panel.waitFor({ state: 'visible', timeout: 5000 });
  }
  //=============================================================================
  private async count(el: Locator): Promise<number> {
    const txt = (await el.textContent())?.trim() ?? '0';
    const n = Number(txt);
    return Number.isFinite(n) ? n : 0;
  }
  //=============================================================================
  /** Error count shown on the status-bar counter (always in the DOM). */
  errorCount(): Promise<number> {
    return this.count(this.errorCountEl);
  }
  //=============================================================================
  /** Warning count shown on the status-bar counter (always in the DOM). */
  warningCount(): Promise<number> {
    return this.count(this.warningCountEl);
  }
  //=============================================================================
  tabCount(): Promise<number> {
    return this.count(this.tabCountEl);
  }
  //=============================================================================
  /** Wait until the diagnostics service reports the given error count (or more). */
  async waitForErrorCount(min: number, timeout = 5000): Promise<void> {
    await this.page.waitForFunction(
      (target) => {
        const el = document.getElementById('diagnosticErrorCount');
        return el ? Number(el.textContent || '0') >= target : false;
      },
      min,
      { timeout }
    );
  }
  //=============================================================================
  /** Read every visible diagnostic row (panel must be open). */
  async items(): Promise<DiagnosticRow[]> {
    const n = await this.rows.count();
    const out: DiagnosticRow[] = [];
    for (let i = 0; i < n; i++) {
      const row = this.rows.nth(i);
      const cls = (await row.getAttribute('class')) ?? '';
      const severity = cls.includes('nf-diagnostic-error')
        ? 'error'
        : cls.includes('nf-diagnostic-warning')
        ? 'warning'
        : 'info';
      const message = (await row.locator('.nf-diagnostic-message').textContent())?.trim() ?? '';
      const source = (await row.locator('.nf-diagnostic-source').textContent())?.trim() ?? '';
      out.push({ severity, message, source });
    }
    return out;
  }
  //=============================================================================
  rowMainByText(text: string): Locator {
    return this.rows
      .filter({ has: this.page.locator('.nf-diagnostic-message', { hasText: text }) })
      .locator('.nf-diagnostic-main');
  }
}
//=============================================================================
