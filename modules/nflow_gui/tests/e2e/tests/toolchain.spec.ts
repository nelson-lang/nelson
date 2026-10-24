//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Playwright E2E: toolchain detection tests
//=============================================================================
import { test, expect } from '@playwright/test';
//=============================================================================
test.describe('Toolchain build checkbox (sidebar)', () => {
  test('disables build checkbox for C when C toolchain is absent', async ({ page }) => {
    await page.addInitScript(() => {
      try {
        sessionStorage.removeItem('nf-have-c-toolchain');
        sessionStorage.removeItem('nf-have-rust-toolchain');
        localStorage.removeItem('nf-have-c-toolchain');
        localStorage.removeItem('nf-have-rust-toolchain');
        try { (window as any).__nf_bootstrap = undefined; } catch (_e) {}
      } catch (_e) {}
    });

    await page.goto('/');
    await page.waitForSelector('button.activity-icon[data-panel="settings"]', { timeout: 10000 });
    await page.click('button.activity-icon[data-panel="settings"]');
    await page.waitForSelector('#panel-settings .sidebar-content #codegenLang', { timeout: 10000 });
    await page.selectOption('#codegenLang', 'c');
    const disabled = await page.locator('#codegenBuildAfterGen').isDisabled();
    expect(disabled).toBe(true);
  });

  test('enables build checkbox for C when C toolchain is present', async ({ page }) => {
    await page.addInitScript(() => {
      try { sessionStorage.setItem('nf-have-c-toolchain', '1'); } catch (_e) {}
    });
    await page.goto('/');
    await page.waitForSelector('button.activity-icon[data-panel="settings"]', { timeout: 10000 });
    await page.click('button.activity-icon[data-panel="settings"]');
    await page.waitForSelector('#panel-settings .sidebar-content #codegenLang');
    await page.selectOption('#codegenLang', 'c');
    expect(await page.locator('#codegenBuildAfterGen').isEnabled()).toBe(true);
  });

  test('disables build checkbox for Rust when Rust toolchain is absent', async ({ page }) => {
    await page.addInitScript(() => {
      try {
        sessionStorage.removeItem('nf-have-c-toolchain');
        sessionStorage.removeItem('nf-have-rust-toolchain');
        localStorage.removeItem('nf-have-c-toolchain');
        localStorage.removeItem('nf-have-rust-toolchain');
        try { (window as any).__nf_bootstrap = undefined; } catch (_e) {}
      } catch (_e) {}
    });
    await page.goto('/');
    await page.waitForSelector('button.activity-icon[data-panel="settings"]', { timeout: 10000 });
    await page.click('button.activity-icon[data-panel="settings"]');
    await page.waitForSelector('#panel-settings .sidebar-content #codegenLang');
    await page.selectOption('#codegenLang', 'rust');
    expect(await page.locator('#codegenBuildAfterGen').isDisabled()).toBe(true);
  });

  test('enables build checkbox for Rust when Rust toolchain is present', async ({ page }) => {
    await page.addInitScript(() => {
      try { sessionStorage.setItem('nf-have-rust-toolchain', '1'); } catch (_e) {}
    });
    await page.goto('/');
    await page.waitForSelector('button.activity-icon[data-panel="settings"]', { timeout: 10000 });
    await page.click('button.activity-icon[data-panel="settings"]');
    await page.waitForSelector('#panel-settings .sidebar-content #codegenLang');
    await page.selectOption('#codegenLang', 'rust');
    expect(await page.locator('#codegenBuildAfterGen').isEnabled()).toBe(true);
  });
});
//=============================================================================
