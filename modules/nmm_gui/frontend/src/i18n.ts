//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Window strings. The catalogs are locale/nmm-ui-<locale>.json (Crowdin),
// embedded in the binary and served by the 'i18n' request for the current
// Nelson language. An entry missing or left empty in that catalog falls back
// to the English one.
import { rpc, isEmbedded } from './rpc';
//=============================================================================
let locale = 'en_US';
let english: Record<string, string> = {};
let strings: Record<string, string> = {};
export function t(key: string, vars?: Record<string, string | number>): string {
  let text = strings[key] || english[key] || key;
  if (vars) for (const k of Object.keys(vars)) text = text.split('{' + k + '}').join(String(vars[k]));
  return text;
}
//=============================================================================
// Singular or plural form for a count, by the language's own rule: French
// uses the singular for 0 and 1, English only for 1.
export function isSingular(count: number): boolean {
  try {
    return new Intl.PluralRules(locale.replace('_', '-')).select(count) === 'one';
  } catch {
    return count === 1;
  }
}
//=============================================================================
export function stepLabel(step: string): string {
  const key = 'steps.' + step;
  const text = t(key);
  return text === key ? step : text;
}
//=============================================================================
export function translationsLoaded(): boolean {
  return Object.keys(english).length > 0;
}
//=============================================================================
// Retried by the next load while it fails (the interpreter may be busy).
export async function loadTranslations(): Promise<void> {
  try {
    const r = await rpc('i18n');
    if (r && r.ok) {
      locale = r.locale || 'en_US';
      english = r.en_US ? JSON.parse(r.en_US) : {};
      strings = r.strings ? JSON.parse(r.strings) : {};
      if (!isEmbedded()) {
        document.documentElement.lang = locale.split('_')[0];
        document.title = 'Nelson ' + t('title');
      }
    }
  } catch {
    // Keep the English page title until a later load succeeds.
  }
}
//=============================================================================
// For tests: install catalogs directly.
export function setCatalogs(nextLocale: string, en: Record<string, string>, local: Record<string, string>): void {
  locale = nextLocale;
  english = en;
  strings = local;
}
//=============================================================================
