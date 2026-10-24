//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export interface Theme {
  id: string;
  name: string;
}
//=============================================================================
let themes: Theme[] = [];
export const getThemes = (): Theme[] => themes;
export const setThemes = (newThemes: Theme[]): void => {
  themes = newThemes;
};
//=============================================================================
export const applyTheme = (themeId?: string): void => {
  const chosen: Theme = (themeId ? themes.find((theme) => theme.id === themeId) : undefined) ||
    themes[0] || { id: 'default', name: 'Default' };
  if (chosen.id === 'default') {
    document.documentElement.removeAttribute('data-theme');
  } else {
    document.documentElement.setAttribute('data-theme', chosen.id);
  }
};
//=============================================================================
export const loadThemes = async (): Promise<Theme[]> => {
  try {
    const response = await fetch('styles/themes.css');
    const cssText = await response.text();
    const themeRegex = /\/\*\s*Theme Name:\s*([^\*]+)\s*\*\//g;
    const matches = Array.from(cssText.matchAll(themeRegex));

    themes = matches.map((match, index) => {
      const name = match[1].trim();
      const isDefault = index === 0;
      return {
        id: isDefault ? 'default' : name.toLowerCase().replace(/\s+/g, '-'),
        name,
      } as Theme;
    });

    return themes;
  } catch (error) {
    themes = [
      { id: 'default', name: 'Default Dark' },
      { id: 'light', name: 'Light' },
    ];
    return themes;
  }
};
//=============================================================================
