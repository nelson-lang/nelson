//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useState } from 'react';
//=============================================================================
export interface FigurePreferences {
  properties: boolean;
  navigation: boolean;
  details: boolean;
  width: number;
}
//=============================================================================
export const figurePreferencesKey = 'nelson.figure.workspace.v1';
const defaults: FigurePreferences = {
  properties: false,
  navigation: false,
  details: false,
  width: 260,
};
//=============================================================================
export function readFigurePreferences(): FigurePreferences {
  try {
    const saved = JSON.parse(localStorage.getItem(figurePreferencesKey) ?? '{}');
    return {
      properties: typeof saved.properties === 'boolean' ? saved.properties : false,
      navigation: typeof saved.navigation === 'boolean' ? saved.navigation : false,
      details: typeof saved.details === 'boolean' ? saved.details : false,
      width: Number.isFinite(saved.width)
        ? Math.max(220, Math.min(600, saved.width))
        : defaults.width,
    };
  } catch {
    return defaults;
  }
}
//=============================================================================
export function useFigurePreferences() {
  // The inspector is a per-figure workspace, not a global preference. Keep
  // the stored value readable for older sessions, but always start a newly
  // mounted figure with its inspector closed.
  const [preferences, setPreferences] = useState(() => ({
    ...readFigurePreferences(),
    properties: false,
  }));
  useEffect(() => {
    try {
      localStorage.setItem(figurePreferencesKey, JSON.stringify(preferences));
    } catch {
      return;
    }
  }, [preferences]);
  useEffect(() => {
    const update = (event: StorageEvent) => {
      if (event.key === figurePreferencesKey) {
        setPreferences((current) => ({
          ...readFigurePreferences(),
          properties: current.properties,
        }));
      }
    };
    window.addEventListener('storage', update);
    return () => window.removeEventListener('storage', update);
  }, []);
  const change = <Key extends keyof FigurePreferences>(key: Key, value: FigurePreferences[Key]) =>
    setPreferences((current) => ({ ...current, [key]: value }));
  return { preferences, change };
}
//=============================================================================
