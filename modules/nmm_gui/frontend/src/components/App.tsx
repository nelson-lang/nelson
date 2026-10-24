//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef, useSyncExternalStore } from 'react';
import { state, subscribe, getVersion, visible } from '../store';
import { schedulePrefs } from '../prefs';
import { Toolbar } from './Toolbar';
import { PackageList } from './PackageList';
import { Detail } from './Detail';
import { StatusBar } from './StatusBar';
import { DialogHost } from './Dialogs';
import { rpc, isEmbedded } from '../rpc';
//=============================================================================
export function App() {
  // Every state change re-renders the whole (small) window.
  useSyncExternalStore(subscribe, getVersion);
  const searchRef = useRef<HTMLInputElement>(null);
  const shown = visible();
  if (state.loaded && !shown.some(p => p.name === state.selected)) state.selected = shown.length ? shown[0].name : '';
  useEffect(() => { schedulePrefs(); });
  useEffect(() => {
    // "/" focuses the search field, outside text inputs.
    const onKey = (e: KeyboardEvent) => {
      if (isEmbedded()) return; // the desktop owns the global shortcuts
      if (e.key !== '/' || e.ctrlKey || e.metaKey || e.altKey) return;
      const el = document.activeElement;
      if (el && (el.tagName === 'INPUT' || el.tagName === 'TEXTAREA')) return;
      searchRef.current?.focus();
      e.preventDefault();
    };
    document.addEventListener('keydown', onKey);
    return () => document.removeEventListener('keydown', onKey);
  }, []);
  useEffect(() => {
    // Native WebViews do not consistently hand target="_blank" links to the
    // system browser. Route web links through the native host instead.
    const onClick = (event: MouseEvent) => {
      if (!(event.target instanceof Element) || !event.target.closest('.nmm-app')) return;
      const link = event.target.closest<HTMLAnchorElement>('a[href]');
      if (!link || !/^https?:\/\//i.test(link.href)) return;
      event.preventDefault();
      if (isEmbedded()) {
        // The browser hosting the desktop opens web links itself.
        window.open(link.href, '_blank', 'noopener');
        return;
      }
      void rpc('openExternal', { url: link.href });
    };
    document.addEventListener('click', onClick);
    return () => document.removeEventListener('click', onClick);
  }, []);
  return (
    <div className="nmm-app">
      <Toolbar searchRef={searchRef} />
      <div className="body">
        <PackageList shown={shown} />
        <Detail pkg={shown.find(p => p.name === state.selected) || null} />
      </div>
      <StatusBar />
      <DialogHost />
    </div>
  );
}
//=============================================================================
