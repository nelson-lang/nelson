//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { renderFloatingToolbarView } from './components/floating-toolbar-view.js';
import { emitAppEvent, onAppEvent } from './app-events.js';
//=============================================================================
const instances = new WeakMap<HTMLElement, FloatingToolbar>();
class FloatingToolbar {
  private readonly toolbar: HTMLElement;
  private readonly appIcon: HTMLElement | null;
  private blockLayer: HTMLElement | null = null;
  private observer: MutationObserver | null = null;
  private running = false;
  private pending = false;
  private stoppable = false;
  private runnable = false;
  private currentTime = 0;
  private targetTime = 10;
  private progress = 0;
  constructor(toolbar: HTMLElement) {
    this.toolbar = toolbar;
    this.appIcon = document.getElementById('appIcon');

    this.render();
    this.initAvailability();
    this.initTimeDisplay();
    this.initEvents();
  }
  //=============================================================================
  private render() {
    renderFloatingToolbarView(this.toolbar, {
      running: this.running,
      pending: this.pending,
      stoppable: this.stoppable,
      runnable: this.runnable,
      currentTime: this.currentTime,
      targetTime: this.targetTime,
      progress: this.progress,
      onToggleRun: this.onToggleRun,
      onOpenSettings: this.onOpenSettings,
    });
  }
  //=============================================================================
  private readTargetTime(): number {
    const value = Number(
      (document.getElementById('runtimeInput') as HTMLInputElement | null)?.value
    );
    return Number.isFinite(value) && value > 0 ? value : 10;
  }
  //=============================================================================
  private initTimeDisplay() {
    this.targetTime = this.readTargetTime();
    onAppEvent('nf-sim-time', (detail) => {
      const d = detail || {};
      this.currentTime = Math.max(0, Number(d.time) || 0);
      this.progress = Math.max(0, Math.min(100, d.done ? 100 : Number(d.progress) || 0));
      this.targetTime = this.readTargetTime();
      this.render();
    });
    onAppEvent('nf-sim-start', () => {
      this.currentTime = 0;
      this.progress = 0;
      this.targetTime = this.readTargetTime();
      this.render();
    });
    const runtimeInput = document.getElementById('runtimeInput');
    const updateTarget = () => {
      this.targetTime = this.readTargetTime();
      this.render();
    };
    runtimeInput?.addEventListener('input', updateTarget);
    runtimeInput?.addEventListener('change', updateTarget);
  }
  //=============================================================================
  private initAvailability() {
    const nextBlockLayer = document.getElementById('blockLayer');
    if (nextBlockLayer !== this.blockLayer) {
      this.observer?.disconnect();
      this.observer = null;
      this.blockLayer = nextBlockLayer;
    }
    this.updateAvailability();

    if (this.blockLayer && !this.observer && typeof MutationObserver !== 'undefined') {
      this.observer = new MutationObserver(() => this.updateAvailability());
      this.observer.observe(this.blockLayer, { childList: true, subtree: false });
    }
  }
  //=============================================================================
  private updateAvailability() {
    if (!this.blockLayer) {
      this.blockLayer = document.getElementById('blockLayer');
    }
    this.runnable = !!(this.blockLayer && this.blockLayer.childElementCount > 0);
    this.toolbar.style.removeProperty('display');
    this.render();
  }
  //=============================================================================
  private setRunningState(state: boolean) {
    this.running = !!state;
    this.render();

    if (this.appIcon) {
      this.appIcon.classList.toggle('running', this.running);
    }
  }
  //=============================================================================
  private togglePending(isPending: boolean) {
    this.pending = isPending;
    this.toolbar.classList.toggle('pending', isPending);
    this.render();
  }
  //=============================================================================
  private initEvents() {
    onAppEvent('nf-sim-stop', () => {
      this.stoppable = false;
      this.togglePending(false);
      this.setRunningState(false);
    });

    onAppEvent('nf-sim-start', () => {
      this.stoppable = true;
      this.togglePending(false);
      this.setRunningState(true);
    });

    // Every run/pause/resume/stop transition (`phase` is absent from emitters
    // that only know whether the run is going).
    onAppEvent('nf-sim-state', (detail) => {
      this.togglePending(false);
      if (detail?.phase) {
        this.stoppable = detail.phase === 'running' || detail.phase === 'paused';
      }
      this.setRunningState(!!detail?.running);
    });

    this.setRunningState(this.running);
  }
  //=============================================================================
  private onToggleRun = () => {
    this.togglePending(true);
    window.setTimeout(() => {
      if (this.pending) this.togglePending(false);
    }, 1000);
  };
  //=============================================================================
  private onOpenSettings = () => {
    emitAppEvent('nf-inspector-show', { tab: 'simulation' });
  };
  //=============================================================================
  public destroy() {
    if (this.observer) {
      this.observer.disconnect();
    }
  }
  //=============================================================================
  public refresh() {
    this.initAvailability();
  }
}
//=============================================================================
function initFloatingToolbar(): void {
  const toolbar = document.getElementById('floatingToolbar');
  if (!toolbar) return;

  const instance = instances.get(toolbar);
  if (instance) {
    instance.refresh();
  } else {
    instances.set(toolbar, new FloatingToolbar(toolbar));
  }
}
//=============================================================================
if (typeof document !== 'undefined') {
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initFloatingToolbar);
  } else {
    initFloatingToolbar();
  }
}
//=============================================================================
if (typeof window !== 'undefined') {
  try {
    (
      globalThis as unknown as { initFloatingToolbar?: typeof initFloatingToolbar }
    ).initFloatingToolbar = initFloatingToolbar;
  } catch {
    /* ignore in strict sandboxes */
  }
}
//=============================================================================
