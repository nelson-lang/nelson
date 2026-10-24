//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  useCallback,
  useEffect,
  useState,
  type KeyboardEvent as ReactKeyboardEvent,
  type MouseEvent as ReactMouseEvent,
} from 'react';
import {
  clearDiagnostics,
  subscribeDiagnostics,
  type DiagnosticSeverity,
  type NFlowDiagnostic,
} from '../app/diagnostics-service.js';
import { t } from '../utils/i18n.js';
import { getUiPref, setUiPref } from '../app/ui-prefs.js';
import { state } from '../app/state.js';
import { openBlockHelp, openHelpTopic } from '../app/block-help.js';
import { emitAppEvent } from '../app/app-events.js';
import {
  PanelContextMenu,
  clampMenuPosition,
  copyText,
  type PanelContextMenuPosition,
} from './PanelContextMenu.js';
//=============================================================================
const FILTER_KEY = 'nf-diagnostics-filters';
const SOURCE_FILTER_KEY = 'nf-diagnostics-source';
type SourceFilter = NFlowDiagnostic['source'] | 'all';
type DiagnosticContextMenu = PanelContextMenuPosition & { item: NFlowDiagnostic };
// One plain-text line per diagnostic, as pasted into a ticket or a chat:
// severity and source are the raw identifiers, not their translations.
export function formatDiagnosticLine(item: NFlowDiagnostic): string {
  return `${item.severity}: ${item.message} (${item.source})`;
}
//=============================================================================
const SOURCE_HELP_TOPICS: Record<NFlowDiagnostic['source'], string> = {
  editor: 'nflow_workspace',
  model: 'nflow_workspace',
  simulation: 'nflow_solvers',
  application: 'nflow_workspace',
  library: 'nflow_workspace',
};
//=============================================================================
function diagnosticBlock(item: NFlowDiagnostic): { type?: unknown } | null {
  if (!item.blockId) return null;
  const direct = state.blocks.get(item.blockId) as { type?: unknown } | undefined;
  if (direct) return direct;
  return (
    (Array.from(state.blocks.entries()).find(([id]) => String(id) === String(item.blockId))?.[1] as
      | { type?: unknown }
      | undefined) ?? null
  );
}
//=============================================================================
function openDiagnosticHelp(item: NFlowDiagnostic): void {
  if (item.helpTopic) {
    void openHelpTopic(item.helpTopic);
    return;
  }
  const block = diagnosticBlock(item);
  if (block) {
    void openBlockHelp(block);
    return;
  }
  void openHelpTopic(SOURCE_HELP_TOPICS[item.source]);
}
//=============================================================================
function readFilters(): Record<DiagnosticSeverity, boolean> {
  try {
    const parsed = JSON.parse(getUiPref(FILTER_KEY, '') || '') as Partial<
      Record<DiagnosticSeverity, boolean>
    >;
    return {
      error: parsed.error !== false,
      warning: parsed.warning !== false,
      info: parsed.info !== false,
    };
  } catch {
    return { error: true, warning: true, info: true };
  }
}
//=============================================================================
function readSourceFilter(): SourceFilter {
  const stored = getUiPref(SOURCE_FILTER_KEY, '') || 'all';
  const valid: SourceFilter[] = ['all', 'editor', 'model', 'simulation', 'application', 'library'];
  return (valid as string[]).includes(stored) ? (stored as SourceFilter) : 'all';
}
//=============================================================================
function DiagnosticIcon({ severity }: { severity: NFlowDiagnostic['severity'] }): JSX.Element {
  if (severity === 'warning') {
    return (
      <svg viewBox="0 0 24 24" aria-hidden="true">
        <path d="M12 3 2.5 20h19L12 3Z" />
        <path d="M12 9v5M12 17.5v.1" />
      </svg>
    );
  }
  if (severity === 'info') {
    return (
      <svg viewBox="0 0 24 24" aria-hidden="true">
        <circle cx="12" cy="12" r="9" />
        <path d="M12 10v6M12 7.5v.1" />
      </svg>
    );
  }
  return (
    <svg viewBox="0 0 24 24" aria-hidden="true">
      <circle cx="12" cy="12" r="9" />
      <path d="M12 7v6M12 16.5v.1" />
    </svg>
  );
}
//=============================================================================
function ClearIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" aria-hidden="true">
      <path d="M4 7h16M9 7V4h6v3M7 7l1 13h8l1-13M10 11v5M14 11v5" />
    </svg>
  );
}
//=============================================================================
function CopyIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" aria-hidden="true">
      <rect x="9" y="9" width="11" height="11" rx="2" />
      <path d="M5 15V5a2 2 0 0 1 2-2h8" />
    </svg>
  );
}
//=============================================================================
export function DiagnosticsPanel(): JSX.Element {
  const [items, setItems] = useState<readonly NFlowDiagnostic[]>([]);
  const [filters, setFilters] = useState<Record<DiagnosticSeverity, boolean>>(readFilters);
  const [sourceFilter, setSourceFilter] = useState<SourceFilter>(readSourceFilter);
  const [contextMenu, setContextMenu] = useState<DiagnosticContextMenu | null>(null);

  useEffect(() => subscribeDiagnostics(setItems), []);
  const closeContextMenu = useCallback(() => setContextMenu(null), []);

  const severities: Array<{ severity: DiagnosticSeverity; label: string }> = [
    { severity: 'error', label: 'Errors' },
    { severity: 'warning', label: 'Warnings' },
    { severity: 'info', label: 'Information' },
  ];
  const counts = severities.reduce(
    (result, { severity }) => {
      result[severity] = items.filter((item) => item.severity === severity).length;
      return result;
    },
    { error: 0, warning: 0, info: 0 } as Record<DiagnosticSeverity, number>
  );
  // Sources present in the current diagnostics, in a stable display order, so
  // the source picker only offers what the user can actually select.
  const presentSources = (
    ['editor', 'model', 'simulation', 'application', 'library'] as NFlowDiagnostic['source'][]
  ).filter((source) => items.some((item) => item.source === source));
  // A source filter that no longer matches anything (its diagnostics cleared)
  // must not silently hide the list, so treat it as "all".
  const effectiveSource =
    sourceFilter !== 'all' && presentSources.includes(sourceFilter) ? sourceFilter : 'all';
  const visibleItems = items.filter(
    (item) =>
      filters[item.severity] && (effectiveSource === 'all' || item.source === effectiveSource)
  );

  const copyAll = useCallback((list: readonly NFlowDiagnostic[]): void => {
    if (list.length > 0) void copyText(list.map(formatDiagnosticLine).join('\n'));
  }, []);

  const openContextMenu = (event: ReactMouseEvent<HTMLDivElement>, item: NFlowDiagnostic) => {
    event.preventDefault();
    event.stopPropagation();
    setContextMenu({ ...clampMenuPosition(event.clientX, event.clientY, 2), item });
  };

  // Ctrl/Cmd+C copies every visible row, unless the user has selected text in a
  // message (then the browser's own copy of that selection wins).
  const onPanelKeyDown = (event: ReactKeyboardEvent<HTMLDivElement>) => {
    if (!(event.ctrlKey || event.metaKey) || event.key.toLowerCase() !== 'c') return;
    if (window.getSelection()?.toString().trim()) return;
    event.preventDefault();
    copyAll(visibleItems);
  };

  const changeSource = (value: string) => {
    const next = (value as SourceFilter) || 'all';
    setSourceFilter(next);
    setUiPref(SOURCE_FILTER_KEY, next);
  };

  return (
    <div className="nf-diagnostics-panel" tabIndex={0} onKeyDown={onPanelKeyDown}>
      <div className="nf-diagnostics-toolbar">
        <div className="nf-diagnostics-filters" role="group" aria-label={t('Filter diagnostics')}>
          {severities.map(({ severity, label }) => (
            <button
              type="button"
              className={`nf-diagnostics-filter nf-diagnostic-${severity}${
                filters[severity] ? ' active' : ''
              }`}
              aria-pressed={filters[severity]}
              title={t(label)}
              key={severity}
              onClick={() => {
                setFilters((current) => {
                  const next = { ...current, [severity]: !current[severity] };
                  setUiPref(FILTER_KEY, JSON.stringify(next));
                  return next;
                });
              }}
            >
              <span className="nf-diagnostic-icon">
                <DiagnosticIcon severity={severity} />
              </span>
              <span>{t(label)}</span>
              <span className="nf-diagnostics-filter-count">{counts[severity]}</span>
            </button>
          ))}
        </div>
        {presentSources.length > 1 ? (
          <select
            className="nf-diagnostics-source-filter"
            aria-label={t('Filter by source')}
            title={t('Filter by source')}
            value={effectiveSource}
            onChange={(event) => changeSource(event.target.value)}
          >
            <option value="all">{t('All sources')}</option>
            {presentSources.map((source) => (
              <option value={source} key={source}>
                {t(source)}
              </option>
            ))}
          </select>
        ) : null}
        <button
          type="button"
          className="nf-diagnostics-copy"
          aria-label={t('Copy all')}
          title={t('Copy all')}
          disabled={visibleItems.length === 0}
          onClick={() => copyAll(visibleItems)}
        >
          <CopyIcon />
        </button>
        <button
          type="button"
          className="nf-diagnostics-clear"
          aria-label={t('Clear diagnostics')}
          title={t('Clear diagnostics')}
          disabled={items.length === 0}
          onClick={clearDiagnostics}
        >
          <ClearIcon />
        </button>
      </div>
      {items.length === 0 ? (
        <div className="nf-diagnostics-empty" role="status">
          <span className="nf-diagnostics-empty-icon">
            <DiagnosticIcon severity="info" />
          </span>
          <strong>{t('No diagnostics.')}</strong>
          <span>{t('Errors and warnings will appear here.')}</span>
        </div>
      ) : visibleItems.length === 0 ? (
        <div className="nf-diagnostics-empty" role="status">
          <strong>{t('No diagnostics match the active filters.')}</strong>
        </div>
      ) : (
        <div className="nf-diagnostics-list" role="list">
          {visibleItems.map((item) => (
            <div
              role="listitem"
              className={`nf-diagnostic-row nf-diagnostic-${item.severity}`}
              key={item.id}
              onContextMenu={(event) => openContextMenu(event, item)}
            >
              <button
                type="button"
                className="nf-diagnostic-main"
                onClick={() => {
                  // A drag that selected message text should not also jump to the
                  // block; only a plain click navigates.
                  if (window.getSelection()?.toString().trim()) return;
                  if (item.blockId) {
                    emitAppEvent('nf-select-block', { blockId: item.blockId });
                  }
                }}
                disabled={!item.blockId}
              >
                <span className="nf-diagnostic-icon">
                  <DiagnosticIcon severity={item.severity} />
                </span>
                <span className="nf-diagnostic-message" title={item.message}>
                  {item.message}
                </span>
                <span className="nf-diagnostic-source">{t(item.source)}</span>
              </button>
              <button
                type="button"
                className="nf-diagnostic-help"
                aria-label={t('Open related help')}
                title={t('Open related help')}
                onClick={() => openDiagnosticHelp(item)}
              >
                ?
              </button>
            </div>
          ))}
        </div>
      )}
      {contextMenu ? (
        <PanelContextMenu
          position={contextMenu}
          onClose={closeContextMenu}
          items={[
            {
              label: t('Copy'),
              onSelect: () => void copyText(formatDiagnosticLine(contextMenu.item)),
            },
            {
              label: t('Copy all'),
              onSelect: () => copyAll(visibleItems),
            },
          ]}
        />
      ) : null}
    </div>
  );
}
//=============================================================================
export default DiagnosticsPanel;
//=============================================================================
