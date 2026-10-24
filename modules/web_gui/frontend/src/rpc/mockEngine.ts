//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Standalone mock backend for nelson-webview: an in-memory workspace, history,
// filesystem, figures and editor files, plus a tiny expression evaluator, so the
// desktop runs without the C++ engine. It answers the same RPC method names as
// the real backend.
import type {
  BreakpointsResult,
  CellValue,
  CompletionResult,
  DebuggerState,
  DrawPrimitive,
  EditorOpenResult,
  FigureDisplayListResult,
  FigureImageResult,
  FigureListResult,
  FsListResult,
  HistoryListResult,
  MenuNode,
  PingResult,
  ProgressDialogPush,
  UIControl,
  VarBlockParams,
  VarBlockResult,
  VarOpenResult,
  VarSetCellParams,
  WorkspaceListResult,
  WorkspaceVarSummary,
} from './types';
import { decodePointPairs } from '../panels/figureMarkers';
import { mockFigureAction } from './mockFigureActions';
//=============================================================================
// Base64 little-endian float32 pairs, the vertex format of the real display list.
function encodePointPairsMock(points: [number, number][]): string {
  const buffer = new ArrayBuffer(points.length * 8);
  const view = new DataView(buffer);
  points.forEach((pt, i) => {
    view.setFloat32(i * 8, pt[0], true);
    view.setFloat32(i * 8 + 4, pt[1], true);
  });
  const bytes = new Uint8Array(buffer);
  let binary = '';
  for (let i = 0; i < bytes.length; i++) binary += String.fromCharCode(bytes[i]);
  return btoa(binary);
}
//=============================================================================
type DialogShow = {
  id: number;
  kind: 'input' | 'message' | 'question' | 'list' | 'color' | 'file' | 'font';
  title: string;
  message: string;
  icon: string;
  fields: { prompt: string; value: string; lines: number }[];
  buttons: string[];
  defaultButton: string;
  items?: string[];
  multiSelect?: boolean;
  initialSelection?: number[];
  fileMode?: 'open' | 'save' | 'dir';
  defaultPath?: string;
  suggestedName?: string;
  historyKey?: string;
  fontFamily?: string;
  fontSize?: number;
  fontBold?: boolean;
  fontItalic?: boolean;
};
//=============================================================================
type Matrix = number[][];
interface MockVar {
  name: string;
  className: string;
  isChar: boolean;
  charValue?: string;
  strData?: string[][]; // string-array contents (className === 'string')
  structArr?: { fields: string[]; rows: string[][] }; // read-only struct array
  tableGrid?: { columns: string[]; rows: string[][] }; // read-only table grid
  textValue?: string; // read-only text view (function handle, sparse, N-D, etc.)
  data: Matrix;
  props?: string[]; // for graphics-object handles (property completion / display)
}
//=============================================================================
const SURFACE_PROPS = [
  'Type',
  'Parent',
  'Children',
  'Visible',
  'UserData',
  'XData',
  'YData',
  'ZData',
  'CData',
  'FaceColor',
  'EdgeColor',
  'LineStyle',
  'LineWidth',
];
//=============================================================================
interface MockFigure {
  handle: number;
  title: string;
  kind: 'plot' | 'surf' | 'bar';
  series: number[];
  rot: number; // rotation for pseudo-3d feedback
  zoom: number;
  // uidemo extras: interactive controls and a menu bar.
  controls?: UIControl[];
  menus?: MenuNode[];
}
//=============================================================================
type PushFn = (channel: string, payload: unknown) => void;
const BUILTINS = [
  'disp',
  'display',
  'eye',
  'zeros',
  'ones',
  'rand',
  'magic',
  'linspace',
  'plot',
  'surf',
  'bar',
  'sin',
  'sind',
  'sinh',
  'cos',
  'size',
  'length',
  'sum',
  'max',
  'min',
  'clear',
  'clc',
  'who',
  'whos',
  'pi',
  'inv',
];
//=============================================================================
export class MockEngine {
  private preferences: Record<string, string> = {};
  private vars = new Map<string, MockVar>();
  private history: string[] = ['A = eye(3)', 'x = linspace(0, 6.28, 50)', 'plot(x)'];
  private figures: MockFigure[] = [];
  // Last view pushed by a rotation; axesHandle is the targeted subplot (0 = whole
  // figure).
  lastView: { azimuth: number; elevation: number; axesHandle: number } | null = null;
  private figureViews = new Map<number, [number, number]>();
  private figureSizes = new Map<number, [number, number]>();
  private files = new Map<string, string>([
    ['/workspace/hello.m', 'function hello()\n  disp("Hello from Nelson");\nend\n'],
    ['/workspace/demo.m', 'x = linspace(0, 2*pi, 100);\ny = sin(x);\nplot(x, y);\n'],
    ['/workspace/notes.txt', 'Desktop mock file used to verify extension filters.\n'],
    // Seed for the code-analyzer test: line 2 has a `bug` token (error), line 3
    // is over 80 chars (warning).
    [
      'lint.m',
      'x = 1;\ny = bug + 2;\nzzzz = 123456789012345678901234567890123456789012345678901234567890123456789012345;\n',
    ],
    [
      'outline.m',
      'function y = myfun(x)\n  y = x + 1;\nend\n%% Section A\ndisp(1)\n%% Section B\ndisp(2)\n',
    ],
  ]);
  //=============================================================================
  private directories = new Set<string>(['/workspace']);
  private breakpoints = new Map<string, Set<number>>();
  private profileOn = false;
  private debug: {
    running: boolean;
    currentFile: string | null;
    currentLine: number | null;
    stack: { file: string; line: number; function: string }[];
  } | null = null;
  //=============================================================================
  private nextFigure = 0;
  private nextProgressDialog = 1;
  private progressDialogs = new Map<string, ProgressDialogPush>();
  private expandId = 0;
  private cwd = '/workspace';
  private recentDirectories: string[] = [];
  private dialogDirectories = new Map<string, string>();
  constructor(private push: PushFn) {
    this.vars.set('A', { name: 'A', className: 'double', isChar: false, data: identity(3) });
    // A logical, to exercise the true/false rendering.
    this.vars.set('L', {
      name: 'L',
      className: 'logical',
      isChar: false,
      data: [
        [1, 0],
        [0, 1],
      ],
    });
  }
  //=============================================================================
  async handle(method: string, params: Record<string, unknown>): Promise<unknown> {
    switch (method) {
      case 'ping':
        return { pong: true, backend: 'mock', version: '0.0.1' } satisfies PingResult;
      case 'app.heartbeat':
        return { ok: true };
      case 'app.capabilities':
        return { nflow: true, packageManager: true, launchMode: 'none' };
      case 'diagnostics.info':
        return {
          backend: { name: 'mock', version: '0.0.1', launchMode: 'none' },
          runtime: { name: 'mock', profile: 'development' },
          delivery: { files: 0, totalBytes: 0, startupBytes: 0, onDemandBytes: 0 },
          capabilities: {
            modules: ['core', 'graphics', 'nflow_gui', 'nmm_gui'],
            capabilities: ['plots', 'nflow', 'package-manager-core'],
            features: { nflow: true, packageManager: true, plots: true },
            limits: {},
            unavailable: [],
          },
          persistence: { commandHistory: false, preferencesPath: '', workspacePath: '/workspace' },
          browser: {
            userAgent: navigator.userAgent,
            platform: navigator.userAgentData?.platform ?? navigator.platform,
            features: {},
            unsupported: [],
            notes: [],
          },
        };
      case 'client.role':
        // The mock is always the single desktop page (no native figure windows).
        // launchMode 'none' keeps dev/b2b figures docked.
        return { role: 'desktop', launchMode: 'none' };
      case 'prefs.load':
        return { schema: 1, values: { ...this.preferences }, path: '' };
      case 'prefs.save':
        this.preferences = { ...(params.values as Record<string, string>) };
        return { ok: true };

      case 'term.input':
        return this.evalLine(String(params.line ?? ''));
      case 'term.interrupt':
        this.reprompt();
        return { interrupted: true };
      case 'term.resize':
        return { ok: true };
      case 'term.sync':
        queueMicrotask(() => this.reprompt());
        return { ok: true };

      case 'notebook.run':
        return this.notebookRun(String(params.code ?? ''));

      case 'completion.request':
        return this.complete(String(params.line ?? ''));

      case 'workspace.list':
        return this.workspaceList();

      case 'history.list':
        return { entries: this.history.slice() } satisfies HistoryListResult;
      case 'newsfeed.list':
        return {
          feed: {
            title: 'Sample feed (mock)',
            link: 'https://example.org/',
            url: String(params?.url ?? ''),
            items: [
              {
                title: 'First article',
                link: 'https://example.org/first',
                date: 'Mon, 01 Sep 2025 08:00:00 GMT',
                summary: 'Body of the first article.',
                image: '',
              },
              {
                title: 'Second article',
                link: 'https://example.org/second',
                date: 'Tue, 02 Sep 2025 09:30:00 GMT',
                summary: 'Second summary.',
                image: '',
              },
            ],
          },
        };
      case 'tool.dispatch': {
        // Package manager panel (nmm web tool) without the C++ engine.
        const method = String(params?.method ?? '');
        if (method === 'i18n') return { ok: true, locale: 'en_US', en_US: '{}', strings: '' };
        if (method === 'snapshot') {
          return {
            ok: true,
            arch: 'mock',
            registry: null,
            list: {
              ok: true,
              installed: [
                {
                  name: 'sample_toolbox',
                  version: '1.2.0',
                  summary: 'A sample installed package (mock).',
                  installed: { version: '1.2.0', path: '/workspace/sample_toolbox' },
                },
              ],
            },
            search: { ok: true, packages: [] },
          };
        }
        if (method === 'updates') return { count: 0 };
        if (method === 'prefs' || method === 'pendingDeepLink' || method === 'savePrefs') return {};
        return { ok: false, error: `mock engine: ${method} is not available` };
      }
      case 'examples.list':
        return {
          locale: 'en_US',
          diagnostics: [],
          groups: [
            {
              name: 'graphics',
              title: 'graphics',
              source: 'nelson',
              examples: [
                {
                  title: 'Sine wave',
                  description: 'Plot a sine wave (mock).',
                  kind: 'script',
                  file: '/workspace/demo.m',
                  tags: ['plot'],
                  requirements: [],
                },
                {
                  title: 'Animate',
                  description: 'Stream a few frames (mock).',
                  kind: 'command',
                  command: 'animate_demo',
                  tags: [],
                  requirements: ['gui'],
                },
              ],
            },
            {
              name: 'nflow:causal',
              title: 'Causal',
              source: 'nflow',
              examples: [
                {
                  title: 'PID',
                  description: 'A PID loop (mock).',
                  kind: 'model',
                  file: '/workspace/PID_Demo.nflow',
                  tags: ['causal'],
                  requirements: [],
                  blockCount: 6,
                },
              ],
            },
          ],
        };
      case 'history.clear':
        this.history = [];
        this.push('history.changed', {});
        return { ok: true };

      case 'fs.list':
        return this.fsList(String(params.path || this.cwd));
      case 'fs.cwd':
        this.recordRecentDirectory(this.cwd);
        return {
          cwd: this.cwd,
          separator: /^[A-Za-z]:\\/.test(this.cwd) || this.cwd.startsWith('\\') ? '\\' : '/',
          recentDirectories: this.recentDirectories.slice(),
        };
      case 'fs.chdir': {
        const path = String(params.path ?? '').trim();
        if (!/^(?:[A-Za-z]:[\\/]|[\\/]{1,2})/.test(path)) {
          throw new Error('directory does not exist');
        }
        const separator = /^[A-Za-z]:\\/.test(path) || path.startsWith('\\') ? '\\' : '/';
        this.cwd = path.replace(/[\\/]+$/, '') || separator;
        this.recordRecentDirectory(this.cwd);
        return { cwd: this.cwd, separator, recentDirectories: this.recentDirectories.slice() };
      }
      case 'fs.dialog.start': {
        const key = String(params.historyKey ?? '');
        const explicit = String(params.defaultPath ?? '');
        return {
          directory:
            explicit && explicit !== '.' ? explicit : this.dialogDirectories.get(key) || this.cwd,
        };
      }
      case 'fs.dialog.native':
        return { available: false, reason: 'not_webview' };
      case 'fs.dialog.accept': {
        const key = String(params.historyKey ?? '');
        const mode = String(params.mode ?? 'open');
        const path = String(params.path ?? '');
        const slash = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
        const directory = mode === 'dir' ? path : slash > 0 ? path.slice(0, slash) : '';
        if (key && directory) this.dialogDirectories.set(key, directory);
        return { ok: Boolean(key && directory) };
      }
      case 'path.list':
        return { paths: ['/workspace', '/usr/lib/nelson/functions', '/usr/lib/nelson/modules'] };
      case 'profile.info':
        return {
          on: this.profileOn,
          rows: [
            { function: 'myfun', file: 'myfun.m', line: 3, calls: 10, total: 0.42, perCall: 0.042 },
            { function: 'sin', file: '', line: 0, calls: 100, total: 0.01, perCall: 0.0001 },
          ],
        };
      case 'profile.control': {
        const action = String(params.action ?? '');
        if (action === 'on') this.profileOn = true;
        else if (action === 'off') this.profileOn = false;
        else if (action !== 'clear') throw new Error('invalid profiler action');
        return { ok: true, on: this.profileOn };
      }
      case 'profile.export': {
        if (this.profileOn) throw new Error('profiler is running');
        const path = String(params.path ?? '/workspace/profile_result');
        const index = `${path}/index.html`;
        return { ok: true, path, index, url: `file://${index}`, on: this.profileOn };
      }
      case 'help.text': {
        const name = String(params.name ?? '');
        return {
          name,
          text: name
            ? `${name}\n\n  ${name} - (mock) documentation for ${name}.\n\n  Syntax:\n    y = ${name}(x)\n\n  See also: cos, tan.\n`
            : 'Type a function name to see its documentation.',
        };
      }
      case 'fs.upload': {
        // Record the last upload so the e2e can observe the round-trip.
        const path = String(params.path ?? '');
        this.files.set(path, String(params.data ?? ''));
        (window as unknown as { __mockLastUpload?: unknown }).__mockLastUpload = {
          path,
          size: String(params.data ?? '').length,
        };
        return { ok: true, path, size: String(params.data ?? '').length };
      }
      case 'fs.create':
        return this.fsCreate(params);
      case 'fs.download': {
        const path = String(params.path ?? '');
        const data =
          this.files.get(path) ??
          this.files.get(this.workspacePath(path)) ??
          btoa('mock file contents');
        return { ok: true, name: path.split('/').pop() ?? 'file', data };
      }

      case 'var.open':
        return this.varOpen(String(params.name ?? ''));
      case 'var.getBlock':
        return this.varBlock(params as unknown as VarBlockParams);
      case 'var.setCell':
        return this.varSetCell(params as unknown as VarSetCellParams);
      case 'var.setNested':
        return this.varSetNested(
          String(params.name ?? ''),
          String(params.kind ?? ''),
          Number(params.row),
          Number(params.col),
          String(params.value ?? '')
        );
      case 'var.replace':
        return this.varReplace(
          params as unknown as { name: string; values: (number | string)[][] }
        );

      case 'figure.action': {
        const handle = Number(params.handle);
        const figure = this.figures.find((entry) => entry.handle === handle);
        if (!figure) throw new Error('Figure is unavailable');
        const request = params.request as Record<string, unknown>;
        const result = mockFigureAction(
          figure,
          request,
          (await this.handle('figure.getDisplayList', { handle })) as FigureDisplayListResult
        );
        if ('displayList' in result && result.displayList?.view)
          this.figureViews.set(handle, result.displayList.view);
        return result;
      }
      case 'figure.list':
        return {
          figures: this.figures.map((f) => ({
            handle: f.handle,
            title: f.title,
            windowStyle: 'normal' as const,
          })),
        } satisfies FigureListResult;
      case 'figure.close': {
        const handle = Number(params.handle);
        const index = this.figures.findIndex((figure) => figure.handle === handle);
        if (index < 0) throw new Error(`figure ${handle} not available`);
        this.figures.splice(index, 1);
        this.push('figure.closed', { handle });
        return { ok: true, handle };
      }
      case 'figure.getImage':
        return this.figureImage(Number(params.handle));
      case 'figure.getDisplayList':
        return this.figureDisplayList(Number(params.handle));
      case 'uihtml.pull':
        // No uihtml component exists in the mock figures, so the honest answer
        // is an empty page state rather than a fabricated one.
        return { handle: Number(params.handle), dataJson: '[]', revision: 0, events: [] };
      case 'uicontrol.action': {
        const handle = Number(params.handle);
        const value = Number(params.value ?? 0);
        const text = String(params.text ?? '');
        // The mock records the action (for the e2e suite) and keeps the control
        // state so a refresh round-trips it.
        (window as unknown as { __mockLastAction?: unknown }).__mockLastAction = {
          handle,
          value,
          text,
        };
        for (const f of this.figures) {
          const c = f.controls?.find((x) => x.handle === handle);
          if (c) {
            if (
              c.style === 'checkbox' ||
              c.style === 'radiobutton' ||
              c.style === 'slider' ||
              c.style === 'popupmenu' ||
              c.style === 'listbox' ||
              c.style === 'togglebutton'
            ) {
              c.value = value;
            } else if (c.style === 'edit') {
              c.text = text;
            }
            this.push('figure.updated', { handle: f.handle });
          }
        }
        return { ok: true, handle };
      }
      case 'figure.event':
        return this.figureEvent(params);
      case 'figure.keyEvent':
        if (typeof window !== 'undefined') {
          (window as unknown as { __mockLastFigureKeyEvent?: unknown }).__mockLastFigureKeyEvent =
            params;
        }
        return { ok: this.figures.some((figure) => figure.handle === Number(params.handle)) };
      case 'figure.mouseEvent':
        if (typeof window !== 'undefined') {
          (
            window as unknown as { __mockLastFigureMouseEvent?: unknown }
          ).__mockLastFigureMouseEvent = params;
        }
        return {
          ok: this.figures.some((figure) => figure.handle === Number(params.handle)),
          motion: false,
        };
      case 'figure.setSize': {
        this.figureSizes.set(Number(params.handle), [Number(params.width), Number(params.height)]);
        return this.figureDisplayList(Number(params.handle));
      }
      case 'figure.setWindowSize':
        // No native window to resize in the mock backend.
        return { ok: true };
      case 'figure.setView': {
        const figure = this.figures.find((entry) => entry.handle === Number(params.handle));
        if (figure)
          mockFigureAction(figure, { action: 'capture' }, this.figureDisplayList(figure.handle));
        // Record the rotation so the mock axes reflect it, and mirror it to
        // window so the e2e suite can observe the sync.
        const az = Number(params.azimuth);
        const el = Number(params.elevation);
        this.lastView = { azimuth: az, elevation: el, axesHandle: Number(params.axesHandle ?? 0) };
        this.figureViews.set(Number(params.handle), [az, el]);
        if (typeof window !== 'undefined') {
          (window as unknown as { __mockLastView?: unknown }).__mockLastView = this.lastView;
        }
        return this.figureDisplayList(Number(params.handle));
      }
      case 'figure.pan': {
        // Recorded so the e2e suite can observe it.
        if (typeof window !== 'undefined') {
          (window as unknown as { __mockLastPan?: unknown }).__mockLastPan = {
            dxFrac: Number(params.dxFrac),
            dyFrac: Number(params.dyFrac),
          };
        }
        return this.figureDisplayList(Number(params.handle));
      }

      case 'editor.open':
        return this.editorOpen(String(params.path ?? ''));
      case 'editor.save':
        this.files.set(this.workspacePath(String(params.path)), String(params.content ?? ''));
        return { ok: true };
      case 'code.analyze':
        return this.codeAnalyze(String(params.path ?? ''), String(params.source ?? ''));
      case 'debugger.getBreakpoints':
        return this.getBreakpoints(String(params.file ?? ''));
      case 'debugger.toggleBreakpoint':
        return this.toggleBreakpoint(String(params.file ?? ''), Number(params.line));
      case 'debugger.state':
        return (
          this.debug ??
          ({
            running: false,
            currentLine: null,
            currentFile: null,
            stack: [],
          } satisfies DebuggerState)
        );

      case 'dialog.reply':
        return this.dialogReply(params);

      case 'shell.run': {
        const command = String(params.command ?? '');
        const cwd = String(params.cwd || this.cwd);
        if (command.trim() === '') return { output: '', cwd, exitCode: 0 };
        return { output: `[mock] ${command}\n`, cwd, exitCode: 0 };
      }

      default:
        throw new Error(`unknown method "${method}" (mock backend)`);
    }
  }
  //=============================================================================
  // Runs a whole cell, capturing textual output and any figures it creates
  // instead of streaming to the console.
  private notebookRun(code: string): {
    ok: boolean;
    output: string;
    errorId: string;
    errorMessage: string;
    figures: FigureImageResult[];
  } {
    const buf: string[] = [];
    const createdFigures: number[] = [];
    const realPush = this.push;
    (this as unknown as { push: PushFn }).push = (ch: string, payload: unknown) => {
      const p = payload as { text?: string; handle?: number };
      if (ch === 'term.output') buf.push(String(p?.text ?? ''));
      else if (ch === 'figure.created') createdFigures.push(Number(p?.handle));
      else if (ch === 'workspace.changed' || ch === 'history.changed') realPush(ch, payload);
      // prompt / expandable / clear are swallowed during capture
    };
    let ok = true;
    let errorMessage = '';
    try {
      for (const raw of code.split('\n')) {
        const t = raw.trim();
        if (t && !t.startsWith('%')) this.execute(t);
      }
    } catch (err) {
      ok = false;
      errorMessage = (err as Error).message;
    }
    (this as unknown as { push: PushFn }).push = realPush;
    const figures = createdFigures.map((h) => this.figureImage(h));
    // Drop the figures the cell created so they do not also open as desktop
    // windows.
    for (const h of createdFigures) {
      const idx = this.figures.findIndex((f) => f.handle === h);
      if (idx >= 0) this.figures.splice(idx, 1);
      realPush('figure.closed', { handle: h });
    }
    return {
      ok,
      output: stripAnsi(buf.join('')).replace(/\r\n/g, '\n').replace(/\s+$/, ''),
      errorId: ok ? '' : 'Nelson:Notebook:error',
      errorMessage,
      figures,
    };
  }
  //=============================================================================
  private evalLine(line: string): { accepted: boolean } {
    const trimmed = line.trim();
    queueMicrotask(() => {
      if (trimmed.length === 0) {
        this.reprompt();
        return;
      }
      this.history.push(line);
      this.push('history.changed', {});
      try {
        this.execute(trimmed);
      } catch (err) {
        this.out(`\x1b[31m${(err as Error).message}\x1b[0m\r\n`);
      }
      // A modal dialog blocks the interpreter thread: defer the prompt until
      // dialog.reply lands (see dialogReply).
      if (!this.dialogPending) this.reprompt();
    });
    return { accepted: true };
  }
  //=============================================================================
  private dialogPending = false;
  private dialogSeq = 0;
  private showDialog(req: Omit<DialogShow, 'id'>): void {
    this.dialogPending = true;
    this.push('dialog.show', { id: ++this.dialogSeq, ...req });
  }
  //=============================================================================
  private dialogReply(params: Record<string, unknown>): { ok: boolean } {
    this.dialogPending = false;
    const ok = Boolean(params.ok);
    if (!ok) {
      this.out('\x1b[90m(cancelled)\x1b[0m\r\n');
    } else if (typeof params.path === 'string' && params.path) {
      this.out(`ans = '${params.path}'\r\n`);
    } else if (Array.isArray(params.selectedIndices)) {
      this.out(`ans = [${(params.selectedIndices as number[]).join(' ')}]\r\n`);
    } else if (Array.isArray(params.values)) {
      this.out(`ans = '${(params.values as string[]).join(', ')}'\r\n`);
    } else if (typeof params.button === 'string' && params.button) {
      this.out(`ans = '${params.button}'\r\n`);
    } else {
      this.out('ans = ok\r\n');
    }
    this.reprompt();
    return { ok: true };
  }
  //=============================================================================
  private execute(cmd: string): void {
    // Track profiler on/off so profile.info can report it.
    if (/\bprofile\s+on\b/.test(cmd) || /profile\(["']on["']\)/.test(cmd)) this.profileOn = true;
    if (/\bprofile\s+off\b/.test(cmd) || /profile\(["']off["']\)/.test(cmd)) this.profileOn = false;
    if (cmd === 'clc') {
      this.push('term.clear', {});
      return;
    }
    if (cmd === 'clear') {
      this.vars.clear();
      this.push('workspace.changed', {});
      return;
    }
    const suppress = cmd.endsWith(';');
    const body = suppress ? cmd.slice(0, -1).trim() : cmd;
    const changeDirectory = body.match(/^cd\(\s*['"](.+)['"]\s*\)$/i);
    if (changeDirectory) {
      const path = changeDirectory[1];
      if (!/^(?:[A-Za-z]:[\\/]|[\\/]{1,2})/.test(path)) {
        throw new Error('directory does not exist');
      }
      const separator = /^[A-Za-z]:\\/.test(path) || path.startsWith('\\') ? '\\' : '/';
      this.cwd = path.replace(/[\\/]+$/, '') || separator;
      this.recordRecentDirectory(this.cwd);
      return;
    }

    const progressCreate = body.match(/^(?:([A-Za-z_]\w*)\s*=\s*)?uiprogressdlg\s*(?:\((.*)\))?$/i);
    if (progressCreate) {
      const name = progressCreate[1] || 'ans';
      const args = progressCreate[2] || '';
      const title = stringOption(args, 'Title', 'Progress');
      const message = stringOption(args, 'Message', '');
      const value = numberOption(args, 'Value', 0) * 100;
      const indeterminate = logicalOption(args, 'Indeterminate', false);
      const progress: ProgressDialogPush = {
        id: this.nextProgressDialog++,
        visible: true,
        title,
        message,
        value,
        minimum: 0,
        maximum: indeterminate ? 0 : 100,
        indeterminate,
      };
      this.progressDialogs.set(name, progress);
      this.push('dialog.progress', progress);
      return;
    }

    const progressSet = body.match(
      /^set\(\s*([A-Za-z_]\w*)\s*,\s*['"]([^'"]+)['"]\s*,\s*(.*?)\s*\)$/i
    );
    if (progressSet) {
      const progress = this.progressDialogs.get(progressSet[1]);
      if (progress) {
        const property = progressSet[2].toLowerCase();
        const rawValue = progressSet[3];
        if (property === 'windowtitle') progress.title = scalarText(rawValue);
        else if (property === 'labeltext') progress.message = scalarText(rawValue);
        else if (property === 'value') progress.value = Number(rawValue);
        else if (property === 'minimum') progress.minimum = Number(rawValue);
        else if (property === 'maximum') progress.maximum = Number(rawValue);
        else if (property === 'visible') progress.visible = logicalScalar(rawValue);
        progress.indeterminate = progress.minimum === 0 && progress.maximum === 0;
        this.push('dialog.progress', { ...progress });
        return;
      }
    }

    const progressDelete = body.match(/^delete\(\s*([A-Za-z_]\w*)\s*\)$/i);
    if (progressDelete) {
      const progress = this.progressDialogs.get(progressDelete[1]);
      if (progress) {
        this.push('dialog.progress', { ...progress, visible: false });
        this.progressDialogs.delete(progressDelete[1]);
        return;
      }
    }

    // X = surf/plot/bar(...) shows an object display with a clickable "Show all
    // properties" (OSC 8 hyperlink). The mock skips creating the figure so the
    // Command Window keeps focus; the un-assigned form below still creates it.
    const asgFig = body.match(/^([A-Za-z_]\w*)\s*=\s*(surf|plot|bar)\s*\((.*)\)$/);
    if (asgFig) {
      const name = asgFig[1];
      this.vars.set(name, {
        name,
        className: 'surface',
        isChar: false,
        data: [[0]],
        props: SURFACE_PROPS,
      });
      this.push('workspace.changed', {});
      if (!suppress) this.emitObjectDisplay(name);
      return;
    }

    const plotMatch = body.match(/^(plot|surf|bar)\s*\((.*)\)$/);
    if (plotMatch) {
      this.createFigure(plotMatch[1] as MockFigure['kind']);
      return;
    }
    // Bare `figure` opens an empty figure (File > New > New Figure).
    if (/^figure\s*(\(\s*\))?$/.test(body)) {
      this.createFigure('plot');
      return;
    }

    // inputdlg / uialert / uiconfirm: GUI dialog served by DialogHost over
    // dialog.show / dialog.reply. The reprompt is blocked until the browser
    // answers, like the real interpreter thread.
    if (body === 'inputdlg' || body.startsWith('inputdlg')) {
      this.showDialog({
        kind: 'input',
        title: 'Input',
        message: '',
        icon: 'info',
        fields: [{ prompt: 'Name:', value: '', lines: 1 }],
        buttons: [],
        defaultButton: '',
      });
      return;
    }
    if (body === 'uialert' || body.startsWith('uialert')) {
      this.showDialog({
        kind: 'message',
        title: 'Alert',
        message: 'Something happened.',
        icon: 'warning',
        fields: [],
        buttons: [],
        defaultButton: '',
      });
      return;
    }
    // msgbox / errordlg / warndlg / helpdlg → fire-and-forget message modal.
    const msgMatch = body.match(/^(msgbox|errordlg|warndlg|helpdlg)\b/);
    if (msgMatch) {
      const icon =
        msgMatch[1] === 'errordlg'
          ? 'error'
          : msgMatch[1] === 'warndlg'
          ? 'warn'
          : msgMatch[1] === 'helpdlg'
          ? 'help'
          : 'none';
      this.push('dialog.message', {
        title: msgMatch[1] === 'msgbox' ? 'Message' : `${msgMatch[1]}`,
        message: 'This is a message.',
        icon,
      });
      return;
    }
    if (body === 'uisetfont' || body.startsWith('uisetfont')) {
      this.showDialog({
        kind: 'font',
        title: 'Select a font',
        message: '',
        icon: 'info',
        fields: [],
        buttons: [],
        defaultButton: '',
        fontFamily: 'Consolas',
        fontSize: 12,
        fontBold: false,
        fontItalic: false,
      });
      return;
    }
    if (body === 'uisetcolor' || body.startsWith('uisetcolor')) {
      this.showDialog({
        kind: 'color',
        title: 'Select a color',
        message: '',
        icon: 'info',
        fields: [],
        buttons: [],
        defaultButton: '',
        defaultPath: '#3366cc',
      });
      return;
    }
    if (body === 'uigetfile' || body.startsWith('uigetfile')) {
      this.showDialog({
        kind: 'file',
        title: 'Open File',
        message: '',
        icon: 'info',
        fields: [],
        buttons: [],
        defaultButton: '',
        fileMode: 'open',
        defaultPath: this.cwd,
      });
      return;
    }
    if (body === 'listdlg' || body.startsWith('listdlg')) {
      this.showDialog({
        kind: 'list',
        title: 'Select',
        message: 'Pick one or more:',
        icon: 'info',
        fields: [],
        buttons: [],
        defaultButton: '',
        items: ['Alpha', 'Beta', 'Gamma'],
        multiSelect: true,
        initialSelection: [1],
      });
      return;
    }
    if (body === 'uiconfirm' || body.startsWith('uiconfirm')) {
      this.showDialog({
        kind: 'question',
        title: 'Confirm',
        message: 'Proceed?',
        icon: 'question',
        fields: [],
        buttons: ['Yes', 'No'],
        defaultButton: 'Yes',
      });
      return;
    }

    // uidemo: a figure with controls and a menu bar.
    if (body === 'uidemo') {
      this.createUiDemoFigure();
      return;
    }

    // animate_demo: streams a few figure.frame pushes, as drawnow does.
    if (body === 'animate_demo') {
      this.createAnimateDemo();
      return;
    }

    // struct_demo: seeds a scalar struct for the read-only struct view.
    if (body === 'struct_demo') {
      this.vars.set('st', {
        name: 'st',
        className: 'struct',
        isChar: false,
        data: [[0]],
        structArr: {
          fields: ['name', 'value', 'data'],
          rows: [["'Nelson'", '42', '1x3 double']],
        },
      });
      this.push('workspace.changed', {});
      return;
    }

    // table_demo → seed a table so the read-only table view is exercisable.
    if (body === 'table_demo') {
      this.vars.set('tb', {
        name: 'tb',
        className: 'table',
        isChar: false,
        data: [[0]],
        tableGrid: {
          columns: ['Age', 'Name'],
          rows: [
            ['30', 'Alice'],
            ['25', 'Bob'],
            ['42', 'Carol'],
          ],
        },
      });
      this.push('workspace.changed', {});
      return;
    }

    // fh_demo → seed a function handle so the read-only text view is testable.
    if (body === 'fh_demo') {
      this.vars.set('fh', {
        name: 'fh',
        className: 'function_handle',
        isChar: false,
        data: [[0]],
        textValue: '@(x) x + 1',
      });
      this.push('workspace.changed', {});
      return;
    }

    // structarr_demo: seeds a 3-element struct array for the elements x fields grid.
    if (body === 'structarr_demo') {
      this.vars.set('da', {
        name: 'da',
        className: 'struct',
        isChar: false,
        data: [[0]],
        structArr: {
          fields: ['name', 'isdir'],
          rows: [
            ["'.git'", 'true'],
            ["'src'", 'true'],
            ["'readme.md'", 'false'],
          ],
        },
      });
      this.push('workspace.changed', {});
      return;
    }

    // str_demo: seeds a 2x2 string array for the variable editor's string path.
    if (body === 'str_demo') {
      this.vars.set('sa', {
        name: 'sa',
        className: 'string',
        isChar: false,
        data: [[0]],
        strData: [
          ['a', '<missing>'],
          ['c', 'd'],
        ],
      });
      this.push('workspace.changed', {});
      return;
    }

    // dbg_demo: simulates a debug pause at demo.m (K>> prompt + debugger.state).
    if (body === 'dbg_demo') {
      this.debug = {
        running: true,
        currentFile: '/workspace/demo.m',
        currentLine: 2,
        stack: [{ file: '/workspace/demo.m', line: 2, function: 'demo' }],
      };
      this.out('\x1b[33mBreakpoint hit at demo.m line 2\x1b[0m\r\n');
      return;
    }
    if (this.debug && (body === 'dbstep' || body === 'dbstep in' || body === 'dbstep out')) {
      this.debug.currentLine = (this.debug.currentLine ?? 1) + 1;
      this.debug.stack[0].line = this.debug.currentLine;
      return;
    }
    if (this.debug && (body === 'dbcont' || body === 'dbquit')) {
      this.debug = null;
      return;
    }

    // run('file') / run file (the editor's Run button): the mock only echoes a
    // marker so the round trip is observable.
    const runMatch = body.match(/^run\s*(?:\(\s*['"](.+?)['"]\s*\)|\s+['"]?(.+?)['"]?)$/);
    if (runMatch) {
      this.out(`[run] ${(runMatch[1] ?? runMatch[2] ?? '').trim()}\r\n`);
      return;
    }

    // editor / edit [(] 'file' [)]
    const edMatch = body.match(/^(?:editor|edit)\s*(?:\((.*)\)|\s+(\S.*))?$/);
    if (edMatch) {
      const path = (edMatch[1] ?? edMatch[2] ?? '').trim().replace(/^['"]|['"]$/g, '');
      this.push('editor.show', { path });
      return;
    }

    // assignment: name = expr
    const asg = body.match(/^([A-Za-z_]\w*)\s*=\s*(.+)$/);
    if (asg) {
      const v = this.evalExpr(asg[2]);
      v.name = asg[1];
      this.vars.set(asg[1], v);
      this.push('workspace.changed', {});
      if (!suppress) this.displayVar(v);
      return;
    }

    if (/^[A-Za-z_]\w*$/.test(body)) {
      const v = this.vars.get(body);
      if (!v) throw new Error(`Undefined variable or function '${body}'.`);
      if (!suppress) this.displayVar(v);
      return;
    }

    const v = this.evalExpr(body);
    v.name = 'ans';
    this.vars.set('ans', v);
    this.push('workspace.changed', {});
    if (!suppress) this.displayVar(v);
  }
  //=============================================================================
  private evalExpr(expr: string): MockVar {
    const e = expr.trim();
    const str = e.match(/^["'](.*)["']$/);
    if (str)
      return {
        name: '',
        className: 'char',
        isChar: true,
        charValue: str[1],
        data: [[str[1].length]],
      };

    // simple scalar arithmetic: a op b (numbers/pi/scalar variables)
    const arith = e.match(/^([\w.]+)\s*([-+*/])\s*([\w.]+)$/);
    if (arith) {
      const val = (t: string): number | null => {
        if (t === 'pi') return Math.PI;
        const n = Number(t);
        if (!Number.isNaN(n)) return n;
        const v = this.vars.get(t);
        return v && !v.isChar && v.data.length === 1 && v.data[0].length === 1
          ? v.data[0][0]
          : null;
      };
      const a = val(arith[1]);
      const b = val(arith[3]);
      if (a !== null && b !== null) {
        const op = arith[2];
        const r = op === '+' ? a + b : op === '-' ? a - b : op === '*' ? a * b : a / b;
        return { name: '', className: 'double', isChar: false, data: [[round4(r)]] };
      }
    }

    const fn = e.match(/^([A-Za-z_]\w*)\s*\((.*)\)$/);
    if (fn) return this.callFn(fn[1], fn[2]);

    if (this.vars.has(e)) {
      const src = this.vars.get(e)!;
      return { ...src, name: '' };
    }

    const num = Number(e);
    if (!Number.isNaN(num)) return { name: '', className: 'double', isChar: false, data: [[num]] };

    if (e === 'pi') return { name: '', className: 'double', isChar: false, data: [[Math.PI]] };

    throw new Error(`Cannot evaluate expression: ${expr}`);
  }
  //=============================================================================
  private callFn(name: string, argStr: string): MockVar {
    const args = argStr
      .split(',')
      .map((a) => a.trim())
      .filter((a) => a.length > 0);
    const n = (i: number, def = 3) => {
      const a = args[i];
      if (a === undefined) return def;
      const v = Number(a === 'pi' ? Math.PI : a);
      return Number.isNaN(v) ? def : Math.max(1, Math.floor(v));
    };
    const mk = (data: Matrix): MockVar => ({ name: '', className: 'double', isChar: false, data });
    switch (name) {
      case 'eye':
        return mk(identity(n(0)));
      case 'zeros':
        return mk(fill(n(0), n(1, n(0)), 0));
      case 'ones':
        return mk(fill(n(0), n(1, n(0)), 1));
      case 'rand':
        return mk(fill(n(0), n(1, n(0)), 0).map((row) => row.map(() => round4(Math.random()))));
      case 'magic':
        return mk(magic(n(0)));
      case 'linspace': {
        const a = Number(args[0] ?? 0);
        const b = Number(args[1] ?? 1);
        const count = n(2, 100);
        const step = count > 1 ? (b - a) / (count - 1) : 0;
        return mk([Array.from({ length: count }, (_, i) => round4(a + i * step))]);
      }
      case 'pi':
        return mk([[Math.PI]]);
      default:
        throw new Error(`Unknown function '${name}'.`);
    }
  }
  //=============================================================================
  private createFigure(kind: MockFigure['kind']): void {
    const handle = this.nextFigure++;
    const series = Array.from({ length: 60 }, (_, i) =>
      kind === 'bar' ? Math.abs(Math.sin(i / 6)) : Math.sin(i / 6)
    );
    this.figures.push({ handle, title: `Figure ${handle + 1}`, kind, series, rot: 0, zoom: 1 });
    this.push('figure.created', { handle, title: `Figure ${handle + 1}`, windowStyle: 'normal' });
  }
  //=============================================================================
  private createUiDemoFigure(): void {
    const handle = this.nextFigure++;
    const controls: UIControl[] = [
      {
        handle: 9001,
        style: 'pushbutton',
        text: 'Push me',
        units: 'pixels',
        x: 20,
        y: 20,
        w: 110,
        h: 30,
        value: 0,
        min: 0,
        max: 1,
        fontSize: 12,
        enabled: true,
        contextMenu: [
          { handle: 9101, text: 'Copy', hasCallback: true },
          { handle: 9102, text: 'Paste' },
        ],
      },
      {
        handle: 9002,
        style: 'checkbox',
        text: 'check me',
        units: 'pixels',
        x: 20,
        y: 62,
        w: 120,
        h: 22,
        value: 0,
        min: 0,
        max: 1,
        fontSize: 12,
        enabled: true,
      },
      {
        handle: 9003,
        style: 'slider',
        text: '',
        units: 'pixels',
        x: 20,
        y: 96,
        w: 160,
        h: 20,
        value: 40,
        min: 0,
        max: 100,
        fontSize: 12,
        enabled: true,
      },
      {
        handle: 9004,
        style: 'popupmenu',
        text: '',
        items: ['alpha', 'beta'],
        units: 'pixels',
        x: 20,
        y: 128,
        w: 120,
        h: 26,
        value: 1,
        min: 0,
        max: 1,
        fontSize: 12,
        enabled: true,
      },
    ];
    const menus: MenuNode[] = [
      {
        handle: 9200,
        text: 'Actions',
        children: [
          { handle: 9201, text: 'Do Thing', hasCallback: true },
          { handle: 9202, text: 'Sub', separator: true },
        ],
      },
    ];
    this.figures.push({
      handle,
      title: `Figure ${handle + 1}`,
      kind: 'plot',
      series: Array.from({ length: 30 }, (_, i) => Math.sin(i / 4)),
      rot: 0,
      zoom: 1,
      controls,
      menus,
    });
    this.push('figure.created', { handle, title: `Figure ${handle + 1}`, windowStyle: 'normal' });
  }
  //=============================================================================
  private createAnimateDemo(): void {
    const handle = this.nextFigure++;
    const base: MockFigure = {
      handle,
      title: `Figure ${handle + 1}`,
      kind: 'plot',
      series: Array.from({ length: 30 }, (_, i) => Math.sin(i / 4)),
      rot: 0,
      zoom: 1,
      controls: [
        {
          handle: 9300,
          style: 'text',
          text: 'frame-0',
          units: 'pixels',
          x: 20,
          y: 20,
          w: 120,
          h: 20,
          value: 0,
          min: 0,
          max: 1,
          fontSize: 12,
          enabled: false,
        },
      ],
    };
    this.figures.push(base);
    this.push('figure.created', { handle, title: `Figure ${handle + 1}`, windowStyle: 'normal' });
    for (let frame = 1; frame <= 3; frame++) {
      setTimeout(() => {
        base.controls![0].text = `frame-${frame}`;
        this.push('figure.frame', { handle, displayList: this.figureDisplayList(handle) });
      }, 120 * frame);
    }
  }
  //=============================================================================
  private summary(v: MockVar): WorkspaceVarSummary {
    const rows = v.isChar ? 1 : v.data.length;
    const cols = v.isChar ? v.charValue?.length ?? 0 : v.data[0]?.length ?? 0;
    return {
      name: v.name,
      className: v.className,
      size: [rows, cols],
      bytes: v.isChar ? cols : rows * cols * 8,
      value: this.preview(v),
    };
  }
  //=============================================================================
  private preview(v: MockVar): string {
    if (v.isChar) return `'${v.charValue}'`;
    if (v.data.length === 1 && v.data[0].length === 1) return String(v.data[0][0]);
    return `${v.data.length}x${v.data[0]?.length ?? 0} ${v.className}`;
  }
  //=============================================================================
  private workspaceList(): WorkspaceListResult {
    return {
      scope: 'base',
      variables: [...this.vars.values()].map((v) => this.summary(v)),
    };
  }
  //=============================================================================
  private displayVar(v: MockVar): void {
    if (v.props) {
      this.emitObjectDisplay(v.name);
      return;
    }
    if (v.isChar) {
      this.out(`\r\n${v.name} =\r\n\r\n    '${v.charValue}'\r\n\r\n`);
      return;
    }
    const rows = v.data.map((r) => '   ' + r.map((c) => fmt(c)).join('   ')).join('\r\n');
    this.out(`\r\n${v.name} =\r\n\r\n${rows}\r\n\r\n`);
  }
  //=============================================================================
  // Handle display with a clickable (OSC 8) "Show all properties".
  private emitObjectDisplay(name: string): void {
    const v = this.vars.get(name);
    const props = v?.props ?? SURFACE_PROPS;
    const id = ++this.expandId;
    const rest =
      props
        .slice(3)
        .map((p) => `    ${p}: [49x49 double]`)
        .join('\r\n') + '\r\n';
    this.push('term.expandable', { id, text: rest });
    this.push('term.output', {
      text: `\r\n${name} =\r\n\r\n  Surface with properties:\r\n\r\n      Type: 'surface'\r\n    Parent: [1x1 axes]\r\n   Visible: on\r\n  `,
    });
    this.push('term.output', {
      text: `\x1b]8;;nelson-expand:${id}\x1b\\Show all properties\x1b]8;;\x1b\\\r\n`,
    });
  }
  //=============================================================================
  private out(text: string): void {
    this.push('term.output', { text });
  }
  //=============================================================================
  private reprompt(): void {
    this.push('term.prompt', { prompt: '>> ' });
  }
  //=============================================================================
  private complete(line: string): CompletionResult {
    const token = line.split(/[\s(),;=+\-*/]+/).pop() ?? '';
    const dot = token.match(/^([A-Za-z_]\w*)\.(\w*)$/);
    if (dot) {
      const obj = this.vars.get(dot[1]);
      if (obj?.props) {
        const p = dot[2];
        return {
          prefix: p,
          builtin: obj.props.filter((k) => k.startsWith(p)),
          macros: [],
          variables: [],
          files: [],
        };
      }
    }
    const variables = [...this.vars.keys()].filter((k) => k.startsWith(token));
    const builtin = token.length === 0 ? [] : BUILTINS.filter((b) => b.startsWith(token));
    return { prefix: token, builtin, macros: [], variables, files: [] };
  }
  //=============================================================================
  private recordRecentDirectory(path: string): void {
    this.recentDirectories = [
      path,
      ...this.recentDirectories.filter((item) => item !== path),
    ].slice(0, 10);
  }
  //=============================================================================
  private fsList(path: string): FsListResult {
    const dirs = new Set<string>();
    const entries: FsListResult['entries'] = [];
    for (const directory of this.directories) {
      if (directory === path || !directory.startsWith(path === '/' ? '/' : path + '/')) continue;
      const rest = directory.slice((path === '/' ? 0 : path.length) + 1);
      const slash = rest.indexOf('/');
      dirs.add(slash >= 0 ? rest.slice(0, slash) : rest);
    }
    for (const [p, content] of this.files) {
      if (!p.startsWith(path === '/' ? '/' : path + '/')) continue;
      const rest = p.slice((path === '/' ? 0 : path.length) + 1);
      const slash = rest.indexOf('/');
      if (slash >= 0) {
        dirs.add(rest.slice(0, slash));
      } else {
        entries.push({ name: rest, path: p, isDir: false, size: content.length });
      }
    }
    for (const d of dirs) entries.push({ name: d, path: `${path}/${d}`, isDir: true, size: 0 });
    entries.sort((a, b) => Number(b.isDir) - Number(a.isDir) || a.name.localeCompare(b.name));
    return { cwd: path, entries };
  }
  //=============================================================================
  private varOpen(name: string): VarOpenResult {
    const v = this.vars.get(name);
    if (!v) throw new Error(`Undefined variable '${name}'.`);
    if (v.isChar) {
      // Edited as a single string cell.
      return {
        name,
        className: 'char',
        rows: 1,
        cols: 1,
        columns: ['1'],
        editable: true,
        kind: 'char',
      };
    }
    if (v.strData) {
      const rows = v.strData.length;
      const cols = v.strData[0]?.length ?? 0;
      return {
        name,
        className: 'string',
        rows,
        cols,
        columns: Array.from({ length: cols }, (_, i) => String(i + 1)),
        editable: true,
        kind: 'string',
      };
    }
    if (v.structArr) {
      return {
        name,
        className: 'struct',
        rows: v.structArr.rows.length,
        cols: v.structArr.fields.length,
        columns: v.structArr.fields,
        editable: false,
        kind: 'struct',
      };
    }
    if (v.tableGrid) {
      return {
        name,
        className: 'table',
        rows: v.tableGrid.rows.length,
        cols: v.tableGrid.columns.length,
        columns: v.tableGrid.columns,
        editable: false,
        kind: 'table',
      };
    }
    if (v.textValue !== undefined) {
      return {
        name,
        className: v.className,
        rows: 1,
        cols: 1,
        columns: ['value'],
        editable: false,
        kind: 'text',
        text: v.textValue,
      };
    }
    const rows = v.data.length;
    const cols = v.data[0]?.length ?? 0;
    return {
      name,
      className: v.className,
      rows,
      cols,
      columns: Array.from({ length: cols }, (_, i) => String(i + 1)),
      editable: true,
      kind: 'matrix',
    };
  }
  //=============================================================================
  private varBlock(p: VarBlockParams): VarBlockResult {
    const v = this.vars.get(p.name);
    if (!v) throw new Error(`Undefined variable '${p.name}'.`);
    if (v.isChar) {
      return { row0: 0, col0: 0, cells: [[v.charValue ?? '']] };
    }
    if (v.structArr) {
      return {
        row0: 0,
        col0: 0,
        cells: v.structArr.rows.map((r) => r.map((x) => x as CellValue)),
      };
    }
    if (v.tableGrid) {
      return {
        row0: 0,
        col0: 0,
        cells: v.tableGrid.rows.map((r) => r.map((x) => x as CellValue)),
      };
    }
    if (v.textValue !== undefined) {
      return { row0: 0, col0: 0, cells: [[v.textValue]] };
    }
    if (v.strData) {
      const cells: CellValue[][] = [];
      for (let r = p.row0; r < p.row0 + p.rows && r < v.strData.length; r++) {
        const row: CellValue[] = [];
        for (let c = p.col0; c < p.col0 + p.cols && c < v.strData[r].length; c++) {
          row.push(v.strData[r][c]);
        }
        cells.push(row);
      }
      return { row0: p.row0, col0: p.col0, cells };
    }
    const cells: CellValue[][] = [];
    for (let r = p.row0; r < p.row0 + p.rows && r < v.data.length; r++) {
      const row: CellValue[] = [];
      for (let c = p.col0; c < p.col0 + p.cols && c < v.data[r].length; c++) {
        row.push(v.data[r][c]);
      }
      cells.push(row);
    }
    return { row0: p.row0, col0: p.col0, cells };
  }
  //=============================================================================
  private varSetNested(
    name: string,
    kind: string,
    row: number,
    col: number,
    value: string
  ): { ok: boolean } {
    const v = this.vars.get(name);
    if (!v) throw new Error(`Undefined variable '${name}'.`);
    if (kind === 'struct' && v.structArr) {
      v.structArr.rows[row][col] = value;
    } else {
      throw new Error(`'${name}' cell is not editable.`);
    }
    this.push('var.changed', { name });
    return { ok: true };
  }
  //=============================================================================
  private varSetCell(p: VarSetCellParams): { ok: boolean } {
    const v = this.vars.get(p.name);
    if (!v || v.isChar) throw new Error(`Cannot edit '${p.name}'.`);
    const num = Number(p.value);
    if (Number.isNaN(num)) throw new Error('Value must be numeric.');
    v.data[p.row][p.col] = num;
    this.push('workspace.changed', {});
    this.push('var.changed', { name: p.name });
    return { ok: true };
  }
  //=============================================================================
  private varReplace(p: { name: string; values: (number | string)[][] }): { ok: boolean } {
    const v = this.vars.get(p.name);
    if (!v) throw new Error(`Cannot edit '${p.name}'.`);
    if (v.isChar) {
      v.charValue = String(p.values[0]?.[0] ?? '');
    } else if (v.strData) {
      const cols = p.values.reduce((m, r) => Math.max(m, r.length), 0);
      v.strData = p.values.map((r) => {
        const row = r.map((x) => String(x ?? ''));
        while (row.length < cols) row.push('');
        return row;
      });
    } else {
      const cols = p.values.reduce((m, r) => Math.max(m, r.length), 0);
      v.data = p.values.map((r) => {
        const row = r.map((x) => (Number.isNaN(Number(x)) ? 0 : Number(x)));
        while (row.length < cols) row.push(0);
        return row;
      });
    }
    this.push('workspace.changed', {});
    this.push('var.changed', { name: p.name });
    return { ok: true };
  }
  //=============================================================================
  private figureImage(handle: number): FigureImageResult {
    const f = this.figures.find((x) => x.handle === handle);
    if (!f) throw new Error(`Figure ${handle} not found.`);
    const svg = primitivesToSvg(buildPrimitives(f), 640, 400);
    // URI-encoded (not base64) so Unicode glyphs in the SVG are safe.
    const image = `data:image/svg+xml;utf8,${encodeURIComponent(svg)}`;
    return { handle, image, width: 640, height: 400 };
  }
  //=============================================================================
  private figureDisplayList(handle: number): FigureDisplayListResult {
    const f = this.figures.find((x) => x.handle === handle);
    if (!f) throw new Error(`Figure ${handle} not found.`);
    const [w, h] = this.figureSizes.get(handle) ?? [640, 400];
    const res: FigureDisplayListResult = {
      handle,
      width: w,
      height: h,
      controlLayoutWidth: 640,
      controlLayoutHeight: 400,
      primitives: buildPrimitives(f),
    };
    // surf: a rotatable 3D axes reporting the current View. The mock keeps a
    // simple 2D trace instead of the projected surface, enough for the app and the
    // e2e suite.
    if (f.kind === 'surf') {
      res.is3d = true;
      res.view = this.figureViews.get(handle) ?? [-37.5, 30];
      // Single axes filling the figure; enough for the app to target it by handle.
      res.rotatableAxes = [{ handle: handle * 1000 + 1, rect: [0, 0, 1, 1], view: res.view }];
    }
    if (f.controls) {
      res.controls = f.controls;
    }
    if (f.menus) {
      res.menus = f.menus;
    }
    return res;
  }
  //=============================================================================
  private figureEvent(params: Record<string, unknown>): { ok: boolean } {
    const f = this.figures.find((x) => x.handle === Number(params.handle));
    if (!f) return { ok: false };
    if (params.type === 'mouse') f.rot += Number(params.dx ?? 0) * 0.5;
    if (params.type === 'wheel')
      f.zoom = Math.max(0.2, f.zoom * (Number(params.delta ?? 0) > 0 ? 0.9 : 1.1));
    this.push('figure.updated', { handle: f.handle });
    return { ok: true };
  }
  //=============================================================================
  private editorOpen(path: string): EditorOpenResult {
    const resolved = this.files.has(path) ? path : this.workspacePath(path);
    const content = this.files.get(resolved) ?? '';
    return { path, content, language: path.endsWith('.m') ? 'nelson' : 'plaintext' };
  }
  //=============================================================================
  private fsCreate(params: Record<string, unknown>): { ok: true; path: string } {
    const path = this.workspacePath(String(params.path ?? ''));
    if (params.kind === 'dir') {
      this.directories.add(path);
      this.push('fs.changed', { path });
      return { ok: true, path };
    }
    this.files.set(path, String(params.content ?? ''));
    this.push('fs.changed', { path });
    return { ok: true, path };
  }
  //=============================================================================
  private workspacePath(path: string): string {
    const normalized = path.replaceAll('\\', '/');
    return normalized.startsWith('/') ? normalized : `${this.cwd}/${normalized}`;
  }
  //=============================================================================
  // Stand-in for Nelson's CodeAnalyzer: flag lines over 80 chars (warning) and
  // any line containing the token `bug` (error). Enough to exercise the UI.
  private codeAnalyze(path: string, source: string): { diagnostics: unknown[] } {
    if (!/\.m$/i.test(path)) return { diagnostics: [] };
    const diagnostics: unknown[] = [];
    source.split('\n').forEach((text, i) => {
      const line = i + 1;
      if (text.length > 80) {
        diagnostics.push({
          severity: 'warning',
          message: 'Line exceeds 80 characters.',
          ruleName: 'line-too-long',
          lineStart: line,
          lineEnd: line,
          columnStart: 81,
          columnEnd: text.length + 1,
        });
      }
      const bug = text.indexOf('bug');
      if (bug >= 0) {
        diagnostics.push({
          severity: 'error',
          message: 'Suspicious token.',
          ruleName: 'suspicious-token',
          lineStart: line,
          lineEnd: line,
          columnStart: bug + 1,
          columnEnd: bug + 4,
          fixes: [
            {
              title: "Replace 'bug' with 'fixed'",
              safe: true,
              edits: [
                {
                  lineStart: line,
                  lineEnd: line,
                  columnStart: bug + 1,
                  columnEnd: bug + 4,
                  replacementText: 'fixed',
                },
              ],
            },
          ],
        });
      }
    });
    return { diagnostics };
  }
  //=============================================================================
  private getBreakpoints(file: string): BreakpointsResult {
    return { file, lines: [...(this.breakpoints.get(file) ?? [])].sort((a, b) => a - b) };
  }
  //=============================================================================
  private toggleBreakpoint(file: string, line: number): BreakpointsResult {
    let set = this.breakpoints.get(file);
    if (!set) {
      set = new Set();
      this.breakpoints.set(file, set);
    }
    if (set.has(line)) set.delete(line);
    else set.add(line);
    return { file, lines: [...set].sort((a, b) => a - b) };
  }
}
//=============================================================================
function scalarText(value: string): string {
  const trimmed = value.trim();
  const quote = trimmed[0];
  return (quote === "'" || quote === '"') && trimmed.endsWith(quote)
    ? trimmed.slice(1, -1)
    : trimmed;
}
//=============================================================================
function logicalScalar(value: string): boolean {
  const normalized = scalarText(value).toLowerCase();
  return normalized === 'true' || normalized === 'on' || normalized === '1';
}
//=============================================================================
function stringOption(args: string, name: string, fallback: string): string {
  const match = args.match(new RegExp(`["']${name}["']\\s*,\\s*(["'])(.*?)\\1`, 'i'));
  return match ? match[2] : fallback;
}
//=============================================================================
function numberOption(args: string, name: string, fallback: number): number {
  const match = args.match(new RegExp(`["']${name}["']\\s*,\\s*([-+]?\\d*\\.?\\d+)`, 'i'));
  return match ? Number(match[1]) : fallback;
}
//=============================================================================
function logicalOption(args: string, name: string, fallback: boolean): boolean {
  const match = args.match(new RegExp(`["']${name}["']\\s*,\\s*(true|false|on|off|1|0)`, 'i'));
  return match ? logicalScalar(match[1]) : fallback;
}
//=============================================================================
function identity(n: number): Matrix {
  return Array.from({ length: n }, (_, i) =>
    Array.from({ length: n }, (_, j) => (i === j ? 1 : 0))
  );
}
//=============================================================================
function fill(rows: number, cols: number, val: number): Matrix {
  return Array.from({ length: rows }, () => Array.from({ length: cols }, () => val));
}
//=============================================================================
function magic(n: number): Matrix {
  // Simple odd-order magic square; fall back to a filled grid for even n.
  if (n % 2 === 0) return fill(n, n, 0).map((row, i) => row.map((_, j) => i * n + j + 1));
  const m = fill(n, n, 0);
  let i = 0;
  let j = Math.floor(n / 2);
  for (let k = 1; k <= n * n; k++) {
    m[i][j] = k;
    const ni = (i - 1 + n) % n;
    const nj = (j + 1) % n;
    if (m[ni][nj] !== 0) i = (i + 1) % n;
    else {
      i = ni;
      j = nj;
    }
  }
  return m;
}
//=============================================================================
function round4(x: number): number {
  return Math.round(x * 1e4) / 1e4;
}
//=============================================================================
function stripAnsi(s: string): string {
  return (
    s
      // eslint-disable-next-line no-control-regex
      .replace(/\x1b\[[0-9;]*m/g, '')
      // eslint-disable-next-line no-control-regex
      .replace(/\x1b\]8;;[^\x07\x1b]*(?:\x07|\x1b\\)/g, '')
  );
}
//=============================================================================
function fmt(x: number): string {
  const s = Number.isInteger(x) ? String(x) : x.toFixed(4);
  return s.padStart(8);
}
//=============================================================================
// The raster (SVG image) path and the native canvas path consume this same draw
// list.
function buildPrimitives(f: MockFigure): DrawPrimitive[] {
  const W = 640;
  const H = 400;
  const pad = 40;
  const data = f.series;
  const n = data.length;
  const minY = Math.min(...data);
  const maxY = Math.max(...data);
  const spanY = maxY - minY || 1;
  const x = (i: number) => pad + (i / (n - 1)) * (W - 2 * pad);
  const y = (v: number) => H - pad - ((v - minY) / spanY) * (H - 2 * pad);
  const skew = Math.sin((f.rot * Math.PI) / 180) * 12; // rotation feedback
  const y0 = y(minY < 0 && maxY > 0 ? 0 : minY);

  const prims: DrawPrimitive[] = [
    { kind: 'rect', x: 0, y: 0, w: W, h: H, fill: '#ffffff' },
    { kind: 'line', x1: pad, y1: pad, x2: pad, y2: H - pad, stroke: '#cccccc', width: 1 },
    { kind: 'line', x1: pad, y1: H - pad, x2: W - pad, y2: H - pad, stroke: '#cccccc', width: 1 },
    { kind: 'line', x1: pad, y1: y0, x2: W - pad, y2: y0, stroke: '#999999', width: 1 },
  ];

  if (f.kind === 'bar') {
    const bw = (W - 2 * pad) / n - 2;
    for (let i = 0; i < n; i++) {
      const v = data[i];
      prims.push({
        kind: 'rect',
        x: x(i) - bw / 2,
        y: y(Math.max(0, v)),
        w: bw,
        h: Math.abs(y(v) - y(0)),
        fill: '#3daee9',
      });
    }
  } else {
    const points = data.map((v, i) => [x(i) + (i / n) * skew, y(v)] as [number, number]);
    prims.push({
      kind: 'polyline',
      count: points.length,
      pointsB64: encodePointPairsMock(points),
      stroke: f.kind === 'surf' ? '#e9963d' : '#3daee9',
      width: 2,
    });
  }

  prims.push({
    kind: 'text',
    x: W / 2,
    y: 22,
    text: `${f.title} - ${f.kind}`,
    align: 'center',
    color: '#333333',
    size: 14,
  });
  return prims;
}
//=============================================================================
function primitivesToSvg(prims: DrawPrimitive[], W: number, H: number): string {
  const esc = (s: string) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  const body = prims
    .map((p) => {
      switch (p.kind) {
        case 'rect':
          return `<rect x="${p.x}" y="${p.y}" width="${p.w}" height="${p.h}" fill="${p.fill}"/>`;
        case 'line':
          return `<line x1="${p.x1}" y1="${p.y1}" x2="${p.x2}" y2="${p.y2}" stroke="${p.stroke}" stroke-width="${p.width}"/>`;
        case 'polyline':
          return `<polyline points="${decodePointPairs(p.pointsB64, p.count)
            .map((pt) => pt.join(','))
            .join(' ')}" fill="none" stroke="${p.stroke}" stroke-width="${p.width}"/>`;
        case 'text': {
          const anchor = p.align === 'center' ? 'middle' : p.align === 'right' ? 'end' : 'start';
          return `<text x="${p.x}" y="${
            p.y
          }" text-anchor="${anchor}" font-family="sans-serif" font-size="${p.size}" fill="${
            p.color
          }">${esc(p.text)}</text>`;
        }
      }
    })
    .join('');
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}" viewBox="0 0 ${W} ${H}">${body}</svg>`;
}
//=============================================================================
