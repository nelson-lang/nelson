//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef, useState } from "react";
import type { KeyboardEvent } from "react";
import "./assistant-pane.css";
//=============================================================================
// Transport-agnostic chat pane for the Nelson AI assistant. It lives in the
// webview module, which both web front ends (web_gui and nflow_gui) already
// depend on, so neither depends on the other. The host injects a backend: the
// web desktop drives it over its own RPC, NFlow over its native bridge, so the
// same pane renders in every surface. The agent runs on the live interpreter
// and streams reset/token/activity/answer events back through backend.subscribe.
//=============================================================================
// Sentinel model value routing a prompt to the claude-agent-acp backend.
const ACP_MODEL = "claude-agent-acp";
//=============================================================================
export interface AssistantBackend {
  // Provider models offered besides the always-present ACP backend. May reject
  // (then only the ACP backend is offered).
  listModels(): Promise<string[]>;
  // Queue a prompt on the live interpreter. The answer arrives as an 'answer'
  // (or 'error') event, so this only confirms the run was accepted.
  sendPrompt(prompt: string, model: string): Promise<{ accepted?: boolean }>;
  // Subscribe to live assistant events (reset/token/activity/answer/error).
  // Returns an unsubscribe function.
  subscribe(
    handler: (channel: string, payloadText: string) => void,
  ): () => void;
  // Raise the interpreter interrupt so a long model call aborts.
  interrupt(): void;
  // Run a code snippet in the live session (the Run button under a code block).
  runCode(code: string): void;
}
//=============================================================================
// Helpers the pane hands a code action when its button is pressed, so the action
// can drive the pane. `ask` queues a follow-up prompt as a normal agent turn
// (selected model, "Thinking", answer), e.g. to let a failed build ask the agent
// to correct its own script.
export interface AssistantCodeActionHelpers {
  ask: (prompt: string) => void;
}
// A labeled action offered under each code block. Hosts inject these so the
// buttons act on their surface (add a notebook cell, insert at the editor
// cursor, run in the console, ...) instead of the single generic Run. The
// helpers argument is optional so existing one-argument actions still satisfy it.
export interface AssistantCodeAction {
  label: string;
  run: (code: string, helpers: AssistantCodeActionHelpers) => void;
}
export interface AssistantPaneProps {
  backend: AssistantBackend;
  // Optional translator; defaults to identity so the pane works standalone.
  t?: (text: string) => string;
  // Optional example prompts shown on the empty state.
  examplePrompts?: string[];
  // Optional short label of where generated code lands (shown in the header),
  // so it is always clear which surface the actions target.
  targetLabel?: string;
  // Optional host actions under each code block. When omitted, the pane keeps
  // its default Copy + Run (backend.runCode) buttons.
  codeActions?: AssistantCodeAction[];
  // Whether to offer the claude-agent-acp backend in the model selector. Hosts
  // with no local process (the browser/wasm engine, where ACP cannot spawn)
  // pass false; defaults to true to keep every existing surface unchanged.
  offerAcp?: boolean;
  // Optional key enabling up/down prompt-history recall persisted per surface in
  // localStorage. Without it, recall still works in memory while the pane is open.
  historyKey?: string;
}
//=============================================================================
const HISTORY_LIMIT = 50;
//=============================================================================
type Role = "user" | "assistant";
type Message = { role: Role; text: string };
type Segment = { kind: "text"; text: string } | { kind: "code"; code: string };
//=============================================================================
const DEFAULT_EXAMPLE_PROMPTS = [
  "Plot sin(x) from -pi to pi",
  "Compute the sum of the integers from 1 to 100",
  "Create a 3-by-3 magic square and show its row sums",
  "Explain what eig does",
];
//=============================================================================
const modelLabel = (name: string): string =>
  name === ACP_MODEL ? "Claude (ACP)" : name;
//=============================================================================
function splitSegments(text: string): Segment[] {
  const segments: Segment[] = [];
  const fence = /```[\w-]*\r?\n?([\s\S]*?)```/g;
  let last = 0;
  let match: RegExpExecArray | null;
  while ((match = fence.exec(text)) !== null) {
    if (match.index > last) {
      segments.push({ kind: "text", text: text.slice(last, match.index) });
    }
    segments.push({ kind: "code", code: match[1].replace(/\r?\n$/, "") });
    last = fence.lastIndex;
  }
  if (last < text.length) {
    segments.push({ kind: "text", text: text.slice(last) });
  }
  if (segments.length === 0) {
    segments.push({ kind: "text", text });
  }
  return segments;
}
//=============================================================================
export function AssistantPane({
  backend,
  t,
  examplePrompts,
  targetLabel,
  codeActions,
  offerAcp = true,
  historyKey,
}: AssistantPaneProps): JSX.Element {
  const tr = t ?? ((text: string): string => text);
  const examples = examplePrompts ?? DEFAULT_EXAMPLE_PROMPTS;
  const [messages, setMessages] = useState<Message[]>([]);
  const [input, setInput] = useState("");
  const [busy, setBusy] = useState(false);
  const [models, setModels] = useState<string[]>([]);
  const [model, setModel] = useState("");
  const [activity, setActivity] = useState<string[]>([]);
  const [streaming, setStreaming] = useState("");
  const scrollRef = useRef<HTMLDivElement | null>(null);
  const inputRef = useRef<HTMLTextAreaElement | null>(null);
  const stoppedRef = useRef(false);
  // Prompt history: sent prompts oldest-first, the position being recalled
  // (null while editing a live draft) and the draft saved when recall starts.
  const historyRef = useRef<string[]>([]);
  const histIndexRef = useRef<number | null>(null);
  const draftRef = useRef("");

  // Load the persisted history for this surface once, best-effort: a blocked or
  // empty store (private window, cleared data) just starts from no history.
  useEffect(() => {
    if (!historyKey) {
      return;
    }
    try {
      const raw = localStorage.getItem(
        `nelson.assistant.history.${historyKey}`,
      );
      const parsed: unknown = raw ? JSON.parse(raw) : null;
      if (Array.isArray(parsed)) {
        historyRef.current = parsed
          .filter((item): item is string => typeof item === "string")
          .slice(-HISTORY_LIMIT);
      }
    } catch {
      /* no usable history store */
    }
  }, [historyKey]);

  useEffect(() => {
    let cancelled = false;
    backend
      .listModels()
      .then((list) => {
        if (cancelled) {
          return;
        }
        // The claude-agent-acp backend (a full Claude agent) is offered first and
        // selected by default when present; the local providers follow. Hosts
        // with no local process (the browser engine) pass offerAcp={false}.
        const provided = Array.isArray(list) ? list : [];
        const full = offerAcp ? [ACP_MODEL, ...provided] : provided;
        setModels(full);
        setModel(full[0] ?? "");
      })
      .catch(() => {
        if (!cancelled) {
          const fallback = offerAcp ? [ACP_MODEL] : [];
          setModels(fallback);
          setModel(fallback[0] ?? "");
        }
      });
    return () => {
      cancelled = true;
    };
  }, [backend, offerAcp]);

  // Live events the agent emits while it runs: 'reset' starts a new model turn,
  // 'token' streams reply fragments, 'activity' reports tool runs/results, and
  // 'answer'/'error' deliver the final result (the run is queued, not an RPC).
  useEffect(() => {
    return backend.subscribe((channel, payloadText) => {
      if (channel === "reset") {
        setStreaming("");
        return;
      }
      if (channel === "token") {
        try {
          const event = JSON.parse(payloadText) as { text?: string };
          setStreaming((current) => current + (event.text ?? ""));
        } catch {
          /* ignore malformed event */
        }
        return;
      }
      if (channel === "activity") {
        try {
          const event = JSON.parse(payloadText) as {
            kind?: string;
            name?: string;
            text?: string;
          };
          const line =
            event.kind === "result"
              ? `✓ ${event.name ?? ""}${event.text ? `: ${event.text}` : ""}`
              : `▶ ${event.name ?? tr("working")}`;
          setActivity((current) => [...current, line]);
          setStreaming("");
        } catch {
          /* ignore malformed event */
        }
        return;
      }
      if (channel === "answer" || channel === "error") {
        let text = "";
        try {
          text = (JSON.parse(payloadText) as { text?: string }).text ?? "";
        } catch {
          /* keep empty */
        }
        if (channel === "error" && stoppedRef.current) {
          text = tr("(stopped)");
        } else if (text === "") {
          text =
            channel === "answer"
              ? tr("(no response)")
              : tr("The assistant failed.");
        }
        setMessages((current) => [...current, { role: "assistant", text }]);
        setBusy(false);
        setStreaming("");
        setActivity([]);
      }
    });
  }, [backend, tr]);

  useEffect(() => {
    const node = scrollRef.current;
    if (node) {
      node.scrollTop = node.scrollHeight;
    }
  }, [messages, busy, activity, streaming]);

  // Remember a sent prompt for up/down recall: skip an immediate repeat, cap the
  // list, reset the recall cursor and persist when a surface key is set.
  const recordPrompt = (prompt: string): void => {
    const history = historyRef.current;
    if (history[history.length - 1] !== prompt) {
      history.push(prompt);
      if (history.length > HISTORY_LIMIT) {
        history.splice(0, history.length - HISTORY_LIMIT);
      }
    }
    histIndexRef.current = null;
    draftRef.current = "";
    if (historyKey) {
      try {
        localStorage.setItem(
          `nelson.assistant.history.${historyKey}`,
          JSON.stringify(history),
        );
      } catch {
        /* history persistence is best-effort */
      }
    }
  };

  const send = async (text?: string): Promise<void> => {
    const prompt = (text ?? input).trim();
    if (prompt === "" || busy) {
      return;
    }
    recordPrompt(prompt);
    setMessages((current) => [...current, { role: "user", text: prompt }]);
    setInput("");
    setBusy(true);
    setActivity([]);
    setStreaming("");
    stoppedRef.current = false;
    inputRef.current?.focus();
    // The run is queued on the interpreter; the answer arrives as an assistant
    // 'answer'/'error' event, so this call only confirms it was accepted.
    try {
      const result = await backend.sendPrompt(prompt, model);
      if (result.accepted === false) {
        setMessages((current) => [
          ...current,
          {
            role: "assistant",
            text: tr("Nelson is busy. Stop the current execution first."),
          },
        ]);
        setBusy(false);
      }
    } catch {
      setMessages((current) => [
        ...current,
        {
          role: "assistant",
          text: tr(
            "The assistant is unavailable. Check that a provider (such as a local Ollama server) is running and a model is selected.",
          ),
        },
      ]);
      setBusy(false);
    }
  };

  // Raise the interpreter interrupt flag so a long model call aborts, and return
  // the pane to idle at once. Relying only on a trailing 'error' event would leave
  // it stuck in "Thinking..." whenever that event never arrives; a late answer or
  // error still appends its message (stoppedRef marks it as stopped).
  const stop = (): void => {
    stoppedRef.current = true;
    backend.interrupt();
    setBusy(false);
    setStreaming("");
    setActivity([]);
  };

  const moveCaretToEnd = (): void => {
    const apply = (): void => {
      const el = inputRef.current;
      if (!el) {
        return;
      }
      try {
        el.setSelectionRange(el.value.length, el.value.length);
      } catch {
        /* selection APIs can be unavailable */
      }
    };
    if (typeof requestAnimationFrame === "function") {
      requestAnimationFrame(apply);
    } else {
      apply();
    }
  };

  // Up on the first line walks back through earlier prompts (saving the live
  // draft first); Down walks forward and restores the draft past the newest.
  const recallPrevious = (): void => {
    const history = historyRef.current;
    if (history.length === 0) {
      return;
    }
    if (histIndexRef.current === null) {
      draftRef.current = input;
      histIndexRef.current = history.length - 1;
    } else if (histIndexRef.current > 0) {
      histIndexRef.current -= 1;
    }
    setInput(history[histIndexRef.current]);
    moveCaretToEnd();
  };

  const recallNext = (): void => {
    if (histIndexRef.current === null) {
      return;
    }
    const history = historyRef.current;
    if (histIndexRef.current < history.length - 1) {
      histIndexRef.current += 1;
      setInput(history[histIndexRef.current]);
    } else {
      histIndexRef.current = null;
      setInput(draftRef.current);
    }
    moveCaretToEnd();
  };

  const onKeyDown = (event: KeyboardEvent<HTMLTextAreaElement>): void => {
    const el = event.currentTarget;
    const caret = el.selectionStart ?? 0;
    const onFirstLine = !el.value.slice(0, caret).includes("\n");
    const onLastLine = !el.value.slice(el.selectionEnd ?? caret).includes("\n");
    // Recall only from the edge lines, so arrows still move the caret within a
    // multi-line draft.
    if (
      event.key === "ArrowUp" &&
      onFirstLine &&
      historyRef.current.length > 0
    ) {
      event.preventDefault();
      recallPrevious();
      return;
    }
    if (
      event.key === "ArrowDown" &&
      histIndexRef.current !== null &&
      onLastLine
    ) {
      event.preventDefault();
      recallNext();
      return;
    }
    if (event.key === "Enter" && !event.shiftKey) {
      event.preventDefault();
      void send();
    }
  };

  const copyCode = (code: string): void => {
    void navigator.clipboard?.writeText(code).catch(() => undefined);
  };

  const runCode = (code: string): void => {
    backend.runCode(code);
  };

  return (
    <div className="assistant-panel">
      <div className="assistant-panel__header">
        <div
          style={{
            display: "flex",
            alignItems: "baseline",
            gap: "8px",
            minWidth: 0,
          }}
        >
          <span className="assistant-panel__brand">{tr("Assistant")}</span>
          {targetLabel ? (
            <span
              className="assistant-panel__target"
              title={tr("Where the assistant applies generated code")}
              style={{
                fontSize: "11px",
                color: "var(--fg-dim, #9a9a9a)",
                overflow: "hidden",
                textOverflow: "ellipsis",
                whiteSpace: "nowrap",
              }}
            >
              {"→ " + targetLabel}
            </span>
          ) : null}
        </div>
        {models.length > 0 ? (
          <select
            className="assistant-panel__model"
            value={model}
            onChange={(event) => setModel(event.target.value)}
            disabled={busy}
            aria-label={tr("Model")}
          >
            {models.map((name) => (
              <option key={name} value={name}>
                {modelLabel(name)}
              </option>
            ))}
          </select>
        ) : (
          <span className="assistant-panel__model assistant-panel__model--none">
            {tr("no model")}
          </span>
        )}
      </div>
      <div className="assistant-panel__messages" ref={scrollRef}>
        {messages.length === 0 ? (
          <div className="assistant-panel__empty">
            <div className="assistant-panel__empty-title">
              {tr("Ask the assistant to write or run Nelson code.")}
            </div>
            <div className="assistant-panel__examples">
              {examples.map((example) => (
                <button
                  key={example}
                  type="button"
                  className="assistant-panel__chip"
                  onClick={() => void send(tr(example))}
                >
                  {tr(example)}
                </button>
              ))}
            </div>
          </div>
        ) : (
          messages.map((message, index) => (
            <div
              key={index}
              className={`assistant-panel__msg assistant-panel__msg--${message.role}`}
            >
              <div className="assistant-panel__role">
                {message.role === "user" ? tr("You") : tr("Assistant")}
              </div>
              <div className="assistant-panel__bubble">
                {splitSegments(message.text).map((segment, segIndex) =>
                  segment.kind === "code" ? (
                    <div key={segIndex} className="assistant-panel__code">
                      <div className="assistant-panel__code-actions">
                        <button
                          type="button"
                          onClick={() => copyCode(segment.code)}
                        >
                          {tr("Copy")}
                        </button>
                        {codeActions && codeActions.length > 0 ? (
                          codeActions.map((action, actionIndex) => (
                            <button
                              key={actionIndex}
                              type="button"
                              onClick={() =>
                                action.run(segment.code, {
                                  ask: (prompt) => void send(prompt),
                                })
                              }
                            >
                              {action.label}
                            </button>
                          ))
                        ) : (
                          <button
                            type="button"
                            onClick={() => runCode(segment.code)}
                          >
                            {tr("Run")}
                          </button>
                        )}
                      </div>
                      <pre>{segment.code}</pre>
                    </div>
                  ) : (
                    <div key={segIndex} className="assistant-panel__para">
                      {segment.text}
                    </div>
                  ),
                )}
              </div>
            </div>
          ))
        )}
        {busy ? (
          <div className="assistant-panel__progress">
            <div className="assistant-panel__busy">
              <span className="assistant-panel__spinner" />
              {tr("Thinking…")}
              <button
                type="button"
                className="assistant-panel__stop"
                onClick={stop}
                title={tr("Stop the assistant")}
              >
                {tr("Stop")}
              </button>
            </div>
            {activity.length > 0 ? (
              <ul className="assistant-panel__activity">
                {activity.map((line, index) => (
                  <li key={index}>{line}</li>
                ))}
              </ul>
            ) : null}
            {streaming.trim() !== "" &&
            !streaming.trimStart().startsWith("{") ? (
              <div className="assistant-panel__stream">{streaming}</div>
            ) : null}
          </div>
        ) : null}
      </div>
      <div className="assistant-panel__input">
        <textarea
          ref={inputRef}
          value={input}
          onChange={(event) => {
            // Typing leaves history recall and edits a fresh draft.
            histIndexRef.current = null;
            setInput(event.target.value);
          }}
          onKeyDown={onKeyDown}
          placeholder={tr("Ask the assistant…")}
          rows={2}
          disabled={busy}
        />
        {busy ? (
          <button
            type="button"
            className="assistant-panel__send assistant-panel__send--stop"
            onClick={stop}
          >
            {tr("Stop")}
          </button>
        ) : (
          <button
            type="button"
            className="assistant-panel__send"
            onClick={() => void send()}
            disabled={input.trim() === ""}
          >
            {tr("Send")}
          </button>
        )}
      </div>
    </div>
  );
}
//=============================================================================
