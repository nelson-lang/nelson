//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export interface WasmPlayAudioAction {
  type: 'play-audio';
  sampleRate: number;
  channels: number;
  frames: number;
  samples: number[];
}
//=============================================================================
export interface WasmAudioBufferBeginAction {
  type: 'audio-buffer-begin';
  id: number;
  sampleRate: number;
  channels: number;
  frames: number;
}
//=============================================================================
export interface WasmAudioBufferChunkAction {
  type: 'audio-buffer-chunk';
  id: number;
  offset: number;
  frames: number;
  samples?: number[];
  samplesF32Base64?: string;
  samplesF32Buffer?: ArrayBuffer;
}
//=============================================================================
export interface WasmAudioBufferPlayAction {
  type: 'audio-buffer-play';
  id: number;
}
//=============================================================================
export interface WasmAudioControlAction {
  type: 'audio-control';
  id: number;
  command: 'pause' | 'resume' | 'stop';
}
//=============================================================================
export type WasmAudioAction =
  | WasmPlayAudioAction
  | WasmAudioBufferBeginAction
  | WasmAudioBufferChunkAction
  | WasmAudioBufferPlayAction
  | WasmAudioControlAction;
type AudioContextConstructor = new () => AudioContext;
type Uint8ArrayConstructorWithBase64 = Uint8ArrayConstructor & {
  fromBase64?: (encoded: string) => Uint8Array;
};
//=============================================================================
declare global {
  interface Window {
    webkitAudioContext?: AudioContextConstructor;
    __nelsonLastPlayedAudio?: {
      sampleRate: number;
      channels: number;
      frames: number;
    };
  }
}
//=============================================================================
let audioContext: AudioContext | null = null;
let anonymousAudioId = -1;
let audioActionQueue = Promise.resolve();
const audioSessions = new Map<number, WebAudioSession>();
const pendingAudioActions: WasmAudioAction[] = [];
const AUDIO_RESUME_TIMEOUT_MS = 250;
export const WEB_AUDIO_UNLOCK_REQUIRED_EVENT = 'nelson:web-audio-unlock-required';
export const WEB_AUDIO_UNLOCKED_EVENT = 'nelson:web-audio-unlocked';
class WebAudioUnlockRequiredError extends Error {
  constructor() {
    super('Web Audio requires a user gesture before playback can start.');
    this.name = 'WebAudioUnlockRequiredError';
  }
}
//=============================================================================
function requestAudioUnlock(action: WasmAudioAction): void {
  pendingAudioActions.push(action);
  if (typeof window !== 'undefined') {
    window.dispatchEvent(new Event(WEB_AUDIO_UNLOCK_REQUIRED_EVENT));
  }
}
//=============================================================================
function removePendingAudioActions(id: number): void {
  for (let index = pendingAudioActions.length - 1; index >= 0; index--) {
    const action = pendingAudioActions[index];
    if ('id' in action && action.id === id) pendingAudioActions.splice(index, 1);
  }
}
//=============================================================================
export function isWasmAudioUnlockPending(): boolean {
  return pendingAudioActions.length > 0;
}
//=============================================================================
export function resetWasmAudioForTests(): void {
  audioContext = null;
  anonymousAudioId = -1;
  audioActionQueue = Promise.resolve();
  audioSessions.clear();
  pendingAudioActions.length = 0;
}
//=============================================================================
function audioConstructor(): AudioContextConstructor {
  const ctor = window.AudioContext ?? window.webkitAudioContext;
  if (!ctor) throw new Error('Web Audio is not available in this browser.');
  return ctor;
}
//=============================================================================
async function ensureAudioContext(): Promise<AudioContext> {
  audioContext ??= new (audioConstructor())();
  if (audioContext.state === 'suspended') {
    try {
      // Some browsers keep resume() pending until a user gesture instead of
      // rejecting it. Do not leave the audio action queue blocked forever:
      // after a short grace period, expose the shared unlock UI.
      const resume = audioContext.resume().catch(() => undefined);
      await Promise.race([
        resume,
        new Promise<void>((resolve) => window.setTimeout(resolve, AUDIO_RESUME_TIMEOUT_MS)),
      ]);
    } catch {
      // Browsers may reject resume() until a user gesture has occurred.
      // Normalize that browser-specific failure so the shared unlock UI can
      // retry it without losing the queued playback action.
      throw new WebAudioUnlockRequiredError();
    }
  }
  if (audioContext.state !== 'running') throw new WebAudioUnlockRequiredError();
  return audioContext;
}
//=============================================================================
function validateAudioShape(sampleRate: number, channels: number, frames: number): void {
  if (
    !Number.isFinite(sampleRate) ||
    sampleRate <= 0 ||
    !Number.isSafeInteger(channels) ||
    channels <= 0 ||
    channels > 8 ||
    !Number.isSafeInteger(frames) ||
    frames < 0
  ) {
    throw new Error('Invalid WebAssembly audio payload.');
  }
}
//=============================================================================
function validateSamples(samples: number[], expectedLength: number): void {
  if (
    !Array.isArray(samples) ||
    samples.length !== expectedLength ||
    !samples.every(Number.isFinite)
  ) {
    throw new Error('Invalid WebAssembly audio sample payload.');
  }
}
//=============================================================================
function bytesFromBase64(encoded: string): Uint8Array {
  const fromBase64 = (Uint8Array as Uint8ArrayConstructorWithBase64).fromBase64;
  if (typeof fromBase64 === 'function') {
    return fromBase64(encoded);
  }
  const binary = globalThis.atob(encoded);
  const bytes = new Uint8Array(binary.length);
  for (let k = 0; k < binary.length; k++) bytes[k] = binary.charCodeAt(k);
  return bytes;
}
//=============================================================================
function float32SamplesFromBase64(encoded: string, expectedLength: number): Float32Array {
  const bytes = bytesFromBase64(encoded);
  if (bytes.byteLength !== expectedLength * Float32Array.BYTES_PER_ELEMENT) {
    throw new Error('Invalid WebAssembly audio sample payload.');
  }
  return new Float32Array(bytes.buffer, bytes.byteOffset, expectedLength);
}
//=============================================================================
function float32SamplesFromBuffer(buffer: ArrayBuffer, expectedLength: number): Float32Array {
  if (buffer.byteLength !== expectedLength * Float32Array.BYTES_PER_ELEMENT) {
    throw new Error('Invalid WebAssembly audio sample payload.');
  }
  return new Float32Array(buffer);
}
//=============================================================================
function clampSample(sample: number): number {
  return Math.max(-1, Math.min(1, Number.isFinite(sample) ? sample : 0));
}
//=============================================================================
class WebAudioSession {
  readonly channelsData: Float32Array[];
  private source: AudioBufferSourceNode | null = null;
  private state: 'idle' | 'playing' | 'paused' | 'stopped' = 'idle';
  private startedAt = 0;
  private offsetSeconds = 0;
  constructor(
    readonly id: number,
    readonly sampleRate: number,
    readonly channels: number,
    readonly frames: number
  ) {
    validateAudioShape(sampleRate, channels, frames);
    this.channelsData = Array.from({ length: channels }, () => new Float32Array(frames));
  }
  //=============================================================================
  writeChunk(offset: number, frames: number, samples: ArrayLike<number>): void {
    if (
      !Number.isSafeInteger(offset) ||
      offset < 0 ||
      !Number.isSafeInteger(frames) ||
      frames < 0 ||
      offset + frames > this.frames
    ) {
      throw new Error('Invalid WebAssembly audio chunk range.');
    }
    if (samples.length !== this.channels * frames) {
      throw new Error('Invalid WebAssembly audio sample payload.');
    }
    for (let channel = 0; channel < this.channels; channel++) {
      const output = this.channelsData[channel];
      const sampleOffset = channel * frames;
      for (let frame = 0; frame < frames; frame++) {
        output[offset + frame] = clampSample(samples[sampleOffset + frame]);
      }
    }
  }
  //=============================================================================
  async play(): Promise<void> {
    if (this.frames === 0) return;
    await this.startAt(0);
    window.__nelsonLastPlayedAudio = {
      sampleRate: this.sampleRate,
      channels: this.channels,
      frames: this.frames,
    };
  }
  //=============================================================================
  pause(): void {
    if (this.state !== 'playing' || !audioContext) return;
    this.offsetSeconds = Math.min(
      this.frames / this.sampleRate,
      Math.max(0, audioContext.currentTime - this.startedAt)
    );
    this.stopSource();
    this.state = 'paused';
  }
  //=============================================================================
  async resume(): Promise<void> {
    if (this.state !== 'paused') return;
    await this.startAt(this.offsetSeconds);
  }
  //=============================================================================
  stop(): void {
    this.stopSource();
    this.offsetSeconds = 0;
    this.state = 'stopped';
  }
  //=============================================================================
  private async startAt(offsetSeconds: number): Promise<void> {
    const context = await ensureAudioContext();
    this.stopSource();

    const buffer = context.createBuffer(this.channels, this.frames, this.sampleRate);
    for (let channel = 0; channel < this.channels; channel++) {
      buffer.getChannelData(channel).set(this.channelsData[channel]);
    }

    const source = context.createBufferSource();
    source.buffer = buffer;
    source.connect(context.destination);
    source.onended = () => {
      if (this.state === 'playing') {
        this.state = 'idle';
        this.offsetSeconds = 0;
        this.source = null;
      }
    };
    this.source = source;
    this.offsetSeconds = Math.max(0, Math.min(offsetSeconds, this.frames / this.sampleRate));
    this.startedAt = context.currentTime - this.offsetSeconds;
    this.state = 'playing';
    source.start(0, this.offsetSeconds);
  }
  //=============================================================================
  private stopSource(): void {
    const source = this.source;
    if (!source) return;
    this.source = null;
    source.onended = null;
    try {
      source.stop();
    } catch {
      // Some browser implementations throw if stop() is called after natural end.
    }
  }
}
//=============================================================================
export async function playWasmAudioAction(action: WasmPlayAudioAction): Promise<void> {
  validateAudioShape(action.sampleRate, action.channels, action.frames);
  validateSamples(action.samples, action.channels * action.frames);
  if (action.frames === 0) return;

  const session = new WebAudioSession(
    anonymousAudioId--,
    action.sampleRate,
    action.channels,
    action.frames
  );
  session.writeChunk(0, action.frames, action.samples);
  try {
    await session.play();
  } catch (error) {
    if (error instanceof WebAudioUnlockRequiredError) {
      requestAudioUnlock(action);
      return;
    }
    throw error;
  }
}
//=============================================================================
async function processWasmAudioAction(
  action: WasmAudioAction,
  queueOnBlocked = true
): Promise<void> {
  if (action.type === 'play-audio') {
    await playWasmAudioAction(action);
    return;
  }
  if (action.type === 'audio-buffer-begin') {
    audioSessions.get(action.id)?.stop();
    audioSessions.set(
      action.id,
      new WebAudioSession(action.id, action.sampleRate, action.channels, action.frames)
    );
    return;
  }
  const session = audioSessions.get(action.id);
  if (!session) throw new Error(`Unknown WebAssembly audio session: ${action.id}.`);
  if (action.type === 'audio-buffer-chunk') {
    const expectedLength = session.channels * action.frames;
    if (action.samplesF32Buffer !== undefined) {
      session.writeChunk(
        action.offset,
        action.frames,
        float32SamplesFromBuffer(action.samplesF32Buffer, expectedLength)
      );
      return;
    }
    if (action.samplesF32Base64 !== undefined) {
      session.writeChunk(
        action.offset,
        action.frames,
        float32SamplesFromBase64(action.samplesF32Base64, expectedLength)
      );
      return;
    }
    if (action.samples === undefined) {
      throw new Error('Invalid WebAssembly audio sample payload.');
    }
    validateSamples(action.samples, expectedLength);
    session.writeChunk(action.offset, action.frames, action.samples);
    return;
  }
  if (action.type === 'audio-buffer-play') {
    try {
      await session.play();
    } catch (error) {
      if (queueOnBlocked && error instanceof WebAudioUnlockRequiredError) {
        requestAudioUnlock(action);
        return;
      }
      throw error;
    }
    return;
  }
  if (action.command === 'stop') removePendingAudioActions(action.id);
  if (action.command === 'pause') {
    session.pause();
  } else if (action.command === 'resume') {
    await session.resume();
  } else {
    session.stop();
  }
}
//=============================================================================
export function handleWasmAudioAction(action: WasmAudioAction): Promise<void> {
  const result = audioActionQueue.then(() => processWasmAudioAction(action));
  audioActionQueue = result.catch(() => undefined);
  return result;
}
/**
 * Resume the browser audio context from a user gesture and replay actions that
 * were held while autoplay policy kept the context suspended.
 */
export function unlockWasmAudio(): Promise<boolean> {
  const result = audioActionQueue.then(async () => {
    try {
      await ensureAudioContext();
    } catch {
      return false;
    }
    const pending = pendingAudioActions.splice(0);
    for (const action of pending) await processWasmAudioAction(action, false);
    if (typeof window !== 'undefined') {
      window.dispatchEvent(new Event(WEB_AUDIO_UNLOCKED_EVENT));
    }
    return true;
  });
  audioActionQueue = result.then(
    () => undefined,
    () => undefined
  );
  return result;
}
//=============================================================================
