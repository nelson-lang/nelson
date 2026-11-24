//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, expect, it, vi } from 'vitest';
import {
  handleWasmAudioAction,
  isWasmAudioUnlockPending,
  playWasmAudioAction,
  resetWasmAudioForTests,
  unlockWasmAudio,
  WEB_AUDIO_UNLOCK_REQUIRED_EVENT,
} from '../../src/wasm/webAudio';
//=============================================================================
function float32Base64(values: number[]): string {
  const bytes = new Uint8Array(values.length * Float32Array.BYTES_PER_ELEMENT);
  const view = new DataView(bytes.buffer);
  values.forEach((value, index) => {
    view.setFloat32(index * Float32Array.BYTES_PER_ELEMENT, value, true);
  });
  let binary = '';
  bytes.forEach((byte) => {
    binary += String.fromCharCode(byte);
  });
  return btoa(binary);
}
//=============================================================================
afterEach(() => {
  resetWasmAudioForTests();
  vi.unstubAllGlobals();
  vi.useRealTimers();
  window.__nelsonLastPlayedAudio = undefined;
});
//=============================================================================
it('plays planar Nelson audio samples through a browser AudioBuffer', async () => {
  const channels = [new Float32Array(2), new Float32Array(2)];
  const source = {
    buffer: null as AudioBuffer | null,
    connect: vi.fn(),
    start: vi.fn(),
  };
  class FakeAudioContext {
    state: AudioContextState = 'suspended';
    destination = {};
    resume = vi.fn(async () => {
      this.state = 'running';
    });
    createBuffer(channelCount: number, frameCount: number, sampleRate: number): AudioBuffer {
      expect(channelCount).toBe(2);
      expect(frameCount).toBe(2);
      expect(sampleRate).toBe(8000);
      return {
        getChannelData: (channel: number) => channels[channel],
      } as AudioBuffer;
    }
    createBufferSource(): AudioBufferSourceNode {
      return source as unknown as AudioBufferSourceNode;
    }
  }

  vi.stubGlobal('AudioContext', FakeAudioContext);
  await playWasmAudioAction({
    type: 'play-audio',
    sampleRate: 8000,
    channels: 2,
    frames: 2,
    samples: [0, 0.5, -0.5, 1],
  });

  expect([...channels[0]]).toEqual([0, 0.5]);
  expect([...channels[1]]).toEqual([-0.5, 1]);
  expect(source.connect).toHaveBeenCalledOnce();
  expect(source.start).toHaveBeenCalledOnce();
  expect(window.__nelsonLastPlayedAudio).toEqual({
    sampleRate: 8000,
    channels: 2,
    frames: 2,
  });
});
//=============================================================================
it('queues playback and exposes an unlock action when autoplay keeps audio suspended', async () => {
  const source = {
    buffer: null as AudioBuffer | null,
    connect: vi.fn(),
    start: vi.fn(),
  };
  const context = {
    state: 'suspended' as AudioContextState,
    destination: {},
    resume: vi.fn(async () => undefined),
    createBuffer: vi.fn(() => ({
      getChannelData: () => new Float32Array(2),
    })) as unknown as AudioContext['createBuffer'],
    createBufferSource: vi.fn(() => source) as unknown as AudioContext['createBufferSource'],
  } as unknown as AudioContext;
  const unlockRequested = vi.fn();
  window.addEventListener(WEB_AUDIO_UNLOCK_REQUIRED_EVENT, unlockRequested);
  vi.stubGlobal(
    'AudioContext',
    class {
      constructor() {
        return context;
      }
    }
  );

  await handleWasmAudioAction({
    type: 'play-audio',
    sampleRate: 8000,
    channels: 1,
    frames: 2,
    samples: [0, 0.5],
  });
  expect(source.start).not.toHaveBeenCalled();
  expect(unlockRequested).toHaveBeenCalledOnce();
  expect(isWasmAudioUnlockPending()).toBe(true);

  context.resume = vi.fn(async () => {
    context.state = 'running';
  });
  await expect(unlockWasmAudio()).resolves.toBe(true);
  expect(source.start).toHaveBeenCalledOnce();
  expect(isWasmAudioUnlockPending()).toBe(false);
});
//=============================================================================
it('does not wait forever when resume remains pending without a user gesture', async () => {
  vi.useFakeTimers();
  const context = {
    state: 'suspended' as AudioContextState,
    destination: {},
    resume: vi.fn(() => new Promise<void>(() => undefined)),
    createBuffer: vi.fn(() => ({
      getChannelData: () => new Float32Array(2),
    })) as unknown as AudioContext['createBuffer'],
    createBufferSource: vi.fn(() => ({
      connect: vi.fn(),
      start: vi.fn(),
    })) as unknown as AudioContext['createBufferSource'],
  } as unknown as AudioContext;
  const unlockRequested = vi.fn();
  window.addEventListener(WEB_AUDIO_UNLOCK_REQUIRED_EVENT, unlockRequested);
  vi.stubGlobal(
    'AudioContext',
    class {
      constructor() {
        return context;
      }
    }
  );

  const playback = handleWasmAudioAction({
    type: 'play-audio',
    sampleRate: 8000,
    channels: 1,
    frames: 2,
    samples: [0, 0.5],
  });
  await vi.runAllTimersAsync();
  await playback;

  expect(unlockRequested).toHaveBeenCalledOnce();
  expect(isWasmAudioUnlockPending()).toBe(true);
});
//=============================================================================
it('assembles chunked audio sessions and supports pause/resume/stop controls', async () => {
  const channels = [new Float32Array(4)];
  const sources: {
    buffer: AudioBuffer | null;
    connect: ReturnType<typeof vi.fn>;
    start: ReturnType<typeof vi.fn>;
    stop: ReturnType<typeof vi.fn>;
    onended: (() => void) | null;
  }[] = [];
  class FakeAudioContext {
    state: AudioContextState = 'running';
    currentTime = 0;
    destination = {};
    resume = vi.fn(async () => {
      this.state = 'running';
    });
    createBuffer(channelCount: number, frameCount: number, sampleRate: number): AudioBuffer {
      expect(channelCount).toBe(1);
      expect(frameCount).toBe(4);
      expect(sampleRate).toBe(4000);
      return {
        getChannelData: (channel: number) => channels[channel],
      } as AudioBuffer;
    }
    createBufferSource(): AudioBufferSourceNode {
      const source = {
        buffer: null as AudioBuffer | null,
        connect: vi.fn(),
        start: vi.fn(),
        stop: vi.fn(),
        onended: null as (() => void) | null,
      };
      sources.push(source);
      return source as unknown as AudioBufferSourceNode;
    }
  }

  const context = new FakeAudioContext();
  vi.stubGlobal(
    'AudioContext',
    class {
      constructor() {
        return context;
      }
    }
  );
  await handleWasmAudioAction({
    type: 'audio-buffer-begin',
    id: 10,
    sampleRate: 4000,
    channels: 1,
    frames: 4,
  });
  await handleWasmAudioAction({
    type: 'audio-buffer-chunk',
    id: 10,
    offset: 0,
    frames: 2,
    samples: [0, 0.5],
  });
  await handleWasmAudioAction({
    type: 'audio-buffer-chunk',
    id: 10,
    offset: 2,
    frames: 2,
    samples: [-0.5, 1],
  });
  await handleWasmAudioAction({ type: 'audio-buffer-play', id: 10 });
  expect([...channels[0]]).toEqual([0, 0.5, -0.5, 1]);
  expect(sources[0].start).toHaveBeenCalledWith(0, 0);

  context.currentTime = 0.25;
  await handleWasmAudioAction({ type: 'audio-control', id: 10, command: 'pause' });
  expect(sources[0].stop).toHaveBeenCalledOnce();
  await handleWasmAudioAction({ type: 'audio-control', id: 10, command: 'resume' });
  expect(sources[1].start).toHaveBeenCalledWith(0, 0.001);
  await handleWasmAudioAction({ type: 'audio-control', id: 10, command: 'stop' });
  expect(sources[1].stop).toHaveBeenCalledOnce();
});
//=============================================================================
it('decodes compact float32 base64 chunks for long portable audio buffers', async () => {
  const channels = [new Float32Array(2), new Float32Array(2)];
  const source = {
    buffer: null as AudioBuffer | null,
    connect: vi.fn(),
    start: vi.fn(),
  };
  class FakeAudioContext {
    state: AudioContextState = 'running';
    destination = {};
    createBuffer(channelCount: number, frameCount: number, sampleRate: number): AudioBuffer {
      expect(channelCount).toBe(2);
      expect(frameCount).toBe(2);
      expect(sampleRate).toBe(8000);
      return {
        getChannelData: (channel: number) => channels[channel],
      } as AudioBuffer;
    }
    createBufferSource(): AudioBufferSourceNode {
      return source as unknown as AudioBufferSourceNode;
    }
  }

  vi.stubGlobal('AudioContext', FakeAudioContext);
  await handleWasmAudioAction({
    type: 'audio-buffer-begin',
    id: 11,
    sampleRate: 8000,
    channels: 2,
    frames: 2,
  });
  await handleWasmAudioAction({
    type: 'audio-buffer-chunk',
    id: 11,
    offset: 0,
    frames: 2,
    samplesF32Base64: float32Base64([0, 0.5, -0.5, 1]),
  });
  await handleWasmAudioAction({ type: 'audio-buffer-play', id: 11 });

  expect([...channels[0]]).toEqual([0, 0.5]);
  expect([...channels[1]]).toEqual([-0.5, 1]);
  expect(source.start).toHaveBeenCalledWith(0, 0);
});
//=============================================================================
it('accepts transferred float32 buffers for streamed portable audio', async () => {
  const channels = [new Float32Array(2)];
  const source = {
    buffer: null as AudioBuffer | null,
    connect: vi.fn(),
    start: vi.fn(),
  };
  class FakeAudioContext {
    state: AudioContextState = 'running';
    destination = {};
    createBuffer(): AudioBuffer {
      return {
        getChannelData: (channel: number) => channels[channel],
      } as AudioBuffer;
    }
    createBufferSource(): AudioBufferSourceNode {
      return source as unknown as AudioBufferSourceNode;
    }
  }

  vi.stubGlobal('AudioContext', FakeAudioContext);
  await handleWasmAudioAction({
    type: 'audio-buffer-begin',
    id: 12,
    sampleRate: 8000,
    channels: 1,
    frames: 2,
  });
  await handleWasmAudioAction({
    type: 'audio-buffer-chunk',
    id: 12,
    offset: 0,
    frames: 2,
    samplesF32Buffer: new Float32Array([0.25, -0.25]).buffer,
  });
  await handleWasmAudioAction({ type: 'audio-buffer-play', id: 12 });

  expect([...channels[0]]).toEqual([0.25, -0.25]);
  expect(source.start).toHaveBeenCalledWith(0, 0);
});
//=============================================================================
it('serializes bursty chunked playback and controls while AudioContext resumes', async () => {
  const events: string[] = [];
  let resumeAudio: (() => void) | null = null;
  const sources: {
    start: ReturnType<typeof vi.fn>;
    stop: ReturnType<typeof vi.fn>;
    onended: (() => void) | null;
  }[] = [];
  class FakeAudioContext {
    state: AudioContextState = 'suspended';
    currentTime = 0.25;
    destination = {};
    resume = vi.fn(
      () =>
        new Promise<void>((resolve) => {
          resumeAudio = () => {
            this.state = 'running';
            events.push('context-resume');
            resolve();
          };
        })
    );
    createBuffer(): AudioBuffer {
      events.push('buffer');
      return {
        getChannelData: () => new Float32Array(4),
      } as AudioBuffer;
    }
    createBufferSource(): AudioBufferSourceNode {
      const source = {
        buffer: null as AudioBuffer | null,
        connect: vi.fn(),
        start: vi.fn((_when = 0, offset = 0) => events.push(`start:${offset}`)),
        stop: vi.fn(() => events.push('stop')),
        onended: null as (() => void) | null,
      };
      sources.push(source);
      return source as unknown as AudioBufferSourceNode;
    }
  }

  vi.stubGlobal('AudioContext', FakeAudioContext);
  await handleWasmAudioAction({
    type: 'audio-buffer-begin',
    id: 10,
    sampleRate: 4000,
    channels: 1,
    frames: 4,
  });
  await handleWasmAudioAction({
    type: 'audio-buffer-chunk',
    id: 10,
    offset: 0,
    frames: 4,
    samples: [0, 0.5, -0.5, 1],
  });

  const pending = Promise.all([
    handleWasmAudioAction({ type: 'audio-buffer-play', id: 10 }),
    handleWasmAudioAction({ type: 'audio-control', id: 10, command: 'pause' }),
    handleWasmAudioAction({ type: 'audio-control', id: 10, command: 'resume' }),
    handleWasmAudioAction({ type: 'audio-control', id: 10, command: 'stop' }),
  ]);

  await Promise.resolve();
  expect(events).toEqual([]);
  expect(resumeAudio).not.toBeNull();
  resumeAudio?.();
  await pending;

  expect(events).toEqual([
    'context-resume',
    'buffer',
    'start:0',
    'stop',
    'buffer',
    'start:0',
    'stop',
  ]);
  expect(sources).toHaveLength(2);
});
//=============================================================================
