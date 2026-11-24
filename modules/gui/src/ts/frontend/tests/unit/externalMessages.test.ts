//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it } from 'vitest';
import {
  parseNelsonDesktopExternalMessage,
  postNelsonDesktopExternalReply,
  postMessageReplyTargetOrigin,
} from '../../src/desktop/externalMessages';
//=============================================================================
describe('desktop external postMessage API', () => {
  it('accepts explicit Nelson run messages', () => {
    expect(
      parseNelsonDesktopExternalMessage({
        nelson: 'web-desktop',
        id: 'doc-1',
        type: 'run',
        command: 'surf(peaks)',
      })
    ).toEqual({
      nelson: 'web-desktop',
      id: 'doc-1',
      type: 'run',
      command: 'surf(peaks)',
    });
  });

  it('accepts script messages with optional execution', () => {
    expect(
      parseNelsonDesktopExternalMessage({
        nelson: 'web-desktop',
        type: 'script',
        file: 'demo.m',
        content: 'x = 1',
        run: true,
      })
    ).toEqual({
      nelson: 'web-desktop',
      id: undefined,
      type: 'script',
      file: 'demo.m',
      name: undefined,
      content: 'x = 1',
      run: true,
    });
  });

  it('ignores accidental or incomplete messages', () => {
    expect(parseNelsonDesktopExternalMessage({ type: 'run', command: '42' })).toBeNull();
    expect(parseNelsonDesktopExternalMessage({ nelson: 'web-desktop', type: 'run' })).toBeNull();
    expect(parseNelsonDesktopExternalMessage(null)).toBeNull();
  });

  it('uses wildcard replies for opaque embedded origins', () => {
    expect(postMessageReplyTargetOrigin('https://docs.example.test')).toBe(
      'https://docs.example.test'
    );
    expect(postMessageReplyTargetOrigin('null')).toBe('*');
    expect(postMessageReplyTargetOrigin('')).toBe('*');
  });

  it('posts ack/error replies without leaking delivery failures', () => {
    const delivered: unknown[] = [];
    const source = {
      postMessage(message: unknown, origin: string) {
        delivered.push({ message, origin });
      },
    };
    const request = {
      nelson: 'web-desktop' as const,
      id: 'reply-1',
      type: 'run' as const,
      command: '1+1',
    };

    postNelsonDesktopExternalReply(source, 'https://docs.example.test', request, true);
    postNelsonDesktopExternalReply(source, 'null', request, false, 'boom');
    expect(delivered).toEqual([
      {
        origin: 'https://docs.example.test',
        message: { nelson: 'web-desktop', id: 'reply-1', type: 'ack', requestType: 'run' },
      },
      {
        origin: '*',
        message: {
          nelson: 'web-desktop',
          id: 'reply-1',
          type: 'error',
          requestType: 'run',
          error: 'boom',
        },
      },
    ]);

    expect(() =>
      postNelsonDesktopExternalReply(
        {
          postMessage() {
            throw new Error('closed');
          },
        },
        'https://docs.example.test',
        request,
        true
      )
    ).not.toThrow();
  });
});
//=============================================================================
