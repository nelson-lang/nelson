//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useState } from 'react';
import {
  isWasmAudioUnlockPending,
  unlockWasmAudio,
  WEB_AUDIO_UNLOCKED_EVENT,
  WEB_AUDIO_UNLOCK_REQUIRED_EVENT,
} from '../wasm/webAudio';
//=============================================================================
export function AudioUnlockHost(): JSX.Element | null {
  const [visible, setVisible] = useState(() => isWasmAudioUnlockPending());
  const [unlocking, setUnlocking] = useState(false);

  useEffect(() => {
    const show = () => setVisible(true);
    const hide = () => {
      setVisible(false);
      setUnlocking(false);
    };
    window.addEventListener(WEB_AUDIO_UNLOCK_REQUIRED_EVENT, show);
    window.addEventListener(WEB_AUDIO_UNLOCKED_EVENT, hide);
    setVisible(isWasmAudioUnlockPending());
    return () => {
      window.removeEventListener(WEB_AUDIO_UNLOCK_REQUIRED_EVENT, show);
      window.removeEventListener(WEB_AUDIO_UNLOCKED_EVENT, hide);
    };
  }, []);

  if (!visible) return null;

  const enableAudio = async () => {
    setUnlocking(true);
    const unlocked = await unlockWasmAudio();
    setUnlocking(false);
    if (!unlocked) setVisible(true);
  };

  return (
    <div className="audio-unlock" role="status" aria-live="polite">
      <div className="audio-unlock__message">Audio playback is waiting for browser permission.</div>
      <button type="button" onClick={() => void enableAudio()} disabled={unlocking}>
        {unlocking ? 'Enabling audio…' : 'Enable sound'}
      </button>
    </div>
  );
}
//=============================================================================
