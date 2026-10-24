//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
type Uint8ArrayConstructorWithBase64 = Uint8ArrayConstructor & {
  fromBase64?: (encoded: string) => Uint8Array;
};
//=============================================================================
export function base64Bytes(encoded: string): Uint8Array {
  const fromBase64 = (Uint8Array as Uint8ArrayConstructorWithBase64).fromBase64;
  if (typeof fromBase64 === 'function') {
    return fromBase64(encoded);
  }
  const decoded = globalThis.atob(encoded);
  const bytes = new Uint8Array(decoded.length);
  for (let i = 0; i < decoded.length; i++) bytes[i] = decoded.charCodeAt(i);
  return bytes;
}
//=============================================================================
