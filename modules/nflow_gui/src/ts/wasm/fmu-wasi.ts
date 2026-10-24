//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as cli from '@bytecodealliance/preview2-shim/cli';
import * as filesystem from '@bytecodealliance/preview2-shim/filesystem';
import * as io from '@bytecodealliance/preview2-shim/io';
import * as clocks from '@bytecodealliance/preview2-shim/clocks';
import * as random from '@bytecodealliance/preview2-shim/random';
//=============================================================================
export const wasiImports = Object.fromEntries(
  [
    ['cli', cli],
    ['filesystem', filesystem],
    ['io', io],
    ['clocks', clocks],
    ['random', random],
  ].flatMap(([namespace, values]) =>
    Object.entries(values).map(([name, value]) => [
      `wasi:${namespace}/${name.replace(/[A-Z]/g, (letter) => '-' + letter.toLowerCase())}`,
      value,
    ])
  )
);
//=============================================================================
