%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function platforms = nmm_valid_platforms()
  % The platform tags nmm understands. 'all' is the portable (source) target;
  % every other value is a concrete architecture as returned by computer('arch')
  % and used as a key of a binary package "artifacts" map.
  % 'wasm32' is the WebAssembly (browser) engine; its binary packages are
  % Emscripten side modules loaded by the same gateway manager.
  platforms = {'all', 'win32', 'woa64', 'win64', 'maci64', 'maci32', 'maca64', 'glnxa64', 'glnxa64arm', 'glnxa32', 'wasm32'};
end
%=============================================================================
