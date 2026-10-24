%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
previousTarget = getenv('NELSON_DL_TARGET');
restoreTarget = onCleanup(@() setenv('NELSON_DL_TARGET', previousTarget));
%=============================================================================
setenv('NELSON_DL_TARGET', '');
asserts.isequal(dltarget(), computer('arch'));
if iswasm()
  asserts.isequal(getdynlibext(), '.so');
end
%=============================================================================
% a wasm32 cross build describes Emscripten side modules on every host
setenv('NELSON_DL_TARGET', 'wasm32');
asserts.isequal(dltarget(), 'wasm32');
asserts.isequal(getdynlibext(), '.so');
%=============================================================================
setenv('NELSON_DL_TARGET', 'win64');
asserts.checkerror('dltarget()', sprintf(_('Unsupported NELSON_DL_TARGET value: %s'), 'win64'));
%=============================================================================
setenv('NELSON_DL_TARGET', '');
asserts.isequal(dltarget(), computer('arch'));
%=============================================================================
