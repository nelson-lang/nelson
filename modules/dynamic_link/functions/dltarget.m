%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function arch = dltarget()
  % Architecture the dynamic-link tools build for: the running architecture,
  % or 'wasm32' when the NELSON_DL_TARGET environment variable selects a
  % cross build of an Emscripten side module (getdynlibext, dlgeneratemake and
  % dlmake follow the same variable).
  arch = getenv('NELSON_DL_TARGET');
  if isempty(arch)
    arch = computer('arch');
    return
  end
  if ~strcmp(arch, 'wasm32')
    error('Nelson:dynamic_link:invalidTarget', _('Unsupported NELSON_DL_TARGET value: %s'), arch);
  end
end
%=============================================================================
