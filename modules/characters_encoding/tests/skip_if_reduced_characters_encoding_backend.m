%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function skip_if_reduced_characters_encoding_backend()
  if iswasm()
    skip_testsuite(['WebAssembly packages the reduced characters_encoding ', ...
      'runtime backend: UTF-8 stream conversion is available, but ICU charset ', ...
      'builtins are not compiled in this browser build.']);
  end
end
%=============================================================================
