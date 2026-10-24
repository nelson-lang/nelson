%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function deploytool(varargin)
  narginchk(0, 0);
  nargoutchk(0, 0);
  __load_compiler__();
  __nelsonc_editor__();
end
