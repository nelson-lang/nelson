%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = anonymousFunctionScopeFreeRunCase()
  % same as anonymousFunctionScopeFreeCase, with the handle created and
  % called by a script run in this function's workspace
  keep = 40;
  varlock('local', 'keep');
  run([fileparts(mfilename('fullpath')), '/anonymousFunctionScopeFreeScript.m']);
  varunlock('local', 'keep');
  r = r + keep;
end
