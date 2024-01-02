%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/17
% <-- Short Description -->
% 'locales' directory renamed as 'locale' (more standard).
%=============================================================================
if isdir([modulepath('nelson', 'bin'), '/../lib']) || ...
  isdir([modulepath('nelson', 'bin'), '/../lib64'])
  assert_istrue(isdir([modulepath('nelson', 'root'), '/../locale']));
else
  assert_istrue(isdir([modulepath('nelson', 'root'), '/locale']));
end
%=============================================================================
