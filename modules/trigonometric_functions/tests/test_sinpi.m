%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('sinpi'), -1);
assert_isequal(nargout('sinpi'), 1);
%=============================================================================
X = [0 1/2 1 3/2 2];
R = sinpi(X);
REF = [0     1     0    -1     0];
arch = computer('arch');
if strcmp(arch, 'win32')
  assert_isapprox(R, REF, 1e-15);
else
  assert_isequal(R, REF);
end
%=============================================================================
