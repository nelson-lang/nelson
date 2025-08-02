%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
p = path();
if ispc()
  r = addpath('C:/', '-end');
  REF = [r, ';C:/'];
else
  r = addpath('/', '-end');
  REF = [r, ':/'];
end
np = path();
assert_isequal(np, REF);
