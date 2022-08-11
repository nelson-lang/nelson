%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
p = str2func('pause');
b = backgroundPool();
NumWorkers = b.NumWorkers;
for k = [1:(NumWorkers*2) + 2]
  f(k) = parfeval(b, p, 0, 5);
end
R = {f.State};
assert_istrue(iscellstr(R));
assert_isequal(length(R), (NumWorkers*2) + 2);
%=============================================================================
