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
p = str2func('rand');
b = backgroundPool();
N = 500;
B = cell(N);
%=============================================================================
tic()
%=============================================================================
for i = 1:N
    A(i) = parfeval(b, p, 1, N);
end
%=============================================================================
for i = 1:N
    B{i} = fetchOutputs(A(i));
end
%=============================================================================
toc()
%=============================================================================