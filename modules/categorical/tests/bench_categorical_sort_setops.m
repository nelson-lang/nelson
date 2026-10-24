%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical(repmat({'red'; 'green'; 'blue'; 'red'}, 2500, 1));
tic();
S = sort(A);
U = unique(A);
toc();
asserts.isequal(length(S), length(A));
asserts.istrue(length(U) >= 3);
%=============================================================================
