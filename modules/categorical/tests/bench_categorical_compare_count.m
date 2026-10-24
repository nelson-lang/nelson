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
mask = A == 'red';
counts = countcats(A);
toc();
asserts.isequal(sum(mask), 5000);
asserts.isequal(sum(counts), 10000);
%=============================================================================
