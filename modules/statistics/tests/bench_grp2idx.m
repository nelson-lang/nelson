%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--REUSE PROCESS-->
%=============================================================================
s = randi(5000, 100000, 1);
tic();
[g, gN, gL] = grp2idx(s);
t = toc();
asserts.isequal(size(g), size(s));
asserts.isequal(length(gN), length(gL));
asserts.istrue(t >= 0);
%=============================================================================
labels = cell(50000, 1);
for k = 1:length(labels)
  labels{k} = ['group_', int2str(mod(k, 250))];
end
tic();
[g, gN] = grp2idx(labels);
t = toc();
asserts.isequal(size(g), size(labels));
asserts.isequal(length(gN), 250);
asserts.istrue(t >= 0);
%=============================================================================
