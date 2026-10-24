%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
close all
f = figure('Visible', 'off');
xt = [1; 2; 3; 4];
yt = [2; 3; 4; 5];
gt = [1; 1; 2; 2];
tbl = table(xt, yt, gt, 'VariableNames', {'xv', 'yv', 'gv'});
h = scatterhistogram(f, tbl, 'xv', 'yv', 'GroupVariable', 'gv');
asserts.isequal(h.XVariable, 'xv');
asserts.isequal(h.YVariable, 'yv');
asserts.isequal(h.GroupVariable, 'gv');
state = h.UserData;
asserts.isequal(state.Groups.Count, 2);
close(f);
close all
%=============================================================================
