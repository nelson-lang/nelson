%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
t1 = seconds((0:2:999)');
t2 = seconds((1:2:999)');
TT1 = timetable(t1, sin((1:length(t1))' ./ 20), 'VariableNames', {'A'});
TT2 = timetable(t2, cos((1:length(t2))' ./ 20), 'VariableNames', {'B'});
tic();
R = retime(TT1, seconds((0:999)'), 'linear');
S = synchronize(TT1, TT2, 'regular', 'linear', 'TimeStep', seconds(1));
T = S(timerange(seconds(100), seconds(200)), :);
toc();
asserts.isequal(height(R), 1000);
asserts.isequal(height(S), 1000);
asserts.isequal(height(T), 100);
%=============================================================================
