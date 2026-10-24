%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% <--IPC REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
clear('R');
inner = struct('a', {4, {5, int64(6)}}, 'b', 'txt');
REF = {1, {2, {3, inner}}, "str"; sparse(eye(3)), logical([1 0 1]), {}};
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
s.level1.level2.level3.value = pi;
s.level1.level2.names = ["a", "b"; "c", "d"];
s.other = {uint8([1 2 3]), 'end'};
REF = s;
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
