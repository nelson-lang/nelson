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
% Function handles are decomposed to their source, arguments and captured
% variables before serialization and rebuilt on receipt, so they survive an
% ipc transfer (previously the raw definition pointer was sent).
%=============================================================================
clear('R');
REF = @sin;
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(class(R), 'function_handle');
asserts.isequal(func2str(R), func2str(REF));
asserts.isapprox(R(pi / 2), 1, 1e-12);
%=============================================================================
clear('R');
REF = @(x) x .^ 2 + 1;
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(class(R), 'function_handle');
asserts.isequal(R(3), 10);
%=============================================================================
% anonymous function capturing a workspace variable
clear('R');
a = 5;
b = [10 20 30];
REF = @(x) x + a + b;
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R(1), 1 + 5 + [10 20 30]);
%=============================================================================
% function handles nested inside a cell
clear('R');
c = 7;
fa = @cos;
fb = @(x) x * c;
REF = {fa, fb};
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(class(R), 'cell');
f1 = R{1};
f2 = R{2};
asserts.isapprox(f1(0), 1, 1e-12);
asserts.isequal(f2(3), 21);
%=============================================================================
