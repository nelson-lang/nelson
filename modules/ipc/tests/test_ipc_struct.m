%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--ADV-CLI MODE-->
% <--IPC REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
clear('R');
REF = true;
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = uint8([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = int8([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = single([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = uint64([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = int64([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = 3;
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = 3;
ipc(getpid, 'put', REF, 'R', 'local')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = {1; 2};
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = rand(1, 100);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = 'simplified Chinese: 汉字; traditional Chinese: 漢字; pinyin: Hàn Zì';
ipc(getpid, 'put', REF, 'R', 'base')
q = 0; while (~ipc(getpid, 'isvar', 'R', 'base') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(REF, acquirevar('base', 'R'));
%=============================================================================
clear('R');
REF = [1 + 2i; 3 + 4i];
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = ["hello"; "world"];
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
clear('R');
REF = ones(1000, 1000);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(REF, R);
%=============================================================================
k = 10;
clear('R');
REF = sparse(rand(k, k) + i * rand(k, k));
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
k = 10;
clear('R');
REF = sparse(eye(k, k));
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
k = 10;
clear('R');
REF = sparse(logical(eye(k, k)));
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(R, REF);
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
clear('R');
REF = ClassdefPoint(10, 20);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(class(R), 'ClassdefPoint');
asserts.isequal(R.X, 10);
asserts.isequal(R.Y, 20);
asserts.istrue(any(strcmp(methods(R), 'magnitude')));
asserts.istrue(any(strcmp(properties(R), 'Dimension')));
asserts.isequal(magnitude(R), sqrt(500));
%=============================================================================
clear('R');
REF = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, 'RowNames', {'r1', 'r2'});
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.istrue(istable(R));
asserts.isequal(class(R), 'table');
asserts.isequal(R.A, [1; 2]);
asserts.isequal(R.B, [3; 4]);
P = R.Properties;
asserts.isequal(P.RowNames, {'r1'; 'r2'});
%=============================================================================
% NOT SERIALIZABLE
%=============================================================================
clear('R');
REF = figure();
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(size(R), size([]));
L = lastwarn();
asserts.isequal(L, _('Variable not fully serialized.'));
%=============================================================================
