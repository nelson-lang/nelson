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
REF = true;
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = uint8([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = int8([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = single([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = uint64([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = int64([3, 4]);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = 3;
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = 3;
ipc(getpid, 'put', REF, 'R', 'local')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = {1; 2};
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = rand(1, 100);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = 'simplified Chinese: 汉字; traditional Chinese: 漢字; pinyin: Hàn Zì';
ipc(getpid, 'put', REF, 'R', 'base')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(REF, acquirevar('base', 'R'));
%=============================================================================
clear('R');
REF = [1+2i; 3+4i];
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = ["hello"; "world"];
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
clear('R');
REF = ones(1000, 1000);
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(REF, R);
%=============================================================================
k = 10;
clear('R');
REF = sparse(rand(k, k) + i * rand(k,k));
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
k = 10;
clear('R');
REF = sparse(eye(k, k));
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
k = 10;
clear('R');
REF = sparse(logical(eye(k, k)));
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(R, REF);
%=============================================================================
% NOT SERIALIZABLE
%=============================================================================
clear('R');
REF = figure();
ipc(getpid, 'put', REF, 'R')
q = 0;while(~isvar('R') && q < 10), sleep(1), q = q + 1; end
assert_isequal(size(R), size([]));
L = lastwarn();
assert_isequal(L, _('Variable not fully serialized.'));
%=============================================================================
