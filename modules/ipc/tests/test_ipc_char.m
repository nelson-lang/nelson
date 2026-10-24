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
% <--ADV-CLI MODE-->
% <--IPC REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
clear('R');
REF = 'simplified Chinese: 汉字; traditional Chinese: 漢字; pinyin: Hàn Zì';
ipc(getpid, 'put', REF, 'R', 'base')
q = 0; while (~ipc(getpid, 'isvar', 'R', 'base') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(REF, acquirevar('base', 'R'));
%=============================================================================
