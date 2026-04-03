%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/1550
% <-- Short Description -->
% getpid('available') did not work as expected.
%=============================================================================
% <--IPC REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
% <--ADV-CLI MODE-->
%=============================================================================
N = 3;
binpath = modulepath('nelson', 'bin');
nelson_exe = [binpath, '/nelson-gui --timeout 20 &'];
%=============================================================================
sleep(5);
before = getpid('available');
exec = string(repmat(nelson_exe, N, 1));
timeout = repmat(20, N, 1);
[a, b] = system(exec, timeout);
sleep(10);
after = getpid('available');
assert_istrue(length(after) >= 2);
%=============================================================================
