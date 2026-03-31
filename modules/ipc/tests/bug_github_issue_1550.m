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
% <--GUI MODE-->
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelson_exe = [binpath, '/nelson --timeout 10'];
%=============================================================================
exec = string([nelson_exe; nelson_exe; nelson_exe;nelson_exe]);
timeout = [10; 10; 10; 10];
system(exec, timeout);
sleep(10);
R = getpid('available');
assert_istrue(length(R) >= 4);
%=============================================================================
