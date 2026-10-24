%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
addpath([modulepath('interpreter', 'tests'), '/onCleanup']);
R = evalc('demo_onCleanup(0);');
REF = 'Start of demo_onCleanup
Doing some work...
End of demo_onCleanup
Cleanup action executed
';
asserts.isequal(R, REF);
%=============================================================================
cleanupObj = onCleanup(@()[]);
cleanupArray(1) = cleanupObj;
cleanupArray(2) = cleanupObj;
asserts.isequal(size(cleanupArray), [1 2]);
clear cleanupArray cleanupObj;
%=============================================================================
R = evalc('demo_onCleanup(1);');
REF = 'Start of demo_onCleanup
Cleanup action executed
Cleanup called explicitly
Doing some work...
End of demo_onCleanup
';
asserts.isequal(R, REF);
%=============================================================================
R = evalc('demo_onCleanup(2);');
REF = 'Start of demo_onCleanup
Doing some work...
End of demo_onCleanup
';
asserts.isequal(R, REF);
%=============================================================================
