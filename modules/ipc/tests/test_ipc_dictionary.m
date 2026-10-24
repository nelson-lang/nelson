%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
% <--IPC REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% dictionary is a value classdef, so it serializes through the generic class
% path (fields + class type, layout restored on receipt).
%=============================================================================
clear('R');
REF = dictionary(["alpha", "beta", "gamma"], [10, 20, 30]);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(class(R), 'dictionary');
asserts.isequal(numEntries(R), 3);
asserts.isequal(R("beta"), 20);
asserts.isequal(keys(R), keys(REF));
asserts.isequal(R, REF);
%=============================================================================
% numeric keys
clear('R');
REF = dictionary([1 2 3], ["one", "two", "three"]);
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(class(R), 'dictionary');
asserts.isequal(R(2), "two");
asserts.isequal(R, REF);
%=============================================================================
% cell values (mixed content)
clear('R');
REF = dictionary(["m", "v"], {[1 2 3], "hello"});
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(class(R), 'dictionary');
asserts.isequal(R{"m"}, [1 2 3]);
asserts.isequal(R{"v"}, "hello");
%=============================================================================
% empty (unconfigured) dictionary
clear('R');
REF = dictionary();
ipc(getpid, 'put', REF, 'R')
q = 0; while (~isvar('R') && q < 1000), sleep(0.01), q = q + 1; end
asserts.isequal(class(R), 'dictionary');
asserts.isfalse(isConfigured(R));
%=============================================================================
