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
% Implicit expansion (broadcasting) for the two-argument forms of mod and rem.
%=============================================================================
% row vs column
R = mod([10 11 12], [3; 4]);
REF = [1 2 0; 2 3 0];
asserts.isequal(R, REF);
%=============================================================================
R = rem([10 11 12], [3; 4]);
REF = [1 2 0; 2 3 0];
asserts.isequal(R, REF);
%=============================================================================
% negative operands distinguish mod (floored) from rem (truncated)
R = mod([-7 7], [3; -3]);
REF = [2 1; -1 -2];
asserts.isequal(R, REF);
%=============================================================================
R = rem([-7 7], [3; -3]);
REF = [-1 1; -1 1];
asserts.isequal(R, REF);
%=============================================================================
% matrix vs row
R = mod([10 11 12; 20 21 22], [3 4 5]);
REF = [1 3 2; 2 1 2];
asserts.isequal(R, REF);
%=============================================================================
% matrix vs column
R = mod([10 11 12; 20 21 22], [3; 4]);
REF = [1 2 0; 0 1 2];
asserts.isequal(R, REF);
%=============================================================================
% integer class is preserved
R = mod(int16([10 11 12]), int16([3; 4]));
REF = int16([1 2 0; 2 3 0]);
asserts.isequal(R, REF);
asserts.isequal(class(R), 'int16');
%=============================================================================
R = rem(int16([10 11 12]), int16([3; 4]));
REF = int16([1 2 0; 2 3 0]);
asserts.isequal(R, REF);
asserts.isequal(class(R), 'int16');
%=============================================================================
% single class is preserved
R = mod(single([10 11 12]), single([3; 4]));
REF = single([1 2 0; 2 3 0]);
asserts.isequal(R, REF);
asserts.isequal(class(R), 'single');
%=============================================================================
% mixed integer / floating point keeps the integer class
R = mod(int32([10 11 12]), [3; 4]);
REF = int32([1 2 0; 2 3 0]);
asserts.isequal(R, REF);
asserts.isequal(class(R), 'int32');
%=============================================================================
% scalar and same-size operands keep working
asserts.isequal(mod([10 11 12], 3), [1 2 0]);
asserts.isequal(rem([10 11 12], [3 4 5]), [1 3 2]);
%=============================================================================
