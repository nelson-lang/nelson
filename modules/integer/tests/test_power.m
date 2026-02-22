%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = int8([1:5]).^2;
REF = int8([1    4    9   16   25]);
assert_isequal(R, REF);
%=============================================================================
R = int8([1:5]).^int8(2);
REF = int8([1    4    9   16   25]);
assert_isequal(R, REF);
%=============================================================================
msg = message('nelson:runtime:operandsMustBeIntegersOrScalarDouble');
assert_checkerror('R = [1:5].^int8(2)', msg);
%=============================================================================
R = int8(128).^2;
REF = int8(127);
assert_isequal(R, REF);
%=============================================================================
msg = message('nelson:validators:mustBeIntegerFormAtPosition', 2);
assert_checkerror('R = int8(128).^0.5;', msg);
%=============================================================================
msg = message('nelson:validators:mustBePositiveAtPosition', 2);
assert_checkerror('R =  int8([1 2 3; 4 5 6; 7 8 9]).^-1;', msg);
%=============================================================================
