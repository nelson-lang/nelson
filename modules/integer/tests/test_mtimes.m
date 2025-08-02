%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = intmax('int8') * 2;
REF = intmax('int8');
assert_isequal(R, REF);
%=============================================================================
R = intmax('int8') * -2;
REF = intmin('int8');
assert_isequal(R, REF);
%=============================================================================
R = intmax('int8') * intmax('int8');
REF = intmax('int8');
assert_isequal(R, REF);
%=============================================================================
R = intmax('int8') * intmin('int8');
REF = intmin('int8');
assert_isequal(R, REF);
%=============================================================================
R = intmin('int8') * intmax('int8');
REF = intmin('int8');
assert_isequal(R, REF);
%=============================================================================
R = intmin('int8') * intmin('int8');
REF = intmax('int8');
assert_isequal(R, REF);
%=============================================================================
R = int8([1 2]) * 3;
REF =  int8([3 6]);
assert_isequal(R, REF);
%=============================================================================
R = 3 * int8([1 2]);
REF =  int8([3 6]);
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = int8([1 2]) * int8([1;2])', _('At least one input argument must be scalar.'));
%=============================================================================
msg = _('Integers can only be combined with integers of the same class, or scalar doubles.');
assert_checkerror('R = single(3) * int8([1 2]);', msg);
assert_checkerror('R = int8([1 2]) * single(3);', msg);
%=============================================================================
R = intmax('uint8') * intmax('uint8');
REF = intmax('uint8');
assert_isequal(R, REF);
%=============================================================================
R = intmax('uint8') * intmin('uint8');
REF = intmin('uint8');
assert_isequal(R, REF);
%=============================================================================
R = intmin('uint8') * intmax('uint8');
REF = intmin('uint8');
assert_isequal(R, REF);
%=============================================================================
R = intmin('uint8') * intmin('uint8');
REF = intmin('uint8');
assert_isequal(R, REF);
%=============================================================================
R = intmax('uint8') * uint8(1);
REF = intmax('uint8');
assert_isequal(R, REF);
%=============================================================================
R = uint8(3) * uint8(4);
REF = uint8(12);
assert_isequal(R, REF);
%=============================================================================
R = uint8(128) * uint8(4)
REF = intmax('uint8');
assert_isequal(R, REF);
%=============================================================================
