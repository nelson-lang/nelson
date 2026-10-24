%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression test: pathological lexer inputs must raise a catchable error
% instead of overflowing a fixed-size stack buffer and crashing the
% interpreter.
%   (a) A numeric literal longer than the 256-byte lexer buffer used to be
%       copied past the end of the buffer -> stack overflow.
%   (b) More than 256 nested brackets used to be pushed past the end of the
%       256-entry bracket stack -> stack overflow.
% After the fix each raises a lexer error and the interpreter keeps running.
%=============================================================================
% (a) very long numeric literal
R = false;
try
  eval([repmat('1', 1, 400), ';']);
catch
  R = true;
end
assert_istrue(R);
%=============================================================================
% (b) deeply nested brackets
R = false;
try
  eval([repmat('(', 1, 400), '1', repmat(')', 1, 400), ';']);
catch
  R = true;
end
assert_istrue(R);
%=============================================================================
% the interpreter is still alive and usable after the recovered errors
assert_isequal(1 + 1, 2);
%=============================================================================
