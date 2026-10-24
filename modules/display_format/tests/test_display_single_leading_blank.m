%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% disp() of a single value must not start with a blank line (it did not for
% double); the adaptive leading blank only belongs to named variable display.
%=============================================================================
NL = char(10);
assert_isfalse(startsWith(evalc('disp(single(pi))'), NL));
assert_isfalse(startsWith(evalc('disp(single([1.5 2.5]))'), NL));
assert_isfalse(startsWith(evalc('disp(single([1 2; 3 4]))'), NL));
assert_isfalse(startsWith(evalc('disp(single(1 + 2i))'), NL));
assert_isfalse(startsWith(evalc('disp(single([1 + 2i, 3 - 4i]))'), NL));
%=============================================================================
% disp() of a single matches the same shape as a double (no leading blank).
%=============================================================================
assert_isequal(evalc('disp(single(pi))'), evalc('disp(pi)'));
%=============================================================================
% Named display is unchanged: it still opens with a newline and the name.
%=============================================================================
assert_istrue(startsWith(evalc('x = single(pi)'), [NL, 'x =']));
assert_istrue(startsWith(evalc('x = single(1 + 2i)'), [NL, 'x =']));
%=============================================================================
