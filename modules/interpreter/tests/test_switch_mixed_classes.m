%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% switch matches text with text, and one-element values by value, exactly
% across classes; a string compared with anything but text is an error.
%=============================================================================
assert_istrue(switch_case(true, 1));
assert_isfalse(switch_case(true, 2));
assert_istrue(switch_case(false, 0));
assert_isfalse(switch_case(true, 'a'));
assert_istrue(switch_case(1, true));
assert_istrue(switch_case(1, single(1)));
%=============================================================================
assert_istrue(switch_case('a', 97));
assert_istrue(switch_case(97, 'a'));
assert_isfalse(switch_case('ab', 97));
assert_isfalse(switch_case(97, 'ab'));
assert_isfalse(switch_case('a', 'b'));
assert_istrue(switch_case('ab', 'ab'));
%=============================================================================
assert_istrue(switch_case(int8(3), 3));
assert_isfalse(switch_case(int8(3), 3.4));
assert_istrue(switch_case(int8(3), uint8(3)));
assert_istrue(switch_case(uint8(3), int8(3)));
assert_istrue(switch_case(uint8(200), 200));
assert_istrue(switch_case(int16(-3), -3));
assert_isfalse(switch_case(int64(9007199254740993), 9007199254740992));
assert_istrue(switch_case(single(2.5), 2.5));
assert_isfalse(switch_case(3.4, int8(3)));
%=============================================================================
assert_istrue(switch_case(1 + 2i, 1 + 2i));
assert_istrue(switch_case(3, complex(3, 0)));
assert_istrue(switch_case('a', complex(97, 0)));
assert_isfalse(switch_case(NaN, NaN));
%=============================================================================
assert_istrue(switch_case("a", 'a'));
assert_istrue(switch_case('a', "a"));
assert_isfalse(switch_case("a", 'ab'));
assert_istrue(switch_case('ab', {97, 'ab'}));
assert_istrue(switch_case('a', {"a", 97}));
%=============================================================================
assert_checkerror('switch_case("a", 97)', 'Comparison between double and string is not supported.');
assert_checkerror('switch_case(2, "a")', 'Comparison between string and double is not supported.');
assert_checkerror('switch_case(true, "a")', 'Comparison between string and logical is not supported.');
assert_checkerror('switch_case("a", {97, "a"})', 'Comparison between double and string is not supported.');
%=============================================================================
function r = switch_case(value, caseValue)
  r = false;
  switch value
    case caseValue
      r = true;
  end
end
%=============================================================================
