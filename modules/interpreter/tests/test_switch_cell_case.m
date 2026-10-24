%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% A single-element cell in a 'case' must match, exactly like a multi-element
% cell or a bare value.
%=============================================================================
function r = do_switch(v, c)
  r = 'N';
  switch v
    case c
      r = 'Y';
    otherwise
      r = 'N';
  end
end
%=============================================================================
% char switch value
asserts.isequal(do_switch('x', {'x'}), 'Y'); % single-element cell
asserts.isequal(do_switch('x', {'a', 'x'}), 'Y'); % multi-element cell
asserts.isequal(do_switch('x', {'a', 'b'}), 'N'); % no match
asserts.isequal(do_switch('x', 'x'), 'Y'); % bare char
%=============================================================================
% numeric switch value (single-element cell used to raise an error)
asserts.isequal(do_switch(3, {3}), 'Y'); % single-element cell
asserts.isequal(do_switch(3, {1, 3}), 'Y'); % multi-element cell
asserts.isequal(do_switch(3, {1, 2}), 'N'); % no match
asserts.isequal(do_switch(5, 5), 'Y'); % bare scalar
%=============================================================================
% char and string are interchangeable in switch/case
asserts.isequal(do_switch('x', "x"), 'Y'); % string scalar case
asserts.isequal(do_switch("x", 'x'), 'Y'); % string switch value
asserts.isequal(do_switch('x', {"a", "x"}), 'Y'); % cell of strings
%=============================================================================
% reference types remain invalid switch values
asserts.checkerror('switch {1}, case 1, end', _('Switch argument cannot be a reference type (struct or cell array)'));
S.a = 1;
asserts.checkerror('S.a = 1; switch S, case 1, end', _('Switch argument cannot be a reference type (struct or cell array)'));
%=============================================================================
