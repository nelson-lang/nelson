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
assert_isequal(nargin('mustBeBetween'), -3);
assert_isequal(nargout('mustBeBetween'), 0);
%=============================================================================
mustBeBetween([3 4 5], 0, 5);
mustBeBetween([3 4], 0, 5, 'open');
mustBeBetween([0 1 2], 0, 5, 'closedleft');
mustBeBetween([1 2 5], 0, 5, 'closedright');
%=============================================================================
mustBeBetween(int8([-1 0 1]), int8(-1), int8(1));
mustBeBetween(uint16([2 3 4]), uint8(2), uint32(4));
mustBeBetween(single([2.5 3 3.5]), single(2), single(4), 'open');
mustBeBetween(logical([true false]), false, true);
mustBeBetween([complex(3, 4) complex(0, 3)], 3, 5);
%=============================================================================
mustBeBetween(string({'bee', 'cat'}), string('ant'), string('dog'));
mustBeBetween(['b' 'c'], 'a', 'd');
%=============================================================================
A = uint8([2 3; 4 5]);
mustBeBetween(A, uint8([2; 4]), uint8([4 5]));
%=============================================================================
T = table([2; 3; 4], [10; 11; 12], 'VariableNames', {'A', 'B'});
mustBeBetween(T, 2, 4, 'DataVariables', 'A');
mustBeBetween(T, 2, 12, 'DataVariables', {'A', 'B'});
%=============================================================================
assert_checkerror('mustBeBetween([3 4 5], 0, 5, ''open'')', ...
    _('Value must be within the range specified by lower and upper.'), ...
    'Nelson:validators:mustBeBetween');
%=============================================================================
assert_checkerror('mustBeBetween([10 11], 0, 5, 2)', ...
    [_('Invalid input argument at position 2.'), newline, ...
    _('Value must be within the range specified by lower and upper.')], ...
    'Nelson:validators:mustBeBetween');
%=============================================================================
