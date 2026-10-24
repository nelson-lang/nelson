%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('nelson.lang.makeUniqueStrings'), 3);
asserts.isequal(nargout('nelson.lang.makeUniqueStrings'), 2);
%=============================================================================
[res, modified] = nelson.lang.makeUniqueStrings({'a', 'a', 'b', 'a'});
asserts.isequal(res, {'a', 'a_1', 'b', 'a_2'});
asserts.isequal(modified, [false true false true]);
%=============================================================================
res = nelson.lang.makeUniqueStrings({'a', 'b'}, {'a', 'b'});
asserts.isequal(res, {'a_1', 'b_1'});
%=============================================================================
res = nelson.lang.makeUniqueStrings({'a', 'a', 'a'}, 2);
asserts.isequal(res, {'a', 'a_1', 'a'});
res = nelson.lang.makeUniqueStrings({'a', 'a', 'a'}, [false true false]);
asserts.isequal(res, {'a', 'a_1', 'a'});
%=============================================================================
S = ["a", "a"; "b", "b"];
[res, modified] = nelson.lang.makeUniqueStrings(S);
asserts.isequal(res, ["a", "a_1"; "b", "b_1"]);
asserts.isequal(modified, [false true; false true]);
%=============================================================================
asserts.isequal(nelson.lang.makeUniqueStrings('a'), 'a');
%=============================================================================
res = nelson.lang.makeUniqueStrings({'abcd', 'abcd'}, {}, 5);
asserts.isequal(res, {'abcd', 'abc_1'});
res = nelson.lang.makeUniqueStrings({'abcd', 'abcd'}, {}, 4);
asserts.isequal(res, {'abcd', 'ab_1'});
%=============================================================================
asserts.checkerror('nelson.lang.makeUniqueStrings({''a'', ''a''}, {}, 1)', _('Unable to make strings unique within the maximum string length.'));
asserts.checkerror('nelson.lang.makeUniqueStrings({''a''}, [true false])', _('Logical index must have the same number of elements as the input.'));
asserts.checkerror('nelson.lang.makeUniqueStrings({''a''}, 2)', _('Index values must be positive integers within the input size.'));
%=============================================================================
