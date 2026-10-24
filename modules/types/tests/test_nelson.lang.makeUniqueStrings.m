%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
res = nelson.lang.makeUniqueStrings({'abcd', 'abcd'}, {}, 5);
asserts.isequal(res, {'abcd', 'abc_1'});
%=============================================================================
