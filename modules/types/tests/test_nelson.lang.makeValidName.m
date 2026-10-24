%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('nelson.lang.makeValidName'), -2);
asserts.isequal(nargout('nelson.lang.makeValidName'), 2);
%=============================================================================
asserts.isequal(nelson.lang.makeValidName('a b-c'), 'aB_c');
asserts.isequal(nelson.lang.makeValidName('1st order'), 'x1stOrder');
%=============================================================================
[res, modified] = nelson.lang.makeValidName({'a b', 'a-b', '1a'});
asserts.isequal(res, {'aB', 'a_b', 'x1a'});
asserts.isequal(modified, [true true true]);
%=============================================================================
