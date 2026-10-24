%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('nelson.lang.makeValidName'), -2);
asserts.isequal(nargout('nelson.lang.makeValidName'), 2);
%=============================================================================
asserts.isequal(nelson.lang.makeValidName('a b-c'), 'aB_c');
asserts.isequal(nelson.lang.makeValidName('1st order'), 'x1stOrder');
asserts.isequal(nelson.lang.makeValidName(''), 'x');
%=============================================================================
names = {'a b', 'a-b', '1a', '_a'};
[res, modified] = nelson.lang.makeValidName(names);
asserts.isequal(res, {'aB', 'a_b', 'x1a', 'x_a'});
asserts.isequal(modified, [true true true true]);
%=============================================================================
S = ["a b", "a-b"; "ok", "2x"];
[res, modified] = nelson.lang.makeValidName(S);
asserts.isequal(res, ["aB", "a_b"; "ok", "x2x"]);
asserts.isequal(modified, [true true; false true]);
%=============================================================================
asserts.isequal(nelson.lang.makeValidName('a-b', 'ReplacementStyle', 'delete'), 'ab');
asserts.isequal(nelson.lang.makeValidName('a#b', 'ReplacementStyle', 'hex'), 'a0x23b');
asserts.isequal(nelson.lang.makeValidName('9value', 'Prefix', 'var'), 'var9value');
asserts.isequal(nelson.lang.makeValidName('for'), 'xFor');
%=============================================================================
longName = repmat('a', 1, namelengthmax() + 10);
asserts.isequal(length(nelson.lang.makeValidName(longName)), namelengthmax());
%=============================================================================
asserts.checkerror('nelson.lang.makeValidName(''a'', ''ReplacementStyle'')', _('Name-value arguments must be paired.'));
asserts.checkerror('nelson.lang.makeValidName(''a'', ''ReplacementStyle'', ''bad'')', _('Unsupported replacement style.'));
asserts.checkerror('nelson.lang.makeValidName(''a'', ''Prefix'', ''1x'')', _('Prefix must be a valid variable name.'));
%=============================================================================
