%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% S{I} = B on an empty string array grows a string array, gaps are missing.
%=============================================================================
x = strings(0, 0);
x{2} = 'q';
asserts.isequal(class(x), 'string');
asserts.isequal(size(x), [1 2]);
asserts.isequal(strlength(x), [NaN 1]);
asserts.isequal(ismissing(x), [true false]);
asserts.isequal(x(2), "q");
%=============================================================================
x = strings(1, 0);
x{2} = 'q';
asserts.isequal(class(x), 'string');
asserts.isequal(strlength(x), [NaN 1]);
%=============================================================================
x = string.empty;
x{2} = 'q';
asserts.isequal(class(x), 'string');
asserts.isequal(ismissing(x), [true false]);
%=============================================================================
x = strings(0, 0);
x{1} = 'abc';
asserts.isequal(x, "abc");
%=============================================================================
x = strings(1, 2);
x{3} = 'q';
asserts.isequal(strlength(x), [0 0 1]);
%=============================================================================
x = strings(0, 0);
x{1, 2} = 'q';
asserts.isequal(class(x), 'string');
asserts.isequal(strlength(x), [NaN 1]);
%=============================================================================
eval('y = strings(0, 0); y{2} = ''q'';');
asserts.isequal(class(y), 'string');
asserts.isequal(strlength(y), [NaN 1]);
%=============================================================================
z = subsasgn(strings(0, 0), substruct('{}', {2}), 'q');
asserts.isequal(class(z), 'string');
asserts.isequal(strlength(z), [NaN 1]);
%=============================================================================
c = {};
c{2} = 1;
asserts.isequal(class(c), 'cell');
%=============================================================================
