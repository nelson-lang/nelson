%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% double/single(categorical): <undefined> converts to NaN
c = categorical({'a', ''});
R = double(c);
asserts.isequal(R(1), 1);
asserts.istrue(isnan(R(2)));
%=============================================================================
R = single(c);
asserts.isequal(class(R), 'single');
asserts.isequal(R(1), single(1));
asserts.istrue(isnan(R(2)));
%=============================================================================
% integer conversions are unchanged: <undefined> converts to 0
asserts.isequal(int32(c), int32([1 0]));
asserts.isequal(uint8(c), uint8([1 0]));
%=============================================================================
% string(categorical) keeps the shape and first element
% (NB: <undefined> -> string missing is handled by @categorical/string.m but
% the string() builtin does not currently dispatch to that overload, so the
% missing conversion is a known remaining gap.)
s = string(c);
asserts.isequal(s(1), "a");
asserts.isequal(size(s), [1 2]);
%=============================================================================
% cellstr is unchanged: <undefined> stays '<undefined>'
asserts.isequal(cellstr(c), {'a', '<undefined>'});
%=============================================================================
% no undefined element: conversions unchanged
c2 = categorical({'b', 'a', 'b'});
asserts.isequal(double(c2), [2 1 2]);
%=============================================================================
