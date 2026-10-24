%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% string(categorical): <undefined> elements become string missing (regression)
asserts.isequal(ismissing(string(categorical({'a', ''}))), [false true]);
%=============================================================================
% defined elements keep their category name
s = string(categorical({'a', 'b'}));
asserts.isequal(s, ["a" "b"]);
asserts.isfalse(any(ismissing(s)));
%=============================================================================
% mixed: defined value preserved, undefined becomes missing
s = string(categorical({'a', ''}));
asserts.isequal(s(1), "a");
asserts.istrue(ismissing(s(2)));
%=============================================================================
% scalar undefined
asserts.istrue(ismissing(string(categorical({''}))));
%=============================================================================
% cellstr is unchanged: '<undefined>' is preserved (not converted to missing)
asserts.isequal(cellstr(categorical({'a', ''})), {'a', '<undefined>'});
%=============================================================================
% double(categorical) still maps code 0 to NaN
d = double(categorical({'a', ''}));
asserts.isequal(d(1), 1);
asserts.istrue(isnan(d(2)));
%=============================================================================
