%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
years = [120 140 150 158 1582 1584 160 1602 180 190 195 200 201 2004 2021];
tf = leapyear(years);
REF = [true    true    false   false   false   true    true    false   true    false   false   false   false   true    false ];
assert_isequal(tf, REF);
%=============================================================================
tf = leapyear([]);
assert_isequal(tf, logical([]));
%=============================================================================
