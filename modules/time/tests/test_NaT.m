%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
t = NaT();
asserts.isequal(isnat(t), true);
t = NaT(2, 3);
asserts.isequal(size(datenum(t)), [2 3]);
asserts.isequal(isnat(t), true(2, 3));
t = NaT(1, 2, 'TimeZone', 'UTC');
asserts.isequal(t.TimeZone, 'UTC');
asserts.isequal(isnat(t), true(1, 2));
%=============================================================================
