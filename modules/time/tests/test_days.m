%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
d = days([1 2]);
asserts.istrue(isduration(d));
asserts.isequal(seconds(d), [86400 172800]);
asserts.isequal(days(d), [1 2]);
asserts.isequal(days(hours(48)), 2);
%=============================================================================
