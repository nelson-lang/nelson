%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
d = seconds([1 2]);
asserts.istrue(isduration(d));
asserts.isequal(seconds(d), [1 2]);
asserts.isequal(seconds(minutes(2)), 120);
asserts.isequal(size(seconds(d)), [1 2]);
%=============================================================================
