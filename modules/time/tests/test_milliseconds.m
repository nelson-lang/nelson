%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
d = milliseconds([250 500]);
asserts.istrue(isduration(d));
asserts.isequal(seconds(d), [0.25 0.5]);
asserts.isequal(milliseconds(seconds(2)), 2000);
asserts.isequal(milliseconds(duration(0, 0, 0, 1)), 1);
%=============================================================================
