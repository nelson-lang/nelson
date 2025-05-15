%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
cmaps = colormaplist();
assert_istrue(isstring(cmaps));
assert_istrue(iscolumn(cmaps));
assert_istrue(numel(cmaps) >= 18);
assert_istrue(any(contains(cmaps, "jet")));
%=============================================================================