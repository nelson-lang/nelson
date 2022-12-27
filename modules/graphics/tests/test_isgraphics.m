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
f = figure();
assert_istrue(isgraphics(f));
assert_istrue(isgraphics(f, 'figure'));
assert_isfalse(isgraphics(f, 'figure1'));
assert_isfalse(isgraphics(3));
%=============================================================================