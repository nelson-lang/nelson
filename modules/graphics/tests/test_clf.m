%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
x = linspace(0, 10);
y1 = sin(x);
plot(x, y1);
assert_isfalse(isempty(f.Children));
f_clf = clf;
assert_isequal(f_clf, f);
assert_istrue(isempty(f.Children));
%=============================================================================