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
g = groot();
%=============================================================================
a = figure(1);
assert_istrue(isvalid(a));
delete(a)
assert_isfalse(isvalid(a));
%=============================================================================
a = figure(1);
b = figure(2);
c = [a, b];
assert_istrue(all(isvalid(c)));
assert_isequal(g.CurrentFigure.Number, 2);
delete(c);
assert_isfalse(all(isvalid(c)));
assert_istrue(isempty(g.CurrentFigure));
%=============================================================================
