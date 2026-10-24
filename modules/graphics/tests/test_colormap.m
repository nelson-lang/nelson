%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
surf(peaks);
colormap('summer');
r = f.Colormap;
asserts.isapprox(r(1:2, :), [0 0.5000 0.4000; 0.0039 0.5020 0.4000], 1e-4);
%=============================================================================
ax = gca();
m = colormap(ax);
asserts.isapprox(m(1:2, :), [0 0.5000 0.4000; 0.0039 0.5020 0.4000], 1e-4);
%=============================================================================
% built-in colormap names are resolved case-insensitively
f2 = figure();
mLower = colormap(f2, 'jet');
mMixed = colormap(f2, 'Jet');
asserts.isequal(mMixed, mLower);
mUpper = colormap(f2, 'JET');
asserts.isequal(mUpper, mLower);
%=============================================================================
% a string-scalar name with mixed case also resolves
mStr = colormap(f2, "Summer");
asserts.isapprox(mStr(1:2, :), [0 0.5000 0.4000; 0.0039 0.5020 0.4000], 1e-4);
close(f2);
%=============================================================================
