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
% parallelplot default property values aligned with the reference.
%=============================================================================
close all
%=============================================================================
h = parallelplot(magic(4));
assert_isequal(h.FontName, 'Helvetica');
assert_isequal(char(h.LegendVisible), 'off');
assert_isapprox(h.Jitter, 0.1, 1e-12);
assert_isapprox(h.LineAlpha, 0.7, 1e-12);
close all
%=============================================================================
