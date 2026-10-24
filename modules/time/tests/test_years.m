%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
d = years([1 2]);
asserts.istrue(isduration(d));
asserts.isapprox(seconds(d), [31556952 63113904], 1e-8);
asserts.isapprox(years(d), [1 2], 1e-12);
asserts.isequal(size(years(d)), [1 2]);
%=============================================================================
