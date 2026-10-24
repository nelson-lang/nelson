%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.istrue(1 & 1);
asserts.isfalse(1 & 0);
asserts.isfalse(0 & 1);
asserts.isfalse(0 & 0);
%=============================================================================
asserts.istrue(1 | 0);
asserts.isfalse(0 | 0);
asserts.isfalse(NaN & 0);
asserts.istrue(NaN & 1);
asserts.istrue(NaN | 0);
asserts.isfalse(~NaN);
%=============================================================================
values = [3, -2, NaN, Inf];
for k = 1:numel(values)
  x = values(k);
  asserts.isequal(+x, x);
  asserts.isequal(-x, uminus(x));
  asserts.isequal(x.', x);
  asserts.isequal(x', ctranspose(x));
end
%=============================================================================
asserts.isequal(3 .\ 2, 2 / 3);
asserts.isequal(3 \ 2, 2 / 3);
asserts.isequal(0 .\ 1, Inf);
asserts.isequal(0 \ 1, Inf);
asserts.isequal(NaN .\ 1, NaN);
asserts.isequal(NaN \ 1, NaN);
%=============================================================================
asserts.isfalse(NaN && false);
asserts.isfalse(false && NaN);
asserts.istrue(NaN || false);
asserts.istrue(false || NaN);
%=============================================================================
