%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function assert_sparse_isapprox(R, REF, tol)
  asserts.isequal(issparse(R), true);
  asserts.isequal(issparse(REF), true);
  [IR, JR, VR, MR, NR] = IJV(R);
  [IREF, JREF, VREF, MREF, NREF] = IJV(REF);
  asserts.isequal([MR, NR], [MREF, NREF]);
  asserts.isequal(IR, IREF);
  asserts.isequal(JR, JREF);
  asserts.isapprox(VR, VREF, tol);
end
%=============================================================================
