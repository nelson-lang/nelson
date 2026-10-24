%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
% Regression: arrayToMemoryView leaked the copied buffer + shape/strides on every
% conversion (the memoryview was built with obj == NULL). They are now owned by a
% PyCapsule set as the buffer's obj and freed by its destructor when the
% memoryview is released. Creating, using and destroying many memoryviews must
% round-trip correctly and must not crash (a wrong ownership would be a
% use-after-free when the memoryview is used or freed).
%=============================================================================
REF = uint8([1 2; 3 4]);
for k = 1:200
  R = pyrun('', 'A', 'A', REF);
  asserts.isequal(class(R), 'py.memoryview');
  asserts.isequal(R.uint8(), REF);
  clear R
end
%=============================================================================
