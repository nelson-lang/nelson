%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
p = str2func('pause');
b = backgroundPool();
fevalqueue = b.FevalQueue;
cleanupQueue = onCleanup(@() cancelAll(fevalqueue)); %#ok<NLS0004>
f(1) = parfeval(b, p, 0, 1);
f(2) = parfeval(b, p, 0, 1);
asserts.isequal(size(f), [1 2]);
%=============================================================================
clear f;
for k = 1:6
  f(k) = parfeval(b, p, 0, 1);
end
asserts.isequal(size(f), [1 6]);
asserts.istrue(all(isvalid(f)));
delete(f);
%=============================================================================
