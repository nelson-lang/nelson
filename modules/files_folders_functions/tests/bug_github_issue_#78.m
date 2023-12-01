%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/78
% <-- Short Description -->
% [p,f,e]=fileparts('c:/') did not return the good result
%=============================================================================
[p, f, e] = fileparts('c:/');
if ispc()
 assert_isequal(p, 'c:/');
 assert_isequal(f, '');
 assert_isequal(e, '');
else
  assert_isequal('c:', p);
  assert_isequal('', f);
  assert_isequal('', e);
end
%=============================================================================