%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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
  asserts.isequal(p, 'c:/');
  asserts.isequal(f, '');
  asserts.isequal(e, '');
else
  asserts.isequal('c:', p);
  asserts.isequal('', f);
  asserts.isequal('', e);
end
%=============================================================================