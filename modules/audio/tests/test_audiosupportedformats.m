%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('audiosupportedformats'), 0);
assert_isequal(nargout('audiosupportedformats'), 1);
%=============================================================================
formats = audiosupportedformats();
assert_isequal(class(formats), 'struct');
assert_isequal(fieldnames(formats), {'Name'; 'Extension'; 'Subformats'});
%=============================================================================
for k = [1: length(formats)]
  assert_istrue(ischar(formats(k).Name));
  assert_istrue(ischar(formats(k).Extension));
  assert_istrue(iscellstr(formats(k).Subformats) || ischar(formats(k).Subformats));
end
%=============================================================================
