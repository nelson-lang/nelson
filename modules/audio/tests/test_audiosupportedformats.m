%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('audiosupportedformats'), 0);
asserts.isequal(nargout('audiosupportedformats'), 1);
%=============================================================================
formats = audiosupportedformats();
asserts.isequal(class(formats), 'struct');
asserts.isequal(fieldnames(formats), {'Name'; 'Extension'; 'Subformats'});
%=============================================================================
for k = [1:length(formats)]
  asserts.istrue(ischar(formats(k).Name));
  asserts.istrue(ischar(formats(k).Extension));
  asserts.istrue(iscellstr(formats(k).Subformats) || ischar(formats(k).Subformats));
end
%=============================================================================
