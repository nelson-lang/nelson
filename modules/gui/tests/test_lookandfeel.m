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
asserts.isequal(nargin('lookandfeel'), 2);
asserts.isequal(nargout('lookandfeel'), 1);
%=============================================================================
currentlf = lookandfeel();
asserts.istrue(ischar(currentlf));
lfs = lookandfeel('available');
asserts.istrue(iscell(lfs));
for lf = lfs'
  c = lookandfeel(lf{1});
end
if ~isempty(currentlf)
  r = lookandfeel(currentlf);
  asserts.istrue(ischar(r));
end
%=============================================================================
currentstylesheet = lookandfeel('stylesheet');
asserts.istrue(ischar(currentstylesheet));
stylefilename = [modulepath('gui'), '/resources/darkstyle.qss'];
newstyle = fileread(stylefilename);
previousstylesheet = lookandfeel('stylesheet', newstyle);
asserts.isequal(currentstylesheet, previousstylesheet);
r = lookandfeel('stylesheet', previousstylesheet);
%=============================================================================