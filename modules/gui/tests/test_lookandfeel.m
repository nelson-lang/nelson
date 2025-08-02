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
assert_isequal(nargin('lookandfeel'), 2);
assert_isequal(nargout('lookandfeel'), 1);
%=============================================================================
currentlf = lookandfeel();
assert_istrue(ischar(currentlf));
lfs = lookandfeel('available');
assert_istrue(iscell(lfs));
for lf = lfs'
  c = lookandfeel(lf{1});
end
if ~isempty(currentlf)
  r = lookandfeel(currentlf);
  assert_istrue(ischar(r));
end
%=============================================================================
currentstylesheet = lookandfeel('stylesheet');
assert_istrue(ischar(currentstylesheet));
stylefilename = [modulepath('gui'), '/resources/darkstyle.qss'];
newstyle = fileread(stylefilename);
previousstylesheet = lookandfeel('stylesheet', newstyle);
assert_isequal(currentstylesheet, previousstylesheet);
r = lookandfeel('stylesheet', previousstylesheet);
%=============================================================================